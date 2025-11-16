#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "input_scan.asm"

/*
================================================================================
 UI messages
================================================================================

Summary
	Top-bar talk UI for C64. Decodes a source message, stages it in a buffer,
	paints per-speaker color, and time-slices printing via a small state machine.

Public routines
	* prepare_message_for_topbar      ; select speaker, stage text, set color, arm print
	* tick_topbar_message             ; timed renderer, handles line breaks and EoS
	* shutdown_topbar_talking         ; stop printing, clear bar, close mouth if needed
	* clear_topbar_text               ; fill top-bar text cells with CLEAR_CHAR

Description

* Decode and stage:
	* Copy from (src_msg_ptr) into source_msg_base.
	* If byte has bit7 set, clear bit7 and auto-append SPACE_CHAR.
	* Stop on TERMINATOR_CHAR and advance src_msg_ptr past the NUL.
* Color:
	* If copying, choose actor_text_color[owner] else DEFAULT_TEXT_COLOR.
	* Map I/O on, fill vic_color_ram row, map I/O off.
* Render (tick):
	* Countdown gates printing. On zero, clear line and emit up to TOP_BAR_WIDTH
	chars from source_msg_base starting at source_msg_offset.
	* '/' triggers a soft line break. NUL sets DONE.
* Shutdown:
	* Clear text, close mouth unless NO_MOUTH, reset state to IDLE.

State machine
	* IDLE ($00): no work pending.
	* PRINT ($FF): next zero countdown prints a run; persists across line breaks.
	* DONE ($01): end-of-string reached; next tick shuts down to IDLE.

Timing model
	* INITIAL_MSG_COUNTDOWN: one-shot delay before first print batch.
	* std_msg_countdown: reload before each batch.
	* text_delay_factor: added per printed character into cur_msg_countdown (16-bit).
================================================================================
*/

// ------------------------------------------------------------
// Actor text colors for top-bar messages
//
// Indexed by costume ID. Each byte selects a VIC color value
// used when that costume speaks in the top bar.
//
// Values:
//   $01 = white, $07 = yellow, $02 = red, $0E = light blue,
//   $08 = orange, $0F = light gray, $03 = cyan, $0D = light green,
//   $04 = purple, $05 = green.
// ------------------------------------------------------------
* = $D672
actor_text_color:
.byte $01,$07,$02,$0E,$08,$0F,$03,$07,$07,$0F
.byte $01,$0D,$01,$04,$05,$05,$04,$03,$01,$05
.byte $01,$01,$01,$01

.const NO_SPEAKER              = $FF    // talking_costume sentinel: no mapped speaker
.const APPEND_MODE_COPY        = $00    // msg_copy_mode ≥ 0 → copy bytes into buffer
.const APPEND_MODE_SKIP        = $FF    // msg_copy_mode < 0  → scan only, no writes

.const TERMINATOR_CHAR         = $00    // NUL terminator in source strings
.const CLEAR_CHAR              = $00    // NUL used for buffer clears (alias of TERMINATOR_CHAR)
.const LINE_BREAK_CHAR         = $2F    // '/' used as explicit line break marker
.const CLEAR_HIGHBIT_MASK      = $7F    // mask to strip bit7 from encoded chars

.const DEFAULT_TEXT_COLOR      = $0E    // fallback top-bar color when no owner (light blue)
.const INITIAL_MSG_COUNTDOWN   = $0001  // 16-bit initial print countdown (ticks)

.const TOP_BAR_MODE_IDLE       = $00    // idle: nothing scheduled
.const TOP_BAR_MODE_PRINT      = $FF    // armed: print on next tick (negative)
.const TOP_BAR_MODE_DONE       = $01    // reached end-of-string

.const TOP_BAR_WIDTH           = $28    // top-bar width in cells (40)
.const TOP_BAR_LAST_IDX        = TOP_BAR_WIDTH - 1 // last valid cell index (0..$27)

.const TEXT_BAR_LENGTH         = $27    // legacy alias for last index; equals $27 (40 cells)

.label topbar_text_base     = $CC00    // Base address of top-bar character buffer in screen RAM
// Low nibble selects one of 16 colors (bits3–0).
// In multicolor text mode, only bits2–0 are used (8 colors).

.label src_msg_ptr          = $15    // Pointer to current source message string (lo/hi)
.label msg_copy_mode        = $28C1    // Copy/skip mode control flag

//  $00 = shutdown in progress
//  $01 = message ended
//  $FF = copying or post-linebreak active
.label topbar_mode          = $C9    // Current top-bar display mode

//  $00 = idle, $FF = print next tick, $01 = end-of-string
.label source_msg_offset    = $CA    // Current character index in message buffer
.label cur_msg_countdown    = $CB    // 16-bit countdown until next character print
.label std_msg_countdown    = $CD    // Standard countdown template (copied into cur_msg_countdown)
.label text_delay_factor    = $CF    // Per-character delay multiplier (speed control)


/*
================================================================================
 prepare_message_for_topbar
================================================================================
Summary

	Prepares the top-bar message system for the next line: selects speaker/copy mode,
	optionally initializes the speaker’s mouth state, copies the source string into
	the working buffer with “high-bit adds a space” decoding, advances the source
	pointer, colors the top-bar row, and arms the top-bar print countdown/mode.

Global Inputs
	talking_costume            Current speaking costume index or $FF
	actor_for_costume          Table: costume → actor index (bit7=1 if none)
	costume_anim_attrs         Per-costume animation flags; bit6 = NO_MOUTH
	src_msg_ptr                Pointer to NUL-terminated source string
	actor_text_color           Per-costume top-bar text color

Global Outputs
	msg_owner_costume          Latched owner costume or $FF if none
	actor_mouth_state          Owner actor’s mouth set to SPEAK_CLOSED
	msg_copy_mode              APPEND_MODE_COPY or APPEND_MODE_SKIP
	source_msg_base            Destination message buffer (filled if copy)
	src_msg_ptr                Advanced past copied NUL terminator
	vic_color_ram              Top-bar row painted with selected color
	source_msg_offset          Reset to 0
	topbar_mode                Set to TOP_BAR_MODE_PRINT (negative)
	var_msg_flag               Debug flag set
	var_message_length         Reset to 0
	cur_msg_countdown          Set to INITIAL_MSG_COUNTDOWN

Description
	* Determine speaker and copy mode:
		* If talking_costume maps to a valid actor, set owner and prep mouth unless
		NO_MOUTH is set; else run “no speaker” path.
		* Select APPEND_MODE_COPY for normal copy or APPEND_MODE_SKIP if unmapped.
	* Copy/scan loop:
		* If copying, load each byte; if bit7 set, clear bit7, store char, then append
		a space; otherwise store char as-is. Stop on NUL.
		* If skipping, only scan for NUL without writing.
	* Advance src_msg_ptr by the number of bytes consumed including the NUL.
	* If copying, resolve text color:
		* Use actor_text_color[owner] when owner valid; else DEFAULT_TEXT_COLOR.
		* Map I/O on, fill vic_color_ram[0..TOP_BAR_LAST_IDX], then map I/O off.
	* Initialize top-bar state for the printer:
		* Reset source_msg_offset and var_message_length.
		* Set topbar_mode to TOP_BAR_MODE_PRINT and var_msg_flag.
		* Load cur_msg_countdown with INITIAL_MSG_COUNTDOWN.

Notes
	* “High-bit adds a space” decoding: any source byte with bit7=1 becomes its
	  low-7-bit character followed by an inserted SPACE_CHAR in the destination.
	* msg_copy_mode < 0 means “skip” path; ≥ 0 means “copy” path.
================================================================================
*/
* = $279B
prepare_message_for_topbar:
        // ------------------------------------------------------------
        // Select talking costume and determine copy mode
        // ------------------------------------------------------------
        ldx     talking_costume              // load current speaking costume index into X
        bmi     enable_copy_mode_no_speaker  // bit7=1 → no speaker (character_talking = #$FF)

        lda     actor_for_costume,x          // load actor index mapped to this costume
        bpl     init_speaker_and_mouth       // if valid (bit7=0) → proceed to initialize speaker

        ldx     #APPEND_MODE_SKIP            // unmapped costume → skip copying message text
        jmp     commit_copy_mode_flag        // jump to finalize msg_copy_mode setup

        // ------------------------------------------------------------
        // Initialize speaker state and mouth animation
		//
        // - Reset UI/timers and close any prior speaker.
        // - Latch current costume as message owner.
        // - If COSTUME_ATTR_NO_MOUTH_B6 is set, skip mouth prep.
        // - Otherwise set owner actor’s mouth to MOUTH_STATE_SPEAK_CLOSED.
        // ------------------------------------------------------------
init_speaker_and_mouth:
        jsr     shutdown_topbar_talking     // clear UI, zero timers, close prior speaker if needed
        ldx     talking_costume             // X := current speaking costume index
        stx     msg_owner_costume           // latch costume as message owner for color/mouth logic

        lda     costume_anim_attrs,x        // A := anim flags for this costume
        and     #COSTUME_ATTR_NO_MOUTH_B6   // isolate bit6 “do not auto-mouth”
        bne     finalize_copy_mode_after_speaker // if set → skip mouth animation prep

        ldy     actor_for_costume,x         // Y := mapped actor index for this costume
        lda     #MOUTH_STATE_SPEAK_CLOSED   // prepare “speaking, mouth closed” state
        sta     actor_mouth_state,y         // set actor mouth state to initial speaking frame

finalize_copy_mode_after_speaker:
        jmp     commit_copy_mode_flag        // no further actor prep; commit msg_copy_mode

        // ------------------------------------------------------------
        // Enable copy mode when no speaker is active
		//
        // - Reset UI and timing state for a clean start.
        // - Set mode to APPEND_MODE_COPY so the message text is processed normally.
        // ------------------------------------------------------------
enable_copy_mode_no_speaker:
        jsr     shutdown_topbar_talking      // clear UI/timers for a clean start
        ldx     #APPEND_MODE_COPY            // no speaker → enable copying this pass

commit_copy_mode_flag:
        stx     msg_copy_mode               // commit selected copy/skip mode flag

        // ------------------------------------------------------------
        // Initialize copy loop indices
        // ------------------------------------------------------------
        ldy     #$FF                        // source byte index starts at $FF (first INY → 0)
        ldx     #$FF                        // destination index starts at $FF (first INX → 0)

        // ------------------------------------------------------------
        // Copy loop step
        // - Advance src/dst indices.
        // - If copying (msg_copy_mode ≥ 0), handle tagged space expansion.
        // - If skipping (msg_copy_mode < 0), read-only to test for EoS.
        // ------------------------------------------------------------
copy_loop_next:
		iny
		inx
		lda		msg_copy_mode
		bpl		handle_highbit_space
		
        lda     (src_msg_ptr),y              
		jmp 	check_eos_then_loop
		
handle_highbit_space:
        lda     (src_msg_ptr),y              // load next message byte from source
        bpl     store_char_to_buf            // bit7 clear → regular character, skip special handling

        and     #MSK_LOW7BITS                // bit7 set → clear high bit to get original character
        sta     source_msg_base,x            // write decoded character into message buffer
        inx                                 // advance destination index for appended space
        lda     #SPACE_CHAR                  // prepare ASCII space to append after character

store_char_to_buf:
        sta     source_msg_base,x            // write A to destination buffer at index X

check_eos_then_loop:
        bne     copy_loop_next               // nonzero byte → continue copying next char

        // ------------------------------------------------------------
        // End of string: advance pointer
        // ------------------------------------------------------------
        iny                                 // Y := Y + 1 to include the terminator in advance
        tya                                 // A := Y (byte count including #$00)
        clc                                 // clear carry before 16-bit add
        adc     src_msg_ptr                // add to message pointer low byte
        sta     src_msg_ptr                // commit updated low byte
        bcc     maybe_apply_topbar_color    // no carry → high byte unchanged
        inc     src_msg_ptr + 1                // carry → increment high byte of pointer

        // ------------------------------------------------------------
        // Conditional color update
        // - Skip color setup if running in skip mode (msg_copy_mode < 0).
        // - Otherwise proceed to resolve and apply message text color.
        // ------------------------------------------------------------
maybe_apply_topbar_color:
        lda     msg_copy_mode               // load copy/skip mode
        bmi     prep_msg_exit              // negative → skip coloring when not copying

        // ------------------------------------------------------------
        // Resolve message text color
        // ------------------------------------------------------------
        ldx     msg_owner_costume           // X := latched speaker costume
        bpl     load_owner_text_color       // valid owner → use per-actor color

        lda     #DEFAULT_TEXT_COLOR         // no owner → use default UI color
        jmp     apply_topbar_color          // jump to push color to color RAM

load_owner_text_color:
        lda     actor_text_color,x          // A := top-bar text color for owner costume

        // ------------------------------------------------------------
        // Fill top-bar color row
		//
        // - Initialize X to TOP_BAR_LAST_IDX.
        // - Store A into vic_color_ram[X..0] descending.
        // - Covers all visible text cells.
        // ------------------------------------------------------------
apply_topbar_color:
        ldy     #MAP_IO_IN                  // Y := value to map I/O into memory space
        sty     cpu_port                    // $01 := MAP_IO_IN (enable VIC color RAM access)

        ldx     #TOP_BAR_LAST_IDX         // X := last color cell index (0..$27)
fill_topbar_color_loop:
        sta     vic_color_ram,x           // write color A into color RAM at cell X
        dex                                 // X := X - 1
        bpl     fill_topbar_color_loop    // keep filling while X ≥ 0

        ldy     #MAP_IO_OUT                 // Y := value to unmap I/O
        sty     cpu_port                    // $01 := MAP_IO_OUT (disable VIC color RAM access)

        // ------------------------------------------------------------
        // Initialize top-bar message state
        // ------------------------------------------------------------
        lda     #$00                        // A := 0
        sta     source_msg_offset           // reset source message offset

        ldx     #TOP_BAR_MODE_PRINT         // X := $FF
        stx     topbar_mode                 // schedule printing on next tick (negative mode)
        stx     var_msg_flag                // debug: flag new message

        lda     #$00                        // A := 0
        sta     var_message_length          // debug: reset printed-char counter

        lda     #<INITIAL_MSG_COUNTDOWN     // A := low byte of initial countdown
        sta     cur_msg_countdown          // set countdown low byte
        lda     #>INITIAL_MSG_COUNTDOWN     // A := high byte of initial countdown
        sta     cur_msg_countdown + 1          // set countdown high byte

prep_msg_exit:
        rts
		
/*
================================================================================
  shutdown_topbar_talking
================================================================================

Summary
	Stop any ongoing “talking” UI activity. Zero the countdown, set idle mode,
	close the owner’s mouth if eligible, clear owner, then clear top bar.

Global Inputs
	msg_owner_costume           current speaking costume or NO_SPEAKER
	actor_for_costume           costume → actor index; bit7=1 if unmapped/offscreen
	costume_anim_attrs          per-costume flags (includes NO_MOUTH_B6)

Global Outputs
	topbar_mode                 set to TOP_BAR_MODE_IDLE
	cur_msg_countdown (lo/hi)   set to #$0000
	actor_mouth_state           set to MOUTH_STATE_CLOSED if applicable
	msg_owner_costume           set to NO_SPEAKER

Description
	- Write TOP_BAR_MODE_IDLE and zero cur_msg_countdown (lo/hi).
	- If msg_owner_costume = NO_SPEAKER → jump to clear_topbar_text.
	- Map costume to actor; if unmapped/offscreen → drop owner.
	- If COSTUME_ATTR_NO_MOUTH_B6 is set → skip closing mouth.
	- Otherwise set actor_mouth_state to MOUTH_STATE_CLOSED.
	- Store NO_SPEAKER in msg_owner_costume.
	- Fall through to clear_topbar_text to wipe the bar.
================================================================================
*/
* = $2834
shutdown_topbar_talking:
        // ------------------------------------------------------------
        // Reset UI timing and top bar mode
        // ------------------------------------------------------------
        lda     #TOP_BAR_MODE_IDLE
        sta     topbar_mode
        sta     cur_msg_countdown
        sta     cur_msg_countdown + 1

        // ------------------------------------------------------------
        // If there is a message owner, possibly close mouth
        // ------------------------------------------------------------
        ldx     msg_owner_costume                // X := speaker costume or NO_SPEAKER
        bmi     clear_topbar_text                // NO_SPEAKER → clear bar and exit

        ldy     actor_for_costume,x              // Y := actor index; bit7 set = invalid/offscreen
        bmi     drop_message_owner               // offscreen → drop owner

        lda     costume_anim_attrs,x
        and     #COSTUME_ATTR_NO_MOUTH_B6        // mouth auto suppressed?
        bne     drop_message_owner               // yes → skip mouth write

        lda     #MOUTH_STATE_CLOSED
        sta     actor_mouth_state,y              // force mouth closed

drop_message_owner:
        // ------------------------------------------------------------
        // Drop ownership; caller will clear the bar text next
        // ------------------------------------------------------------
        lda     #NO_SPEAKER
        sta     msg_owner_costume

        // Fall through to clear_topbar_text
/*
================================================================================
  clear_topbar_text
================================================================================
Summary
	Clear the top-bar text buffer by writing CLEAR_CHAR from the last valid
	index down to index 0.
================================================================================
*/
* = $2857
clear_topbar_text:
        // ------------------------------------------------------------
        // Y := last index; A := CLEAR_CHAR; clear [Y..0]
        // ------------------------------------------------------------
        ldy     #TOP_BAR_LAST_IDX
        lda     #CLEAR_CHAR
clear_text_loop:
        sta     topbar_text_base,y
        dey
        bpl     clear_text_loop
        rts		
/*
================================================================================
 tick_topbar_message
================================================================================

Summary
	Drive the timed top-bar text renderer. On each call, decrement the
	16-bit countdown. When it reaches zero, either finalize talking or
	print a run of characters onto the top bar according to mode.

Global Inputs
	topbar_mode                 sign: <0 print, ≥0 done
	cur_msg_countdown (lo/hi)   countdown to next action
	std_msg_countdown (lo/hi)   reload value at start of a print run
	source_msg_base             expanded source message buffer
	source_msg_offset           continuation index within source buffer
	text_delay_factor           per-character countdown increment

Global Outputs
	topbar_text_base            on-screen text cells filled left→right
	topbar_mode                 set to PRINT for continuation, DONE at EoS
	cur_msg_countdown (lo/hi)   rebuilt at run start, extended per char
	source_msg_offset           updated at line break or bar full
	var_message_length          debug: characters written this run

Description
	- If countdown > 0: decrement and exit.
	- If countdown == 0:
		- If topbar_mode ≥ 0: call shutdown_topbar_talking and return.
		- If topbar_mode < 0: start/continue a print run:
			- Clear the visible bar.
			- Reload cur_msg_countdown from std_msg_countdown.
			- Copy bytes from source_msg_base starting at source_msg_offset:
				• On TERMINATOR_CHAR → set topbar_mode = DONE and return.
				• On LINE_BREAK_CHAR → save source_msg_offset, set PRINT, return.
				• Otherwise write char to topbar_text_base[X], increment
				var_message_length, and extend cur_msg_countdown by
				text_delay_factor (with carry into high byte).
			- Stop when X == TOP_BAR_WIDTH, then act as a line break
			(save offset, set PRINT).

================================================================================
*/

* = $2862
tick_topbar_message:
        // ------------------------------------------------------------
        // Exit if countdown timer is zero
        // ------------------------------------------------------------
        lda     cur_msg_countdown              // load low byte of countdown
        ora     cur_msg_countdown + 1          // OR with high byte → check if both zero
        beq     tick_topbar_msg_exit           // if 0 → no countdown active, exit

        // ------------------------------------------------------------
        // Decrement 16-bit countdown
        // ------------------------------------------------------------
        lda     cur_msg_countdown              // reload low byte
        bne     dec_delay_lo_byte          	   // if not zero → skip high-byte borrow
        dec     cur_msg_countdown + 1          // low byte was zero → borrow from high
dec_delay_lo_byte:
        dec     cur_msg_countdown              // decrement low byte of countdown

        // ------------------------------------------------------------
        // Check if countdown still active
		//
        // If any byte nonzero, the countdown is ongoing and routine exits.
        // ------------------------------------------------------------
        lda     cur_msg_countdown              // load low byte of countdown
        ora     cur_msg_countdown + 1          // combine with high byte to test both
        bne     tick_topbar_msg_exit           // nonzero → countdown active, skip printing

        // ------------------------------------------------------------
        // Countdown reached zero → decide action by mode sign
        // ------------------------------------------------------------
        lda     topbar_mode                  // load top-bar renderer mode
        bpl     shutdown_topbar_talking_2    // if mode ≥ 0 → message finished; reset talking

        // ------------------------------------------------------------
        // Print mode - Initialize new print batch
		//
        // Clear current top-bar text and reload standard countdown.
        // ------------------------------------------------------------
        jsr     clear_topbar_text           // clear visible bar before writing this batch

        lda     std_msg_countdown           // load standard countdown
        sta     cur_msg_countdown           // store as new countdown
        lda     std_msg_countdown + 1
        sta     cur_msg_countdown + 1              

        // ------------------------------------------------------------
        // Copy characters from source to topbar this run
        // ------------------------------------------------------------
        ldy     source_msg_offset           // load previous source offset (resume where last print stopped)
        ldx     #$00                        // reset X to 0 → start writing at leftmost top-bar column

read_next_char:
        lda     source_msg_base,y           // A := next source byte at Y
        iny                                 // Y := Y + 1 (advance source index)

        // ------------------------------------------------------------
        // Handle end-of-string condition
		//
        // If the byte is #$00, mark message as finished and exit.
        // ------------------------------------------------------------
        cmp     #TERMINATOR_CHAR            // check for terminator byte
        bne     check_line_break            // not zero → continue to line-break test

        lda     #TOP_BAR_MODE_DONE          // mark completion: message fully printed
        sta     topbar_mode                 // set mode flag for next frame’s reset
        rts                                 // done → return to main loop

        // ------------------------------------------------------------
        // Handle explicit line break
		//
        // On LINE_BREAK_CHAR, save continuation offset and schedule the
        // next print batch by setting a "print" mode.
        // ------------------------------------------------------------
check_line_break:
        cmp     #LINE_BREAK_CHAR            // test for explicit '/' line break
        bne     regular_character           // not a break → treat as regular char

        // Save continuation point and schedule another print batch
commit_line_break_and_pause:
        lda     #TOP_BAR_MODE_PRINT         // print again on next tick
        sta     topbar_mode                 // set renderer mode to “continue printing”
        sty     source_msg_offset           // remember next source byte index
        rts                                 // return to wait for next countdown

        // ------------------------------------------------------------
        // Handle regular printable character
		//
        // Writes it to the top-bar buffer, updates debug counter,
        // and extends the countdown based on text speed factor.
        // ------------------------------------------------------------
regular_character:
        sta     topbar_text_base,x          // write character A into bar at column X
        inc     var_message_length          // debug counter: track number of chars printed

        // Per-character: extend countdown by text_delay_factor
        lda     cur_msg_countdown           // load current countdown low byte
        clc                                 // clear carry before addition
        adc     text_delay_factor           // add per-character countdown increment
        sta     cur_msg_countdown           // update low byte of countdown
        bcc     end_of_bar_check            // if no carry → high byte unchanged
        inc     cur_msg_countdown + 1       // if carry → increment high byte

        // ------------------------------------------------------------
        // Check bar width and pause at end of line
		//
        // If X reaches TOP_BAR_WIDTH, treat as a line break and schedule
        // the next print batch.
        // ------------------------------------------------------------
end_of_bar_check:
        inx                                 // advance to next top-bar column
        cpx     #TOP_BAR_WIDTH              // at bar width limit?
        bne     read_next_char              // no → continue printing this run

        // Bar full → treat as a line break and pause printing
        jmp     commit_line_break_and_pause // save offset, schedule next batch
		jmp 	tick_topbar_msg_exit		// Unreachable code

        // ------------------------------------------------------------
        // Finalize and exit
		//
        // If mode is not "print", reset talking then return.
        // ------------------------------------------------------------
shutdown_topbar_talking_2:
        jsr     shutdown_topbar_talking     // close mouth, drop owner, reset UI

tick_topbar_msg_exit:
        rts                                 // end of tick; nothing else to do
/*
================================================================================
print_message_wait_for_button
================================================================================

Summary
    Print a zero-terminated message on the top bar, then block until the
    joystick fire button is pressed. Handles VIC banking and optional
    screen unblank/blank around the prompt.

Arguments
    print_msg_ptr    pointer to zero-terminated message string

Global Inputs
    raster_irq_init_pending_flag      nonzero means raster init path is active

Description
    - Map in I/O to access screen and color RAM.
    - If raster init is pending:
        • Unblank screen.
        • Set memory layout to CC00 screen and F800 charset.
        • Set normal text mode preset.
    - Clear the top bar, copy the message bytes until $00.
    - Poll joystick until fire is pressed.
    - Clear the top bar again.
    - If raster init was pending, blank the screen.
    - Map out I/O and return.
================================================================================
*/
* = $3B15
print_message_wait_for_button:
        // ------------------------------------------------------------
        // Map in I/O to access screen/color/VIC registers
        // ------------------------------------------------------------
        ldy     #MAP_IO_IN                   
        sty     cpu_port                     

        // ------------------------------------------------------------
        // If raster IRQ init pending, ensure screen is on and set
        // VIC memory layout and text mode
        // ------------------------------------------------------------
        lda     raster_irq_init_pending_flag      // pending raster init?
        beq     entry_clear_topbar           // no → keep current video config

        lda     vic_screen_control_reg_1     
        ora     #VIC_CTRL1_UNBLANK_MASK      // set bit4 → unblank screen
        sta     vic_screen_control_reg_1     

        lda     #VIC_LAYOUT_CC00_F800        // CC00 screen, F800 charset
        ldy     #CTRL2_INT_REGION_PRESET     // standard text-mode preset
        sta     vic_memory_layout_reg        // commit memory layout
        sty     vic_screen_control_reg_2     // commit text mode

entry_clear_topbar:
        // ------------------------------------------------------------
        // Clear the top bar text area
        // ------------------------------------------------------------
        jsr     clear_topbar_text            // fill topbar row with spaces

        // ------------------------------------------------------------
        // Copy message string to the top bar
        // ------------------------------------------------------------
        ldy     #$00                         // Y := src index into message
copy_msg_next_char:
        lda     (print_msg_ptr),y            // A := message[Y]
        beq     wait_for_fire_press          // $00 terminator → stop copying
        sta     topbar_text_base,y           // write char to screen at top bar
        iny                                  // Y := Y+1
        bne     copy_msg_next_char           // continue until terminator

wait_for_fire_press:
        // ------------------------------------------------------------
        // Poll joystick and wait until fire button is pressed
        // ------------------------------------------------------------
        jsr     joy_latch                    // sample joystick state into A
        and     #JOY1_FIRE_MASK              // isolate fire bit
        bne     wait_for_fire_press          // loop until fire is pressed

        // ------------------------------------------------------------
        // Clear the top bar message
        // ------------------------------------------------------------
        jsr     clear_topbar_text            // erase the message from the bar

        // ------------------------------------------------------------
        // If raster init pending, re-blank the screen
        // ------------------------------------------------------------
        lda     raster_irq_init_pending_flag      // was special raster setup active?
        beq     exit_pmwfb                   // no → keep screen state as-is

        lda     vic_screen_control_reg_1     
        and     #VIC_CTRL1_BLANK_MASK        // clear bit4 → blank screen
        sta     vic_screen_control_reg_1     

exit_pmwfb:
        // ------------------------------------------------------------
        // Map out I/O and return
        // ------------------------------------------------------------
        ldy     #MAP_IO_OUT                  
        sty     cpu_port                     
        rts                                  
