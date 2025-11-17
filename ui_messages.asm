/*
================================================================================
 UI messages
================================================================================
Summary
	This module implements the C64 “talk bar” UI: a single top-row text area that
	shows dialogue lines with per-speaker color, timed line reveals, and basic
	mouth animation control.

	prepare_message_for_topbar is the entry point for staging a new line. 
	
	It chooses the speaking costume, decides whether to actually copy text or just 
	scan it, decodes the source string into a working buffer (including the “tagged 
	char adds a space” rule), advances the script pointer past the terminator, and
	applies the correct top-bar color (either per-costume or a default). It also
	arms the timed printer by resetting the internal offset, flagging print mode,
	and initializing the countdown and debug counters.

	tick_topbar_message is the per-frame driver. It decrements a 16-bit countdown
	and, when it reaches zero in “print” mode, clears the visible bar, reloads a
	baseline countdown, then emits a batch of characters from the staged buffer
	starting at source_msg_offset. The routine understands three control symbols:
	the terminator (ends the message and switches to DONE), an explicit line-break
	marker (commits a continuation offset and pauses until the next batch), and the
	implicit break when the bar width is reached (treated like a soft line break).
	Each printed character extends the countdown by a configurable per-character
	delay, so longer runs naturally space themselves out.

	shutdown_topbar_talking acts as the global “stop talking” hook. It resets the
	mode and countdown, optionally closes the speaking actor’s mouth (if the
	costume is mapped to an actor and mouth movement is not suppressed), clears the
	owner costume, and then clears the top-bar text. This keeps the visual bar and
	mouth animation in sync whenever a line finishes or is aborted.

	clear_topbar_text is a small utility that wipes the entire top-bar character
	row using the configured clear character. It is used both by the timed path and
	by the immediate “prompt” path to guarantee a clean slate before and after
	messages.

	print_message_wait_for_button is a separate immediate-mode helper for modal
	prompts. It temporarily ensures I/O and VIC layout are appropriate, clears the
	bar, copies a simple zero-terminated string directly into the top-bar text
	cells, then polls the joystick fire button until the user acknowledges. After
	that it clears the bar again, restores blanking if the raster-init bootstrap
	was active, and releases I/O access.

	Overall, the file ties together four main concerns: decoding and staging of
	script text, colorizing the top bar per speaker, a tick-based state machine for
	smooth text display, and cleanup hooks that keep dialogue, UI, and
	actor mouth states consistent.

State machine
	* IDLE ($00): no work pending.
	* DONE ($01): end-of-string reached; next tick shuts down to IDLE.
	* PRINT ($FF): next zero countdown prints a run; persists across line breaks.

Timing model
	* INITIAL_MSG_COUNTDOWN: one-shot delay before first print batch.
	* std_msg_countdown: reload before each batch.
	* text_delay_factor: added per printed character into cur_msg_countdown (16-bit).
	
Public routines
	prepare_message_for_topbar
		Selects the speaking costume, stages the encoded source string into a working
		buffer, and chooses the top-bar text color. Arms the timed printer by
		initializing state, mouth animation (when applicable), and the initial
		countdown.

	shutdown_topbar_talking
		Resets the top-bar message state and countdown, closes the speaking actor’s
		mouth when appropriate, and clears ownership. Falls through to clear the
		visible top-bar text so talking UI and animation end in sync.

	tick_topbar_message
		Drives the timed top-bar renderer: decrements a countdown and, on expiry,
		either prints the next batch of characters or finalizes the message. Handles
		explicit line breaks, implicit breaks at bar width, and end-of-string
		termination.

	print_message_wait_for_button
		Displays a one-shot message on the top bar, optionally adjusting VIC layout,
		and then blocks until the joystick fire button is pressed. Clears the
		message afterward and restores screen blanking if a special raster-init path
		was active.

Private routines		
	clear_topbar_text
		Fills the entire top-bar character row with the clear character. Ensures no
		residual text remains before or after a talk sequence or modal prompt.
				
================================================================================
Techniques used in this file
================================================================================

1) Staged text buffer with tagged-space decoding
   - Source messages are copied from src_msg_ptr into source_msg_base with a
     small decoder: bytes with bit7 set are turned into (char & $7F) followed
     by an inserted SPACE_CHAR. This lets the script compress “char+space”
     pairs into a single encoded byte while keeping the renderer simple.

2) Mode + countdown–driven state machine
   - The top-bar printer is a tiny state machine keyed by topbar_mode and a
     16-bit cur_msg_countdown. tick_topbar_message decrements the countdown on
     each call; when it hits zero, sign(topbar_mode) decides whether to keep
     printing (PRINT) or shut down (IDLE/DONE).

3) Batched rendering with per-character delay
   - Instead of printing a whole line at once, the renderer emits a batch of
     characters whenever the countdown expires. Each printed character adds a
     configurable text_delay_factor into the countdown, so longer bursts
     automatically space themselves out and the perceived speed can be tuned.

4) Explicit and implicit line-break handling
   - The staged buffer supports both explicit line breaks (a dedicated
     LINE_BREAK_CHAR) and implicit ones when the bar width is reached. In both
     cases, the code records source_msg_offset as the next resume point and
     leaves topbar_mode in PRINT so subsequent ticks naturally continue from
     the right place.

5) Per-speaker color mapping via lookup tables
   - Costume-specific text colors are stored as a simple costume_text_color[]
     table. Once a speaker is resolved, prepare_message_for_topbar picks either
     that costume’s color or a DEFAULT_TEXT_COLOR and fills the top-bar color
     row in one tight loop.

6) Copy vs. skip mode for message staging
   - msg_copy_mode toggles between “COPY” (stage text into source_msg_base) and
     “SKIP” (only scan for the terminator). This lets scripts reuse the same
     source pointer advancement logic even when there is no mapped speaker or
     no real UI update is needed.

7) Mouth animation coupling via costume/actor mapping
   - Talking behavior is tied to actor mouths through indirection: a costume is
     mapped to an actor index, which in turn drives actor_mouth_state with
     simple state writes (SPEAK_CLOSED / MOUTH_CLOSED). Flags in
     costume_anim_attrs allow specific costumes to opt out (NO_MOUTH_B6),
     making the mouth logic table-driven rather than hard-coded per actor.

8) Immediate-mode vs. timed-mode messaging
   - The file cleanly separates timed dialogue (prepare_message_for_topbar +
     tick_topbar_message) from immediate prompts (print_message_wait_for_button).
     The former uses the state machine and staged buffer; the latter writes
     directly into the top-bar row and waits in a tight input loop, reusing
     clear_topbar_text in both paths.

9) Simple I/O banking for safe VIC access
   - All color and prompt operations wrap their screen writes in “map I/O in /
     map I/O out” pairs and, when needed, temporarily adjust VIC layout and
     blank/unblank bits. This keeps VIC configuration changes localized and
     makes the messaging routines safe to call from different engine states.

================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "input_scan.asm"


/*
 ------------------------------------------------------------
 Costume text colors for top-bar messages

 Indexed by costume ID. 
 Each byte selects a VIC color value used for the top bar text
 when that costume speaks.
 ------------------------------------------------------------
*/
* = $D672
costume_text_color:
.byte $01,$07,$02,$0E,$08,$0F,$03,$07,$07,$0F
.byte $01,$0D,$01,$04,$05,$05,$04,$03,$01,$05
.byte $01,$01,$01,$01

.const NO_SPEAKER              	= $FF    // talking_costume sentinel: no mapped speaker

.const APPEND_MODE_COPY        	= $00    // msg_copy_mode ≥ 0 → copy bytes into buffer
.const APPEND_MODE_SKIP        	= $FF    // msg_copy_mode < 0  → scan only, no writes

.const TERMINATOR_CHAR         	= $00    // NUL terminator in source strings
.const CLEAR_CHAR              	= $00    // NUL used for buffer clears (alias of TERMINATOR_CHAR)
.const LINE_BREAK_CHAR         	= $2F    // '/' used as explicit line break marker

.const DEFAULT_TEXT_COLOR      	= $0E    // fallback top-bar color when no owner (light blue)
.const INITIAL_MSG_COUNTDOWN   	= $0001  // 16-bit initial print countdown (ticks)

.const TOP_BAR_MODE_IDLE    	= $00    // idle: nothing scheduled
.const TOP_BAR_MODE_DONE    	= $01    // reached end-of-string
.const TOP_BAR_MODE_PRINT   	= $FF    // armed: print on next tick (negative)

.const TOP_BAR_WIDTH        	= $28    // top-bar width in cells (40)
.const TOP_BAR_LAST_IDX     	= TOP_BAR_WIDTH - 1 // last valid cell index (0..$27)

.label topbar_text_base     = $CC00  // Base address of top-bar character buffer in screen RAM
.label src_msg_ptr          = $15    // Pointer to current source message string (lo/hi)
.label msg_copy_mode        = $28C1  // Copy/skip mode control flag
.label topbar_mode          = $C9    // Current top-bar display mode
.label source_msg_offset	= $CA    // Current character index in message buffer
.label cur_msg_countdown_lo = $CB    // 16-bit countdown until next character print (lo)
.label cur_msg_countdown_hi = $CC    // 16-bit countdown until next character print (hi)
.label std_msg_countdown_lo = $CD    // Standard countdown template (copied into cur_msg_countdown) (lo)
.label std_msg_countdown_hi = $CE    // Standard countdown template (copied into cur_msg_countdown) (hi)
.label text_delay_factor    = $CF    // Per-character delay increment


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
	talking_costume            Current speaking costume index
	actor_for_costume          Table: costume → actor index (bit7=1 if none)
	costume_anim_attrs         Per-costume animation flags; bit6 = NO_MOUTH
	src_msg_ptr                Pointer to NUL-terminated source string
	costume_text_color           Per-costume top-bar text color

Global Outputs
	msg_owner_costume          Latched owner costume
	actor_mouth_state          Owner actor’s mouth set to SPEAK_CLOSED if applicable
	msg_copy_mode              APPEND_MODE_COPY or APPEND_MODE_SKIP
	source_msg_base            Destination message buffer (filled if copy)
	src_msg_ptr                Advanced past copied NUL terminator
	source_msg_offset          Reset to 0
	topbar_mode                Set to TOP_BAR_MODE_PRINT
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
		* Use costume_text_color[owner] when owner valid; else DEFAULT_TEXT_COLOR.
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
        // Resolve talking costume
		//
		// Skip message if there isn't a valid actor for the costume
        // ------------------------------------------------------------
		// Is there a costume talking?
        ldx     talking_costume              // load current speaking costume index into X
        bmi     enable_copy_mode_no_speaker  // bit7=1 → no speaker (character_talking = NO_SPEAKER)

		// Is there a valid actor for this costume?
        lda     actor_for_costume,x          // load actor index mapped to this costume
        bpl     init_speaker_and_mouth       // if valid (bit7=0) → proceed to initialize speaker

		// There isn't
        ldx     #APPEND_MODE_SKIP            // unmapped costume → skip copying message text
        jmp     commit_copy_mode_flag        // jump to finalize msg_copy_mode setup

        // ------------------------------------------------------------
        // Initialize speaker state and mouth animation
        // ------------------------------------------------------------
init_speaker_and_mouth:
        // Reset any prior top bar talking
        jsr     shutdown_topbar_talking     
		
        // Latch current costume as message owner
        ldx     talking_costume             
        stx     msg_owner_costume           

		// Costume moves mouth?
        lda     costume_anim_attrs,x        
        and     #COSTUME_ATTR_NO_MOUTH_B6   		// isolate bit6 “do not auto-mouth”
        bne     finalize_copy_mode_after_speaker 	// if set → skip mouth animation prep

		// It doesn't, keep actor's mouth closed
        ldy     actor_for_costume,x         
        lda     #MOUTH_STATE_SPEAK_CLOSED   
        sta     actor_mouth_state,y         

finalize_copy_mode_after_speaker:
        jmp     commit_copy_mode_flag        // no further actor prep; commit msg_copy_mode

        // ------------------------------------------------------------
        // Enable copy mode when no speaker is active
		//
        // - Reset UI and timing state for a clean start.
        // - Set mode to APPEND_MODE_COPY so the message text is processed normally.
        // ------------------------------------------------------------
enable_copy_mode_no_speaker:
        jsr     shutdown_topbar_talking      
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
		// Copy or skip? COPY -> in_copy_mode, SKIP -> continue
		lda		msg_copy_mode
		bpl		in_copy_mode
		
		// Consume chars but skip them
        lda     (src_msg_ptr),y              
		jmp 	check_eos_then_loop
		
in_copy_mode:
        lda     (src_msg_ptr),y              // load next message byte from source
		
		// Bit 7 of character set?
        bpl     store_char_to_buf            // bit7 clear → regular character, skip special handling

        // bit7 set → clear high bit to get original character, then append a space
		and     #MSK_LOW7BITS                
        sta     source_msg_base,x            // write decoded character into message buffer
        inx                                  // advance destination index for appended space
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
		
		// Add byte count (including terminator) to src_msg_ptr
        clc                                 
        adc     src_msg_ptr                 
        sta     src_msg_ptr                 
        bcc     maybe_apply_topbar_color    
        inc     src_msg_ptr + 1             

        // ------------------------------------------------------------
        // Conditional color update
        // - Skip color setup if running in skip mode (msg_copy_mode < 0).
        // - Otherwise proceed to resolve and apply message text color.
        // ------------------------------------------------------------
maybe_apply_topbar_color:
		// Copy or skip? COPY -> continue, SKIP -> exit
        lda     msg_copy_mode               
        bmi     prep_msg_exit               // negative → skip coloring when not copying

        // ------------------------------------------------------------
        // Resolve message text color
        // ------------------------------------------------------------
		// Is there an owner for the message?
        ldx     msg_owner_costume           
        bpl     load_owner_text_color       

        // no owner → use default UI color
		lda     #DEFAULT_TEXT_COLOR         
        jmp     apply_topbar_color          

		// There's an owner, resolve its text color
load_owner_text_color:
        lda     costume_text_color,x          

        // ------------------------------------------------------------
        // Fill top-bar color with the color in .A
        // ------------------------------------------------------------
apply_topbar_color:
		// Map I/O in
        ldy     #MAP_IO_IN                  
        sty     cpu_port                    

		// Write color A into color RAM
        ldx     #TOP_BAR_LAST_IDX           // X := last color cell index (0..$27)
fill_topbar_color_loop:
        sta     vic_color_ram,x           	
        dex                                 
        bpl     fill_topbar_color_loop    	

		// Map I/O out
        ldy     #MAP_IO_OUT                 
        sty     cpu_port                    

        // ------------------------------------------------------------
        // Initialize top-bar message state
        // ------------------------------------------------------------
        // reset source message offset
		lda     #$00                        
        sta     source_msg_offset           

		// schedule printing on next tick (negative mode)
        ldx     #TOP_BAR_MODE_PRINT         
        stx     topbar_mode                 
        stx     var_msg_flag                // debug: flag new message

        // debug: reset message printed length counter
		lda     #$00                        
        sta     var_message_length          

		// Set initial countdown
        lda     #<INITIAL_MSG_COUNTDOWN     
        sta     cur_msg_countdown_lo        
        lda     #>INITIAL_MSG_COUNTDOWN     
        sta     cur_msg_countdown_hi        

prep_msg_exit:
        rts
		
/*
================================================================================
  shutdown_topbar_talking
================================================================================
Summary
	Stop any ongoing “talking” UI activity. Zero the countdown, set idle mode,
	close the owner’s mouth if eligible, clear owner, then clear top bar.
	This is used to sync talking animation with dialogue messages being cleared.

Global Inputs
	msg_owner_costume           current speaking costume or NO_SPEAKER
	actor_for_costume           costume → actor index; bit7=1 if unmapped/offscreen
	costume_anim_attrs          per-costume flags (includes NO_MOUTH_B6)

Global Outputs
	topbar_mode                 set to TOP_BAR_MODE_IDLE
	cur_msg_countdown (lo/hi)   set to zero
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
        // Reset top bar countdown and mode
        // ------------------------------------------------------------
        lda     #TOP_BAR_MODE_IDLE
        sta     topbar_mode
        sta     cur_msg_countdown_lo		//#00
        sta     cur_msg_countdown_hi		//#00

        // ------------------------------------------------------------
        // If there is a message owner, possibly close mouth
        // ------------------------------------------------------------
        ldx     msg_owner_costume                // X := speaker costume or NO_SPEAKER
        bmi     clear_topbar_text                // NO_SPEAKER → tail call clear bar and exit

		// There is a costume speaking - resolve the actor speaking
        ldy     actor_for_costume,x              // Y := actor index; bit7 set = invalid/offscreen
        bmi     drop_message_owner               // offscreen → drop owner

		// Check if the costume has its mouth "auto suppressed"
        lda     costume_anim_attrs,x
        and     #COSTUME_ATTR_NO_MOUTH_B6        // mouth auto suppressed?
        bne     drop_message_owner               // yes → skip mouth write

		// It doesn't, so close its mouth
        lda     #MOUTH_STATE_CLOSED
        sta     actor_mouth_state,y              

drop_message_owner:
        // Drop message ownership
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
	source_msg_base             source message buffer
	source_msg_offset           continuation index within source buffer
	text_delay_factor           per-character countdown increment

Global Outputs
	topbar_mode                 set to PRINT for continuation, DONE at EoS
	cur_msg_countdown (lo/hi)   rebuilt at run start, extended per char
	source_msg_offset           updated at line break or bar full

Description
	- If countdown == 0: exit (no countdown active)
	- If countdown > 0: decrement once.
	- If countdown did not reach 0 now, exit.
	- If countdown reached 0 now:
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
        // Exit if no countdown is active (countdown is already zero)
        // ------------------------------------------------------------
        lda     cur_msg_countdown_lo        // load low byte of countdown
        ora     cur_msg_countdown_hi        // OR with high byte → check if both zero
        beq     tick_topbar_msg_exit        // if 0 → no countdown active, exit

        // ------------------------------------------------------------
        // Decrement 16-bit countdown
        // ------------------------------------------------------------
        lda     cur_msg_countdown_lo        
        bne     dec_countdown_lo_byte     	
        dec     cur_msg_countdown_hi        
dec_countdown_lo_byte:
        dec     cur_msg_countdown_lo        

        // ------------------------------------------------------------
        // Check if countdown still active
		//
        // If any countdown byte nonzero, the countdown is ongoing and routine exits.
        // ------------------------------------------------------------
        lda     cur_msg_countdown_lo        // load low byte of countdown
        ora     cur_msg_countdown_hi        // combine with high byte to test both
        bne     tick_topbar_msg_exit        // nonzero → countdown active, skip printing

        // ------------------------------------------------------------
        // Countdown reached zero → decide action by mode sign
        // ------------------------------------------------------------
        lda     topbar_mode                 // load top-bar renderer mode
        bpl     shutdown_topbar_trampoline  // if mode ≥ 0 → message finished; reset talking

        // ------------------------------------------------------------
        // Print mode - Initialize new print batch
		//
        // Clear current top-bar text and reload standard countdown.
        // ------------------------------------------------------------
        jsr     clear_topbar_text           // clear visible bar before writing this batch

		// Reset new countdown := standard countdown
        lda     std_msg_countdown_lo        
        sta     cur_msg_countdown_lo        
        lda     std_msg_countdown_hi
        sta     cur_msg_countdown_hi              

        // ------------------------------------------------------------
        // Copy characters from source to topbar this run
        // ------------------------------------------------------------
        ldy     source_msg_offset           // load previous source offset (resume where last print stopped)
        ldx     #$00                        // reset X to 0 → start writing at leftmost top-bar column

read_next_char:
        lda     source_msg_base,y           // A := next source byte at Y
        iny                                 

        // ------------------------------------------------------------
        // Handle end-of-string condition
		//
        // If the byte is TERMINATOR_CHAR, mark message as finished and exit.
        // ------------------------------------------------------------
        cmp     #TERMINATOR_CHAR            // check for terminator byte
        bne     check_line_break            // not zero → continue to line-break test

		// Terminator char found - mark completion
        lda     #TOP_BAR_MODE_DONE          // mark completion: message fully printed
        sta     topbar_mode                 // set mode flag for next frame’s reset
        rts                                 

        // ------------------------------------------------------------
        // Handle explicit line break
		//
        // On LINE_BREAK_CHAR, save continuation offset and schedule the
        // next print batch by setting a "print" mode.
        // ------------------------------------------------------------
check_line_break:
        cmp     #LINE_BREAK_CHAR            // test for explicit '/' line break
        bne     regular_character           // not a break → treat as regular char

        // Explicit line break - save continuation point and schedule another print batch
commit_line_break_and_pause:
        lda     #TOP_BAR_MODE_PRINT         
        sta     topbar_mode                 // set renderer mode to “continue printing” on next tick
        sty     source_msg_offset           // remember next source byte index
        rts                                 

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
        lda     cur_msg_countdown_lo        // load current countdown low byte
        clc                                 // clear carry before addition
        adc     text_delay_factor           // add per-character countdown increment
        sta     cur_msg_countdown_lo        // update low byte of countdown
        bcc     end_of_bar_check            // if no carry → high byte unchanged
        inc     cur_msg_countdown_hi        // if carry → increment high byte

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
        // Reset talking then return.
        // ------------------------------------------------------------
shutdown_topbar_trampoline:
        jsr     shutdown_topbar_talking		// close mouth, drop owner, reset UI

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
    print_msg_ptr    				pointer to zero-terminated message string

Global Inputs
    raster_irq_init_pending_flag    nonzero means raster init path is active

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
        lda     raster_irq_init_pending_flag // pending raster init?
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
        lda     (print_msg_ptr),y            
        beq     wait_for_fire_press          // $00 terminator → stop copying
        sta     topbar_text_base,y           
        iny                                  
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
        lda     raster_irq_init_pending_flag // was special raster setup active?
        beq     exit_pmwfb                   // no → keep screen state as-is

		// Raster init pending, re-blank the screen
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

/*
Pseudo-code (8-bit details abstracted)
// -----------------------------------------------------------------------------
// prepare_message_for_topbar 
// -----------------------------------------------------------------------------
function prepare_message_for_topbar():
    // 1) Decide who is speaking and whether we will copy text or just scan it
    if there_is_no_valid_talking_costume():
        // No speaker: reset state but still stage the text normally
        shutdown_topbar_talking()
        msg_copy_mode = COPY
    else if costume_has_no_mapped_actor(talking_costume):
        // Costume not mapped to an on-screen actor: skip staging text
        msg_copy_mode = SKIP
    else:
        // Valid speaker:
        // - reset previous talking state
        // - record which costume owns this line
        // - optionally set the actor’s mouth to a “speaking” pose
        shutdown_topbar_talking()
        msg_owner_costume = talking_costume

        if costume_is_allowed_to_move_mouth(msg_owner_costume):
            actor = actor_for_costume(msg_owner_costume)
            actor_mouth_state[actor] = SPEAK_CLOSED
        end if

        msg_copy_mode = COPY
    end if

    // 2) Decode and stage the source text into a working buffer
    //
    // Source:  src_msg_ptr → 0-terminated encoded string
    // Target:  source_msg_base (decoded, ready to print)
    //
    // Encoding rule: some characters are “tagged to add a space”:
    //   - A tagged character decodes to [character] followed by [space].
    //
    src_index = 0
    dst_index = 0

    while true:
        encoded_char = read_source_char(src_msg_ptr, src_index)
        src_index += 1

        if msg_copy_mode == COPY:
            if encoded_char_has_space_tag(encoded_char):
                base_char = remove_space_tag(encoded_char)
                source_msg_base[dst_index] = base_char
                dst_index += 1
                source_msg_base[dst_index] = SPACE_CHAR
                dst_index += 1
            else:
                source_msg_base[dst_index] = encoded_char
                dst_index += 1
            end if
        else:
            // SKIP mode: just advance over the encoded bytes,
            // only caring about where the terminator is.
        end if

        if encoded_char_is_terminator(encoded_char):
            break
        end if
    end while

    // Advance the source pointer by the number of bytes we just consumed
    src_msg_ptr += src_index

    // 3) Apply text color to the top bar if we actually copied text
    if msg_copy_mode == COPY:
        if msg_owner_costume != NO_SPEAKER:
            color = costume_text_color[msg_owner_costume]
        else:
            color = DEFAULT_TEXT_COLOR
        end if

        // Ensure video memory is accessible and fill the top-bar color row
        enter_video_access_mode()
        for col in 0 .. TOP_BAR_WIDTH-1:
            set_topbar_color(col, color)
        end for
        exit_video_access_mode()
    end if

    // 4) Initialize message state for the tick-based printer
    source_msg_offset   = 0              // resume point within staged buffer
    topbar_mode         = PRINT_MODE     // negative/“active” state
    var_msg_flag        = PRINT_MODE     // debug mirror
    var_message_length  = 0              // debug character counter
    cur_msg_countdown   = INITIAL_MSG_COUNTDOWN
end function


// -----------------------------------------------------------------------------
// shutdown_topbar_talking
// -----------------------------------------------------------------------------
function shutdown_topbar_talking():
    // Reset mode and countdown so nothing more will print automatically
    topbar_mode        = IDLE_MODE
    cur_msg_countdown  = 0

    // If there is an owning costume, try to close its mouth appropriately
    if msg_owner_costume != NO_SPEAKER:
        if costume_has_mapped_actor(msg_owner_costume):
            if costume_is_allowed_to_move_mouth(msg_owner_costume):
                actor = actor_for_costume(msg_owner_costume)
                actor_mouth_state[actor] = MOUTH_CLOSED
            end if
        end if
    end if

    // Forget the owner and clear the visible text
    msg_owner_costume = NO_SPEAKER
    clear_topbar_text()
end function


// -----------------------------------------------------------------------------
// clear_topbar_text 
// -----------------------------------------------------------------------------
function clear_topbar_text():
    // Replace every character cell in the top bar with the “clear” character
    for col in 0 .. TOP_BAR_WIDTH-1:
        topbar_text_base[col] = CLEAR_CHAR
    end for
end function


// -----------------------------------------------------------------------------
// tick_topbar_message 
// -----------------------------------------------------------------------------
function tick_topbar_message():
    // If there is no active countdown, do nothing
    if cur_msg_countdown == 0:
        return
    end if

    // Decrement the countdown by one tick
    cur_msg_countdown -= 1

    // If the countdown has not yet reached zero, nothing else to do this frame
    if cur_msg_countdown > 0:
        return
    end if

    // Countdown just hit zero → act based on the topbar_mode
    if mode_is_non_print(topbar_mode):
        // Either DONE or already IDLE → shut down and stop
        shutdown_topbar_talking()
        return
    end if

    // PRINT mode: emit a “batch” of characters for this frame
    clear_topbar_text()

    // Reload the baseline countdown for this batch; per-character delay
    // will be added as we print each character.
    cur_msg_countdown = std_msg_countdown

    // Use the stored offset to resume within the staged message buffer
    src_index = source_msg_offset
    col       = 0

    // Emit characters until we hit: terminator, explicit line break, or bar width
    while true:
        ch = source_msg_base[src_index]
        src_index += 1

        // End-of-string: mark DONE for the next tick and stop
        if ch == TERMINATOR_CHAR:
            topbar_mode = DONE_MODE
            return
        end if

        // Explicit line break: save continuation point and keep PRINT mode
        if ch == LINE_BREAK_CHAR:
            source_msg_offset = src_index
            topbar_mode       = PRINT_MODE
            return
        end if

        // Regular display character: write it and account for it
        topbar_text_base[col] = ch
        var_message_length   += 1

        // Add a per-character delay increment into the countdown,
        // so longer lines accumulate more time before the next batch.
        cur_msg_countdown += text_delay_factor

        col += 1

        // If we filled the bar, treat this as an implicit line break
        if col == TOP_BAR_WIDTH:
            source_msg_offset = src_index
            topbar_mode       = PRINT_MODE
            return
        end if

        // Otherwise loop to fetch the next character of this batch
    end while
end function


// -----------------------------------------------------------------------------
// print_message_wait_for_button 
// -----------------------------------------------------------------------------
function print_message_wait_for_button():
    // 1) Ensure we can access video registers and screen/color memory
    enter_video_access_mode()

    // 2) If a special raster-init path is active, temporarily unblank and
    //    configure the screen for a standard text layout
    if raster_irq_init_pending_flag != 0:
        unblank_screen()
        set_screen_layout_to_topbar_text_mode()
    end if

    // 3) Clear the top bar before showing the message
    clear_topbar_text()

    // 4) Copy the provided message into the top bar until a terminator is seen
    src_index = 0
    while true:
        ch = read_byte(print_msg_ptr + src_index)
        if ch == 0:
            break
        end if
        topbar_text_base[src_index] = ch
        src_index += 1
    end while

    // 5) Wait until the joystick fire button is pressed
    while not joystick_fire_pressed():
        // poll input until the user confirms
        continue
    end while

    // 6) Clear the message from the top bar
    clear_topbar_text()

    // 7) If we temporarily unblanked/configured the screen, restore blank state
    if raster_irq_init_pending_flag != 0:
        blank_screen()
    end if

    // 8) Release video access and return
    exit_video_access_mode()
end function
*/