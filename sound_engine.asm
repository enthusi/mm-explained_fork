/*
================================================================================
  Voice instruction bit layout
================================================================================

The instruction byte encodes multiple independent actions. A copy is shifted
right in instr_bit_cursor to expose bits 0–5 and 7 in carry, while the
unmodified value in instr_header_raw is used to test bit 6 via BIT.

* Bit 0 (gate):

	* Checked first (via LSR → C).
	* If voice_sid_range_flags has bit 6 set (V=1), carry controls gate:
		* C=1 → set bit 0 of voice_ctrl_shadow (trigger note).
		* C=0 → clear bit 0 of voice_ctrl_shadow (release note).
	* If V=0, gate handling is skipped (no SID-voice update for this slot).

* Bit 1 (stop voice):

	* Next LSR moves bit 1 into C.
	* If C=1 → sets stop_sound_cleanup_mode to #$FF and calls
	stop_logical_voice for this logical voice, then returns.
	* This is an immediate “stop this voice” instruction.

* Bit 2 (frequency/duration/glissando, together with bit 6):

	* Next LSR moves bit 2 into C; BIT on instr_header_raw sets V to bit 6.
	* If C=0 (bit 2 clear) → call clear_voice_duration_and_glissando and skip
	operand reads; frequency is unchanged.
	* If C=1 (bit 2 set) → always read 2 bytes and update frequency:
		* If V=1 (bit 6 set) → clear duration and glissando, do not read them.
		* If V=0 (bit 6 clear) → read 2 bytes for duration and 2 bytes for
		  glissando, updating both.

* Bit 3 (ADSR, gated by SID-range flag):

	* BIT voice_sid_range_flags sets V from its bit 6, then another LSR moves
	bit 3 into C.
	* If V=0 or C=0 → ADSR handling is skipped.
	* If V=1 and C=1:
		* Temporarily force gate off (clear bit 0 of voice_ctrl_shadow and call
		  update_voice_control).
		* Read 2 bytes to update ADSR (attack/decay and sustain/release).
		* Re-apply the original gate state from voice_instr_header bit 0
		  (re-trigger or re-release with the new ADSR).

* Bit 4 (extended operand sub-commands: PWM, filter, duration):

	* If bit 4 set, the next byte is loaded into instr_op_flags; its low
	three bits encode sub-actions:
	* Operand bit 0 (PWM; only when SID-range flag V=1):
		* If set → read 2 bytes and update this voice’s PWM via
		voice_pwm_reg_ofs_tbl.
	* Operand bit 1 (filter cutoff):
		* If set → call update_filter_and_volume, then read 2 bytes into
	filter cutoff registers.
	* Operand bit 2 (duration):
		* If set → clear glissando and read 2 bytes to set a new duration.

* Bit 5 (waveform):

	* If set → keep low nibble of voice_ctrl_shadow, but replace the high nibble
	using the next operand byte’s high bits (low four bits are effectively
	ignored). This updates waveform selection without disturbing gate/flags in
	the low nibble.

* Bit 6 (mode modifier for bit 2):

	* Not consumed via instr_bit_cursor for control flow; instead tested
	via BIT on instr_header_raw to decide whether the Bit-2 section reads
	duration/glissando (bit 6 clear) or just clears them (bit 6 set) after
	updating frequency.

* Bit 7 (repetition and backward offset):

	* After processing bits 0–5, instr_bit_cursor is shifted two more times;
	the second LSR’s carry contains bit 7.
	* If bit 7 clear → no repetition; the routine calls
	update_instruction_ptr_and_offset and returns.
	* If bit 7 set → repetition loop:
		* On each pass, read an 8-bit offset and subtract it from both
		  voice_instr_ptr and voice_instr_offset, moving the instruction
		  pointer backwards.
		* If voice_instr_repcount is zero, read a new count from the stream.
		  Otherwise, just decrement the existing count.
		* When the count reaches #$01, the last pass decrements it to #$00, skips
		  over the stored offset and count, advances pointers past this instruction
		  via update_instruction_ptr_and_offset, and then continues with the next
		  instruction.

In summary, the byte is a packed bitfield: bits 0–5 and 6 control local voice
actions (gate, frequency, timing, ADSR, PWM/filter/duration, waveform), while
bit 7 + trailing operands implement a small backward-loop mechanism with an
explicit repeat counter and offset.

================================================================================

Bit 0  – Gate on/off:
          If SID-range flag (V=1): C=1→trigger, C=0→release. Else ignored.

Bit 1  – Stop voice:
          Immediately stops the logical voice and returns.

Bit 2  – Frequency/duration/glissando (uses bit 6 as modifier):
          0 → clear duration & glissando.
          1 → read 2 bytes: set frequency.
                If bit 6=1 → clear duration & glissando.
                If bit 6=0 → read 4 more bytes: set duration & glissando.

Bit 3  – ADSR (only when SID-range flag V=1):
          Read 2 bytes (ADSR).
          Gate is forced off, ADSR updated, then original gate action reapplied.

Bit 4  – Operand sub-actions (next byte = operand bits):
          Operand bit 0 (and V=1): set PWM (read 2 bytes).
          Operand bit 1: set filter cutoff (read 2 bytes).
          Operand bit 2: set duration (clear glissando, read 2 bytes).

Bit 5  – Waveform:
          Next byte’s high nibble replaces waveform bits (low nibble preserved).

Bit 6  – Modifier for Bit 2:
          0 → enable duration+glissando reads.
          1 → suppress duration/glissando (clear them).

Bit 7  – Repetition:
          Read offset; subtract from instruction pointer each pass.
          First pass loads repeat count; later passes decrement it.
          Last pass clears counter, skips operands, and advances past instruction.

================================================================================

Voice instruction encoding: byte + operand layouts

Byte 0: instruction header
--------------------------------
  b7  b6  b5  b4  b3  b2  b1  b0
  |   |   |   |   |   |   |   +-- Gate: 1=trigger, 0=release (SID-range only)
  |   |   |   |   |   |   +------ Stop voice: 1=stop+RTS
  |   |   |   |   |   +---------- F/D/G block (see b6)
  |   |   |   |   +-------------- ADSR block (SID-range only)
  |   |   |   +------------------ Operand sub-block (PWM/filter/duration)
  |   |   +---------------------- Waveform update
  |   +-------------------------- Modifier for F/D/G (b2)
  +------------------------------ Repetition (offset + count present)

Special case
------------
  INS = $00:
    - Layout: [00]
    - No operands. Clears repeat counter and cuts voice.

General operand stream layout (non-zero instruction)
----------------------------------------------------
After the instruction byte come zero or more operand blocks in this fixed order:

  [F-block] [A-block] [O-block] [W-block] [R-block]

Each block’s presence and size:

1) F-block (Frequency / Duration / Glissando) - controlled by b2,b6
   - Present only if b2=1.
   - Layout:

     If b2=0:
       F-block absent, 0 bytes (duration+gliss cleared if b2=0).

     If b2=1 and b6=1:
       F-block = [freq_lo] [freq_hi]                 (2 bytes)

     If b2=1 and b6=0:
       F-block = [freq_lo] [freq_hi]                 (2 bytes)
                 [dur_lo]  [dur_hi]                  (2 bytes)
                 [glis_lo] [glis_hi]                 (2 bytes)
                 → 6 bytes total

2) A-block (ADSR envelope) - controlled by b3 and SID-range flag
   - Present only if b3=1 AND this voice is in SID range (V=1 from
     voice_sid_range_flags).
   - Layout:

     A-block = [adsr_attack_decay] [adsr_sustain_release]   (2 bytes)

3) O-block (Operand sub-actions: PWM / filter / duration) - controlled by b4
   - Present only if b4=1.
   - First, a 1-byte operand bitfield:

     O-block header:
       [op_bits]
         bit 0 → PWM update (SID-range only)
         bit 1 → Filter cutoff update
         bit 2 → Duration update (clear gliss, set duration)

   - Then, depending on op_bits:

     If bit 0=1 and SID-range V=1:
       + [pwm_lo] [pwm_hi]            (2 bytes)
     If bit 1=1:
       + [filter_lo] [filter_hi]      (2 bytes)
     If bit 2=1:
       + [dur2_lo] [dur2_hi]          (2 bytes, clears gliss)

   - So O-block layouts:

     b4=0:
       O-block absent, 0 bytes.

     b4=1:
       Minimum (op_bits=00000000):
         [op_bits]                                (1 byte)
       Maximum (op_bits bits 0,1,2 all set, SID-range):
         [op_bits]
         [pwm_lo] [pwm_hi]
         [filter_lo] [filter_hi]
         [dur2_lo] [dur2_hi]                      (1+6 = 7 bytes)

4) W-block (Waveform) - controlled by b5
   - Present only if b5=1.
   - Layout:

     W-block = [waveform_mask]                    (1 byte)
       (high nibble replaces waveform bits, low nibble of control preserved)

5) R-block (Repetition) - controlled by b7
   - Present only if b7=1.
   - Layout (always 2 bytes at the very end):

     R-block = [offset] [repeat_count]            (2 bytes)
       offset       → 8-bit backwards delta applied to instruction pointer
       repeat_count → initial or updated repeat counter

   - On first use for this instruction, repeat_count is loaded from the
     stream; subsequent passes just decrement it. When it reaches #$01, the
     final pass decrements to #$00 and then skips these 2 bytes.

Example layouts
---------------
Let INS be a non-zero instruction byte:

  Case A: Only repetition (b7=1, all others 0)
    Layout: [INS]
            [offset] [repeat_count]

  Case B: F only (b2=1,b6=1; all others 0)
    Layout: [INS]
            [freq_lo] [freq_hi]

  Case C: F + full D/G (b2=1,b6=0; all others 0)
    Layout: [INS]
            [freq_lo] [freq_hi]
            [dur_lo]  [dur_hi]
            [glis_lo] [glis_hi]

  Case D: F + ADSR + waveform + repetition (SID-range, b2=1,b6=0,b3=1,b5=1,b7=1)
    Layout: [INS]
            [freq_lo] [freq_hi]
            [dur_lo]  [dur_hi]
            [glis_lo] [glis_hi]
            [adsr_AD] [adsr_SR]
            [waveform_mask]
            [offset]  [repeat_count]

  Case E: F + O-block (all sub-bits set) + waveform + repetition
          (SID-range, b2=1,b6=0,b4=1, op_bits bits 0..2=1, b5=1,b7=1)
    Layout: [INS]
            [freq_lo]   [freq_hi]
            [dur_lo]    [dur_hi]
            [glis_lo]   [glis_hi]
            [op_bits]
            [pwm_lo]    [pwm_hi]
            [filter_lo] [filter_hi]
            [dur2_lo]   [dur2_hi]
            [waveform_mask]
            [offset]    [repeat_count]

Maximum-length instruction (SID-range)
--------------------------------------
  b2=1, b6=0, b3=1, b4=1 (op_bits bits 0,1,2=1), b5=1, b7=1:

  Operand bytes:
    F-block : 6
    A-block : 2
    O-block : 1 + 6
    W-block : 1
    R-block : 2
    → Total = 6 + 2 + 7 + 1 + 2 = 18 operand bytes

  Full instruction layout:
    [INS] + 18 operand bytes  → 19 bytes total in the stream.

================================================================================
*/

#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sid_voice_controller.asm"
#import "voice_primitives.asm"

* = $4755
voice_freq_reg_ofs_tbl:
        // Per-voice base offsets into the SID frequency register pairs.
        // Indexed by logical voice index; used to locate the correct
        // SID FREQ LO/HI register pair for that voice.
        .byte $00, $07, $0E, $15

* = $4759
voice_pwm_reg_ofs_tbl:
        // Per-voice base offsets into the SID PWM (pulse width) register
        // pairs. Indexed by logical voice index when programming PWM.
        .byte $02, $09, $10

* = $475C
voice_alloc_set_mask_tbl:
        // Bitmasks for marking voices as allocated. Entry N corresponds
        // to (1 << N); AND/OR with voices_allocated to set the slot’s bit.
        .byte $01, $02, $04, $08, $10, $20, $40

* = $4763
voice_alloc_clear_mask_tbl:
        // Bitmasks for clearing voice allocation bits. Entry N is the
        // complement of (1 << N), so ANDing with it frees that voice.
        // The final $00 entry is used as a sentinel / “no mask” value.
        .byte $FE, $FD, $FB, $F7, $EF, $DF, $BF, $00

.label voice_instr_saved          = $4813    // Saved Y during ADSR handling for the current instruction
.label logical_voice_idx          = $480d    // Logical voice index currently being decoded (mirrors X at entry)
.label instr_bit_cursor           = $480e    // Shifting copy of the instruction header used for per-bit tests
.label instr_op_flags             = $480f    // Operand sub-action flags (PWM/filter/duration) from bit-4 block
.label instr_header_raw           = $4810    // Raw, unshifted instruction header for BIT tests (e.g., bit 6)

.label update_instruction_ptr_and_offset = $0  // External routine: advance voice instruction PC and offset

.const VOICE_RSRC_NOT_IN_MEMORY   = $01     // A return code meaning sound resource not resident / pointers invalid
.const STOP_SOUND_MODE_FULL       = $FF     // Full stop/cleanup mode value for stop_sound_cleanup_mode
.const VOICE_GATE_MASK            = $01     // Gate bit mask for voice_ctrl_shadow (1=trigger, 0=release)
.const VOICE_GATE_CLEAR_MASK      = $FE     // Inverted gate mask (AND to clear gate bit in voice_ctrl_shadow)

* = $4A6B
decode_voice_instruction:
		// ------------------------------------------------------------
		// Validate voice resource for this logical voice and abort early
		// if its sound data is not resident (performing full cleanup).
		// ------------------------------------------------------------
        ldx     logical_voice_idx                     // X := current logical voice index for this decode pass
        jsr     update_voice_base_and_instruction_ptr // Ensure voice’s sound resource is loaded and base/PC are valid
        cmp     #$01                                  // Sound not resident?
        bne     voice_instr_resource_loaded           // If resident → proceed with instruction decoding
        jsr     stop_sound_full_cleanup               // Sound not resident → stop and fully clean up this voice
        rts                                           

		// ------------------------------------------------------------
		// Set up the per-voice instruction read pointer from the current
		// voice instruction PC so subsequent operand fetches use (voice_read_ptr),Y.
		// ------------------------------------------------------------
voice_instr_resource_loaded:
        lda     voice_instr_pc_lo,x                   
        sta     <voice_read_ptr                       
        lda     voice_instr_pc_hi,x                   
        sta     >voice_read_ptr                       

		// Store debug/status flag = non-zero instruction header
        lda     #$ff                                  
        sta     $4812                                 

		// ------------------------------------------------------------
		// Fetch the next instruction header byte from the stream and
		// branch into the bitfield decoder only when it is non-zero.
		// ------------------------------------------------------------
        ldy     #$00                                  // Y := 0 (start reading at current PC)
        lda     (voice_read_ptr),y                    
        bne     instr_header_nonzero                  // Non-zero header → decode bits

		// ------------------------------------------------------------
		// Handle zero instruction header: clear this voice’s repeat counter
		// and treat the instruction as an immediate stop (no further decoding).
		// ------------------------------------------------------------
        sta     $4812                                 // Debug/status flag = zero instruction header
        sta     voice_instr_repcount,x                // Clear repeat counter for this voice
        jmp     stop_voice_immediate                  // Treat #$00 as “stop voice now”

		// ------------------------------------------------------------
		// Latch the non-zero instruction header in both raw and “bit cursor”
		// form so later tests can both shift through individual bits and
		// re-check the original flags (notably bit 6) as needed.
		// ------------------------------------------------------------
instr_header_nonzero:
        sta     voice_instr_header,x                  // Latched copy of instruction header for this voice
        sta     instr_bit_cursor                      // Shifting copy used to walk bits 0–7 via LSR
        sta     instr_header_raw                      // Raw, unmodified header (for BIT tests on bit 6)

		// ------------------------------------------------------------
        // Bit 0 - gate trigger/release (SID-range only, via voice_sid_range_flags)
		//
		// Decode bit 0 of the instruction header as a gate trigger/release,
		// but only for voices marked as mapped to a physical SID slot by
		// voice_sid_range_flags (via V from the BIT test).
		// ------------------------------------------------------------
        bit     voice_sid_range_flags                 // Set V from SID-range flag (bit 6:=voice in physical SID range)
        lsr     instr_bit_cursor                      // Shift bit 0 of header into C (gate: 1=trigger, 0=release)
        bvc     check_stop_voice_bit                  // If V=0 → voice not in SID range, skip gate changes

		// ------------------------------------------------------------
		// Update this voice’s control shadow gate bit from decoded header bit 0,
		// mapping it to either a trigger (set gate) or release (clear gate),
		// while leaving the change staged in the shadow copy (not yet pushed to SID).
		// ------------------------------------------------------------
        lda     voice_ctrl_shadow,x                   // A := shadow control register for this voice
        bcc     clear_voice_gate_bit                  // If C=0 → header bit0=0 → clear gate (release)
        ora     #VOICE_GATE_MASK                      // C=1 → header bit0=1 → set gate bit (trigger)
        jmp     commit_voice_control_shadow           // Store updated gate state in shadow
clear_voice_gate_bit:
        and     #VOICE_GATE_CLEAR_MASK                // Clear gate bit 0 to force note-off
commit_voice_control_shadow:
        sta     voice_ctrl_shadow,x                   // Save updated control shadow (later pushed to SID)

		// ------------------------------------------------------------
		// Bit 1 - Stop sound
		//
		// Decode bit 1 of the instruction header as an immediate stop flag;
		// if set, the voice is terminated right away and no further bits of
		// this instruction are processed.
		// ------------------------------------------------------------
check_stop_voice_bit:
        lsr     instr_bit_cursor                      // Shift bit 1 of header into C (stop flag)
        bcc     check_freq_dur_gliss_bit              // If bit1=0 → no immediate stop, continue decoding

		// ------------------------------------------------------------
		// Handle an instruction that requests an immediate voice stop: select
		// full cleanup mode, stop the logical voice X, and return without
		// decoding any remaining bits or operands for this instruction.
		// ------------------------------------------------------------
stop_voice_immediate:
        lda     #STOP_SOUND_MODE_FULL_CLEANUP         // Select full cleanup when stopping this voice
        sta     stop_sound_cleanup_mode               
        txa                                           // Copy voice index into A for stop routine
        jsr     stop_logical_voice                    // Stop logical voice X (and free resources)
        tax                                           
        rts                                           

		// ------------------------------------------------------------
        // Bit 2 (+ bit 6) - frequency / duration / glissando
		//
		// Decode bit 2 (with bit 6 as a modifier) to control the timing fields:
		// either clear duration/glissando outright or, if set, read a new
		// frequency and optionally new duration and glissando values.
		// ------------------------------------------------------------
check_freq_dur_gliss_bit:
        lsr     instr_bit_cursor                      // Shift bit 2 of header into C
        bit     instr_header_raw                      // Copy original bit 6 into V (used as modifier)
        bcc     clear_dur_gliss_as_bit2_clear         // If bit2=0 → just clear duration/glissando and skip reads

		// ------------------------------------------------------------
        // Bit 2 set - read frequency (always)
		//
		// For bit 2 set, always consume and apply a new 16-bit frequency value
		// for this voice; bit 6 (via V) then decides whether we also refresh
		// duration/glissando or simply clear them after updating frequency.
		// ------------------------------------------------------------
        iny                                           // advance to freq_lo operand
        lda     (voice_read_ptr),y                    // Read freq_lo
        sta     voice_freq_lo,x                       // Store voice frequency low byte
        iny                                           // freq_hi operand
        lda     (voice_read_ptr),y                    
        sta     voice_freq_hi,x                       

        bvs     clear_dur_gliss_after_freq            // If bit6=1 → clear dur/gliss, do not read them

		// ------------------------------------------------------------
        // Bit 2 set, bit 6 clear - read duration
		//
		// When bit 2 is set and bit 6 is clear, extend the timing update by
		// reading a 16-bit duration from the stream and storing it as this
		// voice’s new playback duration.
		// ------------------------------------------------------------
        iny                                           // advance to duration_lo
        lda     (voice_read_ptr),y                    // Read duration_lo
        sta     voice_duration_lo,x                   // Store voice duration low byte
        iny                                           // duration_hi
        lda     (voice_read_ptr),y                    
        sta     voice_duration_hi,x                   

		// ------------------------------------------------------------
		// Consume and apply the 16-bit glissando delta for this voice, then
		// continue into the ADSR (bit 3) handling for this same instruction header.
		// ------------------------------------------------------------
        iny                                           // advance to glissando_lo
        lda     (voice_read_ptr),y                    // Read glissando_lo
        sta     voice_gliss_lo,x                      // Store glissando low byte
        iny                                           // glissando_hi
        lda     (voice_read_ptr),y                    
        sta     voice_gliss_hi,x                      

        jmp     check_adsr_bit                        // Continue with Bit 3 (ADSR) handling

		// ------------------------------------------------------------
		// For bit2 set with bit6 set, leave the new frequency in place but
		// actively clear any existing duration/glissando so timing reverts
		// to “no explicit duration/glide” for this voice.
		// ------------------------------------------------------------
clear_dur_gliss_after_freq:
        jsr     clear_voice_duration_and_glissando    // Bit2=1, bit6=1 → clear duration and glissando
        jmp     check_adsr_bit                        // Continue with ADSR bit check

		// ------------------------------------------------------------
		// Clear duration and glissando when header bit 2 is clear, preserving
		// the current frequency so only the timing/glide state is reset.
		// ------------------------------------------------------------
clear_dur_gliss_as_bit2_clear:
        jsr     clear_voice_duration_and_glissando    // Bit2=0 → clear duration and glissando, leave frequency alone

		// ------------------------------------------------------------
        // Bit 3 - ADSR update (SID-range only)
		//
		// Decode bit 3 as an ADSR update request, but only for voices marked
		// as mapped to a physical SID slot; if either the SID-range flag (V)
		// is clear or bit 3 itself is clear, the current ADSR envelope is left
		// untouched and decoding continues with the operand sub-block.
		// ------------------------------------------------------------
check_adsr_bit:
        bit     voice_sid_range_flags                 // Set V from SID-range flags again
        lsr     instr_bit_cursor                      // Shift bit 3 of header into C
        bvc     check_operand_block_bit               // V=0 → no physical SID voice → skip ADSR changes
        bcc     check_operand_block_bit               // Bit3=0 → no ADSR update requested

		// ------------------------------------------------------------
        // Release note, change ADSR, then re-apply original gate action
		// ------------------------------------------------------------
		
		// Temporarily force a note-off for this voice
		sty     voice_instr_saved                     // Save current Y (operand index) for later restore

        lda     voice_ctrl_shadow,x                   // A := current control shadow
        and     #VOICE_GATE_CLEAR_MASK                // Clear gate bit to force note-off
        sta     voice_ctrl_shadow,x                   // Save gate-off state in shadow
        jsr     update_voice_control                  // Push gate-off state to SID registers

		// ------------------------------------------------------------
		// Read and apply the new ADSR envelope (attack/decay and sustain/release),
		// then restore this voice’s original gate state so the note resumes
		// (or stays released) using the updated envelope parameters.
		// ------------------------------------------------------------
        ldy     voice_instr_saved                     // Restore Y (instruction operand position)
        ldx     logical_voice_idx                     // Restore logical voice index into X

		// ------------------------------------------------------------
        // Read new ADSR envelope (2 bytes)
		//
		// Load the two-byte ADSR envelope from the stream (attack/decay and
		// sustain/release) and cache it for this voice so subsequent gate
		// changes use the freshly programmed envelope shape.
		// ------------------------------------------------------------
        iny                                           // Y → ADSR attack/decay
        lda     (voice_read_ptr),y                    // Read ADSR attack/decay byte
        sta     voice_adsr_attack_decay,x             // Store ADSR attack/decay for this voice
        iny                                           // ADSR sustain/release
        lda     (voice_read_ptr),y                    
        sta     voice_adsr_sustain_release,x          

		// ------------------------------------------------------------
        // Re-apply original gate bit from voice_instr_header
		//
		// Reconstruct the control shadow’s gate bit based on the original
		// instruction header so, after the ADSR change, the voice resumes
		// (or remains released) with the exact same trigger/release state.
		// ------------------------------------------------------------
        lda     voice_instr_header,x                  // A := original instruction header for this voice
        and     #VOICE_GATE_MASK                      // Isolate original gate bit (bit 0)
        ora     voice_ctrl_shadow,x                   // Merge original gate bit back into control shadow
        sta     voice_ctrl_shadow,x                   // Save updated control shadow (with new ADSR)

		// ------------------------------------------------------------
        // Bit 4 - operand sub-actions: PWM / filter / duration
		//
		// Decode bit 4 to decide whether an extra operand bitfield follows;
		// if set, the next byte encodes optional PWM, filter, and duration
		// updates that are applied before waveform and repetition handling.
		// ------------------------------------------------------------
check_operand_block_bit:
        lsr     instr_bit_cursor                      // Shift bit 4 of header into C
        bcc     check_waveform_bit                    // Bit4=0 → no operand sub-block, skip to waveform

		// ------------------------------------------------------------
		// Consume the operand bitfield that encodes sub-actions for this
		// instruction (PWM, filter cutoff, and/or duration), caching it in
		// instr_op_flags so individual bits can be tested in turn.
		// ------------------------------------------------------------
        iny                                           // Y  → operand bitfield
        lda     (voice_read_ptr),y                    // Read operand bitfield for PWM/filter/duration
        sta     instr_op_flags                        // Save operand flags for subsequent tests

        bit     voice_sid_range_flags                 // Set V from SID-range flags (PWM only applies in-range)

		// ------------------------------------------------------------
        // Operand bit 0 - PWM (SID-range only)
		//
		// Decode operand bit 0 as a PWM update request, but only honor it for
		// voices marked as SID-backed; if either the SID-range flag (V) is
		// clear or the bit is 0, PWM registers are left unchanged.
		// ------------------------------------------------------------
check_operand_pwm_bit:
        lsr     instr_op_flags                        // Shift op bit0 into C
        bvc     check_operand_filter_bit              // V=0 → no PWM support for this voice
        bcc     check_operand_filter_bit              // Bit0=0 → no PWM update requested

		// ------------------------------------------------------------
		// Apply a new 16-bit PWM width to the physical SID voice mapped from
		// this logical voice by using the per-voice PWM register offset, then
		// restore X to the logical voice index for subsequent decoding.
		// ------------------------------------------------------------
        iny                                           // Y → PWM lo operand
        lda     voice_pwm_reg_ofs_tbl,x               // A := SID register offset for this voice’s PWM pair
        tax                                           // X := PWM register offset (physical SID index)

        lda     (voice_read_ptr),y                    // Read PWM low byte
        sta     voice1_freq_reg_lo,x                  // Write to corresponding SID low register (PWM mapped)
        iny                                           // Y := Y+1 → PWM hi operand
        lda     (voice_read_ptr),y                    // PWM high byte
        sta     voice1_freq_reg_hi,x                  
        ldx     logical_voice_idx                     

		// ------------------------------------------------------------
        // Operand bit 1 - filter cutoff
		//
		// Decode operand bit 1 as a filter cutoff update request; if set, the
		// instruction will refresh global filter state and program a new
		// cutoff value before continuing.
		// ------------------------------------------------------------
check_operand_filter_bit:
        lsr     instr_op_flags                        // Shift op bit1 into C
        bcc     check_operand_duration_bit            // Bit1=0 → no filter update requested

		// ------------------------------------------------------------
		// Advance past the sub-bitfield header, refresh the shared filter/volume
		// state, then read and apply a new 16-bit cutoff value into the SID’s
		// filter cutoff registers.
		// ------------------------------------------------------------
        iny                                           // Y → prepare for filter update side-effects
        jsr     update_filter_and_volume              // Refresh filter/volume state before writing cutoff
		
        iny                                           // Y → filter_lo operand
        lda     (voice_read_ptr),y                    // Read filter cutoff low byte
        sta     filter_cutoff_freq_lo_reg             // Store into filter cutoff low register
        iny                                           // filter_hi operand
        lda     (voice_read_ptr),y                    
        sta     filter_cutoff_freq_hi_reg             

		// ------------------------------------------------------------
        // Operand bit 2 - duration (clears glissando)
		//
		// Decode operand bit 2 as a duration override request; if set, the
		// instruction clears any existing glissando and programs a new
		// 16-bit duration for this voice before moving on to waveform logic.
		// ------------------------------------------------------------
check_operand_duration_bit:
        lsr     instr_op_flags                        // Shift op bit2 into C
        bcc     check_waveform_bit                    // Bit2=0 → no additional duration update

		// ------------------------------------------------------------
		// Clear any previous timing/glide state, then read and apply a fresh
		// 16-bit duration for this voice so the instruction’s operand can
		// override whatever duration was previously active.
		// ------------------------------------------------------------
        jsr     clear_voice_duration_and_glissando    // Clear any existing duration/glissando before update
		
        iny                                           // Y → new duration_lo
        lda     (voice_read_ptr),y                    // Read new duration low byte
        sta     voice_duration_lo,x                   // Store duration low for this voice
        iny                                           // duration_hi
        lda     (voice_read_ptr),y                    
        sta     voice_duration_hi,x                   

		// ------------------------------------------------------------
        // Bit 5 - waveform: replace high nibble of voice_ctrl_shadow
		//
		// Decode bit 5 as a waveform update request; if set, the instruction
		// will consume one operand byte to replace only the high nibble of
		// the control shadow, leaving gate and low control flags intact.
		// ------------------------------------------------------------
check_waveform_bit:
        lsr     instr_bit_cursor                      // Shift bit 5 of header into C
        bcc     check_repeat_bit                      // Bit5=0 → keep existing waveform bits

		// ------------------------------------------------------------
		// Replace this voice’s waveform bits by merging in the operand byte’s
		// high nibble while preserving the existing gate state and low flags.
		// ------------------------------------------------------------
        lda     voice_ctrl_shadow,x                   // A := current control shadow
        and     #$0f                                  // Preserve low nibble (gate and low flags)
        iny                                           // Y := Y+1 → waveform operand
        ora     (voice_read_ptr),y                    // OR in high nibble from operand (waveform selection)
        sta     voice_ctrl_shadow,x                   // Save updated control shadow with new waveform

		// ------------------------------------------------------------
        // Bit 7 - repetition (offset + repeat count)
		//
		// Decode bit 7 as a “repeat this instruction” flag; when set, a trailing
		// offset/count pair will drive a backward loop in the instruction
		// stream, otherwise execution just advances to the next instruction.
		// ------------------------------------------------------------
check_repeat_bit:
        lsr     instr_bit_cursor                      // Shift bit 6 (ignored here) out of cursor
        lsr     instr_bit_cursor                      // Shift bit 7 of header into C (repeat flag)
        bcc     exit_pvi                              // Bit7=0 → no repetition; fall through to exit

		// ------------------------------------------------------------
		// Check whether this is the final scheduled repetition for the current
		// instruction; if the counter equals #$01, handle the “last pass” path,
		// otherwise continue with the generic repeat-iteration logic.
		// ------------------------------------------------------------
        lda     voice_instr_repcount,x                // A := current repeat counter for this instruction
        cmp     #$01                                  // Is this the last pending repetition?
        bne     handle_repeat_iteration               // If not #$01 → normal repeat handling

		// ------------------------------------------------------------
        // Last repetition: clear counter, skip offset+count, advance and continue
		//
		// Handle the final repetition for this instruction by zeroing its counter,
		// skipping over the stored offset/count operands, advancing the PC/offset
		// to the next instruction in the stream, and immediately decoding that
		// next instruction.
		// ------------------------------------------------------------
        dec     voice_instr_repcount,x                // Set counter to #$00 (no further repeats)
        iny                                           // Y → skip offset byte
        iny                                           // Y → skip repeat-count byte
        jsr     update_instruction_ptr_and_offset     // Advance PC/offset past this instruction and its operands
        jmp     loop_to_next_voice_instruction        // Decode next instruction at the updated PC

		// ------------------------------------------------------------
        // Repetition in progress or just starting (counter may still be zero)
		//
		// Apply the repeat offset for this iteration by subtracting the encoded
		// backward delta from the per-voice instruction PC, effectively jumping
		// execution back in the stream before re-decoding the instruction body.
		// ------------------------------------------------------------
handle_repeat_iteration:
        iny                                           // Y → offset operand
        lda     voice_instr_pc_lo,x                   // A := current instruction PC low byte
        sec                                           
        sbc     (voice_read_ptr),y                    // Subtract offset from PC.lo
        sta     voice_instr_pc_lo,x                   // Store adjusted PC.lo
        bcs     apply_repeat_offset_to_instr_pc       // If no borrow → hi-byte unchanged
        dec     voice_instr_pc_hi,x                   // Borrow → decrement PC.hi

		// ------------------------------------------------------------
		// Mirror the same backward offset into the loop-offset bookkeeping
		// variables so any diagnostic or relative offset tracking stays aligned
		// with the adjusted instruction PC.
		// ------------------------------------------------------------
apply_repeat_offset_to_instr_pc:
        lda     voice_instr_loop_ofs_lo,x             // A := loop-offset tracking low byte
        sec                                           
        sbc     (voice_read_ptr),y                    // Apply same offset to loop-offset low byte
        sta     voice_instr_loop_ofs_lo,x             // Store adjusted loop-offset low byte
        bcs     load_or_advance_repeat_count          // No borrow → hi byte unchanged
        dec     voice_instr_loop_ofs_hi,x             // Borrow → decrement loop-offset high byte

		// ------------------------------------------------------------
		// Consume the repeat-count operand and either initialize the per-voice
		// repeat counter on first use or, if already non-zero, fall through to
		// simply decrement it for subsequent iterations.
		// ------------------------------------------------------------
load_or_advance_repeat_count:
        iny                                           // Y → repeat-count operand
        lda     voice_instr_repcount,x                // A := current repeat counter
        bne     decrement_repeat_counter              // If non-zero → just count down

		// ------------------------------------------------------------
        // First time: read repeat count from stream
		//
		// Initialize the per-voice repeat counter from the stream on first use
		// and immediately loop back to decode the same instruction at its
		// newly adjusted position.
		// ------------------------------------------------------------
        lda     (voice_read_ptr),y                    // Read initial repeat count
        sta     voice_instr_repcount,x                // Initialize repeat counter for this instruction
        jmp     loop_to_next_voice_instruction        // Re-enter decoder to execute repeated instruction

		// ------------------------------------------------------------
		// Finalize repeat bookkeeping (count down if needed) and re-enter the
		// decoder at the updated instruction pointer to process the next pass.
		// ------------------------------------------------------------
decrement_repeat_counter:
        dec     voice_instr_repcount,x                // Decrement existing repeat counter by 1

loop_to_next_voice_instruction:
        jmp     decode_voice_instruction              // Tail-call decoder for the next/looped instruction

		// ------------------------------------------------------------
		// Finalize a non-repeating instruction by advancing the per-voice
		// instruction PC/offset past all operands, then return to the caller.
		// ------------------------------------------------------------
exit_pvi:
        jsr     update_instruction_ptr_and_offset     
        rts                                           
