#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "voice_allocation.asm"

* = $4C76
setup_music_pointers:
        // ------------------------------------------------------------
        // Compute absolute pointers for each music track based on the
        // current music_to_start_ptr, then patch the inlined music
        // engine entry JSR target and update music_in_progress_ptr.
        // X is preserved across this routine.
        // ------------------------------------------------------------
        txa
        pha

        // ------------------------------------------------------------
        // music_track_ptrs[X] := music_to_start_ptr + music_track_offsets[X]
        // Tracks are processed from index 8 down to 0.
        // ------------------------------------------------------------
        ldx     #$08

music_track_ptr_loop:
        // Low byte: ptr_lo[X] := offsets_lo[X] + music_to_start_ptr.hi
        lda     music_track_offsets_lo,x
        clc
        adc     >music_to_start_ptr
        sta     music_track_ptrs_lo,x

        // High byte: ptr_hi[X] := offsets_hi[X] + music_to_start_ptr.lo + carry
        lda     music_track_offsets_hi,x
        adc     <music_to_start_ptr
        sta     music_track_ptrs_hi,x

        dex
        bpl     music_track_ptr_loop

        // ------------------------------------------------------------
        // Set music_in_progress_ptr := music_to_start_ptr and compute
        // inlined_music_address := music_to_start_ptr + $14D.
        // The patched address is written into the JSR operand used by
        // jump_to_music_code.
        // ------------------------------------------------------------
        lda     >music_to_start_ptr
        sta     >music_in_progress_ptr
        clc
        adc     #$4d
        sta     <inlined_music_address

        lda     <music_to_start_ptr
        sta     <music_in_progress_ptr
        adc     #$01
        sta     >inlined_music_address

        pla
        tax
        rts


* = $4CAA
stop_music:
        // ------------------------------------------------------------
        // Stop music playback and reset related state:
        //   - Clear music voice-use masks and playback flag
        //   - Clear refcounts for special sounds
        //   - Optionally decrement shared sound refcount
        //   - Reset priorities for voices 0–2
        //   - Clear additional music/voice usage tracking bytes
        // ------------------------------------------------------------
        lda     #$00
        sta     music_voices_in_use_2
        sta     music_playback_in_progress

        jsr     clear_refcount_of_sounds_1_and_2

        // ------------------------------------------------------------
        // If shared sound refcount is non-zero, decrement it.
        // (Uses intermediate table locations at $5459 / $546D.)
        // ------------------------------------------------------------
        ldx     $5459
        ldx     $546d
        beq     reset_voice_priorities
        jsr     dec_sound_refcount

reset_voice_priorities:
        // ------------------------------------------------------------
        // Reset priorities for voices 0–2 to #$02 (default priority)
        // ------------------------------------------------------------
        lda     #$02
        sta     voice_priority_0
        sta     voice_priority_1
        sta     voice_priority_2

        // ------------------------------------------------------------
        // Clear music voices-in-use mask and related tracking bytes
        // ------------------------------------------------------------
        lda     #$00
        sta     music_voices_in_use
        sta     $5467
        sta     $5468
        sta     $5469

        rts


* = $53CA
jump_to_inlined_music_routine:
        // ------------------------------------------------------------
        // Trampoline that jumps to an inlined, self-modified music
        // routine. The operand at inlined_music_ptr ($53CB/$53CC) is
        // patched at runtime to point to the current handler.
        // ------------------------------------------------------------

        jmp     $ffff              // Patched at runtime
