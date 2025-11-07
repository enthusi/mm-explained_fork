/*
================================================================================
  generate_random_byte
================================================================================

Summary
	Produce an 8-bit pseudo-random value by advancing a 24-bit LFSR eight steps
	and mixing the updated low byte with the caller’s X via a gated shift/add
	accumulator.

Arguments
	.X  Secondary seed and mixer source.

Returns
	.A  Pseudo-random byte.

Global Inputs
	rng_state_hi   24-bit LFSR state, high byte
	rng_state_mid  24-bit LFSR state, mid  byte
	rng_state_lo   24-bit LFSR state, low  byte

Global Outputs
	rng_state_hi   advanced by 8 LFSR steps
	rng_state_mid  advanced by 8 LFSR steps
	rng_state_lo   advanced by 8 LFSR steps
	mix_gate       temp copy of rng_state_lo used by mixer (clobbered)
	mix_src_x      temp copy of X used by mixer (clobbered)

Description
	- LFSR advance (8 iterations):
		• Feedback bit = (rng_state_hi bit6) XOR (rng_state_mid bit7).
		• Rotate left the 24-bit state through carry (hi → mid → lo).
	- Mixer:
		• Initialize A := 0.
		• Use mix_gate (shifted MSB→C) to conditionally add shifted copies of
		mix_src_x into A until mix_src_x becomes zero.
	- Output is the mixed accumulator in A.
================================================================================
*/

.label rng_state_hi   = $dd    // 24-bit LFSR state: high byte
.label rng_state_mid  = $de    // 24-bit LFSR state: mid byte
.label rng_state_lo   = $df    // 24-bit LFSR state: low  byte

.label mix_src_x      = $e1    // copy of X used by mixer (shift/add source)
.label mix_gate       = $e2    // copy of rng_state_lo; shifted to gate adds via carry

* = $D7D4
generate_random_byte:
        // ----------------------------------------------------
        // Step LFSR 8 times (Y counts down from 7)
        // feedback bit := (rng_state_hi bit6) XOR (rng_state_mid bit7)
        // ----------------------------------------------------
        ldy     #$07            // Y := 7 → will run 8 total iterations (counts 7..0)

advance_lfsr_8_steps:
        lda     rng_state_hi    // A := high byte; source for feedback bit extraction
        asl                     // Shift left: bit6→bit7; C := old bit7(rng_state_hi)
                                //   Now A bit7 holds old bit6; C holds old bit7

        eor     rng_state_mid   // A := A XOR mid; mixes old hi.bit6 with mid bits
                                //   Targeting mid.bit7 specifically for feedback parity

        asl                     // Shift left: C := feedback = old(A bit7)
                                //   C now holds (hi.bit6 XOR mid.bit7)

        rol     rng_state_hi    // ROL through C: inject feedback into hi LSB; hi<<=1
                                //   C := old hi.bit7; Z/N per result

        rol     rng_state_mid   // Propagate carry into mid; mid<<=1
                                //   C := old mid.bit7

        rol     rng_state_lo    // Propagate carry into low; low<<=1
                                //   C := old low.bit7 (discarded); completes 24-bit step

        dey                     // Y := Y - 1
        bpl     advance_lfsr_8_steps // Loop until Y >= 0 → total 8 steps (Y:7..0)

        // ----------------------------------------------------
        // Mixer initialization
        // mix_gate := rng_state_lo (updated LFSR low)
        // mix_src_x := X (secondary seed)
        // ----------------------------------------------------
        lda     rng_state_lo        // Load mixer seed from LFSR low byte
        sta     mix_gate            // mix_gate := rng_state_lo (will shift to gate adds)
        stx     mix_src_x           // mix_src_x := X copy used as shift/add source

        // ----------------------------------------------------
        // Mixer loop: nonlinear accumulation using ASL/LSR/ADC
        // Produces final byte in A
        // ----------------------------------------------------
        lda     #$00                // A := 0 (accumulator init for mixer)
        beq     mix_loop_entry      // always branch; prior C is irrelevant (ASL below sets it)

mix_accumulate_with_carry:
        lsr     mix_src_x           // If gate=1: shift X-source once here...
        adc     mix_src_x           // ...then add it into A with carry (nonlinear mix)

mix_loop_entry:
        asl     mix_gate            		// Gate bit := msb(mix_gate); C := gate; mix_gate <<= 1
        bcs     mix_accumulate_with_carry 	// If gate=1 → do gated add path (extra shift+add)
        lsr     mix_src_x           		// Else gate=0 → shift X-source once this iteration
        bne     mix_loop_entry      		// Loop while any bits remain in mix_src_x
        rts                         		// Return with mixed byte in A
