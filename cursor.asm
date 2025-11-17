/*
================================================================================
  Cursor physics module
================================================================================

Goal:
	take the current cursor speed, gently slow it down (drag), add any new push
	from the joystick, then move the cursor and keep it inside the screen.

1) Apply drag (the “coast down”)
	- Think of drag as friction in air: if you stop touching the stick, the cursor
	  shouldn’t stop instantly - it should glide and then settle.
	- The code makes a “drag” value that’s basically half of the current speed,
	  with sign preserved.
	- It then subtracts that drag from the speed. Result: speed shrinks toward zero
	  smoothly every frame (nice easing).

2) Read joystick and add acceleration
	- It looks up the stick direction (neutral / 8 directions).
	- Each direction maps to a tiny delta: +1, −1, or 0 per axis.
	- That delta is turned into a tuned fractional step (using masks),
	  then added to the current speed.
		• If you hold the stick, these tiny steps accumulate, so the cursor accelerates.
		• If you let go, there’s no new push, only drag, and the cursor coasts to a stop.

3) Integrate velocity into position (fixed-point)
	Note: this is not analytical integration, it's numerical integration.
	Also known as Euler integration.
	
	- Speed is kept as 8.8 fixed-point (integer byte = whole pixels,
	  fractional byte = sub-pixels).
	- It first adds the fractional parts (carrying into the integer when they overflow),
	  then adds the integer parts to the on-screen position.
	- This gives smooth, sub-pixel-feeling motion even though the screen is pixels.

4) Clamp to screen edges
	- If the new X or Y would go off-screen:
		• It zeros the speed on that axis (so you stop “sliding” along the wall),
		• and snaps the position to the nearest edge (left/right or top/bottom).
		• The fractional accumulator is set to a “sticky” value so you don’t immediately
		  re-overflow and jitter at the boundary.  

Mental model
	- Drag: halves your current speed → smooth easing.
	- Acceleration: tiny steps per frame from the joystick → ramps speed up/down.
	- Integration: frac + frac (carry) then int + int → smooth sub-pixel feel.
	- Bounds: if you hit the wall, kill speed on that axis and pin to the edge.

Inputs it relies on:
	- Current velocity (X/Y, integer + fractional bytes).
	- Current position (X/Y pixels).
	- Masks and the drag shift count chosen by set_cursor_physics
	  (so regions can feel heavier/lighter).
	- The joystick direction index → per-axis deltas.

Outputs it updates:
	- New velocity (after drag + input).
	- New position (after integration).
	- Fractional accumulators (for sub-pixel smoothness).

================================================================================

Derive “half-speed” drag using a 7× left-shift trick

	A 16-bit right shift would cleanly divide the velocity by 2,
	but the 6502 has no direct 16-bit ROR pair instruction. Instead,
	the code performs seven ASL/ROL steps, which pushes the low
	byte’s high bits into the high byte. After seven shifts, the
	new high byte (>drag) contains the same bit pattern as what
	the low byte (<drag) would have had after one logical right
	shift. In other words:

	  result_hi = (orig_lo >> 1)
	  carry     = original bit 9 of the value

	This clever inversion makes “7× left” equal to “1× right”
	for the portion we care about, and the carry bit now holds
	the original sign information. Only >drag (the high byte) is
	used afterward, effectively producing drag = speed ÷ 2.

	drag_shift_count = 7 ensures this transformation is consistent
	every frame.

================================================================================
Cursor physics engine — technical summary of core techniques
================================================================================
• Fixed-point Euler integration (8.8):
    Positions update via pos += vel each frame, with vel split into fractional
    and integer bytes. Fractional overflow carries into the integer step, giving
    smooth sub-pixel motion without floating point.

• Shift-based half-speed drag (7× ASL/ROL):
    Drag = velocity ÷ 2 computed by shifting a 16-bit copy left seven times,
    which yields (orig_lo >> 1) in the high byte and exposes the original sign
    in carry. Subtracting this produces exponential decay toward zero.

• Boundary clamping with hysteresis:
    Out-of-range positions zero velocity on that axis, clamp to an edge, and set
    the fractional accumulator to $FF to prevent jitter from immediate re-overflow.

• Data-driven physics profiles:
    Regions map to handler indices, which map to physics profiles. Each profile
    supplies tuned accel masks and a drag shift count, allowing different cursor
    “feel” per zone without touching the physics routine. This feature was
	effectively unused, as all regions have the same profile.
	
================================================================================
C64/6502-specific techniques and optimizations
================================================================================

• 16-bit rotation using ASL/ROL (explained above)

• Table-driven joystick direction decoding:
    A 4-bit joystick nibble indexes a direction lookup table (0–8). Per-index
    dx/dy tables emit {0,+1,−1}. This removes branches and normalizes odd input
    combinations into clean 8-way vectors.

• Active-low FIRE edge detection:
    Bit expression (curr XOR prev) AND prev isolates 1→0 transitions on an
    active-low FIRE bit. Returns $10 on new press while updating the stored state.

• Active-low and two’s-complement idioms:
    Sign is derived using cmp #$80, bpl/bmi, and carry logic. Negative cases use
    X=$FF for sign extension, keeping arithmetic branch-light and in native 6502
    style.
	
• Branchless acceleration mapping (EOR mask + carry sign):
    Joystick deltas {+1,−1} are turned into tuned fractional steps using a
    cmp → ror → eor mask sequence. Carry holds the sign so integer and fractional
    adds share direction with no branching.
	
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "ui_interaction.asm"
#import "cursor_physics.inc"

		
.const X_RIGHT_LIMIT               	= $A0     // compare threshold (valid if < $A0)
.const X_MAX_CLAMP                 	= $9F     // max in-range X after clamp
.const Y_BOTTOM_LIMIT              	= $C0     // compare threshold (valid if < $C0)
.const Y_MAX_CLAMP                 	= $BF     // max in-range Y after clamp
.const Y_MIN_CLAMP                 	= $08     // minimum allowed Y
.const OUT_OF_BOUNDS_REGION 	   	= $8A

.label drag_lo                 		= $CB8C   // temp 16-bit drag (lo)
.label drag_hi						= $CB8D	  // temp 16-bit drag (hi)
.label drag_sub_hi             		= $CB8E   // temp: sign-extend byte for hi subtract ($00/$FF)
.label x_frac_accum_prev       		= $CB7E   // prior X fractional accumulator (smoothing)
.label y_frac_accum_prev       		= $CB80   // prior Y fractional accumulator

/*
================================================================================
  compute_corrected_cursor_pos
================================================================================
Summary
    Compute adjusted cursor coordinates and a scaled 16-bit position used by
    downstream logic. Applies two Y bump thresholds and a fixed vertical bias,
    then assembles a 16-bit value from row base and X position, left-shifts it,
    and returns the overflow and high byte.

Global Inputs
    cursor_x_pos            current cursor X in pixels
    cursor_y_pos            current cursor Y in pixels
	
Vars/State
    pos16_lo                low byte of assembled 16-bit position (written)
    pos16_hi                high byte of assembled 16-bit position (written)
    raw_row_base_lo         low byte of current row base address

Returns
    A  						overflow bit from the 16-bit left shift (0 or 1)
    X  						high byte of ( {pos16_hi:pos16_lo} << 1 )
    Y  						corrected Y = cursor_y_pos after bumps + vertical bias

Description
    - Read cursor_y_pos and, if it meets each threshold, add 1 per threshold.
    - Add fixed vertical bias and place result in Y.
    - Build 16-bit value:
        • pos16_lo := raw_row_base_lo + POS16_LO_BIAS
        • pos16_hi := cursor_x_pos + X_OFFSET_PIX
    - Perform a 16-bit left shift across {pos16_hi:pos16_lo}.
        • X := shifted high byte
        • A := final carry (overflow) from the shift

Notes
    - Caller interprets (X, A) as a coarse column/overflow pair; Y carries the
      corrected row for subsequent table lookups.
================================================================================
*/
* = $F3BC
compute_corrected_cursor_pos:
        // ------------------------------------------------------------
        // Compute adjusted cursor coordinates and scaled position
        //
        // If cursor_y_pos ≥ $90 → add 1
        // If cursor_y_pos ≥ $08 → add 1
        // Then Y := cursor_y_pos + $28 (vertical offset correction)
        //
        // pos16_lo := raw_row_base_lo + $80     (row temp)
        // pos16_hi := cursor_x_pos + 6 (column temp)
        //
        // Performs a 16-bit left shift on {pos16_hi:pos16_lo}.
        //   - X = high byte of shifted value
        //   - A = overflow bit from shift (0 or 1)
        //
        // Returns:
        //   .A = carry/overflow from shift
        //   .X = high byte of (16-bit value << 1)
        //   .Y = corrected cursor_y_pos + $28
        // ------------------------------------------------------------

        lda     cursor_y_pos
        cmp     #Y_BUMP1_THRESH
        adc     #$00
        cmp     #Y_BUMP2_THRESH
        adc     #$00
        adc     #Y_CORR_BIAS
        tay

        clc
        lda     raw_row_base_lo
        adc     #POS16_LO_BIAS
        sta     pos16_lo

        lda     cursor_x_pos
        adc     #X_OFFSET_PIX
        sta     pos16_hi

        lda     pos16_lo
        asl     
        lda     pos16_hi
        rol     
        tax

        lda     #$00
        rol     
        rts

/*
================================================================================
  update_cursor_grid_coords
================================================================================
Summary
        Convert cursor pixel coordinates to coarse units for UI logic:
			X_quarter := floor(cursor_x_pos / 4)
			Y_half    := floor((cursor_y_pos + 8) / 2)

Global Inputs
        cursor_x_pos
        cursor_y_pos

Global Outputs
        cursor_x_pos_quarter_relative      	4-px units
        cursor_y_pos_half                  	2-px units, biased by +8 before /2

Description
        - X: shift right twice to divide by 4 with floor.
        - Y: add a fixed bias to align the hotspot, then shift right once.
================================================================================
*/
* = $F649
update_cursor_grid_coords:
        // Compute X_quarter := floor(cursor_x_pos / 4)
        lda     cursor_x_pos
        lsr
        lsr
        sta     cursor_x_pos_quarter_relative
		
        // Compute Y_half := floor((cursor_y_pos + CURSOR_Y_BIAS) / 2)
        clc
        lda     cursor_y_pos
        adc     #CURSOR_Y_BIAS
        lsr
        sta     cursor_y_pos_half

        rts
/*
================================================================================
  cursor_physics_step
================================================================================
Summary
        Apply viscous drag (~½ of current speed), add joystick-driven
        acceleration, integrate fixed-point velocity into cursor position, and
        clamp to on-screen bounds with a minimum Y.

Global Inputs
        cursor_x_vel_frac, cursor_x_vel_int       8.8 fixed-point X velocity
        cursor_y_vel_frac, cursor_y_vel_int       8.8 fixed-point Y velocity
        cursor_x_pos, cursor_y_pos                cursor pixel positions
        x/y_frac_accum_prev					      prior sub-pixel accumulators
        drag_shift_count                          expected = 7 (build ½-speed drag)
        joy_state                                 joystick raw state (low nibble = direction)
        joy_dir_idx_tbl                           nibble → direction index (0..8)
        joy_dir_dx/dy_tbl				          per-index axis deltas {0,+1,−1}
        accel_h_mask, accel_v_mask                maps delta selector to tuned frac step

Global Outputs
        cursor_x/y_vel_frac, cursor_x/y_vel_int   updated after drag + accel
        cursor_x/y_pos                			  advanced (and possibly clamped)
        x/y_frac_accum_prev      				  updated sub-pixel accumulators

Description
	• Drag (per axis):
		– Copy 16-bit velocity to a temp “drag”.
		– Perform 7× {ASL low; ROL high}. After 7 steps, (>drag) == (orig low >> 1).
		– Use the propagated carry (original bit9) to derive a sign byte ($00/$FF)
		for the high-byte subtract. Subtract drag from velocity to approximate ½·v.
	• Input:
		– Decode direction index from joy_state’s low nibble.
		– Fetch dx/dy deltas {0,+1,-1}. Use CMP #$80 + ROR to derive a signed
		high-byte contribution (0 or $FF) while preserving sign in C.
		– EOR with accel_*_mask to map selector to tuned fractional step; ADC into
		vel_frac, then add the saved hi contribution into vel_int.
	• Integrate:
		– Add vel_frac to the prior accumulator; carry feeds vel_int.
		– position := position + vel_int (8.8 scheme: only integer byte moves pixels).
	• Clamp:
		– If X ≥ X_RIGHT_LIMIT: zero velocity; clamp to $00 (moving left) or X_MAX_CLAMP
		(moving right). Set frac accumulator to $FF for stability at the edge.
		– Same for Y with Y_BOTTOM_LIMIT / Y_MAX_CLAMP, then enforce Y ≥ Y_MIN_CLAMP.

Notes
        • Fixed-point format is 8.8: *_vel_int is the signed pixel step; *_vel_frac
          accumulates sub-pixel motion. Carry from frac math is intentionally reused.
        • The “7× left shift = 1× logical right (low)” trick avoids a full 16-bit ROR;
          it yields exactly the half-speed low-byte we need while exposing sign in C.
================================================================================
*/
* = $F6B6
cursor_physics_step:
        // ------------------------------------------------------------
        // Drag for X: drag := speed >> 1 using 7×(ASL/ROL), signed subtract
        // ------------------------------------------------------------
        lda     cursor_x_vel_frac
        sta     drag_lo
        lda     cursor_x_vel_int
        sta     drag_hi

        ldx     drag_shift_count                // Load shift counter (normally 7 for ½-speed calc)
        beq     drag_x_finalize_sign            // Skip loop if zero (no shift needed)

drag_x_shift_loop:
        asl     drag_lo                         // Shift low byte left, bit7 → carry
        rol     drag_hi                         // Rotate carry into high byte (16-bit shift)
        dex                                     // Decrement shift counter
        bne     drag_x_shift_loop               // Repeat until all 7 shifts completed

drag_x_finalize_sign:
        bcc     drag_x_nonnegative              // If original bit9 was 0 → treat as non-negative

        dex                                     // X was 0 after loop → make it $FF for negative sign-extend
        jmp     store_drag_hi_sub_x             // Use $FF as the high subtract byte

drag_x_nonnegative:
        cpx     drag_lo                         // With X=0: set C=1 iff drag_lo==0 (avoid borrow on SBC)

store_drag_hi_sub_x:
        stx     drag_sub_hi                     // Store high subtract byte: $00 (non-neg) or $FF (neg)

        // ------------------------------------------------------------
        // speed := speed − drag (for X axis)
		//
        //  - Uses C from prior step: C=1 → pure subtract, C=0 → borrow.
        //  - Low byte subtracts (>drag) which equals (orig lo >> 1).
        //  - High byte subtracts sign-extend byte ($00 or $FF) to keep sign.
        // ------------------------------------------------------------
        lda     cursor_x_vel_frac              	// A := X velocity fractional byte
        sbc     drag_hi                       	// A := A − (>drag)  (borrow per C)
        sta     cursor_x_vel_frac              	// commit new fractional velocity

        lda     cursor_x_vel_int               	// A := X velocity integer byte
        sbc     drag_sub_hi                    	// A := A − ($00/$FF)  (sign-correct subtract)
        sta     cursor_x_vel_int               	// commit new integer velocity

        // ------------------------------------------------------------
        // Drag for Y: same procedure
        // ------------------------------------------------------------
        lda     cursor_y_vel_frac
        sta     drag_lo
        lda     cursor_y_vel_int
        sta     drag_hi

        ldx     drag_shift_count
        beq     drag_y_finalize_sign

drag_y_shift_loop:
        asl     drag_lo
        rol     drag_hi
        dex
        bne     drag_y_shift_loop

drag_y_finalize_sign:
        bcc     drag_y_nonnegative

        dex
        jmp     store_drag_hi_sub_y

drag_y_nonnegative:
        cpx     drag_lo

store_drag_hi_sub_y:
        stx     drag_sub_hi

        lda     cursor_y_vel_frac
        sbc     drag_hi
        sta     cursor_y_vel_frac
        lda     cursor_y_vel_int
        sbc     drag_sub_hi
        sta     cursor_y_vel_int

        // ------------------------------------------------------------
        // Decode joystick direction index (low nibble of joy_state)
        // ------------------------------------------------------------
        lda     joy_state
        and     #MSK_LOW_NIBBLE
        tax
        ldy     joy_dir_idx_tbl,x

        // ------------------------------------------------------------
        // Horizontal acceleration from table delta → apply mask and add
        // ------------------------------------------------------------
        lda     joy_dir_dx_tbl,y                // A := horizontal delta for this direction: 0, +1 ($01), or -1 ($FF)
        beq     check_vertical_delta            // 0 → no horizontal accel; skip to vertical

        cmp     #$80                            // Prepare sign in C: $FF sets C=1, $01 leaves C=0
        php                                     // Save flags (including C=sign) for later restore
        ror                                     // Convert A to hi-byte contribution: $FF→$FF, $01→$00
        plp                                     // Restore original C (the sign) for subsequent ADC
        pha                                     // Push hi-byte contribution (0 or $FF) for later add to vel_int

        eor     accel_h_mask                    // Convert 0/FF selector into tuned frac magnitude (mask encodes step)
        adc     cursor_x_vel_frac               // Add signed frac step (C still holds sign from earlier)
        sta     cursor_x_vel_frac               // Commit updated fractional velocity

        pla                                     // Pull hi-byte contribution (0 for +, $FF for −)
        adc     cursor_x_vel_int                // Add signed integer step with same carry-in (sign)
        sta     cursor_x_vel_int                // Commit updated integer velocity

check_vertical_delta:
        // ------------------------------------------------------------
        // Vertical acceleration from table delta → apply mask and add
        // ------------------------------------------------------------
        lda     joy_dir_dy_tbl,y
        beq     integrate_x_velocity

        cmp     #$80
        php
        ror
        plp
        pha

        eor     accel_v_mask                    // mask expected = $FF
        adc     cursor_y_vel_frac
        sta     cursor_y_vel_frac

        pla
        adc     cursor_y_vel_int
        sta     cursor_y_vel_int

        // ------------------------------------------------------------
        // Integrate X: position += speed_hi; manage sub-pixel accumulator
        // ------------------------------------------------------------
integrate_x_velocity:
        clc                                     
        lda     x_frac_accum_prev               // A := prior sub-pixel accumulator (X axis)
        adc     cursor_x_vel_frac               // A := A + current fractional velocity (propagates carry)
        tax                                     // X := new sub-pixel accumulator for storage

        lda     cursor_x_pos                    // A := current X pixel position
        adc     cursor_x_vel_int                // A := A + integer velocity + carry from frac sum

        cmp     #X_RIGHT_LIMIT                  // If new X < limit → still on screen?
        bcc     commit_x_in_range               // Yes: store position/accum and continue

        // Out of bounds: stop motion and clamp to nearest edge
        ldx     #$00                            // X := 0 (used to zero vel bytes / later as accum)
        lda     cursor_x_vel_int                // A := signed integer velocity (to test direction)
        stx     cursor_x_vel_frac               // Zero fractional velocity (stop further drift)
        stx     cursor_x_vel_int                // Zero integer velocity (hard stop at boundary)

        bpl     clamp_x_right_edge              // If prior vel_int ≥ 0 → we were moving right → clamp to right edge
        lda     #$00                            // Else moving left → clamp X to left boundary (0)
        jmp		commit_x_in_range

clamp_x_right_edge:
        lda     #X_MAX_CLAMP                    // Right boundary: max in-range X ($9F)

commit_x_clamped:
        dex                                     // Set frac accumulator to $FF at edge (sticky full-step)
                                                //   (keeps sub-pixel from immediately re-carrying across)
commit_x_in_range:
        sta     cursor_x_pos                    // Commit X position (clamped or in-range)
        stx     x_frac_accum_prev               // Save updated fractional accumulator

        // ------------------------------------------------------------
        // Integrate Y: position += speed_hi; manage sub-pixel accumulator
        // ------------------------------------------------------------
        clc
        lda     y_frac_accum_prev
        adc     cursor_y_vel_frac
        tax

        lda     cursor_y_pos
        adc     cursor_y_vel_int

        cmp     #Y_BOTTOM_LIMIT
        bcc     commit_y_in_range

        // Out of bounds: zero speed and clamp to edge based on sign
        ldx     #$00
        lda     cursor_y_vel_int
        stx     cursor_y_vel_frac
        stx     cursor_y_vel_int

        // ------------------------------------------------------------
        // Enforce max Y
        // ------------------------------------------------------------
        bpl     clamp_y_bottom_edge
        lda     #$00
		jmp		commit_y_in_range

clamp_y_bottom_edge:
        lda     #Y_MAX_CLAMP

commit_y_clamped:
        dex

commit_y_in_range:
        sta     cursor_y_pos
        stx     y_frac_accum_prev

        // ------------------------------------------------------------
        // Enforce minimum Y
        // ------------------------------------------------------------
        lda     cursor_y_pos
        cmp     #Y_MIN_CLAMP
        bcs     drag_exit

        lda     #Y_MIN_CLAMP
        sta     cursor_y_pos

drag_exit:
        rts		
/*
================================================================================
  update_cursor_physics_from_hotspot
================================================================================
Summary
    Selects the cursor physics profile (acceleration masks + drag strength)
    from the current interaction region. Out-of-bounds uses the default
    “bottom/standard” profile; certain regions map to an “upper-half” profile.

Global Inputs
    hotspot_entry_ofs                   current region id
    hotspot_type[]        				region id → handler index
    cursor_physics_for_region[]         handler index → profile (0=standard,1=upper)
    h_acceleration_masks[]              per-profile horizontal accel mask
    v_acceleration_masks[]              per-profile vertical   accel mask
    drag_shift_counts[]                 per-profile drag shift count (e.g., 7)

Global Outputs
    accel_h_mask                        selected horizontal accel mask
    accel_v_mask                        selected vertical   accel mask
    drag_shift_count                    selected drag strength (shift count)

Description
    • If hotspot_entry_ofs == OUT_OF_BOUNDS_REGION:
        X := 0 (default profile) → load constants and RTS.
		
    • Else:
        Y := hotspot_type[X]
        X := cursor_physics_for_region[Y]    		// 0 = standard, 1 = room viewport
        Load:
            accel_h_mask := h_acceleration_masks[X]
            accel_v_mask := v_acceleration_masks[X]
            drag_shift_count := drag_shift_counts[X]

Notes
    Profiles are table-driven so tuning (snappier vs heavier feel) can vary by
    on-screen area without touching the update code in cursor_physics_step.
================================================================================
*/
* = $F7DA
update_cursor_physics_from_hotspot:
        // ------------------------------------------------------------
        // Resolve physics profile index (X) from hotspot_entry_ofs
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs               // X := current interaction region id
        cpx     #OUT_OF_BOUNDS_REGION           // Compare: is region OOB (outside any zone)?
        bne     fetch_handler_index             // No → look up handler → profile via tables
		
        ldx     #$00                            // Yes → force default profile index 0
        jmp     load_constants                  

fetch_handler_index:
        ldy     hotspot_type,x  				// Y := handler index for this region
        ldx     cursor_physics_for_region,y     // X := physics profile (1=upper-half, 0=else)

load_constants:
        // ------------------------------------------------------------
        // Install selected physics constants for this region profile
        // ------------------------------------------------------------
        lda     h_acceleration_masks,x          // Load horizontal accel mask for profile
        sta     accel_h_mask                    
        lda     v_acceleration_masks,x          // Load vertical accel mask for profile
        sta     accel_v_mask                    
        lda     drag_shift_counts,x             // Load drag shift count
        sta     drag_shift_count                
        rts                                     

/*
================================================================================
Pseudo-code for cursor_physics_step() (keeping all the 8-bit wizardry):
================================================================================
    //----------------------------------------------------------------------
    // 1) Apply drag to X velocity (approximately vel := vel * 0.5)
    //----------------------------------------------------------------------
    // Make a copy of X velocity into a temporary 16-bit value (drag_hi:drag_lo)
    drag_lo = cursor_x_vel_frac      // low byte
    drag_hi = cursor_x_vel_int       // high byte

    count = drag_shift_count         // normally 7

    if count != 0:
        // Perform 7× {ASL lo; ROL hi}, tracking sign via carry.
        for i in 1..count:
            // Shift left: (drag_hi:drag_lo) <<= 1
            [drag_hi, drag_lo, carry] = shift_left_16(drag_hi, drag_lo)

        // After shifting, carry encodes original sign (bit 9) of the velocity.
        // Negative velocities leave carry=1 here.
        if carry == 1:
            // Negative velocity: set sign-extend byte to $FF
            drag_sub_hi = 0xFF
        else:
            // Non-negative velocity: special case logic to avoid underflow
            // C is set to 1 iff drag_lo == 0, else 0.
            // We only need the effect: choose a carry that keeps subtraction sane.
            carry = (drag_lo == 0) ? 1 : 0
            drag_sub_hi = 0x00
    else:
        // No drag shifts: treat as zero drag
        drag_lo = 0
        drag_hi = 0
        drag_sub_hi = 0
        carry = 1

    // speed := speed − drag   (X axis)
    // low byte subtracts drag_hi (≈ original low >> 1)
    // high byte subtracts drag_sub_hi (0 or 0xFF) for sign correction
    [cursor_x_vel_frac, carry] =
        subtract_with_borrow(cursor_x_vel_frac, drag_hi, carry)
    [cursor_x_vel_int, carry] =
        subtract_with_borrow(cursor_x_vel_int,  drag_sub_hi, carry)


    //----------------------------------------------------------------------
    // 2) Apply drag to Y velocity (same idea as X)
    //----------------------------------------------------------------------
    drag_lo = cursor_y_vel_frac
    drag_hi = cursor_y_vel_int

    count = drag_shift_count

    if count != 0:
        for i in 1..count:
            [drag_hi, drag_lo, carry] = shift_left_16(drag_hi, drag_lo)

        if carry == 1:
            drag_sub_hi = 0xFF
        else:
            carry = (drag_lo == 0) ? 1 : 0
            drag_sub_hi = 0x00
    else:
        drag_lo = 0
        drag_hi = 0
        drag_sub_hi = 0
        carry = 1

    [cursor_y_vel_frac, carry] =
        subtract_with_borrow(cursor_y_vel_frac, drag_hi, carry)
    [cursor_y_vel_int, carry] =
        subtract_with_borrow(cursor_y_vel_int,  drag_sub_hi, carry)


    //----------------------------------------------------------------------
    // 3) Decode joystick direction and add acceleration
    //----------------------------------------------------------------------
    // Extract low nibble from joy_state
    dir_nibble = joy_state & 0x0F

    // Map raw nibble to compressed direction index (0..8)
    dir_index = joy_dir_idx_tbl[dir_nibble]

    // Horizontal delta: {0, +1, -1}
    dx = joy_dir_dx_tbl[dir_index]

    if dx != 0:
        // dx is either +1 (0x01) or -1 (0xFF)
        // Determine sign; C will carry sign info
        // (conceptually: sign = (dx < 0) ? -1 : +1)
        sign = sign_of(dx)          // +1 or -1

        // hi_contrib is 0 for +1, 0xFF for -1
        hi_contrib = (dx < 0) ? 0xFF : 0x00

        // Map selector to tuned fractional step via accel_h_mask
        // Effectively: some profile-dependent magnitude with sign.
        frac_step = map_delta_with_mask(dx, accel_h_mask)

        // Apply fractional acceleration
        [cursor_x_vel_frac, carry] =
            add_signed_byte(cursor_x_vel_frac, frac_step, sign)

        // Apply integer acceleration
        cursor_x_vel_int =
            cursor_x_vel_int + hi_contrib + carry_as_signed(sign)

    // Vertical delta: {0, +1, -1}
    dy = joy_dir_dy_tbl[dir_index]

    if dy != 0:
        sign = sign_of(dy)
        hi_contrib = (dy < 0) ? 0xFF : 0x00

        frac_step = map_delta_with_mask(dy, accel_v_mask)

        [cursor_y_vel_frac, carry] =
            add_signed_byte(cursor_y_vel_frac, frac_step, sign)

        cursor_y_vel_int =
            cursor_y_vel_int + hi_contrib + carry_as_signed(sign)


    //----------------------------------------------------------------------
    // 4) Integrate X: fixed-point Euler integration
    //----------------------------------------------------------------------
    // X fractional accumulator
    tmp = x_frac_accum_prev + cursor_x_vel_frac
    new_frac_accum_x = (tmp & 0xFF)
    carry = (tmp > 0xFF) ? 1 : 0

    // X position: add integer velocity + carry from fractional part
    new_x = cursor_x_pos + cursor_x_vel_int + carry

    // Check right boundary
    if new_x >= X_RIGHT_LIMIT:
        // Out of bounds on X: kill speed and clamp to left or right edge
        old_int_vel_x = cursor_x_vel_int

        cursor_x_vel_frac = 0
        cursor_x_vel_int  = 0

        if old_int_vel_x >= 0:
            // Moving right → clamp to max X
            new_x = X_MAX_CLAMP   // 0x9F
        else:
            // Moving left → clamp to zero
            new_x = 0

        // Sticky fractional accumulator at edge
        new_frac_accum_x = 0xFF

    // Commit X updates
    cursor_x_pos         = new_x
    x_frac_accum_prev    = new_frac_accum_x


    //----------------------------------------------------------------------
    // 5) Integrate Y: fixed-point Euler integration + clamp
    //----------------------------------------------------------------------
    tmp = y_frac_accum_prev + cursor_y_vel_frac
    new_frac_accum_y = (tmp & 0xFF)
    carry = (tmp > 0xFF) ? 1 : 0

    new_y = cursor_y_pos + cursor_y_vel_int + carry

    if new_y >= Y_BOTTOM_LIMIT:
        // Out of bounds on bottom/top side: kill speed and clamp
        old_int_vel_y = cursor_y_vel_int

        cursor_y_vel_frac = 0
        cursor_y_vel_int  = 0

        if old_int_vel_y >= 0:
            // Moving down → clamp to bottom edge
            new_y = Y_MAX_CLAMP    // 0xBF
        else:
            // Moving up beyond the upper clamp path → clamp to 0
            new_y = 0

        new_frac_accum_y = 0xFF

    // Commit Y position and accumulator
    cursor_y_pos      = new_y
    y_frac_accum_prev = new_frac_accum_y

    //----------------------------------------------------------------------
    // 6) Enforce minimum Y (top clamp for UI band)
    //----------------------------------------------------------------------
    if cursor_y_pos < Y_MIN_CLAMP:
        cursor_y_pos = Y_MIN_CLAMP

    // done

================================================================================
Pseudo-code for cursor_physics_step(), abstracting away the 8-bit limitations:
================================================================================

    //------------------------------------------------------------------
    // 1) Apply drag to velocity (smoothly reduce speed toward zero)
    //------------------------------------------------------------------
    // Drag is roughly: velocity = velocity - velocity * 0.5
    // (i.e., exponential decay / easing toward 0)
    cursor_x_velocity *= 0.5
    cursor_y_velocity *= 0.5

    //------------------------------------------------------------------
    // 2) Read joystick and apply acceleration
    //------------------------------------------------------------------
    // Decode joystick into a direction vector:
    //    dir = (dx, dy) where each component is -1, 0, or +1
    direction_index = decode_joystick_direction(joy_state)
    (dx, dy) = direction_from_index(direction_index)

    // Map direction into tuned acceleration per axis.
    // accel_h_mask / accel_v_mask effectively scale how strong the
    // fractional acceleration step is. Here we just represent them
    // as scalar multipliers.
    ax = map_direction_to_accel(dx, accel_h_mask)  // small value: e.g., -a, 0, +a
    ay = map_direction_to_accel(dy, accel_v_mask)

    // Add acceleration into velocity
    cursor_x_velocity += ax
    cursor_y_velocity += ay

    //------------------------------------------------------------------
    // 3) Integrate velocity into position (Euler integration)
    //------------------------------------------------------------------
    // Conceptually:
    //    position ← position + velocity
    // In the real code, velocity is 8.8 fixed-point and there are
    // separate fractional accumulators, but we can treat it as a float.
    cursor_x_pos += cursor_x_velocity
    cursor_y_pos += cursor_y_velocity

    //------------------------------------------------------------------
    // 4) Clamp horizontally and kill speed when hitting walls
    //------------------------------------------------------------------
    // Clamp to [0, X_MAX_CLAMP], but with different behavior depending
    // on which side we hit and which way we were moving.
    if cursor_x_pos >= X_RIGHT_LIMIT:
        // We’re trying to go past the right side
        if cursor_x_velocity >= 0:
            // Moving right → snap to right edge
            cursor_x_pos = X_MAX_CLAMP
        else:
            // Moving left but numerically past limit → snap to left
            cursor_x_pos = 0

        // Stop horizontal motion when we hit the wall
        cursor_x_velocity = 0
        // Fractional accumulator is made “sticky” at the edge in the
        // real code; at this level: we just say “no further drift.”

    //------------------------------------------------------------------
    // 5) Clamp vertically and kill speed when hitting floor/ceiling
    //------------------------------------------------------------------
    // Clamp to [0, Y_MAX_CLAMP] first…
    if cursor_y_pos >= Y_BOTTOM_LIMIT:
        if cursor_y_velocity >= 0:
            // Moving down → snap to bottom edge
            cursor_y_pos = Y_MAX_CLAMP
        else:
            // Moving up but out of range → snap to top (0)
            cursor_y_pos = 0

        // Stop vertical motion on impact
        cursor_y_velocity = 0
        // Same as X: real code tweaks fractional accumulator; we just
        // say “no further drift.”

    //------------------------------------------------------------------
    // 6) Enforce minimum Y (don’t let cursor go above a UI band)
    //------------------------------------------------------------------
    if cursor_y_pos < Y_MIN_CLAMP:
        cursor_y_pos = Y_MIN_CLAMP

    // Done: cursor_x_pos / cursor_y_pos / cursor_*_velocity now reflect
    // drag + joystick acceleration + boundary constraints.
*/