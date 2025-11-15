#importonce
#import "globals.inc"
#import "constants.inc"
#import "ui_interaction.asm"

/*
============================================================
Joystick direction index lookup table
============================================================

Each possible 4-bit joystick reading (bits 0–3 = directions)
is translated into a compact “direction index”.

That index is then used to fetch horizontal and vertical
deltas from the two tables that follow. Only 8 of the 16
possible nibble values correspond to valid 8-way directions.

  Input  Bitmask   Index   Direction
  ------ --------  ------  ----------------
    E     1110        1    up
    A     1010        2    up-left
    B     1011        3    left
    9     1001        4    down-left
    D     1101        5    down
    5     0101        6    down-right
    7     0111        7    right
    6     0110        8    up-right

The remaining input codes map to neutral (no movement).

============================================================
*/
* = $F694
joy_dir_idx_tbl:
        .byte $00,$00,$00,$00,$00        // $0..$4 : neutral
        .byte $06,$08,$07,$00,$04        // $5=↘(6), $6=↗(8), $7=→(7), $8=0, $9=↙(4)
        .byte $02,$03,$00,$05,$01        // $A=↖(2), $B=←(3), $C=0, $D=↓(5), $E=↑(1)
        .byte $00                        // $F      : neutral

/*
============================================================
Joystick direction delta tables
============================================================

Each table defines the signed step applied to cursor speed
for a given joystick direction index (from the lookup table
described above). The deltas use small integer steps:
  0  → no movement
  1  → positive axis motion
  $FF → negative axis motion (−1)

These values form a simple 8-way direction model:
  - joy_dir_dx_tbl controls horizontal motion.
  - joy_dir_dy_tbl controls vertical motion.

Entries are indexed by direction index (1–8). The first
entry (index 0) represents neutral (no input).

============================================================
*/

* = $F6A4
joy_dir_dx_tbl:
        .byte $00, $00, $FF, $FF, $FF, $00, $01, $01, $01

* = $F6AD
joy_dir_dy_tbl:
        .byte $00, $FF, $FF, $00, $01, $01, $01, $00, $FF

/*
============================================================
cursor_physics_for_region
============================================================

Maps each interaction-region handler index to a physics
profile selector used by update_cursor_physics_from_hotspot.

Profile meanings:
  0 = standard / bottom-half cursor physics
  1 = upper-half physics (different acceleration & drag)

This table allows certain on-screen regions (e.g., UI areas
above the horizon line) to have lighter or heavier cursor
movement characteristics.

------------------------------------------------------------
Index : Value  →  Profile
------------------------------------------------------------
  0 : 01  →  upper-half profile
  1 : 00  →  standard
  2 : 00  →  standard
  3 : 00  →  standard
  4 : 00  →  standard
============================================================
*/
* = $F2D7
cursor_physics_for_region:
        .byte $01, $00, $00, $00, $00
		
/*
============================================================
h_acceleration_masks
============================================================
Horizontal acceleration masks per physics profile (index 0..1).
Used with:  A := delta (0/$01/$FF)
            A := (A ROR with sign in C) → $00 or $FF
            A := A EOR h_acceleration_masks[idx]
This maps the 0/$FF selector into a tuned fractional step size.

Profiles:
  0 = standard / bottom-half
  1 = upper-half

Current values are identical for both profiles ($FA).
============================================================
*/
* = $F310
h_acceleration_masks:
        .byte $FA, $FA


/*
============================================================
v_acceleration_masks
============================================================
Vertical acceleration masks per physics profile (index 0..1).
Same usage as the horizontal mask but for the Y axis. With the
current $FF mask, the signed selector ($00 or $FF) passes through
unchanged (full step magnitude).

Profiles:
  0 = standard / bottom-half
  1 = upper-half

Current values: both $FF.
============================================================
*/
* = $F312
v_acceleration_masks:
        .byte $FF, $FF


/*
============================================================
drag_shift_counts
============================================================
Drag strength per physics profile (index 0..1). This is the loop
count for the 16-bit {ASL low; ROL high} sequence that implements
a “virtual right shift” of the low byte, yielding ~½-speed drag.
Typical value is 7.

Profiles:
  0 = standard / bottom-half
  1 = upper-half

Current values: both 7.
============================================================
*/
* = $F314
drag_shift_counts:
        .byte $07, $07
		
.const X_RIGHT_LIMIT               = $A0     // compare threshold (valid if < $A0)
.const X_MAX_CLAMP                 = $9F     // max in-range X after clamp
.const Y_BOTTOM_LIMIT              = $C0     // compare threshold (valid if < $C0)
.const Y_MAX_CLAMP                 = $BF     // max in-range Y after clamp
.const Y_MIN_CLAMP                 = $08     // minimum allowed Y
.const OUT_OF_BOUNDS_REGION = $8A
.const MSK_JOY_FIRE            = $10     // Mask for FIRE button (active-low)

.label prev_fire_bit           = $CB89   // Latched FIRE bit from previous frame (masked to bit4)
.label drag                    = $CB8C   // temp 16-bit drag (lo/hi)
.label drag_sub_hi             = $CB8E   // temp: sign-extend byte for hi subtract ($00/$FF)
.label x_frac_accum_prev       = $CB7E   // prior X fractional accumulator (smoothing)
.label y_frac_accum_prev       = $CB80   // prior Y fractional accumulator


/*
================================================================================
update_cursor_grid_coords
================================================================================

Summary
        Convert cursor pixel coordinates to coarse units for UI logic:
        X_quarter := floor(cursor_x_pos / 4)
        Y_half    := floor((cursor_y_pos + 8) / 2)

Arguments
        None

Global Inputs
        cursor_x_pos
        cursor_y_pos

Global Outputs
        cursor_x_pos_quarter_relative      // 4-px units
        cursor_y_pos_half                  // 2-px units, biased by +8 before /2

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
          – Fetch dx/dy deltas {0,$01,$FF}. Use CMP #$80 + ROR to derive a signed
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

Goal:
	take the current cursor speed, gently slow it down (drag), add any new push
	from the joystick, then move the cursor and keep it inside the screen.

1) Apply drag (the “coast down”)
	- Think of drag as friction in air: if you stop touching the stick, the cursor
	  shouldn’t stop instantly—it should glide and then settle.
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
		• If you let go, there’s no new push—only drag—and the cursor coasts to a stop.

3) Integrate velocity into position (fixed-point)
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

5) Enforce a minimum Y
	- Even after clamping, it guarantees the cursor never dips above a certain line
	  (e.g., a UI band), by forcing Y ≥ Y_MIN_CLAMP.

Mental model
	- Drag: halves your current speed → smooth easing.
	- Acceleration: tiny steps per frame from the joystick → ramps speed up/down.
	- Integration: frac + frac (carry) then int + int → smooth sub-pixel feel.
	- Bounds: if you hit the wall, kill speed on that axis and pin to the edge.

Inputs it relies on (high-level)
	- Current velocity (X/Y, integer + fractional bytes).
	- Current position (X/Y pixels).
	- Masks and the drag shift count chosen by set_cursor_physics
	  (so regions can feel heavier/lighter).
	- The joystick direction index → per-axis deltas.

Outputs it updates
	- New velocity (after drag + input).
	- New position (after integration).
	- Fractional accumulators (for sub-pixel smoothness).

================================================================================

Derive “half-speed” drag using a 7× left-shift trick

A 16-bit right shift would cleanly divide the velocity by 2,
but 6502 has no direct 16-bit ROR pair instruction. Instead,
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
*/
* = $F6B6
cursor_physics_step:
        // ------------------------------------------------------------
        // Drag for X: drag := speed >> 1 using 7×(ASL/ROL), signed subtract
        // ------------------------------------------------------------
        lda     cursor_x_vel_frac
        sta     drag
        lda     cursor_x_vel_int
        sta     drag + 1

        ldx     drag_shift_count                // Load shift counter (normally 7 for ½-speed calc)
        beq     drag_x_finalize_sign            // Skip loop if zero (no shift needed)

drag_x_shift_loop:
        asl     drag                            // Shift low byte left, bit7 → carry
        rol     drag + 1                        // Rotate carry into high byte (16-bit shift)
        dex                                     // Decrement shift counter
        bne     drag_x_shift_loop               // Repeat until all 7 shifts completed

drag_x_finalize_sign:
        bcc     drag_x_nonnegative              // If original bit9 was 0 → treat as non-negative

        dex                                     // X was 0 after loop → make it $FF for negative sign-extend
        jmp     store_drag_hi_sub_x             // Use $FF as the high subtract byte

drag_x_nonnegative:
        cpx     drag                            // With X=0: set C=1 iff drag_lo==0 (avoid borrow on SBC)

store_drag_hi_sub_x:
        stx     drag_sub_hi                     // Store high subtract byte: $00 (non-neg) or $FF (neg)

        // speed := speed − drag
        //  - Uses C from prior step: C=1 → pure subtract, C=0 → borrow.
        //  - Low byte subtracts (>drag) which equals (orig lo >> 1).
        //  - High byte subtracts sign-extend byte ($00 or $FF) to keep sign.
        lda     cursor_x_vel_frac              // A := X velocity fractional byte
        sbc     drag + 1                       // A := A − (>drag)  (borrow per C)
        sta     cursor_x_vel_frac              // commit new fractional velocity

        lda     cursor_x_vel_int               // A := X velocity integer byte
        sbc     drag_sub_hi                    // A := A − ($00/$FF)  (sign-correct subtract)
        sta     cursor_x_vel_int               // commit new integer velocity

        // ------------------------------------------------------------
        // Drag for Y: same procedure
        // ------------------------------------------------------------
        lda     cursor_y_vel_frac
        sta     drag
        lda     cursor_y_vel_int
        sta     drag + 1

        ldx     drag_shift_count
        beq     drag_y_finalize_sign

drag_y_shift_loop:
        asl     drag
        rol     drag + 1
        dex
        bne     drag_y_shift_loop

drag_y_finalize_sign:
        bcc     drag_y_nonnegative

        dex
        jmp     store_drag_hi_sub_y

drag_y_nonnegative:
        cpx     drag

store_drag_hi_sub_y:
        stx     drag_sub_hi

        lda     cursor_y_vel_frac
        sbc     drag + 1
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
        lda     joy_dir_dx_tbl,y                // A := horizontal delta for this direction (0, $01, or $FF)
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
        clc                                     // Clear C: start fresh for frac+frac → carry into integer
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
//F785		
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
detect_fire_press_edge
================================================================================

Summary
        Detect a new joystick FIRE press (active-low) by comparing the current
        state to the previous frame’s stored bit. Returns with A = $10 on a
        fresh press, or A = $00 if unchanged or released.

Global Inputs
        joy_state              // raw joystick state (bit4 = FIRE, active-low)
        prev_fire_bit          // previous frame’s masked FIRE bit

Global Outputs
        prev_fire_bit          // updated to current FIRE bit

Returns
        A  = $10  if FIRE was newly pressed (transition 1→0)
             $00  otherwise
        Z  = 0 if pressed this frame, 1 if not

Description
        - FIRE line is active-low: 1 = released, 0 = pressed.
        - The expression (curr XOR prev) AND prev isolates a 1→0 transition:
              prev=1, curr=0 → new press → A=$10
              otherwise → A=$00
        - Stores the current FIRE bit for next-frame comparison.

Notes
        This routine provides edge detection, not level detection.
        Callers can test the Zero flag or A directly to trigger one-time actions.
================================================================================
*/		
* = $F7CB
detect_fire_press_edge:
        // ------------------------------------------------------------
        // Mask current FIRE bit (active-low) and keep a copy in X
        // ------------------------------------------------------------
        lda     joy_state                      // A := raw joystick state
        and     #MSK_JOY_FIRE                  // A := FIRE bit only (bit4)
        tax                                     // X := current FIRE bit (for state latch)

        // ------------------------------------------------------------
        // Edge detect: (curr XOR prev) AND prev → 1→0 transition only
        //  - Unpressed=1, Pressed=0 → detect new press this frame
        // ------------------------------------------------------------
        eor     prev_fire_bit                  // A := curr ⊕ prev (changed?)
        and     prev_fire_bit                  // A := (changed) ∧ prev → 1→0 edge → $10 else $00

        // ------------------------------------------------------------
        // Latch current FIRE bit for next frame; A is the result
        //  - A=$10 ⇒ new press this frame (Z=0)
        //  - A=$00 ⇒ no new press (Z=1)
        // ------------------------------------------------------------
        stx     prev_fire_bit
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
    hotspot_type[]        region id → handler index
    cursor_physics_for_region[]          handler index → profile (0=standard,1=upper)
    h_acceleration_masks[]               per-profile horizontal accel mask
    v_acceleration_masks[]               per-profile vertical   accel mask
    drag_shift_counts[]                  per-profile drag shift count (e.g., 7)

Global Outputs
    accel_h_mask                         selected horizontal accel mask
    accel_v_mask                         selected vertical   accel mask
    drag_shift_count                     selected drag strength (shift count)

Description
    • If hotspot_entry_ofs == OUT_OF_BOUNDS_REGION:
        X := 0 (default profile) → load constants and RTS.
    • Else:
        Y := hotspot_type[X]
        X := cursor_physics_for_region[Y]    ; 0 = standard, 1 = upper-half
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
        ldx     hotspot_entry_ofs              // X := current interaction region id
        cpx     #OUT_OF_BOUNDS_REGION           // Compare: is region OOB (outside any zone)?
        bne     fetch_handler_index             // No → look up handler → profile via tables
        ldx     #$00                            // Yes → force default profile index 0
        jmp     load_constants                  

fetch_handler_index:
        ldy     hotspot_type,x  // Y := handler index for this region
        ldx     cursor_physics_for_region,y    // X := physics profile (1=upper-half, 0=else)

load_constants:
        // ------------------------------------------------------------
        // Install selected physics constants for this region profile
        // ------------------------------------------------------------
        lda     h_acceleration_masks,x          // Load horizontal accel mask for profile X
        sta     accel_h_mask                    // → apply to horizontal movement logic
        lda     v_acceleration_masks,x          // Load vertical accel mask for profile X
        sta     accel_v_mask                    // → apply to vertical movement logic
        lda     drag_shift_counts,x             // Load drag shift count (e.g., 7 for half-speed)
        sta     drag_shift_count                // → controls damping strength in cursor_physics_step
        rts                                     // Done updating physics parameters
