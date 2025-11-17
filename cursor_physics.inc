#importonce

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
