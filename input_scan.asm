/*
===============================================================================
 Keyboard and joystick scanning
===============================================================================
 
Summary:
	This module provides a lightweight input layer for the C64 built over the CIA1 chip.

	It prioritizes joystick responsiveness, throttles keyboard scanning via a
	small countdown, resolves a single key (with Shift handling), and exposes
	a simple “key ready” interface for consumers.
 
Public routines:
    kbd_scan
        Orchestrates input sampling. First checks the joystick; if any joystick
        line is active, it refreshes the scan delay and returns immediately to
        keep controls responsive. Otherwise, when the delay elapses, it probes
        the keyboard matrix, captures column/row snapshots, resolves one key
        (ignores “unused” entries and filters repeats), handles Shift by
        switching to the shifted half of the key table, and if a new key was
        found it writes the byte into kbd_keycode and sets kbd_key_ready.
 
    joy_latch
        A fast probe of CIA1 Port B bits 0–4 (active-low). It releases the
        matrix by driving Port A high, configures Port B as inputs, reads PRB,
        stores the raw snapshot, then masks & compares against #%00011111 to
        set CPU flags. Contract: Z=1 if idle (no inputs active), Z=0 if any
        direction or fire is pressed; A holds the compare result.
 
    kbd_key_read
        Consumer helper. If kbd_key_ready is set, it clears the flag and returns
        the byte in A (kbd_keycode). If no key is ready, it returns
        immediately without side effects. Intended usage: call this 
        after kbd_scan to fetch and consume keys one at a time.
 
Private routines:
  	kbd_count_zero_bits
  		Counts 0-bits in A (MSB-first scan).
 
Notes:
    • Joystick lines are active-low; a pressed switch reads as 0.
    • The keyboard is an 8×8 matrix; scanning toggles DDRA/DDRB roles to take
      column and row snapshots, then combines them into a table index.
    • A small delay gate reduces CPU usage and key bounce; joystick activity
      resets this delay to favor responsive game controls.
  	• Actual joystick input handling is not performed here, it merely preempts the keyboard if active.
	
===============================================================================
CIA1 Registers (used for joystick and keyboard scanning)
===============================================================================

DDRA  - Data Direction Register A - cia1_ddra
		Sets each bit of Port A as input (0) or output (1).
		Register located at address $DC02 on the C64

DDRB  - Data Direction Register B - cia1_ddrb
		Same as DDRA but for Port B.
		Register located at address $DC03.

PRA   - Port A Data Register - cia1_pra
		Holds output values for Port A when bits are outputs,
		or reads current pin states when bits are inputs.
		Register located at address $DC00.

PRB   - Port B Data Register - cia1_prb
		Reads (or writes) data on Port B lines.
		For joystick/keyboard input, this is the register read
		to detect pressed lines.
		Register located at address $DC01.

Usage notes:
------------
- Port A and Port B together form an 8×8 input matrix shared by
the keyboard and joysticks.
- DDRA/DDRB configure direction (1 = output, 0 = input).
- PRA/PRB hold or read the pin states depending on direction.
- For joystick polling:
	DDRA = $FF   → all outputs
	PRA  = $FF   → drive lines high (release)
	DDRB = $00   → all inputs
	read PRB     → 0 = pressed, 1 = released (active-low logic)
	 
===============================================================================
  How the keyboard matrix is read (via CIA1 Port A and Port B)
===============================================================================
 
The C64 keyboard is a matrix of 8×8 switches: 8 “row” lines and 8 “column” lines.
Each key connects one specific row to one specific column when pressed.
These 16 lines are shared between the keyboard and the two joysticks.

CIA1 Port A and Port B handle the matrix as follows:

	Port A (PRA/DDRA) → drives the 8 row lines (outputs)
	Port B (PRB/DDRB) → reads the 8 column lines (inputs)

The process:
	1) Configure directions:
		 DDRA = $FF   → Port A as outputs (used to select one row at a time)
		 DDRB = $00   → Port B as inputs  (used to read the column states)

	2) To detect which key in a row is pressed:
		 - Write a byte to Port A with only one bit cleared (0), all others set (1).
		   Example: writing %11111110 drives row 0 low and releases the others.
		 - Read Port B: each bit corresponds to a column.
		   If a key in the active row is pressed, that column’s bit reads 0.

	3) Repeat step 2 for each of the 8 rows, cycling the 0 across the bits in Port A.
		 This scans all rows in turn, building a full 8×8 keyboard state map.

Logic summary:
	 Row line = output, column line = input.
	 0 written to a row “selects” it.
	 0 read from a column = key pressed (active-low).
	 1 read = key released (line pulled high internally).

Note:
	- The joystick shares these lines; moving the joystick grounds certain matrix
	lines, which is why reading both simultaneously requires care.
	- To poll the joystick alone, all rows (Port A) are driven high (%11111111)
	so that no keyboard rows are selected and only joystick switches are seen.

================================================================================
Algorithms & General Techniques
================================================================================

• Prioritized input multiplexing:
    kbd_scan polls the joystick first and, if any line is active, immediately
    refreshes a 16-bit delay and returns. Keyboard scanning is deferred, giving
    joystick input strict priority over keyboard events.

• Throttled keyboard scanning with 16-bit down-counter:
    A hi:lo countdown (kbd_delay_hi:lo) gates keyboard scans. While non-zero,
    it is decremented each call; only when it reaches zero does the routine
    probe the matrix. This both reduces CPU load and helps with key bounce.

• De-duplication of key events:
    kbd_last_keycode remembers the last published key. If a scan resolves the
    same keycode again, it is discarded. Only transitions to a new keycode are
    published, providing simple repeat filtering.

================================================================================
C64 / 6502 / 8-bit Techniques
================================================================================

• Two-stage matrix probing:
    1) Quick probe: a single “all columns selected” read (PRA=$00, DDRA=$FF,
       DDRB=$00, read PRB) tests for any keyboard activity at all.
    2) Precise snapshots: if activity is detected, the routine captures a
       column snapshot (A drives, B reads) and a row snapshot (B drives, A reads)
       to local state for decoding.

• Joystick latch using active-low mask and Z flag:
    joy_latch reads cia1_prb, stores joy_state, then ANDs with JOY2_MASK and
    CMPs against JOY2_MASK. The Z flag directly encodes “no joystick input”
    vs “some line active,” providing a cheap joystick-activity test.

• Shift-state encoded in bit 6 of keymap base:
    kbd_keymap_base stores 0 or $40; BIT kbd_keymap_base then BVC checks bit6.
    This uses the 6502’s overflow flag (V) as a cheap single-bit state test.

• Precomputed keymap for normal and shifted matrices:
    The 8×8 matrix is encoded as two back-to-back 64-byte tables (normal and
    shifted), addressed as (col*8 + row + keymap_base). This avoids any
    runtime translation of CIA matrix positions to keycodes.

• Port-release protocol for joystick:
    Before reading joystick, the code forces:
        - DDRA = $FF, PRA = $FF, DDRB = $00
      ensuring the matrix is released and the joystick sees proper pull-ups.

================================================================================
*/
#importonce
#import "registers.inc"
#import "constants.inc"
#import "globals.inc"

.label kbd_cols          	= $2057    // CIA1 capture of keyboard column lines
.label kbd_col_zeros_m1   	= $2058    // (# of zero bits in columns) - 1, or #$FF if none
.label kbd_rows            	= $2059    // CIA1 capture of keyboard row lines
.label kbd_row_zeros_m1    	= $205A    // (# of zero bits in rows) - 1, or #$FF if none
.label kbd_lsb0_idx        	= $205B    // output from kbd_count_zero_bits: lowest zero-bit index
.label kbd_col_lsb0_idx    	= $205C    // least significant 0-bit column index
.label kbd_last_keycode    	= $205D    // last accepted key; used to filter duplicates
.label kbd_keymap_base     	= $205E    // 0 or $40: selects normal vs shifted key matrix
.label kbd_col_mask   		= $205F    // OR-mask applied to columns when shifted
.label kbd_row_mask   		= $2060    // OR-mask applied to rows when shifted
.label prev_fire_bit        = $CB89    // Latched FIRE bit from previous frame (masked to bit4)
.label kbd_delay_lo        	= $FE4D    // lo byte of polling delay
.label kbd_delay_hi        	= $FE4E    // hi byte of polling delay

.const KBD_KEY_LSHIFT               = $7B       // Keycode for Left Shift (unshifted matrix)
.const KBD_KEY_RSHIFT               = $7D       // Keycode for Right Shift (unshifted matrix)
.const KBD_DELAY_INIT_HI            = $1C       // Initial high byte for keyboard scan delay
.const KBD_DELAY_INIT_LO            = $20       // Initial low byte for keyboard scan delay
.const KBD_KEY_IGNORED              = $5F       // Sentinel keycode: treated as “no key” / unused entry
.const KBD_LSHIFT_COL_MASK          = %1000_0000// Column bitmask for Left Shift line (used to suppress ghosting)
.const KBD_RSHIFT_COL_MASK          = %0001_0000// Column bitmask for Right Shift line (used to suppress ghosting)
.const KBD_LSHIFT_ROW_MASK          = %0000_0010// Row bitmask for Left Shift line (used to suppress ghosting)
.const KBD_RSHIFT_ROW_MASK          = %0100_0000// Row bitmask for Right Shift line (used to suppress ghosting)
.const KBD_KEYMAP_SHIFT_BASE        = $40       // Base offset into keymap table for shifted matrix
.const JOY2_MASK                    = %0001_1111// Mask for joystick #2 bits (directions + fire, active-low)
.const MSK_JOY_FIRE            	    = $10       // Mask for FIRE button (active-low)
.const ALL_OUTPUTS                  = $FF       // DDRA/DDR B value: all pins configured as outputs
.const ALL_INPUTS                   = $00       // DDRA/DDR B value: all pins configured as inputs
.const KEY_SPACE               		= $20    	// pause/resume

/*
  Matrix of key inputs mapped from the keycodes scanned
  A value of #$5F is ignored. 
  Special cases - the Function keys, which yield values in the 01-08 range
 */
* = $21BE
kbd_keymap:
.byte $C8,$33,$35,$37,$39,$2B,$5F,$31 // DEL	3 		5 	7 	9 	+ 	Pound	1
.byte $0D,$77,$72,$79,$69,$70,$2A,$5F // <ret>	W		R	Y	I	P	*		Left
.byte $CA,$61,$64,$67,$6A,$6C,$3B,$5F // Right	A		D	G	J	L	;		Ctrl
.byte $04,$34,$36,$38,$30,$2D,$CF,$32 // F7		4		6	8	0	-	Home	2
.byte $01,$7A,$63,$62,$6D,$2E,$7D,$20 // F1		Z		C	B	M	.	RShift	Space
.byte $02,$73,$66,$68,$6B,$3A,$3D,$D0 // F3		S		F	H	K	:	=		Commodore
.byte $03,$65,$74,$75,$6F,$40,$5F,$71 // F5		E		T	U	O	@	Up		Q
.byte $CD,$7B,$78,$76,$6E,$2C,$2F,$5F // Down	LShift 	X 	V	N	,	/		Stop

// Shifted key matrix (used when kbd_keymap_base=$40)
.byte $C9,$23,$25,$27,$29,$2B,$5F,$21
.byte $0D,$57,$52,$59,$49,$50,$2A,$5F
.byte $CB,$41,$44,$47,$4A,$4C,$29,$5F
.byte $08,$24,$26,$28,$30,$2D,$CE,$22
.byte $05,$5A,$43,$42,$4D,$3E,$7D,$20
.byte $06,$53,$46,$48,$4B,$28,$3D,$5F
.byte $07,$45,$54,$55,$4F,$40,$5F,$51
.byte $CC,$7B,$58,$56,$4E,$3C,$3F,$5F

// Filler bytes to match original - unused
.byte $20, $44, $22, $F0, $FB, $60

/*
===============================================================================
  kbd_scan
===============================================================================
 
Summary:
    Prioritizes joystick responsiveness and throttles keyboard scans with a
    delay gate. If joystick activity is detected, refreshes the delay and
    returns immediately; otherwise probes the keyboard matrix, resolves a
    single key (with Shift handling and de-dup), and publishes it.
 
Arguments:
    saved_task_idx			Optional gate: if zero, skip (re)priming the poll delay.
 
Variables / State:
    kbd_delay_lo            Low byte of the 16-bit countdown that throttles
                            	keyboard scans (helps debounce and save CPU).
    kbd_delay_hi            High byte of the 16-bit countdown.
    kbd_cols           		Captured column snapshot (active-low; 0 means pulled).
    kbd_rows              	Captured row snapshot (active-low; 0 means pulled).
    kbd_col_zeros_m1       	(# of zero bits in columns)-1; $FF means none.
    kbd_row_zeros_m1       	(# of zero bits in rows)-1; $FF means none.
    kbd_col_lsb0_idx        Index of least-significant active (zero) column.
    kbd_lsb0_idx            Index of least-significant active (zero) row (set by counter).
    kbd_last_keycode       	Last published key; used to filter repeats.
    kbd_keymap              Keymap for normal and shifted halves (base + $40).
    kbd_keymap_base         Base offset into key matrix (0 normal, $40 shifted).
    kbd_col_mask     		Column OR-mask applied under Shift interplay.
    kbd_row_mask     		Row OR-mask applied under Shift interplay.
 
I/O Registers:
    cia1_pra         		CIA1 PRA   - Port A data (drive/select).
    cia1_prb         		CIA1 PRB   - Port B data (sense).
    cia1_ddra    			CIA1 DDRA  - sets Port A direction.
    cia1_ddrb    			CIA1 DDRB  - sets Port B direction.
 
Returns:
    kbd_keycode           	Set to resolved keycode when a new relevant key is found.
    kbd_key_ready           Set to #$01 when kbd_keycode is updated; else left at #$00.
    kbd_delay_lo/hi         Refreshed when joystick is active; decremented otherwise.
    Registers               No fixed return value guaranteed; Z/A may reflect last compare.
 
Description:
    -On entry, cached matrix snapshots are cleared to “no activity” (all 1s). 
 
  	-If required, the routine (re)primes a 16-bit poll delay; on subsequent calls it first polls the
    joystick (active-low on PRB bits 0–4). Any joystick activity refreshes the delay and
    returns immediately to keep controls responsive.
 
    -When the delay elapses, the routine performs a quick probe of the keyboard matrix:
    DDRA=$FF, DDRB=$00, PRA=$00, then checks PRB against $FF. 
 
  	-If activity is present, it takes two precise snapshots: 
  	columns (PRB while A drives) and rows (PRA while B drives) - and counts zero bits in each, 
  	recording the least-significant active row/column.
 
  	-If either side shows multiple actives, it applies shift-aware OR masks to suppress ambiguous lines and re-counts.
 
    -The final matrix index is (column*8 + row + base), with base taken from kbd_keymap_base (0 or $40). 
 
  	-Keys mapping to #$5F are ignored.
  	-Identical repeats are filtered using kbd_last_keycode. 
  	-If the key is Left/Right Shift, the routine arms shifted mapping (sets kbd_keymap_base and 
  	adjustment masks) and exits.
 
  	-Otherwise it writes the key into kbd_keycode and asserts kbd_key_ready.
===============================================================================
*/
* = $2062
kbd_scan:
		ldy #$FF
		sty kbd_cols           	// clear cached column snapshot to “no activity” (all 1s = pulled-up idle)
		sty kbd_rows            // clear cached row snapshot to “no activity”

		// ------------------------------------------------------------
		// Skip (re)initializing the poll delay on entry
		//
		// If saved_task_idx == 0 we proceed to input polling immediately,
		// otherwise we (re)prime the 16-bit delay counter.
		// ------------------------------------------------------------
		lda saved_task_idx
		beq poll_input

		// ------------------------------------------------------------
		// Prime the 16-bit poll delay
		//
		// The keyboard will only be scanned once this countdown elapses,
		// which helps throttle CPU usage and tame key bounce.
		// ------------------------------------------------------------
		lda #KBD_DELAY_INIT_LO
		sta kbd_delay_lo
		lda #KBD_DELAY_INIT_HI
		sta kbd_delay_hi

poll_input:
		// ------------------------------------------------------------
		// Poll joystick first (priority) 
		//
		// joy_latch leaves Z=1 when idle,
		// Z=0 when any joystick line (bits 0–4) is active (pressed = low).
		// ------------------------------------------------------------
		jsr joy_latch
		beq kbd_delay_check		// joystick idle → continue to delay/keyboard

		// ------------------------------------------------------------
		// Joystick active
		//
		// Refresh the delay and exit immediately to keep
		// joystick responsiveness high. Keyboard scan is deferred.
		// ------------------------------------------------------------
		lda #KBD_DELAY_INIT_LO
		sta kbd_delay_lo
		lda #KBD_DELAY_INIT_HI
		sta kbd_delay_hi
		rts

kbd_delay_check:
		// If the 16-bit delay is zero, scan the keyboard now.
		lda kbd_delay_lo
		ora kbd_delay_hi
		beq kbd_probe_matrix

		// Otherwise, decrement the 16-bit delay
		lda kbd_delay_lo
		bne delay_decrease_lo
		dec kbd_delay_hi
delay_decrease_lo:
		dec kbd_delay_lo

		// ------------------------------------------------------------
		// Delay just reached zero? 
		//
		// Take the fast path:
		// 		fall through the “publish-or-kbd_exit_2” gate by forcing A ≠ 0 so the
		// 		later BEQ won’t trigger the early kbd_exit_2 path.
		// ------------------------------------------------------------
		lda kbd_delay_lo
		ora kbd_delay_hi
		bne kbd_probe_matrix
		lda #KEY_SPACE			// Return the space bar key code (simulate a pause?) to steer kbd_publish_gate
		jmp kbd_publish_gate

kbd_probe_matrix:
		// ------------------------------------------------------------
		// Read keyboard matrix using CIA1 (quick probe, then precise snapshots)
		//
		// Quick probe: configure matrix and test if ANY key is pressed.
		// 1) DDRA = $FF (Port A = outputs: drive columns)
		//    DDRB = $00 (Port B = inputs: read rows)
		// 2) PRA  = $00 (drive all columns LOW to enable sensing)
		// 3) Read PRB and compare to $FF:
		//       if PRB != $FF → at least one row was pulled LOW → some key/line active.
		// ------------------------------------------------------------
		ldy #ALL_OUTPUTS
		sty cia1_ddra     		// DDRA = $FF (A as outputs)
		lda #ALL_INPUTS
		sta cia1_ddrb     		// DDRB = $00 (B as inputs)
		sta cia1_pra         	// PRA  = $00 (select all columns: active-low)
		cpy cia1_prb         	// PRB vs $FF ? (any 0 → activity)
		bne kbd_input_detected
		jmp kbd_no_input

kbd_input_detected:
		// ------------------------------------------------------------
		// Precise capture #1: COLUMN snapshot.
		// 
		//  Keep A driving columns LOW and read B:
		//    PRB bits clear (0) mark the columns whose lines are pulled LOW through a pressed key.
		// ------------------------------------------------------------
		lda #ALL_OUTPUTS
		sta cia1_ddra     		// DDRA = $FF (A still outputs)
		lda #ALL_INPUTS
		sta cia1_ddrb     		// DDRB = $00 (B inputs)
		sta cia1_pra         	// PRA  = $00 (all columns selected)
		lda cia1_prb         	// PRB  = column sense
		sta kbd_cols

		// ------------------------------------------------------------
		// Precise capture #2: ROW snapshot.
		//
		//  Swap roles: drive rows (B) LOW and read columns (A).
		//    PRA bits clear (0) mark the rows whose lines are pulled LOW through a pressed key.
		// ------------------------------------------------------------
		lda #ALL_OUTPUTS
		sta cia1_ddrb     		// DDRB = $FF (B outputs)
		lda #ALL_INPUTS
		sta cia1_ddra     		// DDRA = $00 (A inputs)
		sta cia1_prb         	// PRB  = $00 (drive all rows LOW)
		lda cia1_pra         	// PRA  = row sense
		sta kbd_rows

kbd_count_bits:
		// ------------------------------------------------------------
		// Count 0-bits in the captured COLUMN snapshot.
		//
		// On return from kbd_count_zero_bits:
		//   A  = (#zero_bits - 1), or #$FF if there are no zeros
		//   kbd_lsb0_idx = index (0..7) of the least-significant zero bit (if any)
		// ------------------------------------------------------------
		lda kbd_cols
		jsr kbd_count_zero_bits
		bmi kbd_no_input         // no zero bits → no active columns → abort
		sta kbd_col_zeros_m1     // save (#zeros - 1) for columns

		// Latch the column index of the least-significant active line.
		lda kbd_lsb0_idx
		sta kbd_col_lsb0_idx

		// Repeat the same zero-bit count for the ROW snapshot.
		lda kbd_rows
		jsr kbd_count_zero_bits
		bmi kbd_no_input         // no zero bits → no active rows → abort
		sta kbd_row_zeros_m1     // save (#zeros - 1) for rows

		// ------------------------------------------------------------
		// If either rows or columns have multiple active lines, we need to adjust
		// the matrix (e.g., shift masks / ghosting protection). 
		// 
		//  Recall:
		//    result 0  → exactly one active line
		//    result >0 → multiple active lines
		//    result $FF→ none (already handled by BMI above)
		// 
		//  Here A holds kbd_row_zeros_m1; OR with kbd_col_zeros_m1:
		//    nonzero → at least one side has multiple actives → adjust
		//    zero    → both sides have exactly one active line → no adjustment
		// ------------------------------------------------------------
		ora kbd_col_zeros_m1
		bne kbd_apply_shift_masks

		// ------------------------------------------------------------
		// Build the key matrix index: index = (column * 8) + row + base_offset
		// where base_offset is 0 for normal, $40 for shifted matrix.
		// ------------------------------------------------------------
		lda kbd_col_lsb0_idx
		asl 
		asl 
		asl    					// A = column  8
		clc
		adc kbd_lsb0_idx        // + row
		adc kbd_keymap_base     // + base (0 or $40)
		
		// Put the final index in Y for table lookup.
		tay

		// ------------------------------------------------------------
		// Lookup keycode and ignore “unused” entries (#$5F).
		// ------------------------------------------------------------
		lda kbd_keymap,Y
		cmp #KBD_KEY_IGNORED
		beq kbd_no_input

		// ------------------------------------------------------------
		// De-duplicate: publish only when the key differs from the last one.
		// ------------------------------------------------------------
		cmp kbd_last_keycode
		sta kbd_last_keycode
		beq kbd_exit

		// ------------------------------------------------------------
		// Shift handling: if the detected key is a Shift, arm shifted mapping
		// (set base offset and adjustment masks) and exit; the next scan will
		// use the shifted key matrix.
		// ------------------------------------------------------------
		lda kbd_keymap,Y
		cmp #KBD_KEY_LSHIFT
		bne not_left_shift
		
		// Left Shift → base = $40, column mask = %1000_0000, row mask = %0000_0010
		lda #KBD_KEYMAP_SHIFT_BASE
		sta kbd_keymap_base
		lda #KBD_LSHIFT_COL_MASK
		sta kbd_col_mask
		lda #KBD_LSHIFT_ROW_MASK
		sta kbd_row_mask
		bne kbd_exit

not_left_shift:
		cmp #KBD_KEY_RSHIFT
		bne not_right_shift

		// Right Shift → base = $40, column mask = %0001_0000, row mask = %0100_0000
		lda #KBD_KEYMAP_SHIFT_BASE
		sta kbd_keymap_base
		lda #KBD_RSHIFT_COL_MASK
		sta kbd_col_mask
		lda #KBD_RSHIFT_ROW_MASK
		sta kbd_row_mask
		bne kbd_exit

not_right_shift:
		jmp kbd_publish_gate

		// ------------------------------------------------------------
		// Ignore path: treat this scan as “no relevant input” and reset transient state.
		// ------------------------------------------------------------
kbd_no_input:
		lda #$00
		sta kbd_keymap_base    	// clear shifted mapping (return to base matrix)
		lda #$FF
		sta kbd_last_keycode    // invalidate last key so the next real key will publish

kbd_exit:
		// ------------------------------------------------------------
		// Funnel to the common exit gate with Z=1:
		// load A=0 so the following BEQ will skip publishing and just exit
		// ------------------------------------------------------------
		lda #$00

kbd_publish_gate:
		// ------------------------------------------------------------
		// Publish gate:
		//   Z=1 (A==0)  → no key to publish → exit
		//   Z=0 (A!=0)  → publish A as the keycode and set the “ready” flag
		// ------------------------------------------------------------
		beq kbd_exit_2
		sta kbd_keycode         // publish keycode
		lda #TRUE
		sta kbd_key_ready       // mark “key available”

kbd_exit_2:
		rts
/*
===============================================================================
  kbd_apply_shift_masks 
===============================================================================
Matrix offset & bitmask adjustment

When a Shift key has been detected, the decoder switches to a shifted keymap by setting
kbd_keymap_base’s bit 6 and precomputing adjustment masks for columns/rows. 

This section checks that bit (via BIT/BVC) and, if active, conditionally ORs the snapshots 
with the masks - but only when the corresponding “clear count” indicates multiple active lines. 
This helps suppress ambiguous multi-line activations (e.g., shift interplay / ghosting). 

After masking, control transfers to kbd_count_bits to re-derive the least-significant active 
row/column indices.
===============================================================================
*/
* = $2168
kbd_apply_shift_masks:
		// ------------------------------------------------------------
		// Shifted mapping enabled?  
		// 
		// BIT copies bit6 of kbd_keymap_base into V.
		// If V=0 (bit6 clear → offset==0), no adjustment needed → kbd_exit_2.
		// ------------------------------------------------------------
		bit kbd_keymap_base
		bvc kbd_exit_3

		// ------------------------------------------------------------
		// Load precomputed adjustment masks (set by L/R Shift handlers):
		//   X = mask for columns, Y = mask for rows.
		// ------------------------------------------------------------
		ldx kbd_col_mask
		ldy kbd_row_mask
		jmp adjust_column_bits

kbd_exit_3:
		jmp kbd_exit

adjust_column_bits:
		// ------------------------------------------------------------
		//  Decide whether to adjust the captured COLUMN bits.
		//
		//  kbd_col_zeros_m1 holds (number_of_zero_bits - 1).
		//    = 0  → exactly one active (zero) column
		//    > 0  → multiple active columns
		//  We only apply the mask when there are multiple actives (ghosting/shift interplay).
		// ------------------------------------------------------------
		lda kbd_col_zeros_m1
		beq adjust_row_bits     // exactly one → skip masking columns
		txa
		ora kbd_cols			// force masked columns high (ignore them)
		sta kbd_cols

adjust_row_bits:
		// ------------------------------------------------------------
		// Same policy for ROW bits:
		//
		// kbd_row_zeros_m1 = (number_of_zero_bits - 1)
		// Apply row mask only when multiple rows are active.
		// ------------------------------------------------------------
		lda kbd_row_zeros_m1
		beq count_bits_2        // exactly one → skip masking rows
		tya
		ora kbd_rows            // force masked rows high (ignore them)
		sta kbd_rows

count_bits_2:
		// Recount zeros after adjustments to re-derive row/column indices.
		jmp kbd_count_bits
/*
===============================================================================
  kbd_count_zero_bits
===============================================================================
Counts 0-bits in A (MSB-first scan). Returns (#zeros-1) or #$FF if none.
Also records the least-significant zero bit index into kbd_lsb0_idx.
 
Arguments:
	A                               Input bitfield to scan (8 bits).
 
Returns:
    A                               (# of zero bits) - 1, or #$FF when no zeros.
    kbd_lsb0_idx					Index (0..7) of least-significant zero bit if any.
===============================================================================
*/
* = $2194
kbd_count_zero_bits:
		// Y = bit index (7..0), X = zero-counter
		ldy #$07
		ldx #$00

check_for_zero_bit:
		// Shift MSB into carry
		asl 

		// If carry set, bit was 1 → skip counting
		bcs next_bit_index

		// Bit was 0: increment counter and record current index
		inx
		sty kbd_lsb0_idx

next_bit_index:
		dey

		// Loop while Y >= 0
		bpl check_for_zero_bit

		// Return X-1 in A (or #$FF when X=0)
		dex
		txa
		rts
		brk
/*
===============================================================================
  joy_latch
===============================================================================
	Reads joystick #2 via CIA1. Z=0 when any input is active. Stores raw state.

Returns:
	A                                Masked joystick state (bits 0–4).
	Z-flag                           Clear when any line is active; set otherwise.

Steps to read joystick (CIA1) — active-low logic

	1) Configure port directions:
	   DDRA = $FF  (Port A outputs)
	   DDRB = $00  (Port B inputs)

	2) Release the matrix so joystick lines can be read cleanly:
	   PRA = $FF   (drive all Port A lines high)

	3) Read PRB and mask joystick bits:
	   value = PRB & $1F

	4) Interpret (active-low):
	   value == $1F → no inputs active
	   any bit 0    → some input active
 
===============================================================================
*/
* = $21A6
joy_latch:
		// ------------------------------------------------------------
		// CIA1 setup: make Port A all outputs and drive them high.
		//
		// This releases the keyboard/joystick lines to the pull-ups so Port B can be sampled.
		// ------------------------------------------------------------
		ldy #ALL_OUTPUTS
		sty cia1_ddra
		
		// ------------------------------------------------------------
		// Drive Port A high (read-all mask for matrix scanning / joystick sense).
		// ------------------------------------------------------------
		sty cia1_pra

		// ------------------------------------------------------------
		// Configure Port B as inputs (we'll read joystick here).
		// ------------------------------------------------------------
		lda #ALL_INPUTS
		sta cia1_ddrb

		// ------------------------------------------------------------
		// Read Port B: joystick lines are active-low.
		//
		// Bits 0–4 correspond to directions + fire (1 = idle, 0 = pressed).
		// ------------------------------------------------------------
		lda cia1_prb
		sta joy_state

		// ------------------------------------------------------------
		// Keep only joystick bits, then compare against the joystick bitmask to set flags:
		// if all five bits are 1 → no inputs activated → Z = 1
		// if any bit is 0       → some input activated → Z = 0
		// ------------------------------------------------------------
		and #JOY2_MASK
		cmp #JOY2_MASK
		rts
/*
===============================================================================
  kbd_key_read
===============================================================================
Process a key

Consumer helper: if a key is ready, clear the ready flag and return it in A.
Otherwise, return immediately.
 
Returns:
    A                                kbd_keycode when a key was ready.
    kbd_key_ready                    Cleared to #$00 when a key is consumed.
===============================================================================
*/
* = $2244
kbd_key_read:
		lda kbd_key_ready

		// Is there a key to process?
		bne keycode_is_ready

		// No: return
		rts

keycode_is_ready:
		// Clear the ready flag to avoid reprocessing
		lda #FALSE
		sta kbd_key_ready

		// Return the key in A
		lda kbd_keycode
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
        joy_state              	raw joystick state (bit4 = FIRE, active-low)
        prev_fire_bit          	previous frame’s masked FIRE bit

Global Outputs
        prev_fire_bit          	updated to current FIRE bit

Returns
        A  						$10  if FIRE was newly pressed (transition 1→0)
								$00  otherwise
					
        Z  						0 if pressed this frame
								1 if not

Description
        - FIRE line is active-low: 1 = released, 0 = pressed.
        - The expression (curr XOR prev) AND prev isolates a 1→0 transition:
              prev=1, curr=0 → new press → A=$10
              otherwise → A=$00
        - Stores the current FIRE bit for next-frame comparison.

Notes
        This routine provides edge detection, not level detection.
================================================================================
*/		
* = $F7CB
detect_fire_press_edge:
        // ------------------------------------------------------------
        // Mask current FIRE bit (active-low) and keep a copy in X
        // ------------------------------------------------------------
        lda     joy_state                      // A := raw joystick state
        and     #MSK_JOY_FIRE                  // A := FIRE bit only (bit4)
        tax                                    // X := current FIRE bit (for state latch)

        // ------------------------------------------------------------
        // Edge detect
		//
		// (curr XOR prev) AND prev → 1→0 transition only
        //  - Unpressed=1, Pressed=0 → detect new press this frame
        // ------------------------------------------------------------
        eor     prev_fire_bit                  // A := curr ⊕ prev (changed?)
        and     prev_fire_bit                  // A := (changed) ∧ prev → 1→0 edge → $10 else $00

        // ------------------------------------------------------------
        // Latch current FIRE bit for next frame
		//
        //  - A=$10 ⇒ new press this frame (Z=0)
        //  - A=$00 ⇒ no new press (Z=1)
        // ------------------------------------------------------------
        stx     prev_fire_bit
        rts
		
/*
procedure kbd_scan(savedTaskIdx):
    // Initialize cached snapshots to "no activity" (all lines released)
    keyboardColumnsSnapshot = ALL_LINES_IDLE      // equivalent to $FF
    keyboardRowsSnapshot    = ALL_LINES_IDLE      // equivalent to $FF

    // Optionally prime the delay counter
    if savedTaskIdx != 0:
        keyboardDelay = INITIAL_KEYBOARD_DELAY    // 16-bit value

    // 1) Poll joystick first (priority)
    joystickIsIdle = joy_latch()   // returns true if no joystick direction/fire is active

    if not joystickIsIdle:
        // Joystick is active → refresh delay and exit early
        keyboardDelay = INITIAL_KEYBOARD_DELAY
        return

    // 2) If delay is non-zero, decrement it and maybe synthesize a SPACE key
    if keyboardDelay != 0:
        keyboardDelay = keyboardDelay - 1

        if keyboardDelay == 0:
            // Special behavior: as soon as the delay reaches zero, pretend
            // the SPACE key was pressed and go straight to "publish"
            candidateKey = KEY_SPACE
            goto PUBLISH_GATE
        else:
            // Delay not yet expired; nothing more to do this call
            return

    // 3) Delay has expired and joystick is idle → probe keyboard matrix

    // 3a) Quick probe: check if *any* key/line is active
    anyLineActive = quickKeyboardProbe()
    if not anyLineActive:
        // No relevant input; reset some state and exit
        clearShiftMapping()
        invalidateLastKey()
        return

    // 3b) Precise snapshots: capture columns and rows
    keyboardColumnsSnapshot = captureColumnSnapshot()
    keyboardRowsSnapshot    = captureRowSnapshot()

    // 3c) Count zero bits in columns (active lines) and remember least-significant one
    (colZeroCountMinus1, colLeastSignificantIndex) =
        countZeroBits(keyboardColumnsSnapshot)

    if noZeros(colZeroCountMinus1):
        // No active columns → treat as no input
        clearShiftMapping()
        invalidateLastKey()
        return

    // Save column count and index
    columnActiveCountMinus1 = colZeroCountMinus1
    columnIndexLSB          = colLeastSignificantIndex

    // 3d) Count zeros for rows
    (rowZeroCountMinus1, rowLeastSignificantIndex) =
        countZeroBits(keyboardRowsSnapshot)

    if noZeros(rowZeroCountMinus1):
        // No active rows → treat as no input
        clearShiftMapping()
        invalidateLastKey()
        return

    rowActiveCountMinus1 = rowZeroCountMinus1
    rowIndexLSB          = rowLeastSignificantIndex

    // 3e) If either side has multiple active lines, we may need to apply
    // shift-aware masks to suppress ghosting
    if (rowActiveCountMinus1 > 0) or (columnActiveCountMinus1 > 0):
        applyShiftMasksIfNeeded()   // calls kbd_apply_shift_masks, then re-counts bits
        // After this call, we conceptually re-execute the "count bits" phase
        // and end up again with:
        //   columnIndexLSB, rowIndexLSB, columnActiveCountMinus1, rowActiveCountMinus1

    // 3f) Build matrix index and look up key
    matrixIndex = (columnIndexLSB * 8) + rowIndexLSB + keymapBaseOffset
    candidateKey = keymap[matrixIndex]

    // Ignore "unused" entries
    if candidateKey == KEY_IGNORED:
        clearShiftMapping()
        invalidateLastKey()
        return

    // 3g) De-duplicate: only publish when key changes
    if candidateKey == lastKeycode:
        // No new key
        return
    lastKeycode = candidateKey

    // 3h) Shift handling: if we just detected a Shift key,
    // enable shifted base and masks and exit (no key published yet)
    if candidateKey == KEY_LSHIFT:
        keymapBaseOffset = SHIFT_BASE_OFFSET
        columnMask       = LSHIFT_COLUMN_MASK
        rowMask          = LSHIFT_ROW_MASK
        return

    if candidateKey == KEY_RSHIFT:
        keymapBaseOffset = SHIFT_BASE_OFFSET
        columnMask       = RSHIFT_COLUMN_MASK
        rowMask          = RSHIFT_ROW_MASK
        return

    // 3i) Normal publish path
PUBLISH_GATE:
    if candidateKey == 0:
        // A=0 → nothing to publish
        return

    keycode   = candidateKey
    keyReady  = TRUE
    return

procedure applyShiftMasksIfNeeded():
    // If shifted mapping is not active (bit6 of keymapBaseOffset is clear),
    // skip adjustments and force an exit from the scan.
    if not shiftMappingEnabled(keymapBaseOffset):
        clearShiftMapping()
        return

    // Load precomputed masks set by the Shift key handlers
    columnAdjustmentMask = columnMask
    rowAdjustmentMask    = rowMask

    // Adjust columns only if there are multiple active columns
    if columnActiveCountMinus1 > 0:
        // Force masked columns to "inactive" (logic-high) so they no longer count
        keyboardColumnsSnapshot =
            keyboardColumnsSnapshot OR columnAdjustmentMask

    // Adjust rows only if there are multiple active rows
    if rowActiveCountMinus1 > 0:
        keyboardRowsSnapshot =
            keyboardRowsSnapshot OR rowAdjustmentMask

    // After masking, we recompute zero counts and least-significant indices
    (columnActiveCountMinus1, columnIndexLSB) =
        countZeroBits(keyboardColumnsSnapshot)

    (rowActiveCountMinus1, rowIndexLSB) =
        countZeroBits(keyboardRowsSnapshot)

    // Control then flows back into the “re-counted” logic in kbd_scan
    return

function countZeroBits(byteValue):
    // Scan bits from most significant to least significant.
    // We track how many bits are zero, and we remember the index
    // of the least-significant zero bit we see.

    zeroCount     = 0
    leastZeroIndex = NONE   // will end up 0..7 if any zero is found

    // Bits are indexed [7..0], 7=MSB, 0=LSB
    for bitIndex from 7 down to 0:
        if bitAt(byteValue, bitIndex) == 0:
            zeroCount += 1
            // Because we scan MSB-first but overwrite the index every time,
            // the final stored index will be the least-significant zero bit.
            leastZeroIndex = bitIndex

    if zeroCount == 0:
        // Encoded as A=$FF in the assembly
        return (NO_ZEROS_SENTINEL, leastZeroIndex)
    else:
        // Return (#zeros − 1), mirroring the original convention
        return (zeroCount - 1, leastZeroIndex)

function noZeros(countMinus1):
    return (countMinus1 == NO_ZEROS_SENTINEL)


function joy_latch() -> bool:
    // 1) Configure ports:
    //    - Port A as outputs, all lines driven HIGH (release matrix)
    //    - Port B as inputs
    setPortADirection(OUTPUT_ALL)
    writePortA(ALL_HIGH)
    setPortBDirection(INPUT_ALL)

    // 2) Read joystick raw state from Port B
    rawState = readPortB()
    joy_state = rawState            // stored globally

    // 3) Mask out joystick bits (directions + fire)
    masked = rawState AND JOYSTICK_MASK   // bits 0..4 only

    // 4) Interpret active-low:
    //    - masked == all-ones → no joystick switch is pressed
    //    - any bit 0          → at least one switch pressed
    if masked == JOYSTICK_MASK:
        joystickIsIdle = true
    else:
        joystickIsIdle = false

    return joystickIsIdle


function kbd_key_read() -> byte:
    if keyReady == FALSE:
        // No key pending; return without changing state
        return NO_KEY_AVAILABLE     // (caller can ignore this or check keyReady directly)

    // A key is waiting:
    keyReady = FALSE
    return keycode


function detect_fire_press_edge() -> byte:
    // FIRE bit is active-low:
    //   1 = released, 0 = pressed

    // 1) Extract current FIRE bit from global joystick state
    currentFireBit = joy_state AND FIRE_BIT_MASK    // usually mask = $10

    // 2) Edge detection: detect transition from released (1) to pressed (0).
    //
    // The original code uses:
    //   A = (current XOR previous) AND previous
    //
    // Truth table (for FIRE bit):
    //   prev=1, curr=1 → no change → result=0
    //   prev=1, curr=0 → falling edge → result=$10
    //   prev=0, curr=1 → rising edge → result=0
    //   prev=0, curr=0 → no change → result=0
    //
    // We preserve this behavior conceptually:
    fireChanged     = (currentFireBit != prev_fire_bit)
    wasReleasedLast = (prev_fire_bit != 0)          // i.e., bit was 1
    isPressedNow    = (currentFireBit == 0)

    if fireChanged and wasReleasedLast and isPressedNow:
        result = FIRE_BIT_MASK      // $10: newly pressed this frame
    else:
        result = 0                  // no new press

    // 3) Latch current FIRE bit for next frame’s comparison
    prev_fire_bit = currentFireBit

    // result is returned in A in the original; here we just return it
    return result

*/