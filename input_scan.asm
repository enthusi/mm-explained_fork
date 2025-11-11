/*
 * ===============================================================================
 * Module: Keyboard and joystick scanning
 *
 * Summary:
 *   This module provides a lightweight input layer for the C64 built over the CIA1 chip.
 *
 *   It prioritizes joystick responsiveness, throttles keyboard scanning via a
 *   small countdown, resolves a single key (with Shift handling), and exposes
 *   a simple “key ready” interface for consumers.
 *
 * Public routines:
 *   kbd_scan
 *       Orchestrates input sampling. First checks the joystick; if any joystick
 *       line is active it refreshes the scan delay and returns immediately to
 *       keep controls responsive. Otherwise, when the delay elapses, it probes
 *       the keyboard matrix, captures column/row snapshots, resolves one key
 *       (ignores #$5F “unused” entries and filters repeats), handles Shift by
 *       switching to the shifted half of the key table, and if a new key was
 *       found it writes the byte into kbd_keycode and sets kbd_key_ready.
 *
 *   joy_latch
 *       A fast probe of CIA1 Port B bits 0–4 (active-low). It releases the
 *       matrix by driving Port A high, configures Port B as inputs, reads PRB,
 *       stores the raw snapshot, then masks & compares against #%00011111 to
 *       set CPU flags. Contract: Z=1 if idle (no inputs active), Z=0 if any
 *       direction or fire is pressed; A holds the compare result.
 *
 *   kbd_key_read
 *       Consumer helper. If kbd_key_ready is set, it clears the flag and returns
 *       the byte in A (kbd_keycode). If no key is ready, it returns
 *       immediately without side effects. Intended usage: call this 
 *       after kbd_scan to fetch and consume keys one at a time.
 *
 * Private routines:
 * 	kbd_count_zero_bits
 * 		Counts 0-bits in A (MSB-first scan).
 *
 * Typical usage:
 *       ; one-time setup: point tables, ensure CIA1 is defaulted
 *   main_loop:
 *       JSR kbd_scan          ; advances internal state, may publish a key
 *       JSR kbd_key_read            ; A = key if ready, else returns quickly
 *       BCC .continue              ; (example branch; you may test kbd_key_ready)
 *   .continue:
 *       JMP main_loop
 *
 * Notes:
 *   • Joystick lines are active-low; a pressed switch reads as 0.
 *   • The keyboard is an 8×8 matrix; scanning toggles DDRA/DDR B roles to take
 *     column and row snapshots, then combines them into a table index.
 *   • A small delay gate reduces CPU usage and key bounce; joystick activity
 *     resets this delay to favor responsive game controls.
 * 	• Actual joystick input handling is not performed here, it merely preempts the keyboard if active.
 * ===============================================================================
 */

/*
 * ---------------------------------------------------------------
 * CIA1 Registers (used for joystick and keyboard scanning)
 * ---------------------------------------------------------------
 * DDRA  - Data Direction Register A - cia1_ddra
 *           Sets each bit of Port A as input (0) or output (1).
 *           Register located at address $DC02 on the C64
 *
 * DDRB  - Data Direction Register B - cia1_ddrb
 *           Same as DDRA but for Port B.
 *           Register located at address $DC03.
 *
 * PRA   - Port A Data Register - cia1_pra
 *           Holds output values for Port A when bits are outputs,
 *           or reads current pin states when bits are inputs.
 *           Register located at address $DC00.
 *
 * PRB   - Port B Data Register - cia1_prb
 *           Reads (or writes) data on Port B lines.
 *           For joystick/keyboard input, this is the register read
 *           to detect pressed lines.
 *           Register located at address $DC01.
 *
 * Usage notes:
 * ------------
 * - Port A and Port B together form an 8×8 input matrix shared by
 *   the keyboard and joysticks.
 * - DDRA/DDRB configure direction (1 = output, 0 = input).
 * - PRA/PRB hold or read the pin states depending on direction.
 * - For joystick polling:
 *    DDRA = $FF   → all outputs
 *    PRA  = $FF   → drive lines high (release)
 *    DDRB = $00   → all inputs
 *    read PRB     → 0 = pressed, 1 = released (active-low logic)
 */

/*
 * -------------------------------------------------------------------------------
 * How the keyboard matrix is read (via CIA1 Port A and Port B)
 * -------------------------------------------------------------------------------
 *
 * The C64 keyboard is a matrix of 8×8 switches: 8 “row” lines and 8 “column” lines.
 * Each key connects one specific row to one specific column when pressed.
 * These 16 lines are shared between the keyboard and the two joysticks.
 *
 * CIA1 Port A and Port B handle the matrix as follows:
 *
 *   Port A (PRA/DDRA) → drives the 8 row lines (outputs)
 *   Port B (PRB/DDRB) → reads the 8 column lines (inputs)
 *
 * The process:
 * 1) Configure directions:
 *        DDRA = $FF   → Port A as outputs (used to select one row at a time)
 *        DDRB = $00   → Port B as inputs  (used to read the column states)
 *
 * 2) To detect which key in a row is pressed:
 *        - Write a byte to Port A with only one bit cleared (0), all others set (1).
 *          Example: writing %11111110 drives row 0 low and releases the others.
 *        - Read Port B: each bit corresponds to a column.
 *          If a key in the active row is pressed, that column’s bit reads 0.
 *
 * 3) Repeat step 2 for each of the 8 rows, cycling the 0 across the bits in Port A.
 *        This scans all rows in turn, building a full 8×8 keyboard state map.
 *
 * Logic summary:
 *        Row line = output, column line = input.
 *        0 written to a row “selects” it.
 *        0 read from a column = key pressed (active-low).
 *        1 read = key released (line pulled high internally).
 *
 * Note:
 * - The joystick shares these lines; moving the joystick grounds certain matrix
 *   lines, which is why reading both simultaneously requires care.
 * - To poll the joystick alone, all rows (Port A) are driven high (%11111111)
 *   so that no keyboard rows are selected and only joystick switches are seen.
 * -------------------------------------------------------------------------------
 */
#importonce
#import "registers.inc"
#import "globals.inc"

/*
 * ===========================================
 * Public variables
 * ===========================================
 */

/*
 * ===========================================
 * Internal variables / state
 * ===========================================
 */
.label kbd_cols         	= $2057    // CIA1 capture of keyboard column lines
.label kbd_col_zeros_m1    = $2058    // (# of zero bits in columns) - 1, or #$FF if none
.label kbd_rows            = $2059    // CIA1 capture of keyboard row lines
.label kbd_row_zeros_m1    = $205A    // (# of zero bits in rows) - 1, or #$FF if none
.label kbd_lsb0_idx        = $205B    // output from kbd_count_zero_bits: lowest zero-bit index
.label kbd_col_lsb0_idx    = $205C    // least significant 0-bit column index
.label kbd_last_keycode    = $205D    // last accepted key; used to filter duplicates
.label kbd_keymap_base     = $205E    // 0 or $40: selects normal vs shifted key matrix
.label kbd_col_mask   		= $205F    // OR-mask applied to columns when shifted
.label kbd_row_mask   		= $2060    // OR-mask applied to rows when shifted
.label kbd_delay_lo        = $FE4D    // lo byte of polling delay
.label kbd_delay_hi        = $FE4E    // hi byte of polling delay

/*
 * ===========================================
 * Constants
 * ===========================================
 */
.const KBD_KEY_LSHIFT              = $7B
.const KBD_KEY_RSHIFT              = $7D
.const KBD_DELAY_INIT_HI		 	= $1C
.const KBD_DELAY_INIT_LO        	= $20
.const KBD_KEY_IGNORED             = $5F
.const KBD_LSHIFT_COL_MASK			= %1000_0000
.const KBD_RSHIFT_COL_MASK			= %0001_0000
.const KBD_LSHIFT_ROW_MASK			= %0000_0010
.const KBD_RSHIFT_ROW_MASK			= %0100_0000
.const KBD_KEYMAP_SHIFT_BASE		= $40
.const JOY2_MASK 					= %0001_1111
.const ALL_OUTPUTS					= $FF
.const ALL_INPUTS					= $00

* = $21BE
kbd_keymap:
/*
 * Matrix of key inputs mapped from the keycodes scanned
 * A value of #$5F is ignored. Function keys yield values in the $01–$08 range.
 * Special cases - the Function keys, which yield values in the 01-08 range
 */
.byte $C8,$33,$35,$37,$39,$2B,$5F,$31 // DEL		3 		5 	7 	9 	+ 	Pound	1
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

* = $2062
/*
 * ===========================================
 * Keyboard scan
 *
 * Summary:
 *   Prioritizes joystick responsiveness and throttles keyboard scans with a
 *   delay gate. If joystick activity is detected, refreshes the delay and
 *   returns immediately; otherwise probes the keyboard matrix, resolves a
 *   single key (with Shift handling and de-dup), and publishes it.
 *
 * Arguments:
 *   saved_task_idx		Optional gate: if zero, skip (re)priming the poll delay.
 *
 * Variables / State:
 *   kbd_delay_lo            Low byte of the 16-bit countdown that throttles
 *                           	keyboard scans (helps debounce and save CPU).
 *   kbd_delay_hi            High byte of the 16-bit countdown.
 *   kbd_cols           		Captured column snapshot (active-low; 0 means pulled).
 *   kbd_rows              	Captured row snapshot (active-low; 0 means pulled).
 *   kbd_col_zeros_m1       	(# of zero bits in columns)-1; $FF means none.
 *   kbd_row_zeros_m1       	(# of zero bits in rows)-1; $FF means none.
 *   kbd_col_lsb0_idx        Index of least-significant active (zero) column.
 *   kbd_lsb0_idx            Index of least-significant active (zero) row (set by counter).
 *   kbd_last_keycode       	Last published key; used to filter repeats.
 *   kbd_keymap              Keymap for normal and shifted halves (base + $40).
 *   kbd_keymap_base         Base offset into key matrix (0 normal, $40 shifted).
 *   kbd_col_mask     		Column OR-mask applied under Shift interplay.
 *   kbd_row_mask     		Row OR-mask applied under Shift interplay.
 *
 * I/O Registers:
 *   cia1_pra         		CIA1 PRA   - Port A data (drive/select).
 *   cia1_prb         		CIA1 PRB   - Port B data (sense).
 *   cia1_ddra    			CIA1 DDRA  - sets Port A direction.
 *   cia1_ddrb    			CIA1 DDRB  - sets Port B direction.
 *
 * Returns:
 *   kbd_keycode           	Set to resolved keycode when a new relevant key is found.
 *   kbd_key_ready           Set to #$01 when kbd_keycode is updated; else left at #$00.
 *   kbd_delay_lo/hi         Refreshed when joystick is active; decremented otherwise.
 *   Registers               No fixed return value guaranteed; Z/A may reflect last compare.
 *
 * Description:
 *   On entry, cached matrix snapshots are cleared to “no activity” (all 1s). 
 *
 * 	If required, the routine (re)primes a 16-bit poll delay; on subsequent calls it first polls the
 *   joystick (active-low on PRB bits 0–4). Any joystick activity refreshes the delay and
 *   returns immediately to keep controls responsive.
 *
 *   When the delay elapses, the routine performs a quick probe of the keyboard matrix:
 *   DDRA=$FF, DDRB=$00, PRA=$00, then checks PRB against $FF. 
 *
 * 	If activity is present, it takes two precise snapshots: 
 * 	columns (PRB while A drives) and rows (PRA while B drives) - and counts zero bits in each, 
 * 	recording the least-significant active row/column.
 *
 * 	If either side shows multiple actives, it applies shift-aware OR masks to suppress ambiguous lines and re-counts.
 *
 *   The final matrix index is (column*8 + row + base), 
 * 	with base taken from kbd_keymap_base (0 or $40). 
 *
 * 	Keys mapping to #$5F are ignored.
 * 	Identical repeats are filtered using kbd_last_keycode. 
 * 	If the key is Left/Right Shift, the routine arms shifted mapping (sets kbd_keymap_base and 
 * 	adjustment masks) and exits.
 *
 * 	Otherwise it writes the key into kbd_keycode and asserts kbd_key_ready.
 * ===========================================
 */
kbd_scan:
       ldy #$FF
       sty kbd_cols           	// clear cached column snapshot to “no activity” (all 1s = pulled-up idle)
       sty kbd_rows             // clear cached row snapshot to “no activity”

       /*
        * ----------------------------------------
        * Optional: skip (re)initializing the poll delay on entry.
        *
        * If saved_task_idx == 0 we proceed to input polling immediately,
        * otherwise we (re)prime the 16-bit delay counter.
        * ----------------------------------------
        */
       lda saved_task_idx
       beq poll_input

       /*
        * ----------------------------------------
        * Prime the 16-bit poll delay (hi:lo = $1C:$20).
        *
        * The keyboard will only be scanned once this countdown elapses,
        * which helps throttle CPU usage and tame key bounce.
        * ----------------------------------------
        */
       lda #KBD_DELAY_INIT_LO
       sta kbd_delay_lo
       lda #KBD_DELAY_INIT_HI
       sta kbd_delay_hi
       // ----------------------------------------

poll_input:
       /*
        * ----------------------------------------
        * Poll joystick first (priority). 
        *
        * joy_latch leaves Z=1 when idle,
        * Z=0 when any joystick line (bits 0–4) is active (pressed = low).
        * ----------------------------------------
        */
       jsr joy_latch
       beq kbd_delay_check          // joystick idle → continue to delay/keyboard

       /*
        * ----------------------------------------
        * Joystick active: refresh the delay and kbd_exit_2 immediately to keep
        * joystick responsiveness high. Keyboard scan is deferred.
        * ----------------------------------------
        */
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

       // Otherwise, decrement the 16-bit delay (borrow from hi when lo underflows).
       lda kbd_delay_lo
       bne delay_decrease_lo
       dec kbd_delay_hi
delay_decrease_lo:
       dec kbd_delay_lo

       /*
        * ----------------------------------------
        * Just reached zero? Take the fast path:
        *
        * fall through the “publish-or-kbd_exit_2” gate by forcing A ≠ 0 so the
        * later BEQ won’t trigger the early kbd_exit_2 path.
        * ----------------------------------------
        */
       lda kbd_delay_lo
       ora kbd_delay_hi
       bne kbd_probe_matrix
       lda #$20                        // Return the space bar key code (simulate a pause?) to steer kbd_publish_gate
       jmp kbd_publish_gate

kbd_probe_matrix:
       /*
        * ----------------------------------------
        * Read keyboard matrix using CIA1 (quick probe, then precise snapshots)
        *
        * Quick probe: configure matrix and test if ANY key is pressed.
        * 1) DDRA = $FF (Port A = outputs: drive columns)
        *    DDRB = $00 (Port B = inputs: read rows)
        * 2) PRA  = $00 (drive all columns LOW to enable sensing)
        * 3) Read PRB and compare to $FF:
        *       if PRB != $FF → at least one row was pulled LOW → some key/line active.
        * ----------------------------------------
        */
       ldy #ALL_OUTPUTS
       sty cia1_ddra     	// DDRA = $FF (A as outputs)
       lda #ALL_INPUTS
       sta cia1_ddrb     	// DDRB = $00 (B as inputs)
       sta cia1_pra         // PRA  = $00 (select all columns: active-low)
       cpy cia1_prb         // PRB vs $FF ? (any 0 → activity)
       bne kbd_input_detected
       jmp kbd_no_input

kbd_input_detected:
       /*
        * ----------------------------------------
        * Precise capture #1: COLUMN snapshot.
        *
        * Keep A driving columns LOW and read B:
        *   PRB bits clear (0) mark the columns whose lines are pulled LOW through a pressed key.
        * ----------------------------------------
        */
       lda #ALL_OUTPUTS
       sta cia1_ddra     	// DDRA = $FF (A still outputs)
       lda #ALL_INPUTS
       sta cia1_ddrb     	// DDRB = $00 (B inputs)
       sta cia1_pra         // PRA  = $00 (all columns selected)
       lda cia1_prb         // PRB  = column sense
       sta kbd_cols

       /*
        * ----------------------------------------
        * Precise capture #2: ROW snapshot.
        *
        * Swap roles: drive rows (B) LOW and read columns (A).
        *   PRA bits clear (0) mark the rows whose lines are pulled LOW through a pressed key.
        * ----------------------------------------
        */
       lda #ALL_OUTPUTS
       sta cia1_ddrb     	// DDRB = $FF (B outputs)
       lda #ALL_INPUTS
       sta cia1_ddra     	// DDRA = $00 (A inputs)
       sta cia1_prb         // PRB  = $00 (drive all rows LOW)
       lda cia1_pra         // PRA  = row sense
       sta kbd_rows

kbd_count_bits:
       /*
        * ----------------------------------------
        * Count 0-bits in the captured COLUMN snapshot.
        *
        * On return from kbd_count_zero_bits:
        *   A  = (#zero_bits - 1), or #$FF if there are no zeros
        *   kbd_lsb0_idx = index (0..7) of the least-significant zero bit (if any)
        * ----------------------------------------
        */
       lda kbd_cols
       jsr kbd_count_zero_bits
       bmi kbd_no_input         		// no zero bits → no active columns → abort
       sta kbd_col_zeros_m1             // save (#zeros - 1) for columns

       // Latch the column index of the least-significant active line.
       lda kbd_lsb0_idx
       sta kbd_col_lsb0_idx

       // Repeat the same zero-bit count for the ROW snapshot.
       lda kbd_rows
       jsr kbd_count_zero_bits
       bmi kbd_no_input         		// no zero bits → no active rows → abort
       sta kbd_row_zeros_m1             // save (#zeros - 1) for rows

       /*
        * ----------------------------------------
        * If either rows or columns have *multiple* active lines, we need to adjust
        * the matrix (e.g., shift masks / ghosting protection). 
        *
        * Recall:
        *   result 0  → exactly one active line
        *   result >0 → multiple active lines
        *   result $FF→ none (already handled by BMI above)
        *
        * Here A holds kbd_row_zeros_m1; OR with kbd_col_zeros_m1:
        *   nonzero → at least one side has multiple actives → adjust
        *   zero    → both sides have exactly one active line → no adjustment
        * ----------------------------------------
        */
       ora kbd_col_zeros_m1
       bne kbd_apply_shift_masks

       /*
        * ----------------------------------------
        * Build the key matrix index: index = (column * 8) + row + base_offset
        * where base_offset is 0 for normal, $40 for shifted matrix.
        * ----------------------------------------
        */
       lda kbd_col_lsb0_idx
       asl 
       asl 
       asl                                   	// A = column * 8
       clc
       adc kbd_lsb0_idx                     	// + row
       adc kbd_keymap_base                      // + base (0 or $40)
       // Put the final index in Y for table lookup.
       tay

       /*
        * ----------------------------------------
        * Lookup keycode and ignore “unused” entries (#$5F).
        * ----------------------------------------
        */
       lda kbd_keymap,Y
       cmp #KBD_KEY_IGNORED
       beq kbd_no_input

       /*
        * ----------------------------------------
        * De-duplicate: publish only when the key differs from the last one.
        * ----------------------------------------
        */
       cmp kbd_last_keycode
       sta kbd_last_keycode
       beq kbd_exit

       /*
        * ----------------------------------------
        * Shift handling: if the detected key *is* a Shift, arm shifted mapping
        * (set base offset and adjustment masks) and kbd_exit_2; the next scan will
        * use the shifted key matrix.
        * ----------------------------------------
        */
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

       /*
        * ----------------------------------------
        * Ignore path: treat this scan as “no relevant input” and reset transient state.
        * ----------------------------------------
        */
kbd_no_input:
       lda #$00
       sta kbd_keymap_base               // clear shifted mapping (return to base matrix)
       lda #$FF
       sta kbd_last_keycode        		 // invalidate last key so the next real key will publish

kbd_exit:
       /*
        * Funnel to the common kbd_exit_2 gate with Z=1:
        * load A=0 so the following BEQ will skip publishing and just kbd_exit_2.
        */
       lda #$00

kbd_publish_gate:
       /*
        * ----------------------------------------
        * Publish gate:
        *   Z=1 (A==0)  → no key to publish → kbd_exit_2
        *   Z=0 (A!=0)  → publish A as the keycode and set the “ready” flag
        * ----------------------------------------
        */
       beq kbd_exit_2
       sta kbd_keycode             		// publish keycode
       lda #$01
       sta kbd_key_ready                // mark “key available”

kbd_exit_2:
       rts

/*
 * ===========================================
 * Matrix offset & bitmask adjustment
 *
 *   When a Shift key has been detected, the decoder switches to a shifted keymap by setting
 *   kbd_keymap_base’s bit 6 and precomputing adjustment masks for columns/rows. 
 *
 * 	This section checks that bit (via BIT/BVC) and, if active, conditionally ORs the snapshots 
 * 	with the masks - but only when the corresponding “clear count” indicates multiple active lines. 
 * 	This helps suppress ambiguous multi-line activations (e.g., shift interplay / ghosting). 
 *
 * 	After masking, control transfers to kbd_count_bits to re-derive the least-significant active 
 * 	row/column indices.
 * ===========================================
 */
kbd_apply_shift_masks:
       /*
        * ----------------------------------------
        * Shifted mapping enabled?  
        *
        * BIT copies bit6 of kbd_keymap_base into V.
        * If V=0 (bit6 clear → offset==0), no adjustment needed → kbd_exit_2.
        * ----------------------------------------
        */
       bit kbd_keymap_base
       bvc kbd_exit_3

       /*
        * ----------------------------------------
        * Load precomputed adjustment masks (set by L/R Shift handlers):
        *   X = mask for columns, Y = mask for rows.
        * ----------------------------------------
        */
       ldx kbd_col_mask
       ldy kbd_row_mask
       jmp adjust_column_bits

kbd_exit_3:
       jmp kbd_exit

adjust_column_bits:
       /*
        * ----------------------------------------
        * Decide whether to adjust the captured COLUMN bits.
        *
        * kbd_col_zeros_m1 holds (number_of_zero_bits - 1).
        *   = 0  → exactly one active (zero) column
        *   > 0  → multiple active columns
        * We only apply the mask when there are multiple actives (ghosting/shift interplay).
        * ----------------------------------------
        */
       lda kbd_col_zeros_m1
       beq adjust_row_bits              // exactly one → skip masking columns
       txa
       ora kbd_cols              		// force masked columns high (ignore them)
       sta kbd_cols

adjust_row_bits:
       /*
        * ----------------------------------------
        * Same policy for ROW bits:
        *
        *   kbd_row_zeros_m1 = (number_of_zero_bits - 1)
        * Apply row mask only when multiple rows are active.
        * ----------------------------------------
        */
       lda kbd_row_zeros_m1
       beq count_bits_2                 // exactly one → skip masking rows
       tya
       ora kbd_rows                 	// force masked rows high (ignore them)
       sta kbd_rows

count_bits_2:
       // Recount zeros after adjustments to re-derive row/column indices.
       jmp kbd_count_bits
/*
 * ===========================================
 * Count zero bits
 *
 * Counts 0-bits in A (MSB-first scan). Returns (#zeros-1) or #$FF if none.
 * Also records the least-significant zero bit index into kbd_lsb0_idx.
 *
 * Arguments:
 *   A                               Input bitfield to scan (8 bits).
 *
 * Returns:
 *   A                               (# of zero bits) - 1, or #$FF when no zeros.
 *   kbd_lsb0_idx					Index (0..7) of least-significant zero bit if any.
 * ===========================================
 */
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
 * ===========================================
 * Latch joystick #2 state 
 *
 * Reads joystick #2 via CIA1. Z=0 when any input is active. Stores raw state.
 *
 * Returns:
 *   A                                Masked joystick state (bits 0–4).
 *   Z-flag                           Clear when any line is active; set otherwise.
 *
 * Steps to read joystick (CIA1) — active-low logic
 *
 * 1) Configure port directions:
 *      DDRA = $FF  (Port A outputs)
 *      DDRB = $00  (Port B inputs)
 *
 * 2) Release the matrix so joystick lines can be read cleanly:
 *      PRA = $FF   (drive all Port A lines high)
 *
 * 3) Read PRB and mask joystick bits:
 *      value = PRB & $1F
 *
 * 4) Interpret (active-low):
 *      value == $1F → no inputs active
 *      any bit 0    → some input active
 *
 * ===========================================
 */
joy_latch:
       /*
        * ----------------------------------------
        * CIA1 setup: make Port A all outputs and drive them high.
        *
        * This releases the keyboard/joystick lines to the pull-ups so Port B can be sampled.
        * ----------------------------------------
        */
       ldy #ALL_OUTPUTS
       sty cia1_ddra
       /*
        * ----------------------------------------
        * Drive Port A high (read-all mask for matrix scanning / joystick sense).
        * ----------------------------------------
        */
       sty cia1_pra

       /*
        * ----------------------------------------
        * Configure Port B as inputs (we'll read joystick here).
        * ----------------------------------------
        */
       lda #ALL_INPUTS
       sta cia1_ddrb

       /*
        * ----------------------------------------
        * Read Port B: joystick lines are active-low.
        *
        * Bits 0–4 correspond to directions + fire (1 = idle, 0 = pressed).
        * ----------------------------------------
        */
       lda cia1_prb
       sta joy_state

       /*
        * ----------------------------------------
        * Keep only joystick bits, then compare against the joystick bitmask to set flags:
        *   if all five bits are 1 → no inputs activated → Z = 1
        *   if any bit is 0       → some input activated → Z = 0
        * ----------------------------------------
        */
       and #JOY2_MASK
       cmp #JOY2_MASK
       rts
       brk


/*
 * ===========================================
 * Process a key
 *
 * Consumer helper: if a key is ready, clear the ready flag and return it in A.
 * Otherwise, return immediately.
 *
 * Returns:
 *   A                                kbd_keycode when a key was ready.
 *   kbd_key_ready                    Cleared to #$00 when a key is consumed.
 * ===========================================
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
       lda #$00
       sta kbd_key_ready
       // Return the key in A
       lda kbd_keycode
       rts


/*
 * C64 INPUT MODULE - FLOW
 * =======================================================
 *
 * [kbd_scan]
 * ---------------
 * ENTRY
 *   │
 *   ├─ Clear cached snapshots → kbd_cols=$FF, kbd_rows=$FF
 *   │
 *   ├─ (Optional) Prime delay if saved_task_idx ≠ 0
 *   │
 *   ├─ JSR joy_latch  →  Z=0 (active) / Z=1 (idle)
 *   │
 *   ├─ IF Z=0 (joystick active) THEN
 *   │      ├─ Refresh kbd_delay_hi:lo
 *   │      └─ RETURN
 *   │
 *   └─ ELSE (Z=1: joystick idle)
 *          │
 *          ├─ IF (kbd_delay_hi:lo == 0) THEN
 *          │      └─ GOTO KEYBOARD_SCAN
 *          │
 *          ├─ Decrement 16-bit kbd_delay_hi:lo
 *          │
 *          ├─ IF (kbd_delay_hi:lo == 0) THEN
 *          │      └─ GOTO KEYBOARD_SCAN
 *          │
 *          └─ RETURN
 *
 *
 * KEYBOARD_SCAN
 * -------------
 *   ├─ Quick probe:
 *   │    DDRA=$FF (A out), DDRB=$00 (B in), PRA=$00 (select all columns)
 *   │    IF (PRB == $FF) THEN
 *   │       └─ NO_RELEVANT_INPUT
 *   │
 *   ├─ Precise snapshots:
 *   │    1) Columns: (A drives low, B reads) → kbd_cols = PRB
 *   │    2) Rows:    (B drives low, A reads) → kbd_rows    = PRA
 *   │
 *   ├─ Count zeros:
 *   │    col = kbd_count_zero_bits(kbd_cols)
 *   │          (A = (#zeros−1); kbd_lsb0_idx = least-significant zero)
 *   │          → if A=$FF → NO_RELEVANT_INPUT
 *   │          → save kbd_col_zeros_m1, save kbd_col_lsb0_idx
 *   │
 *   │    row = kbd_count_zero_bits(kbd_rows)
 *   │          (A = (#zeros−1); kbd_lsb0_idx = least-significant zero)
 *   │          → if A=$FF → NO_RELEVANT_INPUT
 *   │          → save kbd_row_zeros_m1
 *   │
 *   ├─ Need adjustment?  (row>0 OR col>0)
 *   │    ├─ YES → kbd_apply_shift_masks → then re-count (same as above)
 *   │    └─ NO  → continue
 *   │
 *   ├─ Build index:
 *   │    idx = (kbd_col_lsb0_idx * 8) + (kbd_lsb0_idx) + kbd_keymap_base  ; base 0 or $40
 *   │    Y   = idx
 *   │
 *   ├─ Lookup:
 *   │    A = kbd_keymap[Y]
 *   │    IF (A == $5F) THEN → NO_RELEVANT_INPUT
 *   │
 *   ├─ De-duplicate:
 *   │    IF (A == kbd_last_keycode) THEN → RETURN
 *   │    kbd_last_keycode = A
 *   │
 *   ├─ Shift handling:
 *   │    IF (A == KBD_KEY_LSHIFT) THEN
 *   │        kbd_keymap_base=$40; col_mask=%1000_0000; row_mask=%0000_0010; RETURN
 *   │    IF (A == KBD_KEY_RSHIFT) THEN
 *   │        kbd_keymap_base=$40; col_mask=%0001_0000; row_mask=%0100_0000; RETURN
 *   │
 *   └─ Publish:
 *        kbd_keycode = A
 *        kbd_key_ready    = #$01
 *        RETURN
 *
 *
 * NO_RELEVANT_INPUT
 * -----------------
 *   kbd_keymap_base       = #$00
 *   kbd_last_keycode= #$FF
 *   RETURN
 *
 *
 * [kbd_apply_shift_masks]
 * --------------------------
 *   IF (kbd_keymap_base bit6 == 0) THEN RETURN
 *   X = kbd_col_mask
 *   Y = kbd_row_mask
 *   IF (kbd_col_zeros_m1 > 0) THEN kbd_cols |= X   ; mask columns when multiple active
 *   IF (kbd_row_zeros_m1 > 0) THEN kbd_rows   |= Y   ; mask rows    when multiple active
 *   RETURN (to re-count step in KEYBOARD_SCAN)
 *
 *
 * [joy_latch]
 * ---------------
 *   DDRA=$FF; PRA=$FF     ; release matrix (drive A high)
 *   DDRB=$00               ; Port B inputs
 *   raw = PRB
 *   joy_state = raw
 *   mask = raw & $1F
 *   Z = (mask == $1F)      ; Z=1 idle, Z=0 any pressed
 *   RETURN
 *
 *
 * [kbd_key_read]
 * -------------
 *   IF (kbd_key_ready == 0) THEN RETURN
 *   kbd_key_ready = 0
 *   A = kbd_keycode
 *   RETURN
 */
