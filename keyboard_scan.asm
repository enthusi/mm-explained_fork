;===============================================================================
; Module: Keyboard scanning (CIA1-based)
;
; Summary:
;   This module provides a lightweight input layer for the C64 built over CIA1.
;   It prioritizes joystick responsiveness, throttles keyboard scanning via a
;   small countdown, resolves a single key (with Shift handling), and exposes
;   a simple “key ready” interface for consumers.
;
; Public routines:
;   scan_keyboard
;       Orchestrates input sampling. First checks the joystick; if any joystick
;       line is active it refreshes the scan delay and returns immediately to
;       keep controls responsive. Otherwise, when the delay elapses, it probes
;       the keyboard matrix, captures column/row snapshots, resolves one key
;       (ignores #$5F “unused” entries and filters repeats), handles Shift by
;       switching to the shifted half of the key table, and if a new key was
;       found it writes the byte into key_code_scanned and sets keycode_ready.
;
;   poll_joystick
;       A fast probe of CIA1 Port B bits 0–4 (active-low). It releases the
;       matrix by driving Port A high, configures Port B as inputs, reads PRB,
;       stores the raw snapshot, then masks & compares against #%00011111 to
;       set CPU flags. Contract: Z=1 if idle (no inputs active), Z=0 if any
;       direction or fire is pressed; A holds the compare result.
;
;   process_key
;       Consumer helper. If keycode_ready is set, it clears the flag and returns
;       the byte in A (key_code_scanned). If no key is ready, it returns
;       immediately without side effects. Intended usage: call this 
;       after scan_keyboard to fetch and consume keys one at a time.
;
; Typical usage:
;       ; one-time setup: point tables, ensure CIA1 is defaulted
;   main_loop:
;       JSR scan_keyboard          ; advances internal state, may publish a key
;       JSR process_key            ; A = key if ready, else returns quickly
;       BCC .continue              ; (example branch; you may test keycode_ready)
;   .continue:
;       JMP main_loop
;
; Notes:
;   • Joystick lines are active-low; a pressed switch reads as 0.
;   • The keyboard is an 8×8 matrix; scanning toggles DDRA/DDR B roles to take
;     column and row snapshots, then combines them into a table index.
;   • A small delay gate reduces CPU usage and key bounce; joystick activity
;     resets this delay to favor responsive game controls.
;	• Actual joystick input handling is not performed here, it merely preempts the keyboard if active.
;===============================================================================
;CIA1 Register Acronyms (used for joystick and keyboard scanning)
; ---------------------------------------------------------------
; DDRA  - Data Direction Register A - cia1_port_a_data_direction_reg
;           Sets each bit of Port A as input (0) or output (1).
;           Register located at address $DC02 on the C64
;
; DDRB  - Data Direction Register B - cia1_port_b_data_direction_reg
;           Same as DDRA but for Port B.
;           Register located at address $DC03.
;
; PRA   - Port A Data Register - cia1_port_a_data_register
;           Holds output values for Port A when bits are outputs,
;           or reads current pin states when bits are inputs.
;           Register located at address $DC00.
;
; PRB   - Port B Data Register - cia1_port_b_data_register
;           Reads (or writes) data on Port B lines.
;           For joystick/keyboard input, this is the register read
;           to detect pressed lines.
;           Register located at address $DC01.
;
; Usage notes:
; ------------
; - Port A and Port B together form an 8×8 input matrix shared by
;   the keyboard and joysticks.
; - DDRA/DDRB configure direction (1 = output, 0 = input).
; - PRA/PRB hold or read the pin states depending on direction.
; - For joystick polling:
;    DDRA = $FF   → all outputs
;    PRA  = $FF   → drive lines high (release)
;    DDRB = $00   → all inputs
;    read PRB     → 0 = pressed, 1 = released (active-low logic)

;-------------------------------------------------------------------------------
; How the keyboard matrix is read (via CIA1 Port A and Port B)
;
; The C64 keyboard is a matrix of 8×8 switches: 8 “row” lines and 8 “column” lines.
; Each key connects one specific row to one specific column when pressed.
; These 16 lines are shared between the keyboard and the two joysticks.
;
; CIA1 Port A and Port B handle the matrix as follows:
;
;   Port A (PRA/DDRA) → drives the 8 row lines (outputs)
;   Port B (PRB/DDRB) → reads the 8 column lines (inputs)
;
; The process:
; 1) Configure directions:
;        DDRA = $FF   → Port A as outputs (used to select one row at a time)
;        DDRB = $00   → Port B as inputs  (used to read the column states)
;
; 2) To detect which key in a row is pressed:
;        - Write a byte to Port A with only one bit cleared (0), all others set (1).
;          Example: writing %11111110 drives row 0 low and releases the others.
;        - Read Port B: each bit corresponds to a column.
;          If a key in the active row is pressed, that column’s bit reads 0.
;
; 3) Repeat step 2 for each of the 8 rows, cycling the 0 across the bits in Port A.
;        This scans all rows in turn, building a full 8×8 keyboard state map.
;
; Logic summary:
;        Row line = output, column line = input.
;        0 written to a row “selects” it.
;        0 read from a column = key pressed (active-low).
;        1 read = key released (line pulled high internally).
;
; Note:
; - The joystick shares these lines; moving the joystick grounds certain matrix
;   lines, which is why reading both simultaneously requires care.
; - To poll the joystick alone, all rows (Port A) are driven high (%11111111)
;   so that no keyboard rows are selected and only joystick switches are seen.
;-------------------------------------------------------------------------------


;===========================================
; Variables
;===========================================
key_code_scanned         = $0030    ; ZP: published keycode when ready
joystick_inputs          = $0033    ; ZP: snapshot of joystick mask (bits 0–4)
keyb_column_bits         = $2057    ; CIA1 capture of keyboard column lines
col_bits_clear_count     = $2058    ; (# of zero bits in columns) - 1, or #$FF if none
keyb_row_bits            = $2059    ; CIA1 capture of keyboard row lines
row_bits_clear_count     = $205A    ; (# of zero bits in rows) - 1, or #$FF if none
zero_lsb_index           = $205B    ; output from count_zero_bits: lowest zero-bit index
zero_lsb_column          = $205C    ; least significant 0-bit column index
last_keycode_scanned     = $205D    ; last accepted key; used to filter duplicates
matrix_offset            = $205E    ; 0 or $40: selects normal vs shifted key matrix
col_adjustment_bitmask   = $205F    ; OR-mask applied to columns when shifted
row_adjustment_bitmask   = $2060    ; OR-mask applied to rows when shifted
keycode_ready            = $2061    ; flag: 1 when key_code_scanned is valid
poll_delay_lo            = $FE4D    ; lo byte of polling delay
poll_delay_hi            = $FE4E    ; hi byte of polling delay
saved_script_index       = $FEA6    ; external flag controlling delay init

cia1_port_a_data_register        = $DC00   ; PRA — CIA1 Port A data register
                                           ; Holds current output values when bits are set as outputs,
                                           ; or reads inputs when bits are configured as inputs.
                                           ; Used for keyboard column drive and joystick sense.

cia1_port_b_data_register        = $DC01   ; PRB — CIA1 Port B data register
                                           ; Reads the current logic levels on Port B input lines.
                                           ; On the C64, bits 0–4 correspond to joystick #2 inputs (active low),
                                           ; and all 8 bits participate in the keyboard matrix scan.

cia1_port_a_data_direction_reg   = $DC02   ; DDRA — CIA1 Port A Data Direction Register
                                           ; Each bit controls the direction of the corresponding line:
                                           ;   1 = output, 0 = input.
                                           ; Setting DDRA=$FF makes all Port A lines outputs.

cia1_port_b_data_direction_reg   = $DC03   ; DDRB — CIA1 Port B Data Direction Register
                                           ; Works like DDRA, but for Port B.
                                           ; Setting DDRB=$00 makes all Port B lines inputs.


KEY_LSHIFT               = #$7B
KEY_RSHIFT               = #$7D
KEY_SCAN_DELAY_HI		 = #$1C
KEY_SCAN_DELAY_LO        = #$20
IGNORED_KEY              = #$5F

* = $2062
;===========================================
; Keyboard scan
;
; Summary:
;   Prioritizes joystick responsiveness and throttles keyboard scans with a
;   delay gate. If joystick activity is detected, refreshes the delay and
;   returns immediately; otherwise probes the keyboard matrix, resolves a
;   single key (with Shift handling and de-dup), and publishes it.
;
; Arguments:
;   saved_script_index ($FEA6)        Optional gate: if zero, skip (re)priming the poll delay.
;   poll_delay_lo ($FE4D)             Low byte of the 16-bit countdown that throttles
;                                     keyboard scans (helps debounce and save CPU).
;   poll_delay_hi ($FE4E)             High byte of the 16-bit countdown.
;
; Variables / state:
;   keyb_column_bits ($2057)          Captured column snapshot (active-low; 0 means pulled).
;   keyb_row_bits ($2059)             Captured row snapshot (active-low; 0 means pulled).
;   col_bits_clear_count ($2058)      (# of zero bits in columns)-1; $FF means none.
;   row_bits_clear_count ($205A)      (# of zero bits in rows)-1; $FF means none.
;   zero_lsb_column ($205C)           Index of least-significant active (zero) column.
;   zero_lsb_index ($205B)            Index of least-significant active (zero) row (set by counter).
;   last_keycode_scanned ($205D)      Last published key; used to filter repeats.
;   matrix_offset ($205E)             Base offset into key matrix (0 normal, $40 shifted).
;   col_adjustment_bitmask ($205F)    Column OR-mask applied under Shift interplay.
;   row_adjustment_bitmask ($2060)    Row OR-mask applied under Shift interplay.
;   key_matrix (table)                Keymap for normal and shifted halves (base + $40).
;
; I/O Registers:
;   cia1_port_a_data_direction_reg    CIA1 DDRA ($DC02) — sets Port A direction.
;   cia1_port_b_data_direction_reg    CIA1 DDRB ($DC03) — sets Port B direction.
;   cia1_port_a_data_register         CIA1 PRA  ($DC00) — Port A data (drive/select).
;   cia1_port_b_data_register         CIA1 PRB  ($DC01) — Port B data (sense).
;
; Returns:
;   key_code_scanned ($0030)          Set to resolved keycode when a new relevant key is found.
;   keycode_ready ($2061)             Set to #$01 when key_code_scanned is updated; else left at #$00.
;   poll_delay_lo/hi                  Refreshed when joystick is active; decremented otherwise.
;   Registers                         No fixed return value guaranteed; Z/A may reflect last compare.
;
; Description:
;   On entry, cached matrix snapshots are cleared to “no activity” (all 1s). If required,
;   the routine (re)primes a 16-bit poll delay; on subsequent calls it first polls the
;   joystick (active-low on PRB bits 0–4). Any joystick activity refreshes the delay and
;   returns immediately to keep controls responsive.
;
;   When the delay elapses, the routine performs a quick probe of the keyboard matrix:
;   DDRA=$FF, DDRB=$00, PRA=$00, then checks PRB against $FF. If activity is present,
;   it takes two precise snapshots — columns (PRB while A drives) and rows (PRA while B
;   drives) — and counts zero bits in each, recording the least-significant active row/
;   column. If either side shows multiple actives, it applies shift-aware OR masks to
;   suppress ambiguous lines and re-counts.
;
;   The final matrix index is (column*8 + row + base), with base taken from matrix_offset
;   (0 or $40). Keys mapping to #$5F are ignored; identical repeats are filtered using
;   last_keycode_scanned. If the key is Left/Right Shift, the routine arms shifted mapping
;   (sets matrix_offset and adjustment masks) and exits; otherwise it writes the key into
;   key_code_scanned and asserts keycode_ready.
;===========================================
scan_keyboard:
       LDY #$FF
       STY keyb_column_bits           ; clear cached column snapshot to “no activity” (all 1s = pulled-up idle)
       STY keyb_row_bits              ; clear cached row snapshot to “no activity”

       ;----------------------------------------
       ; Optional: skip (re)initializing the poll delay on entry.
	   ;
       ; If saved_script_index == 0 we proceed to input polling immediately,
       ; otherwise we (re)prime the 16-bit delay counter.
       ;----------------------------------------
       LDA saved_script_index
       BEQ poll_input

       ;----------------------------------------
       ; Prime the 16-bit poll delay (hi:lo = $1C:$20).
	   ;
       ; The keyboard will only be scanned once this countdown elapses,
       ; which helps throttle CPU usage and tame key bounce.
       ;----------------------------------------
       LDA KEY_SCAN_DELAY_LO
       STA poll_delay_lo
       LDA KEY_SCAN_DELAY_HI
       STA poll_delay_hi
       ;----------------------------------------

poll_input:
       ;----------------------------------------
       ; Poll joystick first (priority). 
	   ;
	   ; poll_joystick leaves Z=1 when idle,
       ; Z=0 when any joystick line (bits 0–4) is active (pressed = low).
       ;----------------------------------------
       JSR poll_joystick
       BEQ delay_count_check          ; joystick idle → continue to delay/keyboard

       ;----------------------------------------
       ; Joystick active: refresh the delay and exit immediately to keep
       ; joystick responsiveness high. Keyboard scan is deferred.
       ;----------------------------------------
       LDA KEY_SCAN_DELAY_LO
       STA poll_delay_lo
       LDA KEY_SCAN_DELAY_HI
       STA poll_delay_hi
       RTS

delay_count_check:
       ; If the 16-bit delay is zero, scan the keyboard now.
       LDA poll_delay_lo
       ORA poll_delay_hi
       BEQ read_input

       ; Otherwise, decrement the 16-bit delay (borrow from hi when lo underflows).
       LDA poll_delay_lo
       BNE delay_decrease_lo
       DEC poll_delay_hi
delay_decrease_lo:
       DEC poll_delay_lo

       ;----------------------------------------
       ; Just reached zero? Take the fast path:
	   ;
       ; fall through the “publish-or-exit” gate by forcing A ≠ 0 so the
       ; later BEQ won’t trigger the early exit path.
       ;----------------------------------------
       LDA poll_delay_lo
       ORA poll_delay_hi
       BNE read_input
       LDA #$20                        ; Return the space bar key code (simulate a pause?) to steer direct_exit_check
       JMP direct_exit_check

read_input:
       ;----------------------------------------
       ; Read keyboard matrix using CIA1 (quick probe, then precise snapshots)
	   ;
       ; Quick probe: configure matrix and test if ANY key is pressed.
       ; 1) DDRA = $FF (Port A = outputs: drive columns)
       ;    DDRB = $00 (Port B = inputs: read rows)
       ; 2) PRA  = $00 (drive all columns LOW to enable sensing)
       ; 3) Read PRB and compare to $FF:
       ;       if PRB != $FF → at least one row was pulled LOW → some key/line active.
       ;----------------------------------------
       LDY #$FF
       STY cia1_port_a_data_direction_reg     ; DDRA = $FF (A as outputs)
       LDA #$00
       STA cia1_port_b_data_direction_reg     ; DDRB = $00 (B as inputs)
       STA cia1_port_a_data_register          ; PRA  = $00 (select all columns: active-low)
       CPY cia1_port_b_data_register          ; PRB vs $FF ? (any 0 → activity)
       BNE input_detected
       JMP no_relevant_input_detected

input_detected:
       ;----------------------------------------
       ; Precise capture #1: COLUMN snapshot.
	   ;
       ; Keep A driving columns LOW and read B:
       ;   PRB bits clear (0) mark the columns whose lines are pulled LOW through a pressed key.
       ;----------------------------------------
       LDA #$FF
       STA cia1_port_a_data_direction_reg     ; DDRA = $FF (A still outputs)
       LDA #$00
       STA cia1_port_b_data_direction_reg     ; DDRB = $00 (B inputs)
       STA cia1_port_a_data_register          ; PRA  = $00 (all columns selected)
       LDA cia1_port_b_data_register          ; PRB  = column sense
       STA keyb_column_bits

       ;----------------------------------------
       ; Precise capture #2: ROW snapshot.
	   ;
       ; Swap roles: drive rows (B) LOW and read columns (A).
       ;   PRA bits clear (0) mark the rows whose lines are pulled LOW through a pressed key.
       ;----------------------------------------
       LDA #$FF
       STA cia1_port_b_data_direction_reg     ; DDRB = $FF (B outputs)
       LDA #$00
       STA cia1_port_a_data_direction_reg     ; DDRA = $00 (A inputs)
       STA cia1_port_b_data_register          ; PRB  = $00 (drive all rows LOW)
       LDA cia1_port_a_data_register          ; PRA  = row sense
       STA keyb_row_bits

count_bits:
       ;----------------------------------------
       ; Count 0-bits in the captured COLUMN snapshot.
	   ;
       ; On return from count_zero_bits:
       ;   A  = (#zero_bits - 1), or #$FF if there are no zeros
       ;   zero_lsb_index = index (0..7) of the least-significant zero bit (if any)
       ;----------------------------------------
       LDA keyb_column_bits
       JSR count_zero_bits
       BMI no_relevant_input_detected         ; no zero bits → no active columns → abort
       STA col_bits_clear_count               ; save (#zeros - 1) for columns

       ; Latch the column index of the least-significant active line.
       LDA zero_lsb_index
       STA zero_lsb_column

       ; Repeat the same zero-bit count for the ROW snapshot.
       LDA keyb_row_bits
       JSR count_zero_bits
       BMI no_relevant_input_detected         ; no zero bits → no active rows → abort
       STA row_bits_clear_count               ; save (#zeros - 1) for rows

       ;----------------------------------------
       ; If either rows or columns have *multiple* active lines, we need to adjust
       ; the matrix (e.g., shift masks / ghosting protection). 
	   ;
	   ; Recall:
       ;   result 0  → exactly one active line
       ;   result >0 → multiple active lines
       ;   result $FF→ none (already handled by BMI above)
	   ;
       ; Here A holds row_bits_clear_count; OR with col_bits_clear_count:
       ;   nonzero → at least one side has multiple actives → adjust
       ;   zero    → both sides have exactly one active line → no adjustment
       ;----------------------------------------
       ORA col_bits_clear_count
       BNE matrix_offset_adjustment

       ;----------------------------------------
       ; Build the key matrix index: index = (column * 8) + row + base_offset
       ; where base_offset is 0 for normal, $40 for shifted matrix.
       ;----------------------------------------
       LDA zero_lsb_column
       ASL A
       ASL A
       ASL A                                  ; A = column * 8
       CLC
       ADC zero_lsb_index                     ; + row
       ADC matrix_offset                      ; + base (0 or $40)
       ; Put the final index in Y for table lookup.
       TAY

       ;----------------------------------------
       ; Lookup keycode and ignore “unused” entries (#$5F).
       ;----------------------------------------
       LDA key_matrix,Y
       CMP IGNORED_KEY
       BEQ no_relevant_input_detected

       ;----------------------------------------
       ; De-duplicate: publish only when the key differs from the last one.
       ;----------------------------------------
       CMP last_keycode_scanned
       STA last_keycode_scanned
       BEQ exit_directly

       ;----------------------------------------
       ; Shift handling: if the detected key *is* a Shift, arm shifted mapping
       ; (set base offset and adjustment masks) and exit; the next scan will
       ; use the shifted key matrix.
       ;----------------------------------------
       LDA key_matrix,Y
       CMP KEY_LSHIFT
       BNE not_left_shift
       ; Left Shift → base = $40, column mask = %1000_0000, row mask = %0000_0010
       LDA #$40
       STA matrix_offset
       LDA #$80
       STA col_adjustment_bitmask
       LDA #$02
       STA row_adjustment_bitmask
       BNE exit_directly

not_left_shift:
       CMP KEY_RSHIFT
       BNE not_right_shift
       ; Right Shift → base = $40, column mask = %0001_0000, row mask = %0100_0000
       LDA #$40
       STA matrix_offset
       LDA #$10
       STA col_adjustment_bitmask
       LDA #$40
       STA row_adjustment_bitmask
       BNE exit_directly

not_right_shift:
       JMP direct_exit_check

       ;----------------------------------------
	   ; Ignore path: treat this scan as “no relevant input” and reset transient state.
       ;----------------------------------------
no_relevant_input_detected:
       LDA #$00
       STA matrix_offset                ; clear shifted mapping (return to base matrix)
       LDA #$FF
       STA last_keycode_scanned         ; invalidate last key so the next real key will publish

exit_directly:
       ; Funnel to the common exit gate with Z=1:
       ; load A=0 so the following BEQ will skip publishing and just exit.
       LDA #$00

direct_exit_check:
       ;----------------------------------------
       ; Common exit gate:
       ;   Z=1 (A==0)  → no key to publish → exit
       ;   Z=0 (A!=0)  → publish A as the keycode and set the “ready” flag
       ;----------------------------------------
       BEQ exit
       STA key_code_scanned             ; publish keycode
       LDA #$01
       STA keycode_ready                ; mark “key available”

exit:
       RTS

;===========================================
; Matrix offset & bitmask adjustment
;
;   When a Shift key has been detected, the decoder switches to a shifted keymap by setting
;   matrix_offset’s bit 6 and precomputing adjustment masks for columns/rows. 
;
;	This section checks that bit (via BIT/BVC) and, if active, conditionally ORs the snapshots 
;	with the masks - but only when the corresponding “clear count” indicates multiple active lines. 
;	This helps suppress ambiguous multi-line activations (e.g., shift interplay / ghosting). 
;
;	After masking, control transfers to count_bits to re-derive the least-significant active 
;	row/column indices.
;===========================================
matrix_offset_adjustment:
       ;----------------------------------------
       ; Shifted mapping enabled?  
	   ;
	   ; BIT copies bit6 of matrix_offset into V.
       ; If V=0 (bit6 clear → offset==0), no adjustment needed → exit.
       ;----------------------------------------
       BIT matrix_offset
       BVC exit_directly_2

       ;----------------------------------------
       ; Load precomputed adjustment masks (set by L/R Shift handlers):
       ;   X = mask for columns, Y = mask for rows.
       ;----------------------------------------
       LDX col_adjustment_bitmask
       LDY row_adjustment_bitmask
       JMP adjust_column_bits

exit_directly_2:
       JMP exit_directly

adjust_column_bits:
       ;----------------------------------------
       ; Decide whether to adjust the captured COLUMN bits.
	   ;
       ; col_bits_clear_count holds (number_of_zero_bits - 1).
       ;   = 0  → exactly one active (zero) column
       ;   > 0  → multiple active columns
       ; We only apply the mask when there are multiple actives (ghosting/shift interplay).
       ;----------------------------------------
       LDA col_bits_clear_count
       BEQ adjust_row_bits               ; exactly one → skip masking columns
       TXA
       ORA keyb_column_bits              ; force masked columns high (ignore them)
       STA keyb_column_bits

adjust_row_bits:
       ;----------------------------------------
       ; Same policy for ROW bits:
	   ;
       ;   row_bits_clear_count = (number_of_zero_bits - 1)
       ; Apply row mask only when multiple rows are active.
       ;----------------------------------------
       LDA row_bits_clear_count
       BEQ count_bits_2                  ; exactly one → skip masking rows
       TYA
       ORA keyb_row_bits                 ; force masked rows high (ignore them)
       STA keyb_row_bits

count_bits_2:
       ; Recount zeros after adjustments to re-derive row/column indices.
       JMP count_bits


;===========================================
; Count zero bits
;
; Counts 0-bits in A (MSB-first scan). Returns (#zeros-1) or #$FF if none.
; Also records the least-significant zero bit index into zero_lsb_index.
;
; Arguments:
;   A                                Input bitfield to scan (8 bits).
;
; Returns:
;   A                                (# of zero bits) - 1, or #$FF when no zeros.
;   zero_lsb_index ($205B)           Index (0..7) of least-significant zero bit if any.
;===========================================
count_zero_bits:
       ; Y = bit index (7..0), X = zero-counter
       LDY #$07
       LDX #$00
check_for_zero_bit:
       ; Shift MSB into carry
       ASL A
       ; If carry set, bit was 1 → skip counting
       BCS next_bit_index
       ; Bit was 0: increment counter and record current index
       INX
       STY zero_lsb_index
next_bit_index:
       DEY
       ; Loop while Y >= 0
       BPL check_for_zero_bit
       ; Return X-1 in A (or #$FF when X=0)
       DEX
       TXA
       RTS
       BRK

;===========================================
; Poll joystick (CIA1 port B bits 0–4)
;
; Reads joystick #2 via CIA1. Z=0 when any input is active. Stores raw state.
;
; Returns:
;   A                                Masked joystick state (bits 0–4).
;   Z-flag                           Clear when any line is active; set otherwise.
;
; Steps to read joystick (CIA1) — active-low logic
; 1) Configure directions:
;      DDRA = $FF  (Port A outputs)
;      DDRB = $00  (Port B inputs)
;
; 2) Release the matrix so joystick lines can be read cleanly:
;      PRA = $FF   (drive all Port A lines high)
;
; 3) Read PRB and mask joystick bits:
;      value = PRB & $1F
;
; 4) Interpret (active-low):
;      value == $1F → no inputs active
;      any bit 0    → some input active
;
;
; Note: For a pure joystick poll (bits 0–4 on PRB), we can simply drive PRA=$FF
; to release the matrix and then read PRB, masking with #%00011111. Any 0 in those
; five bits indicates an active joystick direction or fire.
;===========================================
poll_joystick:
       ;----------------------------------------
       ; CIA1 setup: make Port A all outputs and drive them high.
	   ;
       ; This releases the keyboard/joystick lines to the pull-ups so Port B can be sampled.
       ;----------------------------------------
       LDY #$FF
       STY cia1_port_a_data_direction_reg
       ;----------------------------------------
       ; Drive Port A high (read-all mask for matrix scanning / joystick sense).
       ;----------------------------------------
       STY cia1_port_a_data_register

       ;----------------------------------------
       ; Configure Port B as inputs (we'll read joystick here).
       ;----------------------------------------
       LDA #$00
       STA cia1_port_b_data_direction_reg

       ;----------------------------------------
       ; Read Port B: joystick lines are active-low.
	   ;
       ; Bits 0–4 correspond to directions + fire (1 = idle, 0 = pressed).
       ;----------------------------------------
       LDA cia1_port_b_data_register
       STA joystick_inputs

       ;----------------------------------------
       ; Keep only joystick bits, then compare against #$1F to set flags:
       ;   if all five bits are 1 → no inputs activated → Z = 1
       ;   if any bit is 0       → some input activated → Z = 0
       ;----------------------------------------
       AND #$1F
       CMP #$1F
       RTS
       BRK


* = $21BE
key_matrix:
; Matrix of key inputs mapped from the keycodes scanned
; A value of #$5F is ignored. Function keys yield values in the $01–$08 range.
; Special cases - the Function keys, which yield values in the 01-08 range
.byte $C8,$33,$35,$37,$39,$2B,$5F,$31 ; DEL		3 		5 	7 	9 	+ 	Pound	1
.byte $0D,$77,$72,$79,$69,$70,$2A,$5F ; <ret>	W		R	Y	I	P	*		Left
.byte $CA,$61,$64,$67,$6A,$6C,$3B,$5F ; Right	A		D	G	J	L	;		Ctrl
.byte $04,$34,$36,$38,$30,$2D,$CF,$32 ; F7		4		6	8	0	-	Home	2
.byte $01,$7A,$63,$62,$6D,$2E,$7D,$20 ; F1		Z		C	B	M	.	RShift	Space
.byte $02,$73,$66,$68,$6B,$3A,$3D,$D0 ; F3		S		F	H	K	:	=		Commodore
.byte $03,$65,$74,$75,$6F,$40,$5F,$71 ; F5		E		T	U	O	@	Up		Q
.byte $CD,$7B,$78,$76,$6E,$2C,$2F,$5F ; Down	LShift 	X 	V	N	,	/		Stop

; Shifted key matrix (used when matrix_offset=$40)
.byte $C9,$23,$25,$27,$29,$2B,$5F,$21
.byte $0D,$57,$52,$59,$49,$50,$2A,$5F
.byte $CB,$41,$44,$47,$4A,$4C,$29,$5F
.byte $08,$24,$26,$28,$30,$2D,$CE,$22
.byte $05,$5A,$43,$42,$4D,$3E,$7D,$20
.byte $06,$53,$46,$48,$4B,$28,$3D,$5F
.byte $07,$45,$54,$55,$4F,$40,$5F,$51
.byte $CC,$7B,$58,$56,$4E,$3C,$3F,$5F

;Filler bytes to match original - unused
.byte $20, $44, $22, $F0, $FB, $60
;===========================================
; Process a key
;
; Consumer helper: if a key is ready, clear the ready flag and return it in A.
; Otherwise, return immediately.
;
; Returns:
;   A                                key_code_scanned when a key was ready.
;   keycode_ready                    Cleared to #$00 when a key is consumed.
;===========================================
process_key:
       LDA keycode_ready
       ; Is there a key to process?
       BNE keycode_is_ready
       ; No: return
       RTS
keycode_is_ready:
       ; Clear the ready flag to avoid reprocessing
       LDA #$00
       STA keycode_ready
       ; Return the key in A
       LDA key_code_scanned
       RTS


; C64 INPUT MODULE - FLOW
; =======================================================
;
; [scan_keyboard]
; ---------------
; ENTRY
;   │
;   ├─ Clear cached snapshots → keyb_column_bits=$FF, keyb_row_bits=$FF
;   │
;   ├─ (Optional) Prime delay if saved_script_index ≠ 0
;   │
;   ├─ JSR poll_joystick  →  Z=0 (active) / Z=1 (idle)
;   │
;   ├─ IF Z=0 (joystick active) THEN
;   │      ├─ Refresh poll_delay_hi:lo
;   │      └─ RETURN
;   │
;   └─ ELSE (Z=1: joystick idle)
;          │
;          ├─ IF (poll_delay_hi:lo == 0) THEN
;          │      └─ GOTO KEYBOARD_SCAN
;          │
;          ├─ Decrement 16-bit poll_delay_hi:lo
;          │
;          ├─ IF (poll_delay_hi:lo == 0) THEN
;          │      └─ GOTO KEYBOARD_SCAN
;          │
;          └─ RETURN
;
;
; KEYBOARD_SCAN
; -------------
;   ├─ Quick probe:
;   │    DDRA=$FF (A out), DDRB=$00 (B in), PRA=$00 (select all columns)
;   │    IF (PRB == $FF) THEN
;   │       └─ NO_RELEVANT_INPUT
;   │
;   ├─ Precise snapshots:
;   │    1) Columns: (A drives low, B reads) → keyb_column_bits = PRB
;   │    2) Rows:    (B drives low, A reads) → keyb_row_bits    = PRA
;   │
;   ├─ Count zeros:
;   │    col = count_zero_bits(keyb_column_bits)
;   │          (A = (#zeros−1); zero_lsb_index = least-significant zero)
;   │          → if A=$FF → NO_RELEVANT_INPUT
;   │          → save col_bits_clear_count, save zero_lsb_column
;   │
;   │    row = count_zero_bits(keyb_row_bits)
;   │          (A = (#zeros−1); zero_lsb_index = least-significant zero)
;   │          → if A=$FF → NO_RELEVANT_INPUT
;   │          → save row_bits_clear_count
;   │
;   ├─ Need adjustment?  (row>0 OR col>0)
;   │    ├─ YES → matrix_offset_adjustment → then re-count (same as above)
;   │    └─ NO  → continue
;   │
;   ├─ Build index:
;   │    idx = (zero_lsb_column * 8) + (zero_lsb_index) + matrix_offset  ; base 0 or $40
;   │    Y   = idx
;   │
;   ├─ Lookup:
;   │    A = key_matrix[Y]
;   │    IF (A == $5F) THEN → NO_RELEVANT_INPUT
;   │
;   ├─ De-duplicate:
;   │    IF (A == last_keycode_scanned) THEN → RETURN
;   │    last_keycode_scanned = A
;   │
;   ├─ Shift handling:
;   │    IF (A == KEY_LSHIFT) THEN
;   │        matrix_offset=$40; col_mask=%1000_0000; row_mask=%0000_0010; RETURN
;   │    IF (A == KEY_RSHIFT) THEN
;   │        matrix_offset=$40; col_mask=%0001_0000; row_mask=%0100_0000; RETURN
;   │
;   └─ Publish:
;        key_code_scanned = A
;        keycode_ready    = #$01
;        RETURN
;
;
; NO_RELEVANT_INPUT
; -----------------
;   matrix_offset       = #$00
;   last_keycode_scanned= #$FF
;   RETURN
;
;
; [matrix_offset_adjustment]
; --------------------------
;   IF (matrix_offset bit6 == 0) THEN RETURN
;   X = col_adjustment_bitmask
;   Y = row_adjustment_bitmask
;   IF (col_bits_clear_count > 0) THEN keyb_column_bits |= X   ; mask columns when multiple active
;   IF (row_bits_clear_count > 0) THEN keyb_row_bits   |= Y   ; mask rows    when multiple active
;   RETURN (to re-count step in KEYBOARD_SCAN)
;
;
; [poll_joystick]
; ---------------
;   DDRA=$FF; PRA=$FF     ; release matrix (drive A high)
;   DDRB=$00               ; Port B inputs
;   raw = PRB
;   joystick_inputs = raw
;   mask = raw & $1F
;   Z = (mask == $1F)      ; Z=1 idle, Z=0 any pressed
;   RETURN
;
;
; [process_key]
; -------------
;   IF (keycode_ready == 0) THEN RETURN
;   keycode_ready = 0
;   A = key_code_scanned
;   RETURN
