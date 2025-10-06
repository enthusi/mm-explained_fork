;===========================================
; IEC serial bit-banging via CIA2 PRA
;
; Summary:
;   Low-level send/receive primitives for the Commodore IEC serial bus using
;   CIA2 Port A ($DD00). Bytes are transferred MSB-first by splitting each
;   byte into four two-bit phases (ODD/EVEN clock levels). The module also
;   provides a synchronization preamble that polls CLK-IN, then drives ATN
;   to a defined state before traffic.
;
; Provided routines:
;   send_command_to_drive   Send 3-byte command: track → sector → operation.
;   sync_with_drive         Pre/post delays, poll CLK-IN until set, write ATN_RELEASED.
;   recv_byte_from_serial   Read one byte via four two-bit sampling phases.
;   recv_bit_pair_from_serial
;                          Sample two bits (odd/even) into serial_byte (ROL A → C → ROL serial_byte).
;   send_byte_over_serial   Send one byte via four two-bit transmit phases.
;   send_bit_pair_over_serial
;                          Transmit two bits (ROL serial_byte → C; select PRA mask by C; ODD then EVEN).
;
; Hardware / IO:
;   CIA2 Port A (PRA, $DD00) drives/reads IEC lines through inverters (active-low bus).
;   Writes in this module use full-byte masks; see Variables/Constants for bit meanings.
;   Assumes DDRA configured so: DATA/CLK/ATN outputs are enabled; CLK-IN is readable.
;
; Conventions & invariants:
;   • Bit order: MSB-first on both RX and TX (symmetry via ROL usage).
;   • Two-phase framing: ODD (CLOCK high) then EVEN (CLOCK low) per 2 bits.
;   • ATN polarity: ATN_RELEASED sets PRA ATN bit=1 (line released on the bus).
;   • Timing: fixed cycle gaps (small register shuffles) form the required waveform.
;   • Masks keep TXD (bit 2) high to avoid user-port glitches when writing full PRA.
;
; Variables / constants:
;   See the “Variables / Constants” section below for PRA bit masks, composite
;   phase masks, timing constants (PRE/POST sync delays), and working storage
;   (serial_byte, command_track/sector, PRA address).
;
; Preconditions:
;   • DDRA must set IEC output bits to output and CLK-IN to input.
;   • VIC bank selection (PRA bits 0–1) is clobbered in these routines
;
; Returns / side effects:
;   • Routines commonly clobber A, Y, and condition flags; X is preserved by most
;     wrappers but may be used inside timing gaps—see each routine header.
;   • Bus lines reflect the last phase mask written on exit from send/recv helpers.
;
; Notes / edge cases:
;   • Sync waits while (MASK_CLK_IN & PRA)==0 and proceeds when it reads 1
;     (post-inversion “released” on IEC).
;===========================================
;===========================================
; Variables / Constants
;===========================================

; --- CIA2 Port A bit masks (PRA = $DD00) ---
; NOTE: IEC lines are typically active-low through inverters (e.g., 7406).
;       Writing 0 at PRA asserts the physical line (pulls bus low),
;       writing 1 releases it (bus pulled high).

MASK_TXD      = %00000100   ; PRA bit 2 — User-port TXD output. Kept high (released) in our masks.
MASK_ATN_OUT  = %00001000   ; PRA bit 3 — IEC ATN (output via inverter). 0=assert ATN, 1=release.
MASK_CLK_OUT  = %00010000   ; PRA bit 4 — IEC CLOCK (output via inverter). 0=clock low (assert), 1=release.
MASK_DATA_OUT = %00100000   ; PRA bit 5 — IEC DATA  (output via inverter). 0=data low (assert), 1=release.
MASK_CLK_IN   = %01000000   ; PRA bit 6 — IEC CLOCK input (read only). Polled via BIT during sync.

; --- Composite PRA write masks (phase/preset states) ---

;Note: 
;If we didn’t explicitly set TXD in every mask, some phases would accidentally drive it low
;TXD then held high to avoid user-port side effects while doing full-mask PRA writes

ATN_RELEASED  = MASK_ATN_OUT | MASK_TXD        ; PRA bits set high for ATN & TXD.
                                               ; NOTE: With active-low bus this *releases* ATN at the connector.
                                               ;       Name reflects PRA bit state, not bus polarity — verify intent.

PRESET_BEFORE_SYNC   = MASK_DATA_OUT | MASK_TXD       ; Preset before sync: DATA and TXD high (released). Does not touch ATN.
													  ; (“Ready To Send” staging mask used by send_command_to_drive.)

; Receive phases (two samples per call): odd = CLOCK high, even = CLOCK low
RECV_PHASE_ODD  = MASK_CLK_OUT | MASK_ATN_OUT | MASK_TXD  	; Drive PRA: clock high, ATN/TXD high (released) while sampling bit #1.
RECV_PHASE_EVEN = MASK_ATN_OUT | MASK_TXD                  	; Drive PRA: clock low (by omitting MASK_CLK_OUT), ATN/TXD high for bit #2.

; Transmit phases (two emitted bits per call), select DATA based on C after ROL serial_byte
SEND_ODD_DATA1   = MASK_DATA_OUT | MASK_CLK_OUT | MASK_ATN_OUT | MASK_TXD  	; ODD phase: clock high, DATA=1 (released), ATN/TXD high.
SEND_ODD_DATA0 = MASK_CLK_OUT | MASK_ATN_OUT | MASK_TXD                  	; ODD phase: clock high, DATA=0 (assert) via cleared DATA bit.
SEND_EVEN_DATA1  = MASK_DATA_OUT | MASK_ATN_OUT | MASK_TXD                 	; EVEN phase: clock low,  DATA=1 (released), ATN/TXD high.
SEND_EVEN_DATA0= MASK_ATN_OUT | MASK_TXD                                 	; EVEN phase: clock low,  DATA=0 (assert).

; --- Timing constants for simple busy-wait loops ---
PRE_SYNC_DELAY  = $28                 ; Pre-sync settle loop count before polling CLOCK IN.
POST_SYNC_DELAY = $06                 ; Post-sync settle after writing ATN/assert mask.

; --- Working/storage locations ---
serial_byte     = $4752                ; Shift register for TX/RX bit pairs (ROL into/out of carry).
command_track   = $4753                ; Command parameter: track number to transmit.
command_sector  = $4754                ; Command parameter: sector number to transmit.
cia2_port_a_data_register = $dd00      ; CIA2 Port A (PRA). Read to sample inputs; write masks to drive IEC outputs.

;===========================================
; send_command_to_drive — transmit (track, sector, op) over CIA2 serial
;
; Summary:
;   Presets CIA2 PRA to a known idle/output mask, synchronizes with the drive,
;   then sends a 3-byte command in the order: track → sector → operation.
;   Each byte is emitted MSB-first via send_byte_over_serial (2 bits per phase).
;
; Arguments:
;   .A (accumulator)         Operation code byte (saved internally, sent last).
;   command_track       	 Track number to send as first byte.
;   command_sector		     Sector number to send as second byte.
;
; State:
;   cia2_port_a_data_register ($DD00)
;                            Serial/IEC lines (written with PRESET_BEFORE_SYNC mask).
;   PRESET_BEFORE_SYNC 		     Port bitmask used to preset bus outputs before sync.
;
; Returns:
;   None                     No direct return value.
;   Flags                    Not defined; clobbered by subroutine calls.
;
; Description:
;   Saves A (op code) with PHA, presets PRA using PRESET_BEFORE_SYNC, and calls sync_with_drive
;   (which polls the bus to the desired state and asserts ATN). It then sends the track
;   and sector bytes from memory, restores A (op), and sends it as the final byte.
;   send_byte_over_serial handles bit-level framing and bus handshaking. On exit, no
;   registers are guaranteed; bus lines reflect the final send routine’s last mask.
;===========================================
* = $46c2
send_command_to_drive:
		; Save the operation code (A) so we can transmit it last.
		PHA

		; Preset bus outputs before syncing (mask defined by PRESET_BEFORE_SYNC).
		; NOTE: preserves protocol-defined idle levels for ATN/DATA/CLOCK as required.
		LDA #PRESET_BEFORE_SYNC
		STA cia2_port_a_data_register

		; Synchronize with the drive (poll needed line state, assert ATN, brief settle).
		JSR sync_with_drive

		; Transmit command bytes on the wire in this order:
		;   track → sector → operation
		; Each byte is sent MSB-first by send_byte_over_serial (2 bits per phase).
		LDA command_track
		JSR send_byte_over_serial

		LDA command_sector
		JSR send_byte_over_serial

		; Restore the saved operation code and send it.
		PLA
		JSR send_byte_over_serial

		RTS
;===========================================
; sync_with_drive — poll CLK-IN, assert ATN, and settle (IEC preamble)
;
; Summary:
;   Waits a pre-sync delay, then polls CIA2 PRA until the CLOCK IN bit reads set
;   (after inversion, this usually means the IEC line is released). Once set,
;   releases ATN by writing a defined PRA mask, then waits a post-sync delay so
;   the peer can observe the change before traffic begins.
;
; Constants:
;   PRE_SYNC_DELAY         			Loop count for initial settle.
;   POST_SYNC_DELAY			        Loop count after asserting ATN.
;   MASK_CLK_IN 		            Bit mask used with BIT to test CLOCK IN.
;   ATN_RELEASED 			        PRA mask written to release ATN (and required outputs).
;
; State:
;   cia2_port_a_data_register ($DD00)
;                                  CIA2 Port A; read with BIT, written with STA.
; Returns:
;   None                           No direct return value.
;   Flags                          Undefined on return. Inside the poll:
;                                  Z=1 while (MASK_CLK_IN & $DD00) == 0; Z=0 when it becomes 1.
;
; Clobbers:
;   A, Y                           X preserved.
;
; Description:
;   - Pre-delay: burns cycles to let the bus reach idle.
;   - Poll loop: LDA #MASK_CLK_IN; BIT $DD00; BEQ loop — repeats while the masked bit reads 0,
;                exits when it reads 1 (post-inversion “released” on IEC).
;   - Assert ATN by writing ATN_RELEASED to $DD00 (full-port write).
;   - Post-delay: additional guard time for the device to latch ATN.
;===========================================
sync_with_drive:
		; Pre-sync settle time: allow bus lines to reach idle before polling.
		LDY #PRE_SYNC_DELAY
sync_wait_1:
		DEY
		BNE sync_wait_1

		; Poll CLOCK IN until it reads *set* at PRA:
		;   A ← MASK_CLK_IN (bit mask to test)
		;   BIT $DD00 sets Z=1 when (A & $DD00) == 0  → line reads clear
		;   Loop while Z=1 (clear), proceed when Z=0 (bit reads set/released).
		LDA #MASK_CLK_IN
wait_for_CLOCK_IN_set:
		BIT cia2_port_a_data_register
		BEQ wait_for_CLOCK_IN_set

		; Release ATN and drive required outputs to a known state.
		LDA #ATN_RELEASED
		STA cia2_port_a_data_register

		; Post-sync settle time: give the peer time to observe ATN before traffic.
		LDY #POST_SYNC_DELAY
sync_wait_2:
		DEY
		BNE sync_wait_2
		RTS


;===========================================
; recv_byte_from_serial — read 8 bits (MSB→LSB) via two-bit phases
;
; Summary:
;   Builds one byte from the serial bus by invoking recv_bit_pair_from_serial
;   four times (each call samples two bits using clocked phases). The completed
;   byte is then loaded from serial_byte into A and returned.
;
; Arguments:
;   None
;
; Returns:
;   A (accumulator)               Byte received from the bus.
;   Flags                         From final LDA serial_byte:
;                                 Z set if A == $00, N reflects bit7 of A.
;
; Globals Used:
;   serial_byte		              Shift-accumulator used by the bit-pair routine.
;
; Clobbers:
;   A                             Set to received byte.
;   X                             Preserved.
;   Y                             Preserved by this wrapper (inner routine may use it).
;
; Description:
;   recv_bit_pair_from_serial samples two bits per call by reading CIA2 PRA, rotating
;   DATA IN (bit7) into C, then rotating C into serial_byte (ROL). After four calls,
;   serial_byte contains the full byte assembled MSB-first; this wrapper copies it
;   into A with LDA and returns.
;===========================================
recv_byte_from_serial:
		JSR recv_bit_pair_from_serial
		JSR recv_bit_pair_from_serial
		JSR recv_bit_pair_from_serial
		JSR recv_bit_pair_from_serial
		LDA serial_byte
		RTS

;===========================================
; recv_bit_pair_from_serial — sample two bits (odd/even clock phases) into serial_byte
;
; Summary:
;   Captures two consecutive bits from CIA2 PRA’s DATA IN (bit 7) using two phases:
;   an “odd” phase with CLOCK high and an “even” phase with CLOCK low. Each phase
;   reads PRA, rotates DATA IN → C via ROL A, then rotates C into serial_byte via
;   ROL serial_byte (left shift, insert at bit0). Repeated calls build a byte MSB-first.
;
; Constants/State:
;   RECV_PHASE_ODD   			  PRA output mask for the odd phase (CLOCK high, ATN asserted, etc.).
;   RECV_PHASE_EVEN     		  PRA output mask for the even phase (CLOCK low, ATN asserted, etc.).
;   cia2_port_a_data_register ($DD00)
;                             CIA2 Port A (PRA): read for inputs, written with masks to drive outputs.
;   serial_byte    		      Shift accumulator that receives the two sampled bits.
;
; Returns:
;   serial_byte               Updated: serial_byte <<= 2; two new bits inserted over two phases.
;   Flags                     Clobbered/undefined on return (ROL/TYA/TAY affect N,Z,C).
;
; Clobbers:
;   A, Y, C                   X preserved.
;
; Description:
;   Phase 1 (odd/clock high):
;     • LDY #RECV_PHASE_ODD; LDA $DD00; STY $DD00 to drive outputs.
;     • ROL A    → DATA IN (bit7) moves into carry.
;     • ROL serial_byte → left shift; carry inserted at bit0.
;     • Small timing delay (register shuffles).
;   Phase 2 (even/clock low):
;     • LDY #RECV_PHASE_EVEN; LDA $DD00; STY $DD00.
;     • ROL A; ROL serial_byte as above to capture the next bit.
;     • Short settle; RTS.
;   Note: On IEC the physical lines are typically active-low via inverters; “bit set” at PRA
;         usually corresponds to a released (high) bus line.
;===========================================
recv_bit_pair_from_serial:
		; Odd phase (clock high): sample DATA IN and shift into serial_byte.
		LDY #RECV_PHASE_ODD             ; Output mask: ATN released, CLOCK high (phase 1)
		LDA cia2_port_a_data_register ; Read PRA (bit7 carries DATA IN after inversion)
		STY cia2_port_a_data_register ; Drive odd-phase outputs on PRA
		ROL A                         ; DATA IN (bit7) → C (carry)
		ROL serial_byte               ; serial_byte <<= 1; insert C at bit0

		; Tiny timing gap between phases (do not remove).
		TYA
		PHA
		TYA
		PLA
		TYA

		; Even phase (clock low): sample next bit and shift it in as well.
		LDY #RECV_PHASE_EVEN            ; Output mask: ATN released, CLOCK low (phase 2)
		LDA cia2_port_a_data_register ; Read PRA again for next bit
		STY cia2_port_a_data_register ; Drive even-phase outputs on PRA
		ROL A                         ; DATA IN (bit7) → C
		ROL serial_byte               ; serial_byte <<= 1; insert C at bit0

		; Short settle before returning to caller.
		TYA
		TAY
		RTS
;===========================================
; send_byte_over_serial — emit 8 bits (MSB→LSB) via two-bit phases
;
; Summary:
;   Sends one byte over the serial bus by storing it in serial_byte and invoking
;   send_bit_pair_over_serial four times. Each call transmits two bits using a
;   clock-high (odd) then clock-low (even) phase, for a total of eight bits MSB-first.
;
; Arguments:
;   A (accumulator)          Byte to transmit (MSB sent first).
;
; Returns:
;   None                     No direct return value.
;   Flags                    Undefined on return (clobbered by subcalls).
;
; Globals Used:
;   serial_byte		         Shift source used by the bit-pair sender.
;
; Clobbers:
;   A                        Overwritten when stored to serial_byte.
;   X, Y                     Preserved by this wrapper (subroutine may use them).
;
; Description:
;   Stores A into serial_byte, then performs four phases of send_bit_pair_over_serial.
;   That subroutine rotates the next bit from serial_byte into carry and drives CIA2
;   PRA masks to place DATA and toggle CLOCK (odd/high then even/low), achieving the
;   required handshaked bit timing on the bus.
;===========================================
send_byte_over_serial:
		STA serial_byte
		JSR send_bit_pair_over_serial
		JSR send_bit_pair_over_serial
		JSR send_bit_pair_over_serial
		JSR send_bit_pair_over_serial
		RTS

;===========================================
; send_bit_pair_over_serial — emit 2 bits via CIA2 PRA (odd/even clock phases)
;
; Summary:
;   Transmits two consecutive bits from serial_byte, MSB-first, using two phases:
;   an ODD phase with CLOCK high and an EVEN phase with CLOCK low. Each phase
;   rotates the next data bit into C (ROL serial_byte), selects a PRA mask
;   based on C (DATA=0/1), writes it to $DD00, then waits briefly for timing.
;
; Arguments:
;   serial_byte                    Shift source; top two bits are consumed by this call.
;   SEND_ODD_DATA0                  PRA mask for ODD phase with DATA=0 (CLOCK high).
;   SEND_ODD_DATA1                    PRA mask for ODD phase with DATA=1 (CLOCK high).
;   SEND_EVEN_DATA0                 PRA mask for EVEN phase with DATA=0 (CLOCK low).
;   SEND_EVEN_DATA1                   PRA mask for EVEN phase with DATA=1 (CLOCK low).
;   cia2_port_a_data_register ($DD00)
;                                  CIA2 Port A; written with the selected masks to drive IEC lines.
;
; Returns:
;   None                           Bus lines updated; no register return.
;   Flags                          Clobbered/undefined (uses ROL/TXA/TYA/TAY).
;
; Clobbers:
;   A, Y, C                        X preserved.
;
; Description:
;   Phase ODD:
;     • ROL serial_byte → next MSB into C.
;     • If C=0 use SEND_ODD_DATA0 else SEND_ODD_DATA1; write to $DD00.
;     • Short delay to satisfy bus timing.
;   Phase EVEN:
;     • ROL serial_byte → next MSB into C.
;     • If C=0 use SEND_EVEN_DATA0 else SEND_EVEN_DATA1; write to $DD00.
;     • Short settle; RTS.
;   After one call, serial_byte has been shifted left twice; the two emitted bits
;   were the former bit7 and bit6 (MSB-first stream).
;===========================================
send_bit_pair_over_serial:
		; Emit two bits using two phases: ODD (clock high) then EVEN (clock low).
		; Default to DATA=0 mask for the odd phase; ROL moves next data bit into C.
		LDY #SEND_ODD_DATA0            ; ODD phase mask: CLOCK high, DATA=0 (ATN as required)
		ROL serial_byte               ; bit7 → C (MSB-first stream)
		BCC set_signals_on_bus_1      ; if C=0 keep DATA=0; else pick DATA=1 mask below
		LDY #SEND_ODD_DATA1              ; ODD phase mask: CLOCK high, DATA=1
set_signals_on_bus_1:
		STY cia2_port_a_data_register ; drive odd-phase outputs on PRA

		; Small timing gap between phases (do not remove).
		TXA
		TYA
		TXA
		TAY

		; EVEN phase: repeat with CLOCK low. Default to DATA=0; ROL exposes next bit in C.
		LDY #SEND_EVEN_DATA0           ; EVEN phase mask: CLOCK low, DATA=0
		ROL serial_byte               ; next bit → C
		BCC set_signals_on_bus_2      ; if C=0 keep DATA=0; else select DATA=1 mask
		LDY #SEND_EVEN_DATA1             ; EVEN phase mask: CLOCK low, DATA=1
set_signals_on_bus_2:
		STY cia2_port_a_data_register ; drive even-phase outputs on PRA

		; Short settle before returning.
		TXA
		TAY
		RTS



