/*
 * ===========================================
 * IEC serial bit-banging via CIA2 PRA
 *
 * Summary:
 *   Low-level send/receive primitives for the Commodore IEC serial bus using
 *   CIA2 Port A. Bytes are transferred MSB-first by splitting each
 *   byte into four two-bit phases (ODD/EVEN clock levels). The module also
 *   provides a synchronization preamble that polls CLK-IN, then drives ATN
 *   to a defined state before traffic.
 *
 * Public routines:
 *   iec_send_cmd   		Send 3-byte command: track → sector → operation.
 *   iec_sync         	Pre/post delays, poll CLK-IN until set, write IEC_MASK_ATN_RELEASE.
 *   iec_recv_byte   	Read one byte via four two-bit sampling phases.
 *   iec_send_byte   	Send one byte via four two-bit transmit phases.
 *
 * Private routines:
 *   iec_recv_pair       Sample two bits (odd/even) into iec_shiftreg (ROL A → C → ROL iec_shiftreg).
 *   iec_send_pair       Transmit two bits (ROL iec_shiftreg → C; select PRA mask by C; ODD then EVEN).
 *
 * Hardware / IO:
 *   CIA2 Port A (PRA) drives/reads IEC lines through inverters (active-low bus).
 *   Writes in this module use full-byte masks; see Variables/Constants for bit meanings.
 *   Assumes DDRA configured so: DATA/CLK/ATN outputs are enabled; CLK-IN is readable.
 *
 * Conventions & invariants:
 *   • Bit order: MSB-first on both RX and TX (symmetry via ROL usage).
 *   • Two-phase framing: ODD (CLOCK high) then EVEN (CLOCK low) per 2 bits.
 *   • ATN polarity: IEC_MASK_ATN_RELEASE sets PRA ATN bit=1 (line released on the bus).
 *   • Timing: fixed cycle gaps (small register shuffles) form the required waveform.
 *   • Masks keep TXD (bit 2) high to avoid user-port glitches when writing full PRA.
 *
 * Variables / constants:
 *   See the “Variables / Constants” section below for PRA bit masks, composite
 *   phase masks, timing constants (PRE/POST sync delays), and working storage
 *   (iec_shiftreg, iec_cmd_track/sector, PRA address).
 *
 * Preconditions:
 *   • DDRA must set IEC output bits to output and CLK-IN to input.
 *   • VIC bank selection (PRA bits 0–1) is clobbered in these routines
 *
 * Returns / side effects:
 *   • Routines commonly clobber A, Y, and condition flags; X is preserved by most
 *     wrappers but may be used inside timing gaps—see each routine header.
 *   • Bus lines reflect the last phase mask written on exit from send/recv helpers.
 *
 * Notes / edge cases:
 *   • Sync waits while (IEC_PRA_CLK_IN & PRA)==0 and proceeds when it reads 1
 *     (post-inversion “released” on IEC).
 * ===========================================
 */
#importonce
#import "registers.inc"
#import "globals.inc"
#import "constants.inc"

/*
 * --- CIA2 Port A bit masks (PRA = $DD00) ---
 * NOTE: IEC lines are typically active-low through inverters (e.g., 7406).
 *       Writing 0 at PRA asserts the physical line (pulls bus low),
 *       writing 1 releases it (bus pulled high).
 */

.const IEC_PRA_TXD      = %00000100   // PRA bit 2 — User-port TXD output. Kept high (released) in our masks.
.const IEC_PRA_ATN_OUT  = %00001000   // PRA bit 3 — IEC ATN (output via inverter). 0=assert ATN, 1=release.
.const IEC_PRA_CLK_OUT  = %00010000   // PRA bit 4 — IEC CLOCK (output via inverter). 0=clock low (assert), 1=release.
.const IEC_PRA_DATA_OUT = %00100000   // PRA bit 5 — IEC DATA  (output via inverter). 0=data low (assert), 1=release.
.const IEC_PRA_CLK_IN   = %01000000   // PRA bit 6 — IEC CLOCK input (read only). Polled via BIT during sync.

// --- Composite PRA write masks (phase/preset states) ---

/*
 * Note: 
 * If we didn’t explicitly set TXD in every mask, some phases would accidentally drive it low
 * TXD then held high to avoid user-port side effects while doing full-mask PRA writes
 */

.const IEC_MASK_ATN_RELEASE  = IEC_PRA_ATN_OUT | IEC_PRA_TXD       // PRA bits set high for ATN & TXD.
															/*
															 * NOTE: With active-low bus this *releases* ATN at the connector.
															 * Name reflects PRA bit state, not bus polarity — verify intent.
															 */

.const IEC_MASK_PRESET_BEFORE_SYNC = IEC_PRA_DATA_OUT | IEC_PRA_TXD  	// Preset before sync: DATA and TXD high (released). Does not touch ATN.
																// (“Ready To Send” staging mask used by iec_send_cmd.)

// Receive phases (two samples per call): odd = CLOCK high, even = CLOCK low
.const IEC_RECV_ODD  		= IEC_PRA_CLK_OUT | IEC_PRA_ATN_OUT | IEC_PRA_TXD  	// Drive PRA: clock high, ATN/TXD high (released) while sampling bit #1.
.const IEC_RECV_EVEN 		= IEC_PRA_ATN_OUT | IEC_PRA_TXD                  	// Drive PRA: clock low (by omitting IEC_PRA_CLK_OUT), ATN/TXD high for bit #2.

// Transmit phases (two emitted bits per call), select DATA based on C after ROL iec_shiftreg
.const IEC_SEND_ODD_D1   	= IEC_PRA_DATA_OUT | IEC_PRA_CLK_OUT | IEC_PRA_ATN_OUT | IEC_PRA_TXD  	// ODD phase: clock high, DATA=1 (released), ATN/TXD high.
.const IEC_SEND_ODD_D0 	= IEC_PRA_CLK_OUT  | IEC_PRA_ATN_OUT | IEC_PRA_TXD                  		// ODD phase: clock high, DATA=0 (assert) via cleared DATA bit.
.const IEC_SEND_EVEN_D1  	= IEC_PRA_DATA_OUT | IEC_PRA_ATN_OUT | IEC_PRA_TXD                 		// EVEN phase: clock low,  DATA=1 (released), ATN/TXD high.
.const IEC_SEND_EVEN_D0	= IEC_PRA_ATN_OUT  | IEC_PRA_TXD                                 		// EVEN phase: clock low,  DATA=0 (assert).

// --- Timing constants for simple busy-wait loops ---
.const IEC_PRE_SYNC_DELAY  = $28                 	// Pre-sync settle loop count before polling CLOCK IN.
.const IEC_POST_SYNC_DELAY = $06                 	// Post-sync settle after writing ATN/assert mask.

// --- Working/storage locations ---
.label iec_shiftreg     	= $4752                	// Shift register for TX/RX bit pairs (ROL into/out of carry).
.label iec_cmd_track   	= $4753                	// Command parameter: track number to transmit.
.label iec_cmd_sector  	= $4754                	// Command parameter: sector number to transmit.

/*
 * ===========================================
 * iec_send_cmd — transmit (track, sector, op) over CIA2 serial
 *
 * Summary:
 *   Presets CIA2 PRA to a known idle/output mask, synchronizes with the drive,
 *   then sends a 3-byte command in the order: track → sector → operation.
 *   Each byte is sent via iec_send_byte.
 *
 * Arguments:
 *   .A (accumulator)         Operation code byte (saved internally, sent last).
 *   iec_cmd_track       	 Track number to send as first byte.
 *   iec_cmd_sector		     Sector number to send as second byte.
 *
 * Returns:
 *   None                     No direct return value.
 *   Flags                    Not defined; clobbered by subroutine calls.
 *
 * Description:
 *   Saves A (op code) with PHA, presets PRA using IEC_MASK_PRESET_BEFORE_SYNC, and calls iec_sync
 *   (which polls the bus to the desired state and asserts ATN). 
 *
 * 	It then sends the track and sector bytes from memory, restores A (op), and sends it as the final byte.
 *
 *   iec_send_byte handles bit-level framing and bus handshaking. 
 * 	On exit, no registers are guaranteed; bus lines reflect the final send routine’s last mask.
 * ===========================================
 */
* = $46C2
iec_send_cmd:
		// Save the operation code (A) so we can transmit it last.
		pha

		/*
		 * Preset bus outputs before syncing (mask defined by IEC_MASK_PRESET_BEFORE_SYNC).
		 * NOTE: preserves protocol-defined idle levels for ATN/DATA/CLOCK as required.
		 */
		lda #IEC_MASK_PRESET_BEFORE_SYNC
		sta cia2_pra

		// Synchronize with the drive (poll needed line state, assert ATN, brief settle).
		jsr iec_sync

		/*
		 * Transmit command bytes on the wire in this order:
		 *   track → sector → operation
		 * Each byte is sent MSB-first by iec_send_byte (2 bits per phase).
		 */
		lda iec_cmd_track
		jsr iec_send_byte

		lda iec_cmd_sector
		jsr iec_send_byte

		// Restore the saved operation code and send it.
		pla
		jsr iec_send_byte

		rts
/*
 * ===========================================
 * iec_sync — poll CLK-IN, assert ATN, and settle (IEC preamble)
 *
 * Summary:
 *   Waits a pre-sync delay, then polls CIA2 PRA until the CLOCK IN bit reads set
 *   (after inversion, this usually means the IEC line is released). Once set,
 *   releases ATN by writing a defined PRA mask, then waits a post-sync delay so
 *   the peer can observe the change before traffic begins.
 *
 * Returns:
 *   None                           No direct return value.
 *   Flags                          Undefined on return. Inside the poll:
 *                                  Z=1 while (IEC_PRA_CLK_IN & cia2_pra) == 0; Z=0 when it becomes 1.
 *
 * Clobbers:
 *   A, Y                           X preserved.
 *
 * Description:
 *   - Pre-delay: burns cycles to let the bus reach idle.
 *   - Poll loop: LDA #IEC_PRA_CLK_IN; BIT cia2_pra; BEQ loop — repeats while the masked bit reads 0,
 *                exits when it reads 1 (post-inversion “released” on IEC).
 *   - Assert ATN by writing IEC_MASK_ATN_RELEASE to cia2_pra (full-port write).
 *   - Post-delay: additional guard time for the device to latch ATN.
 * ===========================================
 */
* = $46DC
iec_sync:
		// Pre-sync settle time: allow bus lines to reach idle before polling.
		ldy #IEC_PRE_SYNC_DELAY
sync_wait_1:
		dey
		bne sync_wait_1

		/*
		 * Poll CLOCK IN until it reads *set* at PRA:
		 *   A ← IEC_PRA_CLK_IN (bit mask to test)
		 *   BIT cia2_pra sets Z=1 when (A & cia2_pra) == 0  → line reads clear
		 *   Loop while Z=1 (clear), proceed when Z=0 (bit reads set/released).
		 */
		lda #IEC_PRA_CLK_IN
wait_for_CLOCK_IN_set:
		bit cia2_pra
		beq wait_for_CLOCK_IN_set

		// Release ATN and drive required outputs to a known state.
		lda #IEC_MASK_ATN_RELEASE
		sta cia2_pra

		// Post-sync settle time: give the peer time to observe ATN before traffic.
		ldy #IEC_POST_SYNC_DELAY
sync_wait_2:
		dey
		bne sync_wait_2
		rts
/*
 * ===========================================
 * iec_recv_byte — read 8 bits (MSB→LSB) via two-bit phases
 *
 * Summary:
 *   Builds one byte from the serial bus by invoking iec_recv_pair
 *   four times (each call samples two bits using clocked phases). 
 * 	The completed byte is then loaded from iec_shiftreg into A and returned.
 *
 * Arguments:
 *   None
 *
 * Returns:
 *   A (accumulator)               Byte received from the bus.
 *   Flags                         From final LDA iec_shiftreg:
 *                                 Z set if A == $00, N reflects bit7 of A.
 *
 * Globals Used:
 *   iec_shiftreg		          Shift-accumulator used by the bit-pair routine.
 *
 * Clobbers:
 *   A                             Set to received byte.
 *   X                             Preserved.
 *   Y                             Preserved by this wrapper (inner routine may use it).
 *
 * Description:
 *   iec_recv_pair samples two bits per call by reading CIA2 PRA, rotating
 *   DATA IN (bit7) into C, then rotating C into iec_shiftreg (ROL). After four calls,
 *   iec_shiftreg contains the full byte assembled MSB-first; this wrapper copies it
 *   into A with LDA and returns.
 * ===========================================
 */
* = $46F3
iec_recv_byte:
		jsr iec_recv_pair
		jsr iec_recv_pair
		jsr iec_recv_pair
		jsr iec_recv_pair
		lda iec_shiftreg
		rts
/*
 * ===========================================
 * iec_recv_pair — sample two bits (odd/even clock phases) into iec_shiftreg
 *
 * Summary:
 *   Captures two consecutive bits from CIA2 PRA’s DATA IN (bit 7) using two phases:
 *   an “odd” phase with CLOCK high and an “even” phase with CLOCK low. Each phase
 *   reads PRA, rotates DATA IN → C via ROL A, then rotates C into iec_shiftreg via
 *   ROL iec_shiftreg (left shift, insert at bit0). Repeated calls build a byte MSB-first.
 *
 * Constants/State:
 *   iec_shiftreg    		  Shift accumulator that receives the two sampled bits.
 *
 * Returns:
 *   iec_shiftreg              Updated: iec_shiftreg <<= 2; two new bits inserted over two phases.
 *   Flags                     Clobbered/undefined on return (ROL/TYA/TAY affect N,Z,C).
 *
 * Clobbers:
 *   A, Y, C                   X preserved.
 *
 * Description:
 *
 *   Phase 1 (odd/clock high):
 *     • LDY #IEC_RECV_ODD; LDA cia2_pra; STY cia2_pra to drive outputs.
 *     • ROL A    → DATA IN (bit7) moves into carry.
 *     • ROL iec_shiftreg → left shift; carry inserted at bit0.
 *     • Small timing delay (register shuffles).
 *
 *   Phase 2 (even/clock low):
 *     • LDY #IEC_RECV_EVEN; LDA cia2_pra; STY cia2_pra.
 *     • ROL A; ROL iec_shiftreg as above to capture the next bit.
 *     • Short settle; RTS.
 *
 *   Note: the physical lines are active-low via inverters; “bit set” at PRA
 *         usually corresponds to a released (high) bus line.
 * ===========================================
 */
* = $4703
iec_recv_pair:
		// Odd phase (clock high): sample DATA IN and shift into iec_shiftreg.
		ldy #IEC_RECV_ODD       // Output mask: ATN released, CLOCK high (phase 1)
		lda cia2_pra 			// Read PRA (bit7 carries DATA IN after inversion)
		sty cia2_pra 			// Drive odd-phase outputs on PRA
		rol                     // DATA IN (bit7) → C (carry)
		rol iec_shiftreg        // iec_shiftreg <<= 1; insert C at bit0

		// Tiny timing gap between phases (do not remove).
		tya
		pha
		tya
		pla
		tya

		// Even phase (clock low): sample next bit and shift it in as well.
		ldy #IEC_RECV_EVEN      // Output mask: ATN released, CLOCK low (phase 2)
		lda cia2_pra 			// Read PRA again for next bit
		sty cia2_pra 			// Drive even-phase outputs on PRA
		rol                     // DATA IN (bit7) → C
		rol iec_shiftreg        // iec_shiftreg <<= 1; insert C at bit0

		// Short settle before returning to caller.
		tya
		tay
		rts
/*
 * ===========================================
 * iec_send_byte — emit 8 bits (MSB→LSB) via two-bit phases
 *
 * Summary:
 *   Sends one byte over the serial bus by storing it in iec_shiftreg and invoking
 *   iec_send_pair four times. Each call transmits two bits using a
 *   clock-high (odd) then clock-low (even) phase, for a total of eight bits MSB-first.
 *
 * Arguments:
 *   A (accumulator)          Byte to transmit (MSB sent first).
 *
 * Returns:
 *   None                     No direct return value.
 *   Flags                    Undefined on return (clobbered by subcalls).
 *
 * Globals Used:
 *   iec_shiftreg		     Shift source used by the bit-pair sender.
 *
 * Clobbers:
 *   A                        Overwritten when stored to iec_shiftreg.
 *   X, Y                     Preserved by this wrapper (subroutine may use them).
 *
 * Description:
 *   Stores A into iec_shiftreg, then performs four phases of iec_send_pair.
 *   That subroutine rotates the next bit from iec_shiftreg into carry and drives CIA2
 *   PRA masks to place DATA and toggle CLOCK (odd/high then even/low), achieving the
 *   required handshaked bit timing on the bus.
 * ===========================================
 */
* = $4723
iec_send_byte:
		sta iec_shiftreg
		jsr iec_send_pair
		jsr iec_send_pair
		jsr iec_send_pair
		jsr iec_send_pair
		rts
/*
 * ===========================================
 * iec_send_pair — emit 2 bits via CIA2 PRA (odd/even clock phases)
 *
 * Summary:
 *   Transmits two consecutive bits from iec_shiftreg, MSB-first, using two phases:
 *   an ODD phase with CLOCK high and an EVEN phase with CLOCK low. Each phase
 *   rotates the next data bit into C (ROL iec_shiftreg), selects a PRA mask
 *   based on C (DATA=0/1), writes it to cia2_pra, then waits briefly for timing.
 *
 * Arguments:
 *   iec_shiftreg                    Shift source; top two bits are consumed by this call.
 *
 * Returns:
 *   None                           Bus lines updated; no register return.
 *   Flags                          Clobbered/undefined (uses ROL/TXA/TYA/TAY).
 *
 * Clobbers:
 *   A, Y, C                        X preserved.
 *
 * Description:
 *
 *   Phase ODD:
 *     • ROL iec_shiftreg → next MSB into C.
 *     • If C=0 use IEC_SEND_ODD_D0 else IEC_SEND_ODD_D1; write to cia2_pra.
 *     • Short delay to satisfy bus timing.
 *
 *   Phase EVEN:
 *     • ROL iec_shiftreg → next MSB into C.
 *     • If C=0 use IEC_SEND_EVEN_D0 else IEC_SEND_EVEN_D1; write to cia2_pra.
 *     • Short settle; RTS.
 *
 *   After one call, iec_shiftreg has been shifted left twice; the two emitted bits
 *   were the former bit7 and bit6 (MSB-first stream).
 * ===========================================
 */
* = $4733
iec_send_pair:
		/*
		 * Emit two bits using two phases: ODD (clock high) then EVEN (clock low).
		 * Default to DATA=0 mask for the odd phase; ROL moves next data bit into C.
		 */
		ldy #IEC_SEND_ODD_D0            	// ODD phase mask: CLOCK high, DATA=0 (ATN as required)
		rol iec_shiftreg               	// bit7 → C (MSB-first stream)
		bcc iec_set_phase_odd      		// if C=0 keep DATA=0; else pick DATA=1 mask below
		ldy #IEC_SEND_ODD_D1             // ODD phase mask: CLOCK high, DATA=1
iec_set_phase_odd:
		sty cia2_pra 	// drive odd-phase outputs on PRA

		// Small timing gap between phases (do not remove).
		txa
		tya
		txa
		tay

		// EVEN phase: repeat with CLOCK low. Default to DATA=0; ROL exposes next bit in C.
		ldy #IEC_SEND_EVEN_D0           	// EVEN phase mask: CLOCK low, DATA=0
		rol iec_shiftreg               	// next bit → C
		bcc iec_set_phase_even      	// if C=0 keep DATA=0; else select DATA=1 mask
		ldy #IEC_SEND_EVEN_D1            // EVEN phase mask: CLOCK low, DATA=1
iec_set_phase_even:
		sty cia2_pra 	// drive even-phase outputs on PRA

		// Short settle before returning.
		txa
		tay
		rts