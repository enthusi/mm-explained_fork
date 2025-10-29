#importonce

.label cpu_port = $0001		// CPU I/O port: map I/O/KERNAL/BASIC

//======================================
//Screen control register 1 bits meaning
//======================================
//	Bit 7		High bit of raster compare register (D012)
//	Bit 6		Enable extended color mode
//	Bit 5 		Enable bitmap graphics mode
//	Bit 4		Blank entire screen to same color as border (0 = blank)
//	Bit 3		1 = 25 row display, 0 = 24 rows
//	Bits 0-2	Fine scroll display vertically by X scan lines (0-7)
.label vic_screen_control_reg_1 = $D011    // Vertical scroll + 25/24 row select + raster high bit + display enable
.label vic_raster_line_reg = $D012    // Current raster line (0–255); low 8 bits of beam position
.label vic_memory_layout_reg = $D018    // Screen and character memory base select (video matrix + char set)
.label vic_border_color  = $D020
.label vic_bg0_reg = $D021


.label cia1_pra        	= $DC00   	// PRA — CIA1 Port A data register
                                /*
                                 * Holds current output values when bits are set as outputs,
                                 * or reads inputs when bits are configured as inputs.
                                 * Used for keyboard column drive and joystick sense.
                                 */

.label cia1_prb        	= $DC01   	// PRB — CIA1 Port B data register
                                /*
                                 * Reads the current logic levels on Port B input lines.
                                 * On the C64, bits 0–4 correspond to joystick #2 inputs (active low),
                                 * and all 8 bits participate in the keyboard matrix scan.
                                 */

.label cia1_ddra   		= $DC02   	// DDRA — CIA1 Port A Data Direction Register
                                /*
                                 * Each bit controls the direction of the corresponding line:
                                 *   1 = output, 0 = input.
                                 * Setting DDRA=$FF makes all Port A lines outputs.
                                 */

.label cia1_ddrb   		= $DC03   	// DDRB — CIA1 Port B Data Direction Register
                                /*
                                 * Works like DDRA, but for Port B.
                                 * Setting DDRB=$00 makes all Port B lines inputs.
                                 */

.label cia2_pra 			= $DD00      			// CIA2 Port A (PRA). Read to sample inputs; write masks to drive IEC outputs.
