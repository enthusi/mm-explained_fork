#importonce

.label cpu_port = $0001		// CPU I/O port: map I/O/KERNAL/BASIC


.label vic_sprite0_x_lo_reg           = $D000	// Sprite 0 X low byte (bits 0–7). Bit 8 lives in vic_sprite_x_msb_reg bit 0.
.label vic_sprite0_y_reg              = $D001	// Sprite 0 Y position (0–255). Top-left origin, increases downward.
.label vic_sprite1_y_reg              = $D003	// Sprite 1 Y position (0–255).
.label vic_sprite2_y_reg              = $D005	// Sprite 2 Y position (0–255).
.label vic_sprite3_y_reg              = $D007	// Sprite 3 Y position (0–255).
.label vic_sprite4_y_reg              = $D009   // Sprite 4 Y position (0–255).
.label vic_sprite5_y_reg              = $D00B	// Sprite 5 Y position (0–255).
.label vic_sprite6_y_reg              = $D00D	// Sprite 6 Y position (0–255).
.label vic_sprite7_y_reg              = $D00F	// Sprite 7 Y position (0–255). In this codebase, sprite 7 is the cursor.
.label vic_sprite7_x_lo_reg           = $D00E	// Sprite 7 X low byte (bits 0–7) for the cursor. Bit 8 at vic_sprite_x_msb_reg bit 7.
.label vic_sprite_x_msb_reg       	  = $D010	// X high bits for all 8 sprites. Bit n = MSB of sprite n X (adds +256 when set).
												//   bit0→spr0, bit1→spr1, …, bit7→spr7.

// Sprite horizontal expansion control. Bit n doubles the width of sprite n (2×).
//   0 = normal width, 1 = expanded. Write-only on VIC-II.
//   bit0→spr0, bit1→spr1, …, bit7→spr7.
.label vic_sprite_x_expand_reg         = $D01D


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
