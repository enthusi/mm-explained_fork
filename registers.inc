#importonce

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
