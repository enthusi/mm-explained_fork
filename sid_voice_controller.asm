/*
sound_irq_handler:
.C:481b  AD 19 48    LDA sound_processing_disabled_flag

jump_to_music_code:
.C:48d4  20 00 00    JSR $0000		;Inlined address

init_sound_voices:
.C:48d8  A9 00       LDA #$00

start_sound_for_voice:
.C:4939  A8          TAY

update_instruction_ptr_and_offset:
.C:49fc  AE 0D 48    LDX active_voice

setup_music_pointers:
.C:4c76  8A          TXA

stop_music:
;Stop playback
.C:4caa  A9 00       LDA #$00

swap_voice_settings:
.C:51a5  E0 03       CPX #$03

clear_all_alternate_settings:
.C:52d0  48          PHA

process_arpeggio:
.C:52e5  E0 04       CPX #$04

adjust_waveform_and_filter_for_voice:
.C:5342  E0 03       CPX #$03

jump_to_inlined_music_routine:
inlined_music_ptr = $53cb
.C:53ca  4C FF FF    JMP $FFFF
*/
#importonce

#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sound_engine.asm"

/*
================================================================================
  update_filter_and_volume
================================================================================
Summary
	Read filter and master volume parameters from the voice script
	stream and apply them to the SID filter control and master volume
	registers, keeping software shadows in sync.

Arguments
	Y       Offset from voice_read_ptr for the filter/volume parameter bytes in 
			the current voice script

Global Inputs
	voice_read_ptr             Base pointer to current voice script
	filter_control_reg_copy    Shadow of SID filter control register
	sid_master_volume_saved    Previous cached SID master volume byte

Global Outputs
	filter_control_reg_copy    Updated merged filter control shadow
	filter_control_register    SID filter control hardware register
	sid_master_volume_saved    Updated cached master volume byte
	sid_master_volume          SID master volume hardware register

Returns
	None (updates SID filter control and master volume)

Description
	- Reads a filter/control byte from the voice script via (voice_read_ptr),Y
	and merges it (OR) into filter_control_reg_copy.
	- Writes the merged filter byte both to the shadow copy and to the SID filter 
	control register.
	- Advances Y and reads a master volume byte from the script, storing it into 
	sid_master_volume_saved.
	- Applies the same master volume byte to the SID master volume register so 
	hardware and shadow remain consistent.
================================================================================
*/
* = $49E7
update_filter_and_volume:
		// ------------------------------------------------------------
		// Update SID filter control using next byte from voice script
		// (merge new flags with existing shadow and commit to SID)
		// ------------------------------------------------------------
		lda     (voice_read_ptr),y        // Read filter/flags byte from current voice script position
		ora     filter_control_reg_copy   // Combine new filter bits with existing shadow state
		sta     filter_control_reg_copy   // Update shadow copy used by subsequent updates
		sta     filter_control_register   // Commit combined value to SID filter control register
		iny                               // Advance to next script byte (master volume)
		
		// ------------------------------------------------------------
		// Update SID master volume from voice script and cache in shadow
		// ------------------------------------------------------------
		lda     (voice_read_ptr),y        // Read master volume byte from voice script
		sta     sid_master_volume_saved   // Cache volume in software copy for later reference
		sta     sid_master_volume         // Apply volume to SID master volume hardware register
		rts                               // Done updating filter and volume; return to caller
/*
================================================================================
  update_duration_and_glissando
================================================================================
Summary
	Tick the per-voice duration timer for logical voice X and either
	apply a glissando step to its cached frequency while time remains
	or, when the timer expires, advance the voice script and refresh
	its frequency/envelope instead.

Arguments
	X       Logical voice index

Vars/State
	voice_sid_range_flags   	Cached boolean flag indicating X < LOGICAL_VOICE_LIMIT
	x_saved          			Last logical voice index processed and saved for reuse

Global Inputs
	voice_duration_lo        Per-voice duration counter low byte table
	voice_duration_hi        Per-voice duration counter high byte table
	voice_gliss_lo             Per-voice glissando delta low byte table
	voice_gliss_hi             Per-voice glissando delta high byte table
	voice_freq_lo     Cached per-voice frequency low byte table
	voice_freq_hi     Cached per-voice frequency high byte table

Global Outputs
	voice_duration_lo        Updated 16-bit duration counter low byte
	voice_duration_hi        Updated 16-bit duration counter high byte
	voice_freq_lo     Glissando-adjusted frequency low byte
	voice_freq_hi     Glissando-adjusted frequency high byte

Returns
	None (may call decode_voice_instruction and update_voice_freq_and_env
	as part of expiring the current instruction)

Description
	- Updates a 16-bit duration counter for logical voice X by decrementing 
	the low byte and propagating borrow into the high byte when needed.
	- If the resulting duration is still non-zero, applies one glissando delta 
	step to the cached 16-bit frequency and then commits the new pitch 
	via update_voice_freq_and_env.
	- If the duration underflows to zero, calls	decode_voice_instruction to 
	advance the voice script and then refreshes the voice’s frequency/envelope 
	instead of applying	further glissando.
	- Records whether X is within the primary SID voice range in voice_sid_range_flags 
	and saves X into x_saved for use by other engine code that needs to know 
	which voice was last processed.
================================================================================
*/
* = $4A1B
update_duration_and_glissando:
		// ------------------------------------------------------------
		// Flag whether logical voice X is within the primary SID-voice range
		// (0..LOGICAL_VOICE_LIMIT-1) and cache that as $FF/00 in voice_sid_range_flags
		// ------------------------------------------------------------
		lda     #BTRUE                 // Provisional “true” for X < 3
		cpx     #LOGICAL_VOICE_LIMIT          // Compare logical voice index against 3
		bmi     store_voice_index_flag        // If X < 3 → keep true
		
		lda     #FALSE                        // Else X >= 3 → flag false
store_voice_index_flag:
		sta     voice_sid_range_flags       // Record whether voice slot is one of the first three

		// Preserve caller’s X (logical voice index) across any helper calls
		stx     x_saved

		// ------------------------------------------------------------
		// Duration countdown
		//
		// 16-bit duration countdown: decrement low byte and, on underflow,
		// propagate borrow into the high byte. If the final result is still
		// non-negative (C=1), the duration remains > 0 and we continue by
		// applying a glissando step; otherwise the timer has just expired.
		// ------------------------------------------------------------
		// Decrement low duration byte; carry tells whether it underflowed
		lda     voice_duration_lo,x           
		sec                                   
		sbc     #$01                          
		sta     voice_duration_lo,x           
		bcs     apply_glissando_step          // If no borrow (C=1) → duration still > 0

		// Low byte underflowed → propagate borrow into high duration byte
		lda     voice_duration_hi,x           
		sbc     #$00                          
		sta     voice_duration_hi,x           
		bcs     apply_glissando_step          // If still non-negative → duration not yet expired

		// ------------------------------------------------------------
		// Duration reached zero
		//
		// Advance this voice’s script to the next instruction and then 
		// recompute/commit its new frequency and envelope to SID; 
		// no further glissando is applied on this tick.
		// ------------------------------------------------------------
		jsr     decode_voice_instruction    // Fetch and execute next voice instruction(s)
		jsr     update_voice_freq_and_env 	  // Apply new frequency/ADSR after instruction change
		rts                                   

		// ------------------------------------------------------------
		// Apply glissando
		// 
		// Apply one signed glissando step to the cached 16-bit frequency
		// for logical voice X, then let the caller commit the updated
		// pitch to SID while the duration timer is still non-zero
		// ------------------------------------------------------------
apply_glissando_step:
		lda     voice_freq_lo,x        
		clc                                   
		adc     voice_gliss_lo,x                
		sta     voice_freq_lo,x        

		lda     voice_freq_hi,x        
		adc     voice_gliss_hi,x                
		sta     voice_freq_hi,x        

		// ------------------------------------------------------------
		// Update frequency
		// 
		// Commit the glissando-adjusted frequency (and envelope as well)
		// to the SID registers for this voice, then return with duration
		// still active so subsequent ticks can continue the slide
		// ------------------------------------------------------------
		jsr     update_voice_freq_and_env 	  // Push new frequency to SID (and ADSR if applicable)
		rts                                   // Return with updated pitch while duration > 0
/*
================================================================================
  clear_voice_duration_and_glissando
================================================================================
Summary
	Reset the per-voice duration counter and glissando delta so the
	current logical voice X has no remaining play time and no active
	pitch slide.

Arguments
	X       Logical voice index whose duration and glissando are cleared

Global Inputs
	voice_duration_lo         Low byte of per-voice duration counter
	voice_duration_hi         High byte of per-voice duration counter
	voice_gliss_lo              Low byte of per-voice glissando delta
	voice_gliss_hi              High byte of per-voice glissando delta

Global Outputs
	voice_duration_lo         Cleared to zero for voice X
	voice_duration_hi         Cleared to zero for voice X
	voice_gliss_lo              Cleared to zero for voice X
	voice_gliss_hi              Cleared to zero for voice X

Returns
	None (updates duration and glissando state in RAM for voice X)

Description
	- Writes zero into the 16-bit duration counter for logical voice X.
	- Writes zero into the 16-bit glissando delta for logical voice X.
	- Effectively disables both countdown timing and pitch slide for
	this voice so subsequent ticks see no residual state.
================================================================================
*/
* = $4A5C
clear_voice_duration_and_glissando:
		// ------------------------------------------------------------
		// Clear duration and glissando for current voice slot X (zero both
		// duration bytes and both glissando bytes to disable them)
		// ------------------------------------------------------------
		lda     #$00
		sta     voice_duration_lo,x
		sta     voice_duration_hi,x
		sta     voice_gliss_lo,x
		sta     voice_gliss_hi,x
		rts

/*
================================================================================
  update_voice_freq_and_env
================================================================================
Summary
        Commit the cached 16-bit frequency (or filter cutoff for the special
        logical slot) and ADSR envelope for logical voice X to the SID.
        When arpeggio is active, the routine performs no updates because the
        arpeggio engine owns all pitch changes during its operation.

Arguments
        X       Logical voice index

Global Inputs
        arpeggio_ongoing             Indicates whether arpeggio is currently active
        voice_freq_reg_ofs_tbl  Per-voice SID base register offsets
        voice_freq_lo         Cached low-byte frequency/cutoff table
        voice_freq_hi         Cached high-byte frequency/cutoff table
        voice_adsr_attack_decay          Cached attack/decay envelope values
        voice_adsr_sustain_release       Cached sustain/release envelope values

Global Outputs
        voice1_freq_reg_lo           SID frequency low-byte registers (voice-relative)
        voice1_freq_reg_hi           SID frequency high-byte registers (voice-relative)
        voice1_attack_delay_reg      SID attack/decay registers (voice-relative)
        voice1_sustain_release_reg   SID sustain/release registers (voice-relative)

Returns
        None (writes frequency/cutoff and optionally ADSR to SID registers)

Description
        - If arpeggio_ongoing is nonzero, exit immediately to preserve the
          arpeggio engine’s pitch updates.
        - Resolve the SID register base offset for logical voice X.
        - Write the cached 16-bit frequency into the SID registers; for the
          special logical slot (X == 3), this is interpreted as a filter cutoff 
		  instead of a tone frequency.
        - For logical voices below LOGICAL_VOICE_LIMIT, also update the ADSR
          envelope using the cached attack/decay and sustain/release values.
================================================================================
*/
* = $4BE6
update_voice_freq_and_env:
		// ------------------------------------------------------------
		// Arpeggio guard
		//
		// Skip frequency/ADSR commit when arpeggio is active; arpeggio logic
		// owns pitch changes while the effect is running, so only update
		// registers when no arpeggio is in progress
		// ------------------------------------------------------------
		lda     arpeggio_ongoing              // A := arpeggio active flag
		beq     update_freq_env_no_arpeggio   // Z=1 → no arpeggio → allow updates
		rts                                   // Arpeggio running → skip all changes

update_freq_env_no_arpeggio:
		// ------------------------------------------------------------
		// Register offset resolution
		//
		// Resolve per-voice SID base offset for logical voice X so subsequent
		// stores hit the correct frequency (or cutoff) register block via Y
		// ------------------------------------------------------------
		lda     voice_freq_reg_ofs_tbl,x // A := base offset for this voice in SID register map
		tay                                   // Y := per-voice register offset

		// ------------------------------------------------------------
		// Commit frequency/filter cutoff
		//
		// Push cached 16-bit pitch value for logical voice X into SID:
		// for voices 0–2 this is the tone frequency; for the special
		// logical slot (X == 3) the same value is used as filter cutoff
		// ------------------------------------------------------------
		lda     voice_freq_lo,x        
		sta     voice1_freq_reg_lo,y          
		lda     voice_freq_hi,x        
		sta     voice1_freq_reg_hi,y          

		// ------------------------------------------------------------
		// Physical voice guard
		//
		// Only voices below LOGICAL_VOICE_LIMIT have a full ADSR envelope
		// mapped; treat X >= LOGICAL_VOICE_LIMIT as a filter-only slot and
		// bail out before touching ADSR registers
		// ------------------------------------------------------------
		cpx     #LOGICAL_VOICE_LIMIT          
		bpl     exit_voice_freq_env           // X >= 3 → skip ADSR transfer

		// ------------------------------------------------------------
		// Commit envelope (ADSR)
		//
		// Transfer cached ADSR envelope for this mapped voice into SID:
		// write packed attack/decay and sustain/release nibbles to the
		// per-voice ADSR registers addressed via the base offset in Y
		// ------------------------------------------------------------
		lda     voice_adsr_attack_decay,x         
		sta     voice1_attack_delay_reg,y     
		lda     voice_adsr_sustain_release,x      
		sta     voice1_sustain_release_reg,y  

exit_voice_freq_env:
		rts                                   
/*
================================================================================
  update_voice_control
================================================================================

Summary
	Write the cached control byte for logical voice X to the appropriate
	SID voice control register, if that logical voice is mapped to a
	physical SID channel.

Arguments
	X       Logical voice index (0..LOGICAL_VOICE_LIMIT-1)

Global Inputs
	voice_freq_reg_ofs_tbl   Per-voice SID register base offsets
	voice_ctrl_shadow                Cached SID control bytes per logical voice

Global Outputs
	voice1_control_register       SID voice control register block

Returns
	None (updates SID control register for the selected logical voice)

Description
	- Rejects logical voices X >= LOGICAL_VOICE_LIMIT (no mapped SID voice).
	- Looks up the base SID register offset for logical voice X.
	- Writes the cached control value for X into the corresponding
	SID voice control register.
================================================================================
*/
* = $4C0D
update_voice_control:
		// ------------------------------------------------------------
		// Physical voice guard
		//
		// skip control update entirely if logical voice X has no SID channel
		// ------------------------------------------------------------
		cpx     #LOGICAL_VOICE_LIMIT           
		bpl     exit_update_voice_control      // Branch if out-of-range logical voice

		// ------------------------------------------------------------
		// Register offset resolution
		//
		// Resolve per-voice SID register offset so we can address the correct control register
		// ------------------------------------------------------------
		lda     voice_freq_reg_ofs_tbl,x  // A := base offset for this voice’s SID register block
		tay                                    // Y := offset into SID register space

		// ------------------------------------------------------------
		// Commit control value
		//
		// Commit cached control byte for logical voice X to the mapped SID
		// voice control register at the resolved offset in Y
		// ------------------------------------------------------------
		lda     voice_ctrl_shadow,x               // A := cached control value for logical voice X
		sta     voice1_control_register,y      // Write to correct SID voice control register

exit_update_voice_control:
		rts                                    
