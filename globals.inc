#importonce

.label decomp_src_ptr 		= $27    // 16-bit pointer to compressed input stream (low at $27, high at $28)
.label kbd_keycode         = $0030    // ZP: published keycode when ready
.label kbd_key_ready       = $2061    // flag: 1 when kbd_keycode is valid
.label joy_state     	   = $0033    // ZP: snapshot of joystick mask (bits 0–4)

.label sound_memory_attrs       = $7951    // table: per-sound usage attributes, indexed by resource_index
.label reload_snd_rsrc_ptrs     = $fe70    // flag: non-zero => refresh sound resource pointers after relocation
.label saved_script_index  = $FEA6    	// external flag controlling delay init



