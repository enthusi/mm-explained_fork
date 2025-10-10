#importonce

.label decomp_src_ptr 		= $27    // 16-bit pointer to compressed input stream 
.label kbd_keycode         = $30    // ZP: published keycode when ready
.label joy_state     	   = $33    // ZP: snapshot of joystick mask (bits 0–4)
.label print_msg_ptr	   = $DA	// ZP: 16-bit pointer to message to be printed (print_message_wait_for_button)
.label debug_error_code = $DC
.label kbd_key_ready       = $2061    // flag: 1 when kbd_keycode is valid

// --- Copy/write byte-count accumulator ---
.label disk_copy_count_lo 			= $463C // 16-bit count of bytes to copy/write: low byte
.label disk_copy_count_hi 			= $463D // 16-bit count of bytes to copy/write: high byte
                                      // (write path rounds up to whole sectors: hi += (lo != 0))
									  
.label rsrc_read_offset             = $5678 // Byte offset into resource stream (passed in X to disk_seek_read).
.label rsrc_sector_idx             = $5679 // Sector index in physical chain (passed in Y to disk_seek_read).  

.label saved_script_index  = $FEA6    	// external flag controlling delay init

.label object_ptr_lo_tbl = $705E
.label object_ptr_hi_tbl = $708B
.label room_ptr_hi_tbl = $77F3		//Total rooms: 55
.label room_ptr_lo_tbl = $782A
.label costume_ptr_hi_tbl = $7861		//Total costumes: 25
.label costume_ptr_lo_tbl = $787A
.label sound_ptr_hi_tbl = $78C5			//Total sounds: 70
.label sound_ptr_lo_tbl = $790B
.label script_ptr_hi_tbl = $7997		//Total scripts: 160
.label script_ptr_lo_tbl = $7A37
.label room_layers_ptr_hi_tbl = $7B77		//Total type 4 resources: 2	- room scene graphic layers
.label room_layers_ptr_lo_tbl = $7B79

.label room_mem_attrs = $77BC
.label costume_mem_attrs = $77A3
.label sound_mem_attrs       = $7951    // table: per-sound usage attributes, indexed by resource_index
.label script_mem_attrs = $7AD7

.label reload_snd_rsrc_ptrs     = $FE70    // flag: non-zero => refresh sound resource pointers after relocation
.label rsrc_released_flag = $FE4A

.label room_for_character = $FEFC
.label current_room = $7149


.label active_side_id = $fe9c //This represents the desired Side ID (the ID that identifies each of the game's disk sides)
						//Once disk_ensure_side is called, this parameter is set and execution will not resume until the proper disk side is inserted
.label current_kid = $FF67
						
						
.label pause_game = $0						
.label unpause_game = $0						
.label print_message_wait_for_button = $0 // UI: print message pointed by msg_to_print_ptr, wait for button
.label release_one_costume = $0
.label unassign_actor_for_costume = $0

.label costume_y_destination = $ff15
.label costume_x_destination = $ff2e

