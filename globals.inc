#importonce

.label decomp_src_ptr 		= $27    // 16-bit pointer to compressed input stream 
.label kbd_keycode         = $30    // ZP: published keycode when ready
.label joy_state     	   = $33    // ZP: snapshot of joystick mask (bits 0–4)
.label print_msg_ptr	   = $DA	// ZP: 16-bit pointer to message to be printed (print_message_wait_for_button)
.label debug_error_code = $DC
.label room_bg0_shadow = $8f
.label room_bg1_shadow = $90


.label kbd_key_ready       = $2061    // flag: 1 when kbd_keycode is valid

.label room_scene_left_edge = $0664	 //Left column for the visible room scene
.label room_scene_right_edge = $0665 //Right column for the visible room scene
.label camera_desired_position = $0666
.label camera_current_position = $0667
.label camera_mode = $066a				//02 = follow costume, #03 = pan to position
.label camera_costume_to_follow = $066b	//Index of costume to follow
.label pan_camera_to = $066c				//Position to pan camera to
.label actor_follow_needs_pan = $066d		

// --- Copy/write byte-count accumulator ---
.label disk_copy_count_lo 			= $463C // 16-bit count of bytes to copy/write: low byte
.label disk_copy_count_hi 			= $463D // 16-bit count of bytes to copy/write: high byte
                                      // (write path rounds up to whole sectors: hi += (lo != 0))
									  
.label rsrc_read_offset             = $5678 // Byte offset into resource stream (passed in X to disk_seek_read).
.label rsrc_sector_idx             = $5679 // Sector index in physical chain (passed in Y to disk_seek_read).  

.label saved_script_index  = $FEA6    	// external flag controlling delay init

.label total_objects_in_room = $715c
.label obj_gfx_ptr_tbl = $7291
.label room_obj_ofs_tbl = $72c9		//Offsets of objects in a room resource
.label room_object_index_hi = $7221
.label room_object_index_lo = $723d
.label object_x_start = $715d
.label object_y_start = $7179
.label object_width = $7195
.label object_height = $71b1
.label object_x_destination = $71cd
.label object_y_destination = $71e9
.label object_destination_active = $7205
.label parent_object = $7259
.label parent_linkable = $7275


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
.label mask_bit_patterns_hi = $7b78		
.label mask_bit_patterns_lo = $7b7a
.label room_width = $7139
.label room_height = $713a
.label room_bg0 = $713b
.label room_bg_colors = $713b
.label room_tile_definitions_offset = $713f

.label room_mem_attrs = $77BC
.label costume_mem_attrs = $77A3
.label sound_mem_attrs       = $7951    // table: per-sound usage attributes, indexed by resource_index
.label script_mem_attrs = $7AD7

.label reload_snd_rsrc_ptrs     = $FE70    // flag: non-zero => refresh sound resource pointers after relocation
.label rsrc_released_flag = $FE4A

.label room_for_character = $FEFC
.label current_room = $7149


.label rsrc_resource_index          = $5676 // Resource index within its type (byte).
.label rsrc_resource_type           = $5677 // Resource type identifier (byte).

.label active_side_id = $fe9c //This represents the desired Side ID (the ID that identifies each of the game's disk sides)
						//Once disk_ensure_side is called, this parameter is set and execution will not resume until the proper disk side is inserted
						
						
.label costume_y_destination = $ff15
.label costume_x_destination = $ff2e
.label game_paused = $1679

.label current_room_rsrc = $715B	//Used by draw_room_scene to ensure residence of the room resources

.label costume_for_actor = $fe53		//#FF = actor not in use
.label actor_for_costume = $fe57		//Bit 7 set - actor moving, clear - not moving

/*
===========================================================
 Game script variables
===========================================================
*/
.label current_kid = $ff67
.label game_vars = $ff67
/*
These vars are copies of some game engine variables.
I did not find any usage of them in the final scripts.
They were definitely intended as debugging aids.
*/
.label var_camera_position = $ff69			//This would be var[2]
.label var_msg_flag = $ff6a				//This would be var[3]
.label var_current_room = $ff6b			//This would be var[4]
.label var_active_indirect_object = $ff6c	//This would be var[5]
.label var_destination_x = $ff6d			//This would be var[6]
.label var_destination_y = $ff6e			//This would be var[7]
.label var_music_playback_in_progress = $ff6f	//This would be var[8]
.label var_active_verb = $ff70				//This would be var[9]
.label var_message_length = $ff71			//This would be var[10]
//;FF72 and onwards are no longer used as debugging variables
.label kid_ids = $ffc8
.label first_kid_id = $FFC8
.label second_kid_id = $FFC9
.label third_kid_id = $FFCA
.label costume_room_idx = $fefc	 //The CURRENT room where a costume is present


//Placeholders for routines not modularized yet
.label print_message_wait_for_button = $0 // UI: print message pointed by msg_to_print_ptr, wait for button
.label release_one_costume = $0
.label unassign_actor_for_costume = $0
.label reset_sentence_queue_system = $0
.label execute_room_exit_script = $0
.label execute_room_entry_script = $0
.label open_shutter_flag = $0
.label clear_actors_visibility_in_scene = $0
.label init_sprites_sound = $0
.label raster_setup = $0
.label assign_available_actor = $0
.label setup_room_scene_rsrc = $0


