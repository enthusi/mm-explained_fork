#importonce

.label fill_dest_ptr            = $19   // 16-bit destination pointer (lo at $19, hi at $1A); advanced after each store
.label fill_byte_cnt            = $1b   // 16-bit remaining byte count (lo at $1B, hi at $1C); decremented to zero

/*
Video memory mode values
	#00		Do nothing
	#01		Character base: D800	Video matrix: C800	Sprite shape: CBF8
	#02		Character base: D800	Video matrix: CC00	Sprite shape: CFF8
	#03		Copy color data only
*/
.label video_setup_mode = $22

//This variable (frame_buffer) can only take values of #01 or #02, it's just the index
//#01 - frame_buffer_base = CC28, #02 = C828 (yes, they are not in numerical order)
.label frame_buffer = $23
.label frame_buffer_base = $24		//Base address of frame buffer

.label decomp_src_ptr 		= $27    // 16-bit pointer to compressed input stream 
.label decomp_emit_mode    = $29    // current mode flag ($00 = direct, $FF = run). Only bit 7 is tested
.label decomp_emit_rem     = $2A    // remaining outputs for the current operation (stores L, meaning L+1 total outputs)
.label decomp_run_symbol   = $2B    // byte value to output while in run mode

.label kbd_keycode         = $30    // ZP: published keycode when ready
.label joy_state     	   = $33    // ZP: snapshot of joystick mask (bits 0–4)
.label vertical_offset 	  = $35
.label current_sprite_y_pos = $3f

/*
Values observed
#00	Scroll right
#FF	Scroll left
#C8	No scroll
#01	Room scene rendering starts (by opening the "lens" outwards)
*/
.label screen_scroll_flag = $4e

.label cel_seq_tbl           = $84    // cel sequence table for current actor
.label vic_memory_layout_shadow = $be

.label actor = $D8
.label print_msg_ptr	   = $DA	// ZP: 16-bit pointer to message to be printed (print_message_wait_for_button)
.label irq_entry_count = $d9
.label debug_error_code = $DC
.label irq_entry_lock = $e3
.label mask_row_ptr                = $86    // ZP: pointer to current 8×8-tile row within foreground mask layer
.label sprite_shape_data_ptr = $8c
.label video_update_signal = $8e			// synchronization flag - NOT to be confused with vsync
.label room_bg0_shadow = $8f
.label room_bg1_shadow = $90
.label needs_sentence_rebuild = $95

.label video_processed_signal = $166b

/*
===========================================================
 Sprite variables
===========================================================
*/
// Double-buffer selector for sprite attribute buffers. #$00 or #$01.
.label sprite_bank_sel                 = $FD14

// X low byte (bits 0–7) per actor-assigned sprite slot.
.label sprite_0_x_lo                   = $0FCA
.label sprite_1_x_lo                   = $0FCB
.label sprite_2_x_lo                   = $0FCC
.label sprite_3_x_lo                   = $0FCD
.label sprite_4_x_lo                   = $0FCE
.label sprite_5_x_lo                   = $0FCF
.label sprite_6_x_lo                   = $0FD0

// Cursor (sprite 7) X low byte.
.label cursor_sprite_x_lo              = $0FD1

// Per-sprite color nibbles (0–15). Written to $D027..$D02D.
.label sprite_0_color                  = $0FD2
.label sprite_1_color                  = $0FD3
.label sprite_2_color                  = $0FD4
.label sprite_3_color                  = $0FD5
.label sprite_4_color                  = $0FD6
.label sprite_5_color                  = $0FD7
.label sprite_6_color                  = $0FD8

// Cursor (sprite 7) color nibble (0–15). Written to $D02E.
.label cursor_sprite_color             = $0FD9

// Hardware sprite slot indices (0–7). Map logical actors → VIC sprite slots.
.label vic_sprite_idx_0                = $0FDA
.label vic_sprite_idx_1                = $0FDB
.label vic_sprite_idx_2                = $0FDC
.label vic_sprite_idx_3                = $0FDD
.label vic_sprite_idx_4                = $0FDE
.label vic_sprite_idx_5                = $0FDF
.label vic_sprite_idx_6                = $0FE0

// Shadow of $D01D horizontal expansion control. Bit n expands sprite n (2× width).
.label sprite_horizontal_expansion     = $0FEA

// X high bit (bit 8) per sprite as 0/1 bytes. Mirrors packed version below.
.label sprite_0_x_hi                   = $0FE1
.label sprite_1_x_hi                   = $0FE2
.label sprite_2_x_hi                   = $0FE3
.label sprite_3_x_hi                   = $0FE4
.label sprite_4_x_hi                   = $0FE5
.label sprite_5_x_hi                   = $0FE6
.label sprite_6_x_hi                   = $0FE7

// Cursor (sprite 7) X high bit (bit 8) as 0/1 byte.
.label cursor_sprite_x_hi              = $0FE8

// Packed X high bits mirror of $D010. Bit n = MSB of sprite n X.
.label sprites_x_hi_combined           = $0FE9

// Cursor (sprite 7) Y position.
.label cursor_sprite_y                 = $0FEB


// -----------------------------------------------------------------------------
// Shape indices for successive raster IRQ handlers (per-scanline strip control)
// Each byte is a shape index written into the sprite shape table for that pass.
// -----------------------------------------------------------------------------
.label sprite_shape_1                  = $1661
.label sprite_shape_2                  = $1662
.label sprite_shape_3                  = $1663
.label sprite_shape_4                  = $1664
.label sprite_shape_5                  = $1665
.label sprite_shape_6                  = $1666
.label sprite_shape_7                  = $1667
.label sprite_shape_8                  = $1668

// Cursor shape index used across screen banks.
.label sprite_shape_cursor             = $1669

// Pending shapeset switch flag. 0 = no change; 1 or 2 = switch to that set.
.label target_sprite_shapeset          = $166A

//===========================================================
// Cursor state
//===========================================================
// Hide cursor flag. #$01 = hidden, #$00 = visible.
.label hide_cursor_flag               = $166C

// Down-counter for cursor color update cadence.
// Decremented by irq_handler1 each run. When it reaches 0, color advances and
// the counter is reloaded from cursor_color_period.
.label cursor_color_wait_ticks        = $166D

// Current cursor color table index (0..N-1). Advanced when wait counter expires.
.label cursor_color_idx               = $166E

// Reload value for wait counter (ticks per color step). Example: #$06.
.label cursor_color_period            = $166F

// Initial color index used when resetting the cursor color cycle.
.label cursor_color_idx_first         = $1670
//===========================================================
// Cursor physics
//===========================================================
//Multicolor bitmap mode has a resolution of 160x200

.label  cursor_x_pos                   = $CB7F   // cursor X position (pixels, 0..$9F)
.label  cursor_y_pos                   = $CB81   // cursor Y position (pixels, $08..$BF)

.label  cursor_x_vel_frac              = $CB82   // X velocity, fractional byte (8.8 fixed-point)
.label  cursor_x_vel_int               = $CB83   // X velocity, integer byte   (adds to X position)
.label  cursor_y_vel_frac              = $CB84   // Y velocity, fractional byte
.label  cursor_y_vel_int               = $CB85   // Y velocity, integer byte
.label  drag_shift_count        = $CB7D   // expected 7 (build drag via 7× ASL/ROL)

.label  accel_h_mask            = $CB7B   // horizontal acceleration mask (expects $FA)
.label  accel_v_mask            = $CB7C   // vertical   acceleration mask (expects $FF)
.label cursor_x_grid = $cb86		
.label cursor_y_grid = $cb87		//Range: 01-17 (01-23 decimal)
.label cursor_y_pos_half = $fe7d				//Vertical range #08-63 (08-99)
.label cursor_x_pos_quarter_relative = $fe7e			//Horizontal range #00-27 (00-39)			
.label cursor_y_pos_half_off_by_8 = $fe7f	//Vertical range #00-5B (00-91)
.label cursor_x_pos_quarter_absolute = $fe80	//Horizontal range #07-2E (07-46)


// ===========================================================

.label kbd_key_ready       = $2061    // flag: 1 when kbd_keycode is valid

// ===========================================================
.label viewport_left_col = $0664	 //Left column for the visible room scene
.label viewport_right_col = $0665 //Right column for the visible room scene
.label cam_target_pos = $0666
.label cam_current_pos = $0667
.label cam_mode = $066a				//02 = follow costume, #03 = pan to position
.label cam_follow_costume_id = $066b	//Index of costume to follow
.label cam_pan_goal = $066c				//Position to pan camera to
.label actor_follow_needs_pan = $066d		
// ===========================================================
.label color_ram_copy_done = $1677 //Boolean
.label ui_section_semaphore = $1678 //Semaphore used to control access to a section in irq_handler1
.label game_paused_flag = $1679	//#01 if game is paused, #00 otherwise



// --- Copy/write byte-count accumulator ---
.label disk_copy_count_lo 			= $463C // 16-bit count of bytes to copy/write: low byte
.label disk_copy_count_hi 			= $463D // 16-bit count of bytes to copy/write: high byte
                                      // (write path rounds up to whole sectors: hi += (lo != 0))
									  
// ===========================================================
.label rsrc_resource_index          = $5676 // Resource index within its type (byte).
.label rsrc_resource_type           = $5677 // Resource type identifier (byte).
.label rsrc_read_offset             = $5678 // Byte offset into resource stream (passed in X to disk_seek_read).
.label rsrc_sector_idx             = $5679 // Sector index in physical chain (passed in Y to disk_seek_read).  
// ===========================================================

.label room_obj_count = $715c	
.label obj_gfx_ptr_tbl = $7291
.label room_obj_ofs_tbl = $72c9		//Offsets of objects in a room resource
.label room_obj_id_hi_tbl = $7221	//Per-room objects ID table (mapping room object index to global Object ID)
.label room_obj_id_lo_tbl = $723d
.label obj_left_col_tbl = $715D		//Per-room object left column table (for rendering)
.label obj_top_row_tbl = $7179		//Per-room object top row table (for rendering)
.label obj_width_tbl = $7195		//Per-room object width in columns table (for rendering)
.label obj_height_tbl = $71b1		//Per-room object height in rows table (for rendering)
.label object_x_destination = $71cd
.label object_y_destination = $71e9
.label object_destination_active = $7205
.label parent_idx_tbl = $7259			//Room object index of the parent object - WARNING: it's the object index within a room's objects, NOT the object ID
										//For instance, Room 1 (Mansion entrance) has 19 objects. Among them:
										// Object index 3: key (Object ID 0-22)
										// Object index 4: doormat (Object ID 0-21)
										// The key has parent object INDEX = 4 (doormat)

.label ancestor_overlay_req_tbl = $7275	//Requirement for ancestor objects overlay state (boolean: $80 = True, $00 = False)
.label object_attributes = $7301		//Global attributes for objects (indexed by Object ID) - Note: these are NOT memory attributes like used for other resources	


// ===========================================================
.label tile_matrix_ofs   = $7141	// 16-bit offset to tile matrix section
.label color_layer_ofs   = $7143	// 16-bit offset to color layer section
.label mask_layer_ofs    = $7145	// 16-bit offset to mask layer section 
.label mask_indexes_ofs  = $7147	// 16-bit offset to mask indexes section
.label room_exit_script_ptr   = $7156	// 16-bit offset to exit script within room block
.label room_entry_script_ptr  = $7158	// 16-bit offset to entry script within room block


// ===========================================================
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
.label room_gfx_layers_hi = $7B77		//Total type 4 resources: 2	- room scene graphic layers
.label room_gfx_layers_lo = $7B79
.label mask_bit_patterns_hi = $7B78		
.label mask_bit_patterns_lo = $7B7A
.label mask_base_ptr = $fd6d
.label room_width = $7139
.label room_height = $713a
.label room_bg0 = $713b
.label room_bg_colors = $713b
.label room_tile_definitions_offset = $713f
.label current_room = $7149
.label room_gfx_layers_active_ptr = $FE98


// ===========================================================
.label room_mem_attrs = $77BC
.label costume_mem_attrs = $77A3
.label sound_mem_attrs       = $7951    // table: per-sound usage attributes, indexed by resource_index
.label script_mem_attrs = $7AD7
// ===========================================================

.label reload_sound_ptrs_flag     = $FE70    // flag: non-zero => refresh sound resource pointers after relocation
.label rsrc_released_flag = $FE4A
.label saved_script_index  = $FEA6    	// external flag controlling delay init

/*Lights_status values
#00	environment lights off, flashlights off
#01	environment lights off, flashlight on
#02	environment lights on*/
.label global_lights_state = $FEAB

.label room_for_character = $FEFC


.label active_side_id = $fe9c //This represents the desired Side ID (the ID that identifies each of the game's disk sides)
						//Once disk_ensure_side is called, this parameter is set and execution will not resume until the proper disk side is inserted
						
						
.label costume_dest_y = $ff15
.label costume_dest_x = $ff2e
.label dest_x = $fde6
.label dest_y = $fde7

.label current_room_rsrc = $715B	//Used by draw_room_scene to ensure residence of the room resources


/*
===========================================================
 Actor sprites, waypoint and position variables
===========================================================
*/
.label costume_for_actor = $fe53		//#FF = actor not in use
.label actor_for_costume = $fe57		//Bit 7 set - actor moving, clear - not moving
.label active_costume = $fc30
// ===========================================================
.label active_costume_rsrc_hi = $7893
.label active_costume_rsrc_lo = $78ac
.label actor_gfx_base_lo = $cb42				// Base address of the graphics section, for each actor (low byte)
.label actor_gfx_base_hi = $cb46				// Base address of the graphics section, for each actor (hi byte)
.label actor_ofs_cel_hi_tbl = $cb4a				// 8-bit offset (relative to *gfx base*) to the high-byte table of cel addresses, for each actor
.label actor_clip_tbl_lo = $fe90				// Base address of clip table, for each actor (low byte)
.label actor_clip_tbl_hi = $fe94				// Base address of clip table, for each actor (hi byte)
.label actor_cel_seq_tbl_lo = $cb3a				// Base address of cel sequence table, for each actor (low byte)
.label actor_cel_seq_tbl_hi = $cb3e				// Base address of cel sequence table, for each actor (hi byte)
.label target_clip_id            = $FDE9  // Target clip id (per-actor intent)
// ===========================================================
.label current_limb = $fc41
.label actor_limbs_ofs = $fc36
.label limb_frame_idx = $fc42					// Which animation cel to use as _current_ for the actor limb
.label limb_current_cel = $fc62						// Current cel sequence _offset_ for each limb - read the detail in the data structures file
.label limb_target_cel_seq = $fca2			// Which animation cel to use as _base_ for the actor limb
.label limb_current_cel_seq = $fc82
.label limb_flip_tbl = $fc05					// #00 = no horizontal flip for the limb, #80 = horizontal flip for the limb
// Loop count:
// #00 = stop animation when frames are exhausted
// #FF = loop animation from the first frame when frames are exhausted
// any other value - loop once, decrement counter
.label limb_cur_loop_cnt = $fcc2		
.label limb_tgt_loop_cnt = $fce2

// ===========================================================
/*
------------------------------------------------
 Costume animation settings

Bit 7 represents whether a costume animates its mouth when speaking (0 = it animates)
Bit 6 represents the same thing, but it's used in a different routine (prepare_message_for_topbar)
Bits 5-0 represent the sound to be played when walking (footsteps)
Note that Jeff has #00 as he's barefoot

00 - Has speaking animation, no sound played when walking
06 - Has speaking animation, sound 6 played when walking (footsteps)
46 - No speaking animation, sound 6 played when walking (footsteps)
C0 - No speaking animation, no sound played when walking
FF - No speaking animation, sound #3F played when walking
------------------------------------------------
*/
.label costume_anim_attrs = $f0d9
.label character_sprite_y = $fc37
.label sprite_vertical_offset = $cb55
.label actor_sprite_y_extent = $FD4F
.label relative_sprite_index = $fd78
.label relative_sprite_for_actor = $fd7c
.label fifth_byte = $fc03
.label intercel_vertical_displacement = $fc04
.label actor_sprite_base = $fc2e						//Base address of the current sprite being drawn in memory
.label actor_sprite_index = $fc32
.label clip_loop_cnt = $fde8 			// Current clip loop counter
.label actor_clip_loop_cnt = $fd0a		// Clip loop counter, for actor
// ===========================================================
.label actor_current_clip = $fd02		//The current animation set for the actor
.label actor_target_clip = $fd06		//The desired animation set for the actor
.label actor_anim_counter = $fd93
.label costume_clip_set = $ff47		/*The animation set for a costume, limited to the standing range of sets (#04-#07)
										This is used by scripts to determine the direction a costume is facing 
										See the details for "animation sets"*/
.label actor_motion_state        = $FD68  // per-actor motion state
.label actor_visible = $fd6f

/*
===========================================================
Animation state for actor
===========================================================
Only 2 bits are used, bit 5 (#20) and bit 0 (#01).

	Bit 5 		set - draw the actor in the main loop
				clear - ignore actor in main loop
	Bit 0 		set - refresh actor's visibility and graphics

 A refresh is triggered when:
	-the actor gets behind a foreground layer
	-an animation frame of a limb has changed
	-a limb base cell has changed
	-the animation is reset
	-a camera is fixed on a new actor
	-the animation counter has not expired

 An actor being updated in the main loop is triggered when:
	-the animation is reset
	-the actor is stopped
	-the animation counter has not expired
*/
.label actor_animation_state = $FD97

/*
===========================================================
Mouth state for actor
===========================================================
When an actor is speaking, the main_loop will automatically switch between
 the speaking states (01 and FF), thus opening and closing the mouth.

Possible mouth state values
---------------------------
	#00		closed	(default)
	#01		speaking and closed	
	#FF		speaking and open
	#80		permanently open	(will stay open until explicitly closed)
*/
.label actor_mouth_state = $fe9d	


// ===========================================================
.label sprite_buffer = $fd14		//Sprite buffer index (#00 or #01)
.label actor_tile_x_coordinate            = $FD20  // Input: actor tile X column
.label actor_tgt_sprite_x_lo = $fd1c
.label actor_tgt_sprite_x_hi = $fd39
.label actor_sprite_x_hi = $fd58
.label actor_sprite_x_lo = $fd60
.label character_sprite_bkg_colors = $f0c0	  //Background colors for each character
// ===========================================================
.label actor_active_wpt_x = $fd25
.label actor_active_wpt_y = $fd2d
.label actor_tgt_waypoint_x = $fd29
.label actor_tgt_waypoint_y = $fd31
.label actor_y_pos = $fd3d
.label actor_x_pos = $fd41
.label actor_x_dest = $fdd9
.label actor_y_dest = $fddd
.label position_to_waypoint_x_for_actor = $fd45
.label position_to_waypoint_y_for_actor = $fd49

.label actor_box_attr = $fd18
.label actor_box_handler_idx = $fda0
.label actor_box_facing_override = $fda4
.label actor_box_fg_masking = $fda8


.label actor_destination_box = $fdd5
.label actor_box_cur = $fdcd
.label actor_box_prev = $fdd1
.label actor_path_update_flag = $fde1
.label actor_search_depth       = $FE75


/*
===========================================================
 Actor path variables
===========================================================
*/
.label dir_x_bit = $cb58	//#00 = left, #01 = right
.label dir_y_bit = $cb59	//#00 = up, #01 = down	
.label dda_dominant_axis = $cb66	//Which axis of movement is dominant, #00 = horizontal, #01 = vertical

//Possible rename: actor_facing_mask
.label path_direction_for_actor = $fdeb		//#00 = right, #01 = left, #80 = down, #81 = up

.label path_delta_directions_for_actor = $fdef	//#00 = right, #01 = left, #80 = down, #81 = up
.label actor_dda_dom_abs = $fdf3	//The dominant delta value (the largest between the deltas for the 2 axis)
.label actor_dx_accum = $fdf7		//Value of delta x step (how much to move along this axis on each step to match the rate of motion on the other axis)
.label actor_dy_accum = $fdfb		//Value of delta y step
.label actor_dy_abs = $fdff			//Absolute value of delta between current position and path destination (Y axis)
.label actor_dx_abs = $fe03			//Absolute value of delta between current position and path destination (X axis)
.label actor_dir_x_bit = $fe07 //#00 = left, #01 = right
.label actor_dir_y_bit = $fe0b //#00 = up, #01 = down	
.label saved_actor_x_pos = $cb5a
.label saved_actor_y_pos = $cb5b
.label saved_actor_box_attr = $cb5d
.label saved_actor_box_cur = $cb5e
.label saved_actor_path_update_flag = $cb5f

/*
===========================================================
 Game script variables
===========================================================
*/
.label current_kid_idx = $ff67
.label game_vars = $ff67
/*
These vars are copies of some game engine variables.
I did not find any usage of them in the final scripts.
They were definitely intended as debugging aids.
*/
.label var_camera_position = $ff69			//This would be var[2]
.label var_msg_flag = $ff6a				//This would be var[3]
.label var_current_room = $ff6b			//This would be var[4]
.label var_active_io_id_lo = $ff6c	//This would be var[5]
.label var_destination_x = $ff6d			//This would be var[6]
.label var_destination_y = $ff6e			//This would be var[7]
.label var_music_playback_in_progress = $ff6f	//This would be var[8]
.label var_active_verb_id = $ff70				//This would be var[9]
.label var_message_length = $ff71			//This would be var[10]
//FF72 and onwards are  no longer used as debugging variables
.label kid_ids = $ffc8
.label kid1_costume_id = $FFC8
.label kid2_costume_id = $FFC9
.label kid3_costume_id = $FFCA
.label costume_room_idx = $fefc	 //The CURRENT room where a costume is present

/*
 Actor_vars bit meanings when bit is set
 Bit 7 - actor invisible (dead, game won or in the radiation suit)
 Bit 6 - actor frozen (for instance, when it's captured by Edna, Ed or the tentacle)
 Bit 3 - is Weird Ed's enemy 
 Bit 2 - has watched the TV commercial (and thus knows where to send the envelope)
 Bit 1 - is Green Tentacle's friend (after giving him a recording contract)
 Bit 0 - is strong (after doing exercise with the Hunk-o-matic)
*/
.label actor_vars = $fee3

.label room_scene_clicked_flag = $fd77
.label forced_sentence_trigger = $fe8e
//Control mode values
//#00 - cutscene
//#01 - push only, verb forcefully set to "push" (no walking possible) - used for keypad interaction and start screen
//#02 - normal play, disable kid change via Function keys
//#03 - normal play, enable kid change via Function keys
.label control_mode = $fea8
.label saved_control_mode = $fea9

//Sound
.label new_sound_instructions_allowed = $fe71	//00 = not allowed, nonzero = allowed
.label music_to_start_ptr = $fea1
.label music_index = $fea3

//===========================================================
// Interrupt handlers
//===========================================================
.label nmi_handler_lo = $fffa
.label nmi_handler_hi = $fffb
.label irq_handler = $fffe

//Placeholders for routines not modularized yet
.label print_message_wait_for_button = $0 // UI: print message pointed by msg_to_print_ptr, wait for button
.label execute_room_exit_script = $0
.label execute_room_entry_script = $0
.label open_shutter_flag = $0
.label init_sprites_sound = $0
.label raster_setup = $0
.label prepare_video_for_new_room = $0
.label restart_game = $0
.label start_global_script = $0
.label fix_camera_on_actor = $0
.label refresh_items_displayed = $0

//===========================================================
// Interaction region variables
//===========================================================
.label interaction_region = $cb88
.label joystick_button_pressed = $cb89
.label button_presses = $cb8a
.label inv_display_item_offset = $cb8b
.label region_text_color = $cb8f

.label talking_costume = $fe9a
.label msg_owner_costume = $fe9b	//#FF means nobody's talking (and thus the msg bar must be cleared)
						//Otherwise, it has the same value as talking_costume
//Base address for message bar source buffers
.label source_msg_base = $0200

//.label destination_entity = $fe51
.label sentence_bar_needs_refresh = $fe49	//#00 = don't refresh the sentence bar, #01 = refresh it
									//By "refresh", we mean to update its contents
.label init_sentence_ui_flag = $fe8f
.label sentstk_top_idx = $fe17
.label sentstk_free_slots = $fe18	
.label current_verb_id = $fe1f
.label direct_object_idx_lo = $fe20
.label direct_object_idx_hi = $fe21
.label current_preposition = $fe22
//Indirect object idx hi has the following meaning:
//#02 - Can be a recipient with a "Give" instruction
.label indirect_object_idx_lo = $fe23
.label indirect_object_idx_hi = $fe24
.label interrupted_script_offset_lo = $fea4
.label interrupted_script_offset_hi = $fea5
.label interrupted_script_index = $fea7

.label sid_master_volume_saved = $476A

.label inventory_objects = $7031
.label room_obj_idx_hi = $7221
.label room_obj_idx_lo = $723d

//===================================================
// Concurrent script engine variables
//===================================================
.label current_script_slot = $70B8	//Script slot of script being executed
.label script_offsets_hi = $70B9
.label script_offsets_lo = $70C9
//Script states
//Bit 7 set = frozen for cutscene
//#00	Inactive (no script)
//#01	Paused
//#02	Running
.label script_state_for_script_slot = $70D9
.label script_pause_counter_1 = $70e9
.label script_pause_counter_2 = $70f9
.label script_pause_counter_3 = $7109
//Script types
//#00 = object script
//#01 = room script
//#02 = global script
.label script_type_for_script_slot = $7119
.label resource_index_for_script_slot = $7129
.label script_2_mem_attr = $7ad9
