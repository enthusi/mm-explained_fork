#importonce


.const PTR_NULL = $00
.const MSK_LOW_NIBBLE              = $0F    // Mask: keep low 4 bits
.const MSK_HIGH_NIBBLE             = $F0    // Mask: keep high 4 bits
.const MSK_LOW7BITS                = $7F    // Mask: clear bit7 (keep bits 0..6)

/*
------------------------------------------------------------
 Block header layout (4-byte header + payload)
------------------------------------------------------------
*/
.const MEM_HDR_SIZE_LO = $00   // Low byte of block size (total, including header)
.const MEM_HDR_SIZE_HI = $01   // High byte of block size
.const MEM_HDR_NEXT_LO = $02   // Low byte of pointer to next block
.const MEM_HDR_NEXT_HI = $03   // High byte of pointer to next block
.const MEM_HDR_LEN     = $04   // Total header length (size + next = 4 bytes)
/*
------------------------------------------------------------
 Each allocated or free block begins with this header.
 Size fields store the block’s total length in bytes.
 “Next” fields link to the next block in memory order.
------------------------------------------------------------
*/
.const DISK_ERROR_MSG 			    = $3B81
.const SIDE_ID_MSG        = $3B59      // “Put in side # and push joystick button.” (in RAM)


.const RSRC_TYPE_OBJECT      = $01
.const RSRC_TYPE_COSTUME     = $02
.const RSRC_TYPE_ROOM        = $03
.const RSRC_TYPE_ROOM_LAYERS = $04
.const RSRC_TYPE_SCRIPT      = $05
.const RSRC_TYPE_SOUND       = $06
.const RSRC_INDEX_GFX_LAYERS = $00
.const RSRC_INDEX_MASK_LAYER = $01


.const ROOM_MAX_INDEX      = $36       // Highest valid room index.
.const COSTUME_MAX_INDEX   = $18       // Highest valid costume index.
.const SCRIPT_MAX_INDEX    = $9F       // Highest valid script index
.const SOUND_MAX_INDEX     = $45       // Highest valid sound index.

.const VIEW_ROWS = $11
.const VIEW_COLS = $28

.const SCROLL_RIGHT_FLAG    = $00   // screen_scroll_flag: right scroll
.const SCROLL_LEFT_FLAG     = $FF   // screen_scroll_flag: left scroll
.const SCROLL_INIT_FLAG     = $01   // screen_scroll_flag: initial full draw

// --- I/O mapping presets for $01 (cpu_port) ---
.const MAP_IO_OFF                 	= $24     // Map RAM under I/O/KERNAL as needed by this program
.const MAP_IO_ON                   = $25     // Map I/O visible at $D000–$DFFF during IEC transfers

.const COSTUME_DFLT_X_DEST = $14
.const COSTUME_DFLT_Y_DEST = $3C
.const COSTUME_HOLDING_ROOM = $2C

.const COMP_DICT_MAX_OFS = $03		// Max offset of a compression dictionary


.const CAM_DEFAULT_POS = $C8
.const CAM_PAN_THRESHOLD = $14
.const VIEW_HALF_SPAN = $14
.const VIEW_FULL_SPAN_MINUS1 = $27

// Screen clamp
.const SCREEN_Y_MAX                = $91    // Last visible Y coordinate in pixels (bottom of viewport)

.const TILE_DEFS_ADDR = $D800
.const TILE_DEFS_SIZE = $0800

.const MASK_LAYER_COL_0         = $6AE1  // MASK  layer column 0 base
.const COLOR_LAYER_COL_0        = $6D89  // COLOR layer column 0 base

// Actor vars bitmasks
.const ACTOR_IS_INVISIBLE = $80
.const ACTOR_IS_FROZEN = $40



.const MOTION_STOPPED_CODE          = $02    // motion state value: actor stopped
.const ANIM_FLAG_REFRESH            = $20    // animation flag: refresh required
.const ANIM_FLAG_NEEDS_DRAW         = $01    // animation flag: actor must be drawn
.const ANIM_FLAGS_REFRESH_DRAW      = $21    // combined flags (#$20 | #$01)

.const OBJ_TYPE_ACTOR            = $02    // destination_obj_hi value for “actor”
.const COSTUME_ID_PLANT          = $13    // costume id for plant (skip mirroring)

.const DIRECTION_LEFT  			= $01     	// left movement direction code
.const DIRECTION_RIGHT 			= $00     	// right movement direction code
.const DIRECTION_DOWN  			= $80     	// down movement direction code
.const DIRECTION_UP    			= $81     	// up movement direction code

.const MAX_LIMB_IDX    			= $07     	// highest limb index (8 limbs total)

.const ACTOR_COUNT_TOTAL = $04 // total actors (0..3)
.const ACTOR_MAX_INDEX = ACTOR_COUNT_TOTAL - 1 // total actors (0..3)
.const SPRITE_COUNT_TOTAL = $04
.const SPRITE_MAX_INDEX = SPRITE_COUNT_TOTAL - 1

.const CLIP_STAND_RIGHT            = $05    // Standing clip id: facing right
.const CLIP_STAND_LEFT             = $04    // Standing clip id: facing left
.const CLIP_STAND_DOWN             = $06    // Standing clip id: facing down
.const CLIP_STAND_UP               = $07    // Standing clip id: facing up

.const CLIP_WALK_LEFT              = $00    // Walking clip id: left
.const CLIP_WALK_RIGHT             = $01    // Walking clip id: right
.const CLIP_WALK_DOWN              = $02    // Walking clip id: down
.const CLIP_WALK_UP                = $03    // Walking clip id: up

// Control mode values
.const CONTROL_MODE_CUTSCENE       = $00   // Cutscene active → hide cursor

// Generic booleans
.const FALSE                       = $00
.const TRUE                        = $01

// Generic signal set/clear
.const SIGNAL_SET                  = $01
.const SIGNAL_CLEAR				   = $00
.const LOCK_SET                  = $01
.const LOCK_CLEAR				   = $00

.const COLOR_BLACK                 = $00   // Background color 0 = black

//----------------------------------------------------------------
//Values used throughout the game for the vic_screen_control_reg_1
//----------------------------------------------------------------
// #00	game code entry
// #0B	after IRQ handler has exited
// #1B	irq_handler1, raster_setup
// #1C	irq_handler3
// #1D	irq_handler17
// Blank screen bit cleared/set in print_message_wait_for_button
.const VIC_CTRL_BLANK_0       = $00   // $D011 preset: blank screen, vertical scroll = 0
.const VIC_CTRL_BLANK_3       = $0B   // $D011 preset: blank screen, vertical scroll = 3
.const VIC_CTRL_DEFAULT_1       = $1B   // $D011 preset: 25 rows, vertical scroll = 3
.const VIC_CTRL_DEFAULT_2       = $1C   // $D011 preset: 25 rows, vertical scroll = 4
.const VIC_CTRL_DEFAULT_3       = $1D   // $D011 preset: 25 rows, vertical scroll = 5
