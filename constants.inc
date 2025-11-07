#importonce

// Generic booleans
.const FALSE                       = $00
.const TRUE                        = $01

// Generic signal set/clear
.const SIGNAL_SET                  = $01
.const SIGNAL_CLEAR				   = $00
.const LOCK_SET                    = $01
.const LOCK_CLEAR				   = $00

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
.const MAP_IO_OUT                 	= $24     // Map RAM under I/O/KERNAL as needed by this program
.const MAP_IO_IN                   = $25     // Map I/O visible at $D000–$DFFF during IEC transfers

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
.const ACTOR_STATE_STOPPED        = $02    // Actor not moving

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


.const COLOR_BLACK                 = $00   // Background color 0 = black

//----------------------------------------------------------------
//Values used throughout the game for the vic_screen_control_reg_1
//----------------------------------------------------------------
// #00	game code entry
// #0B	after IRQ handler has exited
// #1B	irq_handler1, init_raster_irq_env
// #1C	irq_handler3
// #1D	irq_handler17
// Blank screen bit cleared/set in print_message_wait_for_button
.const VIC_CTRL_BLANK_0       = $00   // $D011 preset: blank screen, vertical scroll = 0
.const VIC_CTRL_BLANK_3       = $0B   // $D011 preset: blank screen, vertical scroll = 3
.const VIC_CTRL_DEFAULT_1       = $1B   // $D011 preset: 25 rows, vertical scroll = 3
.const VIC_CTRL_DEFAULT_2       = $1C   // $D011 preset: 25 rows, vertical scroll = 4
.const VIC_CTRL_DEFAULT_3       = $1D   // $D011 preset: 25 rows, vertical scroll = 5


.const MOUTH_STATE_CLOSED          = $00    // idle: mouth closed (default)
.const MOUTH_STATE_SPEAK_CLOSED    = $01    // speaking cycle: frame with mouth closed
.const MOUTH_STATE_SPEAK_OPEN      = $FF    // speaking cycle: frame with mouth open
.const MOUTH_STATE_HELD_OPEN       = $80    // forced open until explicitly closed

.const COSTUME_ATTR_NO_MOUTH_B7   = $80    // bit7: 1 = do NOT animate mouth (main loop); 0 = animate
.const COSTUME_ATTR_NO_MOUTH_B6   = $40    // bit6: 1 = do NOT animate mouth (prepare_message_for_topbar); 0 = animate
.const COSTUME_ATTR_STEP_SFX_MASK = $3F    // bits5..0: footsteps sound ID

* = $3BA6
game_paused_flag_msg:
        .byte $47,$61,$6D,$65,$20,$70,$61,$75,$73,$65
        .byte $64,$2C,$20,$70,$72,$65,$73,$73,$20,$53
        .byte $50,$41,$43,$45,$20,$74,$6F,$20,$63,$6F
        .byte $6E,$74,$69,$6E,$75,$65,$2E,$20,$20,$00
        // "Game paused, press SPACE to continue.  "

//CC00-CECF - graphics area (including parts not visible)
.const SENTENCE_BAR_BASE = $ced0

//========================
//Verbs
//========================
.const OPEN_VERB = $01
.const CLOSE_VERB = $02
.const GIVE_VERB = $03
.const TURN_ON_VERB = $04
.const TURN_OFF_VERB = $05
.const FIX_VERB = $06
.const NEW_KID_VERB = $07
.const UNLOCK_VERB = $08
.const PUSH_VERB = $09
.const PULL_VERB = $0A
.const USE_VERB = $0B
.const READ_VERB = $0C
.const WALK_TO_VERB = $0D
.const PICK_UP_VERB = $0E
.const WHAT_IS_VERB = $0F

//=========================
//Prepositions
//=========================
.const PREPOSITION_NONE_NEEDED = $00
.const PREPOSITION_WITH = $02
.const PREPOSITION_TO = $04

//=========================
//Control modes
//=========================
.const CUTSCENE_CONTROL_MODE = $00
.const KEYPAD_CONTROL_MODE = $01
.const NO_KID_CHANGE_CONTROL_MODE = $02
.const NORMAL_CONTROL_MODE = $03

.const FIRST_NON_KID_INDEX           = $08    // Recipient index ≥ $08 → not a kid


//Lights_status values
.const LIGHTS_DARKNESS = $00
.const LIGHTS_FLASHLIGHT_ONLY = $01
.const LIGHTS_ENVIRONMENT_ON = $02

.const  GAME_DISK_ID_SIDE1   = $31    // Valid game tag: side 1
.const  GAME_DISK_ID_SIDE2   = $32    // Valid game tag: side 2


.const SENT_STACK_EMPTY_IDX        = $FF    // Token stack sentinel “empty”
.const SENT_STACK_MAX_TOKENS       = $06    // Max tokens in sentence stack

.const SPRITE_BASE_0      = $E000   		// VRAM base for sprite bank 0
.const SPRITE_BASE_1      = $E800   		// VRAM base for sprite bank 1

// ------------------------------------------------------------
// Video setup modes (video_setup_mode)
// ------------------------------------------------------------
.const VID_SETUP_NONE           = $00   // No change; keep current VIC layout
.const VID_SETUP_C800           = $01   // Char @ $D800, screen @ $C800, sprites @ $CBF8 → copy color RAM
.const VID_SETUP_CC00           = $02   // Char @ $D800, screen @ $CC00, sprites @ $CFF8 → copy color RAM
.const VID_SETUP_COPY_COLORS    = $03   // Copy color RAM only, no layout change

// ------------------------------------------------------------
// Sprite shape base addresses for each layout
// ------------------------------------------------------------
.label SPRITE_SHAPE_SET1_ADDR   = $CBF8 // Sprite shape data base for layout C800
.label SPRITE_SHAPE_SET2_ADDR   = $CFF8 // Sprite shape data base for layout CC00


// ----------------------------------------
// Screen row byte offsets (40-column tile mode)
// ----------------------------------------
// Precomputed 16-bit offsets for the start of each tile row
// within the screen buffer. Row stride = 40 bytes = $28.
//
// Rows: 25 total → indices 0..24.
// offset(row) = row * $28
//   e.g., row 0 → $0000, row 7 → $0118, row 24 → $03C0.
//
// Use: screen_base + screen_row_offsets_{hi,lo}[row]
// to address the first cell of a given row.
* = $3EA1
screen_row_offsets_hi:
.byte $00, $00, $00, $00, $00, $00, $00
.byte $01, $01, $01, $01, $01, $01, $02
.byte $02, $02, $02, $02, $02, $02, $03
.byte $03, $03, $03, $03

* = $3EBA
screen_row_offsets_lo:
.byte $00, $28, $50, $78, $A0, $C8, $F0
.byte $18, $40, $68, $90, $B8, $E0, $08
.byte $30, $58, $80, $A8, $D0, $F8, $20
.byte $48, $70, $98, $C0


.const  FRAMEBUF1_BASE     = $C800    
.const  FRAMEBUF2_BASE      = $CC00   

.const SPRITES_ENABLE_ALL          = $FF      // $D015: enable sprites 0–7
.const RASTER_IRQ_LINE_INIT        = $FA      // $D012: raster line 250
.const SPACE_CHAR              = $20    // ASCII space
.const WORD_HARD_STOP           = $40    // In-word hard stop token; forces early termination
.const WORD_TERMINATOR          = $00    // End-of-string marker for word/text data

.const  OWNER_NIBBLE_MASK         = MSK_LOW_NIBBLE  // Mask to extract owner nibble from attributes
.const  OBJ_NAME_OFS              = $0D    // Byte offset to name pointer within object record
.const NO_SPRITE                 = $FF    // Sentinel: no sprite assigned
