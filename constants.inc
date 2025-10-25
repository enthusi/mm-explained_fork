#importonce


.const PTR_NULL = $00

/*------------------------------------------------------------
; Block header layout (4-byte header + payload)
;------------------------------------------------------------
*/
.const MEM_HDR_SIZE_LO = $00   // Low byte of block size (total, including header)
.const MEM_HDR_SIZE_HI = $01   // High byte of block size
.const MEM_HDR_NEXT_LO = $02   // Low byte of pointer to next block
.const MEM_HDR_NEXT_HI = $03   // High byte of pointer to next block
.const MEM_HDR_LEN     = $04   // Total header length (size + next = 4 bytes)
/*------------------------------------------------------------
; Each allocated or free block begins with this header.
; Size fields store the block’s total length in bytes.
; “Next” fields link to the next block in memory order.
;------------------------------------------------------------
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

// --- I/O mapping presets for $01 (processor_port_register) ---
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

