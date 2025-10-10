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

.const ROOM_MAX_INDEX      = $36       // Highest valid room index.
.const COSTUME_MAX_INDEX   = $18       // Highest valid costume index.
.const SCRIPT_MAX_INDEX    = $9F       // Highest valid script index
.const SOUND_MAX_INDEX     = $45       // Highest valid sound index.


// --- I/O mapping presets for $01 (processor_port_register) ---
.const MAP_IO_OFF                 	= $24     // Map RAM under I/O/KERNAL as needed by this program
.const MAP_IO_ON                   = $25     // Map I/O visible at $D000–$DFFF during IEC transfers

.const COSTUME_DFLT_X_DEST = $14
.const COSTUME_DFLT_Y_DEST = $3C
.const COSTUME_HOLDING_ROOM = $2C
