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

