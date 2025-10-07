;===============================================================================
; Disk I/O high-level helpers (C64 ⇄ 1541-style drive over IEC): sector read/write,
; streaming, and physical geometry stepping with robust retry + diagnostics.
;
; Summary:
;   High-level wrapper layer around low-level IEC byte I/O. 
;
;	Provides routines to:
;     • seed and advance a physical “sector chain” (track/sector cursor)
;     • read a chosen sector into a fixed buffer ($0300)
;     • stream bytes across sector boundaries with automatic refills
;     • copy streamed data into RAM
;     • write one or more sectors from a linear memory buffer
;
;   The on-wire read protocol uses 0x01-prefixed escape/control sequences for EOD,
;   sync requests, and error signaling. Writes stream an exact 256-byte payload.
;
; Hardware / Memory Map:
;   • processor_port_register ($0001): maps I/O in/out around IEC calls.
;   • vic_border_color_register ($D020): set green on fatal protocol error (debug).
;   • SECTOR_BUFFER ($0300..$03FF): fixed 256-byte read buffer (page-aligned).
;   • Must execute from RAM: routines rely on self-modifying code (patch abs operands).
;
; Protocol (drive → host, during reads):
;   01 01  = literal 0x01 (escaped, store and continue)
;   01 81  = end-of-data (success → C=0)
;   01 11  = error (failure → C=1)
;   01 21  = sync request (resynchronize, then continue)
;   other  = fatal: border=green, infinite hang (debug visibility)
;
; Calling Conventions (registers / globals):
;   • Track/Sector inputs: X=track, Y=sector for single-sector ops.
;   • Pointers: disk_dest_ptr/disk_src_ptr are (lo/hi) globals
;	• Routines patch abs operands inline:
;       - disk_store_abs / disk_load_abs / disk_dest_store_abs labels.
;   • Streaming state: current_track/current_sector, disk_buf_off advance across sectors.
;   • Geometry: max_sector_index_by_track holds 0-based max sector index per track.
;   • Mapping $01: set to MAP_IO_ON only for the duration of low-level IEC calls.
;
; Error Handling & Diagnostics:
;   • Reads/writes with wrappers retry indefinitely on failure, showing DISK_ERROR_MSG
;     via print_message_wait_for_button until the operation succeeds.
;   • disk_read_sector_into_buffer records last_track_loaded/last_sector_loaded on success.
;   • Fatal/unknown read control code → green border + hang (debug).
;
; Self-Modifying Code (SMC) Sites:
;   • disk_read_sector: patches STA $FFFF,X (disk_store_abs) to disk_dest_ptr.
;   • disk_write_sector: patches LDA $FFFF,X (disk_load_abs) to disk_src_ptr.
;   • disk_stream_copy: patches STA $FFFF (disk_dest_store_abs) to destination,
;     then increments the inlined operand to walk memory.
;
; Public routines:
;   disk_init_chain                 Seed start_track/start_sector and clear chain state.
;   disk_init_chain_and_read        Init chain, load (X=offset,Y=step), fall through to seek+read.
;   disk_seek_read               	Step Y sectors from start_*, set disk_buf_off=X, read into $0300.
;   disk_stream_copy          		Copy N streamed bytes to RAM using inlined STA operand.
;   disk_stream_next_byte           Return next byte from stream; auto-refill on $FF→$00 wrap.
;   disk_write_linear  				Write ceil(N/256) sectors from a linear buffer with retries.
;   disk_reset                      Send DISK_CMD_RESET to the drive (no params).
;
; Private routines:
;   disk_read_sector_into_buffer    Read (current_*) with retry; update last_*_loaded on success.
;   disk_write_sector               Send DISK_CMD_WRITE and stream 256 bytes from disk_src_ptr.
;   disk_next_sector_phys    		++sector; wrap to 0 and ++track when beyond per-track max.
;
; Table:
;   max_sector_index_by_track       Table of per-track **max sector index** (0-based).
;
; Arguments / Returns (conventions used across routines):
;   • Unless stated otherwise, A/X/Y are clobbered by low-level I/O; flags are not a contract.
;   • Carry is used for read/write status in leaf protocol routines (C=0 success, C=1 error).
;   • Globals updated: current_track/current_sector, disk_buf_off, last_*_loaded, disk_src_ptr/disk_dest_ptr.
;
; Edge Cases & Preconditions:
;   • disk_stream_copy requires non-zero 16-bit count; zero would underflow/loop forever.
;   • disk_write_linear rounds byte count up to whole sectors:
;       sectors_to_write = hi + (lo != 0).
;   • disk_next_sector_phys does not clamp at end-of-disk; caller policy applies.
;
; External Dependencies (entry addresses expected to be linked/provided):
;   print_message_wait_for_button, iec_send_cmd, iec_sync,
;   iec_recv_byte, iec_send_byte, disk_read_sector (internal leaf),
;   disk_write_sector (internal leaf) — see individual routine headers below.
;===============================================================================

;===========================================
; Constants
;===========================================
;--- Protocol opcodes (to drive) ---
DISK_CMD_RESET             	= $20     ; Operation: reset the drive
DISK_CMD_READ             	= $30     ; Operation: request a sector read
DISK_CMD_WRITE            	= $40     ; Operation: request a sector write

;--- Stream control markers (drive → host) ---
DISK_RX_ESC  				= $01     ; 0x01 prefix indicates a control sequence follows
DISK_RX_ESC_ESC             = $01     ; 01 01 = literal 0x01 data byte
DISK_RX_ERR                	= $11     ; 01 11 = error condition signaled by drive
DISK_RX_SYNC                = $21     ; 01 21 = host should resynchronize (iec_sync)
DISK_RX_EOD          		= $81     ; 01 81 = end-of-data for the current read

;--- UI / diagnostics ---
DISK_FAULT_BORDER           = $05     ; Green - Border color used by hang loop (visual fault signal)

;--- Static buffers / tables ---
SECTOR_BUFFER               = $0300   ; 256-byte sector buffer (page-aligned)

;--- I/O mapping presets for $01 (processor_port_register) ---
MAP_IO_OFF                 	= $24     ; Map RAM under I/O/KERNAL as needed by this program
MAP_IO_ON                   = $25     ; Map I/O visible at $D000–$DFFF during IEC transfers

DISK_ERROR_MSG 			    = $3B81

;===========================================
; max_sector_index_by_track — max sector index per track (0-based)
;
; Summary:
;   Lookup table indexed by track number. 
;	Each entry stores the **maximum valid sector index** for that track (0-based), not the count.
;
;   Encodings (1541-style zones):
;     $14 = 20 → 21 sectors (0..20)    ; tracks 1–17
;     $12 = 18 → 19 sectors (0..18)    ; tracks 18–24
;     $11 = 17 → 18 sectors (0..17)    ; tracks 25–30
;     $10 = 16 → 17 sectors (0..16)    ; tracks 31–35
;
; Notes:
;   • Entry 0 is a placeholder for “track 0” (unused on 1541).
;   • If you need the count, add 1 to the table value.
;===========================================
* = $45C7
max_sector_index_by_track:
        .BYTE $00                          ;  0: unused placeholder
        .BYTE $14,$14,$14,$14,$14,$14,$14,$14,$14,$14  ;  1–10: 21 sectors/track (max idx 20)
        .BYTE $14,$14,$14,$14,$14,$14,$14,$12,$12,$12  ; 11–20: 21 (11–17), then 19 (18–20)
        .BYTE $12,$12,$12,$12,$11,$11,$11,$11,$11,$11  ; 21–30: 19 (21–24), then 18 (25–30)
        .BYTE $10,$10,$10,$10,$10                      ; 31–35: 17 sectors/track (max idx 16)
		
;===========================================
; Hardware registers (C64 side)
;===========================================
processor_port_register     = $0001   ; CPU I/O port: map I/O/KERNAL/BASIC (bitfields per standard $01)
vic_border_color_register   = $D020   ; VIC-II border color

;===========================================
; External routines
;===========================================
msg_to_print_ptr            = $00DA   ; (lo/hi) pointer consumed by print routine
print_message_wait_for_button = $3B15 ; UI: print message pointed by msg_to_print_ptr, wait for button
iec_send_cmd       			= $46C2   ; Low-level: issues command opcode to the drive
iec_sync             		= $46DC   ; Low-level: timing/handshake on IEC bus
iec_recv_byte       		= $46F3   ; Low-level: receive one byte from serial
iec_send_byte       		= $4723   ; Low-level: send one byte over serial

;--- Command argument latches for low-level routines ---
iec_cmd_track               = $4753   ; Track number latched before iec_send_cmd/read/write
iec_cmd_sector              = $4754   ; Sector index (0-based) latched before iec_send_cmd/read/write

;===========================================
; Public variabales
;===========================================
;--- Streaming offsets (per-byte and per-sector) ---
disk_buf_off                = $4639   ; 0..255 index within SECTOR_BUFFER (auto-wrap triggers refill)
disk_start_off            	= $463A   ; Initial intra-sector byte offset (for first read)
disk_chain_step          	= $463B   ; How many physical sectors to step from (start_track,start_sector)

;--- Copy/write byte-count accumulator ---
disk_copy_count_lo 			= $463C ; 16-bit count of bytes to copy/write: low byte
disk_copy_count_hi 			= $463D ; 16-bit count of bytes to copy/write: high byte
                                      ; (write path rounds up to whole sectors: hi += (lo != 0))

;--- “Start” geometry for a physical sector chain (seed) ---
start_sector                = $4633   ; First sector index (0-based) of the chain
start_track                 = $4634   ; First track number of the chain

;===========================================
; Private variabales
;===========================================
;--- Scratch saves for register preservation around I/O ---
disk_y_save                 = $462F   ; Save area for Y during disk_read_sector_into_buffer
disk_x_save                 = $4630   ; Save area for X during disk_read_sector_into_buffer

;--- High-level read/write shared pointer (alias used by different phases) ---
disk_src_ptr                = $4631   ; (lo/hi) base of 256-byte page to WRITE from
disk_dest_ptr               = $4631   ; (lo/hi) base of destination to READ into (aliases disk_src_ptr)

;--- “Current” physical geometry (cursor advanced by helpers) ---
current_sector              = $4635   ; Sector index (0-based) currently targeted
current_track               = $4636   ; Track number currently targeted

;--- “Last loaded” bookkeeping (filled after a successful read into $0300) ---
last_sector_loaded          = $4637   ; Sector index last read into SECTOR_BUFFER
last_track_loaded           = $4638   ; Track number last read into SECTOR_BUFFER



;===========================================
; disk_init_chain — seed start geometry and reset chain state
;
; Summary:
;   Captures the starting (sector, track) for a physical sector chain and clears
;   related bookkeeping so subsequent readers begin at byte 0 of the first sector.
;
; Arguments:
;   sector (X)                     Starting sector index (0-based).
;   track  (Y)                     Starting track number.
;
; Returns / Updates:
;   start_sector                   	← sector (from X).
;   start_track                    	← track  (from Y).
;   last_sector_loaded             	← 0 (no sector read yet).
;   last_track_loaded              	← 0.
;   disk_start_off               	← 0 (begin at byte 0 within the sector).
;   disk_chain_step             	← 0 (first sector in the chain).
;
; Flags:
;   A/Z/N modified by zero stores; no defined flag contract.
;
; Clobbers:
;   A                              Used to write zeros.
;   X, Y                           Preserved.
;
; Description:
;   Initializes the chain cursor and clears “last loaded” state so a subsequent
;   call to disk_init_chain_and_read / disk_seek_read will position
;   to (start_track, start_sector) and begin reading from offset 0. 
;
; 	This routine does not validate geometry; callers should ensure inputs are in-range.
;===========================================
* = $44D3

disk_init_chain:
        ; Initialize chain start from caller registers
        ; NOTE: This routine expects X=sector, Y=track
        STX start_sector                  
        STY start_track                   

        ; Clear “last loaded” bookkeeping (no sector read yet)
        LDA #$00
        STA last_sector_loaded            
        LDA #$00
        STA last_track_loaded             

        ; Start reading at the beginning of the sector (byte offset = 0)
        LDA #$00
        STA disk_start_off                

        ; Begin at the first sector in the chain (index 0)
        LDA #$00
        STA disk_chain_step            	  
		
        RTS                               
;===========================================
; disk_init_chain_and_read — initialize chain defaults, then tail-call reader
;
; Summary:
;   Initializes the sector “chain” starting point (via disk_init_chain),
;   loads X = disk_start_off and Y = disk_chain_step, and then deliberately
;   falls through into disk_seek_read, which performs
;   the actual positioning and sector load.
;
; Arguments:
;   (implicit)                Uses disk_start_off (intra-sector byte offset) and
;                             disk_chain_step (how many sectors to step forward).
;
; Returns:
;   (control-flow)            No RTS here. Execution continues in
;                             disk_seek_read, which reads the
;                             resolved sector into $0300 and returns from there.
;
; Flags:
;   Modified by disk_init_chain and by the fall-through callee; no flag contract.
;
; Description:
;   1) Call disk_init_chain to seed start_track/start_sector (and any defaults).
;   2) Move disk_start_off → X and disk_chain_step → Y to satisfy the callee’s ABI.
;   3) Fall through (no jump/return) to disk_seek_read(X=offset, Y=index).
;      That routine advances in physical order, sets disk_buf_off, and loads the sector.
;===========================================
disk_init_chain_and_read:
        ; Initialize chain defaults (e.g., start_track/start_sector,
        ; optional disk_start_off/disk_chain_step as set by disk_init_chain)
        JSR disk_init_chain

        ; Prepare arguments for the next routine:
        ;   X = intra-sector byte offset (0..255)
        LDX disk_start_off
        ;   Y = sector-chain index (how many sectors to step forward from start)
        LDY disk_chain_step

        ; Fall through intentionally:
        ;   next label is disk_seek_read
        ;   which expects X=offset and Y=index (no RTS/JMP here by design)

;===========================================
; disk_seek_read — position to Nth sector (physical) and load it
;
; Summary:
;   Sets the physical cursor to the sector that is N steps after the starting
;   (track,sector) in simple physical order (sector++ with wrap to next track),
;   programs the intra-sector byte offset, then reads that sector into $0300.
;
; Arguments:
;   X                          Intra-sector byte offset (0..255) at which subsequent reads begin.
;   Y                          Sector chain index (number of physical sectors to step forward).
;   start_track                Base track of the chain.
;   start_sector               Base sector (0-based) of the chain.
;
; Updates:
;   current_track              Set to the resolved track after stepping Y sectors.
;   current_sector             Set to the resolved sector after stepping Y sectors.
;   disk_buf_off               Set to X (starting byte within the loaded sector).
;   SECTOR_BUFFER ($0300)      Filled with the resolved sector’s data.
;
; Flags:
;   Modified by subroutines; no flag contract on return.
;
; Clobbers:
;   A, X                       Not preserved. Y is consumed via STY (not restored).
;
; Description:
;   1) Initialize (current_track,current_sector) ← (start_track,start_sector).
;   2) If Y > 0, call disk_next_sector_phys Y times to advance in physical order:
;        sector ← sector+1; if beyond per-track max, sector ← 0 and track ← track+1.
;   3) Set disk_buf_off ← X to select the starting byte within the sector.
;   4) Call disk_read_sector_into_buffer to load that sector into $0300; this may retry until success.
;
;   Notes:
;     • “Sector chain” here is purely physical sequencing, not a filesystem link chain.
;     • Caller must handle end-of-disk policy (disk_next_sector_phys does not clamp globally).
;===========================================
disk_seek_read:
        ; Capture caller inputs:
        ;   X = byte offset within target sector (0..255)
        ;   Y = sector-chain index (how many sectors to step forward, 0-based)
        STX disk_start_off                  ; stash intra-sector byte offset
        STY disk_chain_step             	; stash physical sector step count

        ; Initialize physical cursor to the start of the chain
        LDA start_sector
        STA current_sector
        LDA start_track
        STA current_track

        ; If index == 0, we want the first sector: skip advancement
        LDA disk_chain_step
        BEQ sector_chain_index_reached

        ; Advance forward 'disk_chain_step' sectors in physical order:
        ; each step: sector++ ; wrap → sector=0, track++
        TAX                                 ; X ← steps remaining
next_sector_in_chain:
        JSR disk_next_sector_phys       	; current_(track,sector) ← next physical sector
        DEX
        BNE next_sector_in_chain

        ; Program the intra-sector read offset (starting byte within the sector)
sector_chain_index_reached:
        LDA disk_start_off
        STA disk_buf_off

        ; Load the selected sector into SECTOR_BUFFER ($0300); may retry on I/O errors
        JSR disk_read_sector_into_buffer
        RTS
;===========================================
; disk_stream_copy — stream N bytes from sector reader to dest
;
; Summary:
;   Copies a linear stream of N bytes into RAM starting at the destination
;   address provided in X/Y. Each byte is fetched via disk_stream_next_byte
;   (which transparently refills the sector buffer on page wrap). The routine
;   self-modifies the operand of an STA absolute to walk the destination.
;
; Arguments:
;   X                         	Destination address low byte.
;   Y                         	Destination address high byte.
;   disk_copy_count_lo			16-bit byte count to copy (lo/hi). Must be > 0.
;   disk_copy_count_hi
;                             
;
; Updates:
;   Memory at (X:Y) .. (X:Y)+N-1  	Filled with N bytes from the stream.
;   disk_copy_count_*   			Decremented to 0 on completion.
;
; Flags:
;   Z                           Set on return (last ORA yields 0) — not a formal contract.
;   Others                      Unspecified.
;
; Clobbers:
;   A                           Modified.
;   X, Y                        Preserved (reader preserves Y; this routine does not alter X/Y).
;
; Description:
;   1) Patch the low/high operand bytes of `STA $FFFF` (SMC) using X/Y so writes
;      land at the caller’s destination.
;   2) Loop:
;        • A ← disk_stream_next_byte()   	; may trigger a sector refill.
;        • STA (dest)                       ; store to current destination.
;        • Increment the SMC operand (lo then hi) to advance dest by +1.
;        • Decrement the 16-bit byte counter by 1 (borrow from hi when lo is 0).
;        • Continue until the counter becomes 0.
;
; Notes:
;   • **Precondition:** byte count must be non-zero. If both counter bytes are 0,
;     this loop would underflow and not terminate.
;===========================================
disk_stream_copy:
        ; Patch the absolute STA operand with destination address:
        ;   STA $FFFF  ← ($FFFF is self-modified below via disk_dest_store_abs)
        ; Low/high bytes come from X/Y so caller sets the initial dest.
        STX disk_dest_store_abs              
        STY disk_dest_store_abs + 1          

        ; NOTE: This routine assumes the 16-bit byte counter > 0.
        ; If both bytes are 0, the first iteration would underflow and loop forever.
read_loop:
        ; Get next byte from the disk-backed stream.
        ; This may refill SECTOR_BUFFER when the per-sector offset wraps.
        JSR disk_stream_next_byte

        ; Store to the current destination (absolute operand is SMC-patched above).
        STA $FFFF                           ; write A → [disk_dest_ptr]
disk_dest_store_abs = * - 2                  ; points at the STA operand (lo/hi)

        ; Advance destination pointer (16-bit: lo then hi on wrap).
        INC disk_dest_store_abs              ; ++dest.lo
        BNE decrement_counters              ; no wrap → skip hi
        INC disk_dest_store_abs + 1          ; wrapped → ++dest.hi
		
decrement_counters:
        ; Decrement remaining byte count by 1 (16-bit borrow semantics).
        LDA disk_copy_count_lo   			; check low first
        BNE decrement_counter_lo            ; if lo != 0, just dec lo
        DEC disk_copy_count_hi   			; borrow: --hi
		
decrement_counter_lo:
        DEC disk_copy_count_lo   			; --lo

        ; Loop while (hi|lo) != 0 (i.e., bytes remain to copy).
        LDA disk_copy_count_lo
        ORA disk_copy_count_hi
        BNE read_loop

        RTS
;===========================================
; disk_write_linear — write N sectors from a linear buffer (retries on error)
;
; Summary:
;   Writes a sequence of sectors starting at (start_track, start_sector), sourcing
;   each 256-byte payload from successive pages of a linear buffer beginning at disk_src_ptr.
;
;   For each sector: map I/O in, call disk_write_sector.
;	On failure, print DISK_ERROR_MSG and retry the same sector until it succeeds. 
;	After each successful write, advance to the next valid (track, sector), 
;	bump disk_src_ptr by +256, and continue until all sectors have been written.
;
; Arguments:
;   X / Y                         disk_src_ptr (lo/hi): base of the first 256-byte page to write.
;   start_track                   Starting track number.
;   start_sector                  Starting sector index (0-based).
;   disk_copy_count_hi			  Total byte count; sectors_to_write is computed as:
;   disk_copy_count_lo 			  hi + (lo != 0). The hi byte is then used as the loop counter.
;                                 
; Uses / Updates:
;   current_track                 Working track cursor; set from start_track, advanced per sector.
;   current_sector                Working sector cursor; set from start_sector, advanced per sector.
;   disk_src_ptr                  Advanced by +256 after each successful sector.
;   disk_copy_count_hi 			  Countdown of sectors remaining; 0 on completion.
;
; Returns:
;   current_track/current_sector  Positioned at the sector immediately **after** the last one written.
;   (no status flags)             This wrapper does not define a carry/flag contract on return.
;
; Flags:
;   Modified by subroutines and internal ops; callers must not rely on flags after return.
;
; Clobbers:
;   A, X, Y                       Not preserved. Calls disk_write_sector, disk_next_sector_phys,
;                                 print_message_wait_for_button.
;
; Description:
;   1) Initialize disk_src_ptr from X/Y; seed current_track/current_sector from start_*.
;   2) Compute sectors_to_write = hi + (lo != 0); reuse the hi byte as the sector countdown.
;   3) Loop:
;        • Map I/O in ($01 ← MAP_IO_ON).
;        • LDX current_track, LDY current_sector, JSR disk_write_sector.
;        • On C=1 (error): set msg_to_print_ptr to DISK_ERROR_MSG, print, and retry same sector.
;        • On C=0 (success):
;            – Map I/O out ($01 ← MAP_IO_OFF).
;            – JSR disk_next_sector_phys (advance geometry).
;            – INC disk_src_ptr+1 (advance buffer by +256).
;            – DEC disk_copy_count_hi; if not zero, repeat.
;   Note:
;     • This routine can block indefinitely if a sector persistently fails (retries until success).
;     • Preconditions: if both counter bytes are 0, no sectors should be written
;===========================================
disk_write_linear:
        ; Initialize source base pointer for this batch (256-byte pages)
        STX disk_src_ptr                         ; disk_src_ptr.lo  ← X
        STY disk_src_ptr + 1                     ; disk_src_ptr.hi  ← Y

        ; Seed write cursor with starting geometry
        LDA start_sector
        STA current_sector
        LDA start_track
        STA current_track

        ; Round total byte count up to whole sectors:
        ; sectors_to_write = hi + (lo != 0 ? 1 : 0)
        ; NOTE: disk_copy_count_hi is used as the loop counter after this.
        LDA disk_copy_count_lo
        BEQ try_write
        INC disk_copy_count_hi

try_write:
        ; Map I/O visible (CIA/IEC at $D000–$DFFF) for the low-level write
        LDY #MAP_IO_ON
        STY processor_port_register

        ; Write one sector from disk_src_ptr .. disk_src_ptr+$00FF to (current_track,current_sector)
        ; Contract: disk_write_sector returns C=0 on success, C=1 on error
        LDX current_track
        LDY current_sector
        JSR disk_write_sector
        BCC write_succeeded                ; success → advance to next sector

        ; Error path: show message and retry the same sector (I/O still mapped)
        LDA #<DISK_ERROR_MSG
        STA msg_to_print_ptr
        LDA #>DISK_ERROR_MSG
        STA msg_to_print_ptr + 1
        JSR print_message_wait_for_button
        JMP try_write

write_succeeded:
        ; Restore memory map (hide I/O window) before bookkeeping
        LDY #MAP_IO_OFF
        STY processor_port_register

        ; Advance geometry cursor to the next valid sector (wraps across tracks)
        JSR disk_next_sector_phys

        ; Advance source pointer by +256 bytes (next page of the linear buffer)
        INC disk_src_ptr + 1

        ; One fewer sector remains; loop until all have been written
        DEC disk_copy_count_hi
        BNE try_write

        RTS
;===========================================
; disk_stream_next_byte — stream next byte, auto-advance & refill
;
; Summary:
;   Returns the next byte from the current sector buffer at SECTOR_BUFFER+$offset,
;   then post-increments the per-sector offset. When the offset wraps ($FF→$00),
;   advances (track,sector) to the next valid sector and reloads the buffer
;   at $0300 before returning the originally fetched byte.
;
; Arguments:
;   disk_buf_off            0..255 index into SECTOR_BUFFER (post-incremented).
;
; Vars/State:
;   track/sector state      Variables updated by disk_next_sector_phys and
;                           consumed by disk_read_sector_into_buffer (must refer to the
;                           same track/sector pair).
;
; Returns:
;   A                       The next byte from the stream.
;   disk_buf_off            Advanced by 1; wraps to 0 and triggers refill.
;
; Flags:
;   Z, N                    Reflect the returned byte (from PLA).
;   C, V                    Unspecified (callees may alter; this routine does not set).
;
; Clobbers:
;   A                       Holds the returned byte on exit.
;   Y                       Preserved (saved/restored internally).
;   X                       May be clobbered by callees.
;
; Description:
;   1) Save Y, load A ← SECTOR_BUFFER[disk_buf_off], push A.
;   2) Increment disk_buf_off; if it != 0, restore Y, pull A, RTS.
;   3) On wrap (disk_buf_off==0): call disk_next_sector_phys to advance geometry,
;      then disk_read_sector_into_buffer to refill SECTOR_BUFFER. Ensure the track/sector
;      variables used by both routines are the same symbols.
;   4) Reassert disk_buf_off=0 for clarity, restore Y, pull A, RTS.
;
;   Note: This routine can block at page boundaries while the next sector is read.
;===========================================
disk_stream_next_byte:
        ; Save caller's Y (used as index and clobbered by callees)
        STY disk_y_saved_2

        ; Use current per-sector offset as index into the sector buffer
        LDY disk_buf_off

        ; Fetch the byte at SECTOR_BUFFER + offset
        ; (We’ll return this byte in A even if we need to refill the buffer.)
        LDA SECTOR_BUFFER,Y

        ; Preserve the fetched byte while we potentially advance/refill
        ; PLA later will restore A and set N/Z to the returned value.
        PHA

        ; Post-increment the offset; wrap to $00 after $FF
        INC disk_buf_off

        ; If no wrap (offset != 0), we’re still within the same sector → done
        BNE publish_byte

        ; Offset wrapped to 0 → we just consumed the last byte of this sector.
        ; Advance to the next valid (track,sector) and load it into SECTOR_BUFFER ($0300).
        JSR disk_next_sector_phys     			; updates current track/sector (0-based sector index)
        JSR disk_read_sector_into_buffer        ; blocking: retries until the next sector is loaded

        ; Be explicit: start at the beginning of the freshly loaded sector
        LDA #$00
        STA disk_buf_off

publish_byte:
        ; Restore caller’s Y, then restore A = fetched byte (N/Z reflect A)
        LDY disk_y_saved_2
        PLA
        RTS

disk_y_saved_2:
        .BYTE $00                         ; one-byte save area for Y
;===========================================
; disk_next_sector_phys — advance sector index, wrap across tracks
;
; Summary:
;   Increments the current sector index (0-based). If the new index exceeds the
;   track’s maximum valid sector index from max_sector_index_by_track[track], wraps the
;   sector to 0 and advances to the next track. Does not clamp at end-of-disk.
;
; Arguments:
;   current_sector              0-based sector index; incremented and possibly wrapped to 0.
;   current_track               Track number; incremented when sector wraps.
;
; Returns / Updates:
;   current_sector              → sector+1 if within track bounds; else 0 on wrap.
;   current_track               → unchanged if in-bounds; else +1 on wrap.
;
; Flags:
;   Z/N/C                       Updated per last operations (no defined contract for caller).
;
; Clobbers:
;   A, Y                        Used for compare/index; X preserved.
;
; Description:
;   1) current_sector ← current_sector + 1.
;   2) Load max_index ← max_sector_index_by_track[current_track].
;   3) If current_sector ≤ max_index, keep track/sector and return.
;   4) Otherwise, set current_sector ← 0 and increment current_track.
;
;   Note: Caller is responsible for handling end-of-disk (e.g., when current_track
;   passes the last valid track for the medium).
;===========================================
disk_next_sector_phys:
        ; Advance to the next sector index (0-based)
        INC current_sector

        ; Geometry check: compare the new sector index (A) against the
        ; track’s MAX valid sector index from the table (0-based):
        ;   A = current_sector
        ;   Y = current_track (table index)
        LDA current_sector
        LDY current_track
        CMP max_sector_index_by_track,Y          ; A ?= max_sector_index[track]

        ; In range if A ≤ max:
        ;   BCC → A < max  → ok
        ;   BEQ → A == max → ok (exactly the last valid sector)
        BCC exit_1
        BEQ exit_1

        ; Overflowed past last sector on this track → wrap to sector 0
        ; and advance to the next track (no clamp at end-of-disk here).
        LDA #$00
        STA current_sector
        INC current_track

exit_1:
        RTS
;===========================================
; disk_read_sector_into_buffer — fetch one sector into $0300 with retry
;
; Summary:
;   Sets disk_dest_ptr to SECTOR_BUFFER ($0300) and attempts to read the sector
;   identified by current_track / current_sector using disk_read_sector. 
;	I/O space is mapped in only for the call, then unmapped. 
;	On failure, prints DISK_ERROR_MSG and retries until the read succeeds. 
;	On success, records the loaded track/sector and restores caller registers.
;
; Arguments:
;   current_track            Track number of the sector to read (input).
;   current_sector           Sector number of the sector to read (input).
;
; Vars/State:
;   disk_dest_ptr            Destination pointer; set to SECTOR_BUFFER before read.
;
; Returns:
;   SECTOR_BUFFER..+$00FF    Filled with the sector just read.
;   last_track_loaded        Updated to current_track.
;   last_sector_loaded       Updated to current_sector.
;
; Flags:
;   Carry/Z/N                Used internally for flow control; no defined flag
;                            contract on return from this wrapper.
;
; Clobbers:
;   A, X, Y                  X/Y preserved across the routine; originals restored.
;
; Description:
;   1) Save X/Y to disk_x_save/disk_y_save.
;   2) Set disk_dest_ptr = SECTOR_BUFFER ($0300) so disk_read_sector’s SMC store targets $0300.
;   3) Loop:
;        • Map I/O in (write MAP_IO_ON to $01).
;        • Load X=current_track, Y=current_sector; JSR disk_read_sector.
;        • Map I/O out (write MAP_IO_OFF to $01).
;        • If C=0, success → break; else point msg_to_print_ptr at DISK_ERROR_MSG,
;          print message, and retry.
;   4) On success, copy current_track/current_sector into last_*_loaded.
;   5) Restore X/Y and RTS.
;===========================================
* = $45EB
disk_read_sector_into_buffer:
        ; Preserve caller registers used as temporary scratch by this routine
        STX disk_x_save
        STY disk_y_save

        ; Configure destination buffer: disk_dest_ptr ← $0300 (SECTOR_BUFFER)
        LDA #<SECTOR_BUFFER
        STA disk_dest_ptr
        LDA #>SECTOR_BUFFER
        STA disk_dest_ptr + 1

attempt_read_sector:
        ; Map I/O space in (make CIA/VIC etc. visible at $D000–$DFFF)
        LDY #MAP_IO_ON
        STY processor_port_register

        ; Kick off a sector read with current track/sector
        ; Contract: disk_read_sector returns C=0 on success, C=1 on error/special-fail
        LDX current_track
        LDY current_sector
        JSR disk_read_sector

        ; Hide I/O space again (restore RAM under I/O)
        LDY #MAP_IO_OFF
        STY processor_port_register

        ; Success? (Carry clear means the sector was received into disk_dest_ptr)
        BCC read_succeeded

        ; Failure path: set message pointer → print → retry indefinitely
        LDA #<DISK_ERROR_MSG
        STA msg_to_print_ptr
        LDA #>DISK_ERROR_MSG
        STA msg_to_print_ptr + 1
        JSR print_message_wait_for_button

        ; Retry the entire read (same track/sector, same destination)
        JMP attempt_read_sector

read_succeeded:
        ; Record which sector/track ended up in the buffer (status bookkeeping)
        LDA current_sector
        STA last_sector_loaded
        LDA current_track
        STA last_track_loaded

        ; Restore caller’s X/Y and return (A/flags reflect last ops)
        LDX disk_x_save
        LDY disk_y_save
		; buffer at $0300 filled; flags undefined
        RTS
;===========================================
; disk_write_sector — transmit 256-byte buffer to drive (SMC + abs,X stream)
;
; Summary:
;   Sends DISK_CMD_WRITE for (track=X, sector=Y), synchronizes on the IEC bus,
;   then streams exactly 256 bytes from disk_src_ptr..disk_src_ptr+$00FF to the drive.
;   Uses self-modifying code to patch an absolute LDA operand, enabling a tight
;   abs,X read → send loop for throughput.
;
; Arguments:
;   X       		           Track number to write.
;   Y 		                   Sector number to write.
;   disk_src_ptr               Source base address (lo/hi) of 256-byte buffer.
;
; Returns:
;   Carry                      C=0 on completion of the 256-byte transmit.
;   Registers                  A, X, Y clobbered; flags updated.
;
; Flags:
;   Carry                      Explicitly cleared on exit (success path).
;   Z/N                        From last loop ops (no defined contract).
;
; Description:
;   1) Latches (X,Y) into iec_cmd_track/iec_cmd_sector.
;   2) Patches the two-byte absolute operand used by `LDA $FFFF,X`
;      (label: disk_load_abs) with disk_src_ptr (self-modifying code in RAM).
;   3) Issues DISK_CMD_WRITE via iec_send_cmd, then calls
;      iec_sync to align bus timing.
;   4) Loops X=0..$FF:
;        LDA abs(disk_src_ptr)+X  → JSR iec_send_byte → INX
;      When X wraps after $FF, the loop terminates and the routine returns C=0.
;
; Notes:
;   • Routine must execute from RAM (SMC).
;   • Caller should ensure I/O mapping and any broader protocol framing are set
;     appropriately before/after this routine; this routine only sends the sector.
;===========================================
* = $4641

disk_write_sector:
        ; Stash target location for the drive to write to
        STX iec_cmd_track                    ; track  ← X
        STY iec_cmd_sector                   ; sector ← Y

        ; Self-modify the absolute LDA used in the data loop so it reads from disk_src_ptr
        ;   LDA $4000,X  ; operand ($4000) is patched below with disk_src_ptr (lo/hi)
        LDA disk_src_ptr
        STA disk_load_abs                ; set low  byte of source base
        LDA disk_src_ptr + 1
        STA disk_load_abs + 1            ; set high byte of source base

        ; Issue the “write sector” command, then synchronize bus state before data phase
        LDA #DISK_CMD_WRITE
        JSR iec_send_cmd            		; transmits opcode to drive (IEC framing/handshake inside)
        JSR iec_sync                  		; waits for ready timing on the serial bus

        ; Stream exactly 256 bytes: X is the byte index (0..255). When X wraps to 0, we’re done.
        LDX #$00
send_next_byte:
        ; Read next data byte from the caller’s buffer (absolute operand patched above)
        LDA $4000,X                         ; A ← *(disk_load_abs + X)  (absolute operand SMC-patched above)
disk_load_abs = * - 2

        ; Send the byte over the serial bus to the drive
        JSR iec_send_byte

        ; Advance to next byte; continue until X wraps ($FF→$00) after 256 sends
        INX									; X: 00..FF → wraps to 00 after 256 bytes
        BNE send_next_byte					; loop until wrap

        ; Success path (no error signalling from the inner loop): return C=0
        CLC
        RTS
;===========================================
; disk_reset — issue drive reset opcode over IEC
;
; Summary:
;   Sends DISK_CMD_RESET to the disk drive using the low-level transmit
;   routine. No track/sector parameters are used; the command is a single
;   operation byte that instructs the drive to reset itself.
;
; Arguments:
;   (none)                    The routine loads A with DISK_CMD_RESET internally.
;
; Returns:
;   (none)                    No success/failure code is returned by this wrapper.
;   Registers                 A/X/Y clobbered by callees; condition flags updated.
;
; Description:
;   Loads A with DISK_CMD_RESET and calls iec_send_cmd, which performs
;   the IEC handshake and byte framing. 
;
;	After the command is sent, the routine returns immediately; the drive completes 
;	its own reset/initialization.
;
;   Callers should re-synchronize with the bus and/or reinitialize any cached
;   drive state as needed after invoking this routine.
;===========================================
disk_reset:
        ; Load the protocol opcode for a drive reset.
        ; (This command ignores track/sector; only the operation byte matters.)
        LDA #DISK_CMD_RESET              

        ; Transmit the reset command to the drive over IEC.
        ; iec_send_cmd handles the bus handshake and byte framing.
        JSR iec_send_cmd

        ; Return to caller. The drive will proceed with its own reset/init.
        RTS
;===========================================
; disk_read_sector — receive a sector into caller buffer (handles escapes & EOD)
;
; Summary:
;
;   Sends a DISK_CMD_READ for (track=X, sector=Y), synchronizes with the drive,
;   then streams the sector payload into the caller-provided buffer at disk_dest_ptr.
;
;   Bytes are received one-by-one; a prefix 0x01 introduces special sequences:
;     01 01 → literal 0x01 (escaped)          	→ store and continue
;     01 81 → end-of-data (success)          	→ C=0, RTS
;     01 11 → error                            	→ C=1, RTS
;     01 21 → sync request                     	→ resync, continue
;   Any other 01 xx causes a deliberate hang (border=green) for debugging.
;
; Arguments:
;   X 		                      Track number to read.
;   Y 		                      Sector number to read.
;   disk_dest_ptr 		          Destination base address (lo/hi). The routine
;                                 self-modifies an absolute STA $FFFF,X to target disk_dest_ptr.
;
; Returns:
;   Caller must ensure the destination region is large enough; the routine
;   continues writing beyond the first page if EOD is delayed.
;
;   Carry                         C=0 on success (EOD seen), C=1 on error marker.
; 	disk_dest_ptr onward               Data stored starting at disk_dest_ptr; if X wraps, the
;                                 patched high byte is incremented and writes continue
;                                 into the next page until EOD.
;
; Flags:
;   Z/N                           From the last data CMP/STA path (undefined contract).
;
; Clobbers:
;   A, X, Y, C                    X is used as the running byte offset (0..255).
;
; Description:
;
;   1) Latches track/sector into iec_cmd_track/iec_cmd_sector.
;
;   2) Patches the absolute store operand (disk_store_abs) with disk_dest_ptr, so
;      each data byte is stored via STA abs(disk_dest_ptr)+X (self-modified). When X wraps ($FF→$00), the
;      patched high byte is incremented to advance to the next page.
;
;   3) Issues the command via iec_send_cmd, then calls iec_sync.
;
;   4) Receives bytes with iec_recv_byte, interpreting 0x01-prefixed
;      sequences as protocol control. On SYNC request (01 21), performs a sync and
;      resumes the stream without losing position.
;
;   5) On DISK_RX_EOD returns C=0; on DISK_RX_ERR returns C=1. Any unknown
;      01 xx pattern enters hang_loop after painting the border green.
;===========================================
disk_read_sector:
        ; Set command parameters for low-level I/O
        STX iec_cmd_track                  ; track ← X
        STY iec_cmd_sector                 ; sector ← Y

        ; Patch the absolute store used below (self-modifying)
        ;   STA $FFFF,X  ; operand ($FFFF) is replaced with disk_dest_ptr
        LDA disk_dest_ptr
        STA disk_store_abs            		; set low byte of target address
        LDA disk_dest_ptr + 1
        STA disk_store_abs + 1        		; set high byte of target address

        ; Select the “read sector” operation
        LDA #DISK_CMD_READ

        ; Issue command to the drive, then synchronize bus state
        JSR iec_send_cmd
        JSR iec_sync

        ; Use X as the running offset into the destination page
        ; (X will wrap after $FF → $00; we detect that to bump the high byte)
        LDX #$00

next_byte:
        ; Receive one byte from the serial bus
        JSR iec_recv_byte

        ; Is this the start of a special sequence?
        CMP #DISK_RX_ESC
        BNE store_in_buffer                	; no → ordinary data byte, store it

        ; Special sequence: read the discriminator byte
        JSR iec_recv_byte

        ; 01 01 → literal 0x01 (escaped), treat as normal data
        CMP #DISK_RX_ESC_ESC
        BEQ store_in_buffer

        ; 01 81 → end-of-data: signal success (C=0) and return
        CMP #DISK_RX_EOD
        BEQ command_success

        ; 01 11 → error: signal failure (C=1) and return
        CMP #DISK_RX_ERR
        BEQ command_error

        ; 01 21 → sync request: re-sync and continue receiving
        CMP #DISK_RX_SYNC
        BNE hang_loop                      	; unknown 01 xx ⇒ fatal: hang for debugging

        ; Perform requested sync then resume the stream
        JSR iec_sync
        JMP next_byte

store_in_buffer:
        ; Store byte at inlined destination + X
        ; (absolute address below is patched at entry via disk_store_abs)
        STA $FFFF,X                        	; write to disk_dest_ptr + X
disk_store_abs = * - 2

        ; Advance offset; if X wrapped ($FF→$00), bump the high byte of dest
        INX
        BNE next_byte_2                    	; no wrap → keep filling same page
        INC disk_store_abs + 1        		; wrapped → advance to next page

next_byte_2:
        JMP next_byte

command_error:
        SEC                                 ; C=1 ⇒ failure
        RTS

command_success:
        CLC                                 ; C=0 ⇒ success
        RTS

hang_loop:
        ; Fatal/unknown sequence: mark screen border, spin forever
        LDA #DISK_FAULT_BORDER
        STA vic_border_color_register
        JMP hang_loop
