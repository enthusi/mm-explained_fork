
#import "globals.inc"
#import "constants.inc"
#import "memory_mgmt.asm"
#import "disk_high_level.asm"

.label disk_id_check = $0
.label pause_game = $0
.label unpause_game = $0

/*===========================================
 * Private variables & constants
 *-------------------------------------------*/

.const RSRC_DHDR_BYTES              = $04   // On-disk header size (bytes).
.const RSRC_MHDR_BYTES              = $04   // In-memory header size (bytes).

.const RSRC_DHDR_OFF_SIZE_LO        = $00   // Disk header: +0  size.lo   (total bytes, little-endian).
.const RSRC_DHDR_OFF_SIZE_HI        = $01   // Disk header: +1  size.hi.
.const RSRC_DHDR_OFF_SENTINEL       = $02   // Disk header: +2  sentinel byte (must be 0).
.const RSRC_DHDR_OFF_CHECKSUM       = $03   // Disk header: +3  checksum (expected value).

.const RSRC_MHDR_OFF_SIZE_LO        = $00   // Memory header: +0  size.lo   (total bytes, little-endian).
.const RSRC_MHDR_OFF_SIZE_HI        = $01   // Memory header: +1  size.hi.
.const RSRC_MHDR_OFF_TYPE           = $02   // Memory header: +2  resource type id.
.const RSRC_MHDR_OFF_INDEX          = $03   // Memory header: +3  resource index within type.

.const SOUND_PROTECT_1     = $03    // Reserved/non-evictable sound index #3 (skip during eviction).
.const SOUND_PROTECT_2     = $04    // Reserved/non-evictable sound index #4 (skip during eviction).
.const SOUND_PROTECT_3     = $05    // Reserved/non-evictable sound index #5 (skip during eviction).

.const PRIORITY_ROOM       = $00    // Eviction dispatch code: try releasing one ROOM.
.const PRIORITY_COSTUME    = $01    // Eviction dispatch code: try releasing one COSTUME.
.const PRIORITY_SOUND      = $02    // Eviction dispatch code: try releasing EVICTABLE SOUNDS.
.const PRIORITY_SCRIPT     = $03    // Eviction dispatch code: try releasing EVICTABLE SCRIPTS.
.const PRIORITY_WRAP_AT    = $04    // Threshold: if evict_pri_cur ≥ this, set to $FF so INC → $00 (wrap to ROOM).

/*-------------------------------------------*/

.label rsrc_hdr_ptr                 = $4F   // ZP pointer → header base in RAM; used with (rsrc_hdr_ptr),Y indexed accesses.
.label rsrc_payload_ptr             = $E4   // 16-bit pointer → first byte after header (payload start).
.label rsrc_payload_len             = $E6   // 16-bit remaining payload byte count (used during checksum loop).
.label rsrc_chk_accum               = $E8   // Running checksum accumulator (XOR of payload bytes).
.label rsrc_chk_expected            = $E9   // Expected checksum loaded from header (disk header +CHECKSUM).
.label rsrc_retry_rem               = $EA   // Retry counter for load/verify; decremented on failure, reset to 1 before UI prompt.

/* Note: rsrc_hdr_scratch and rsrc_total_bytes alias the same storage ($553B).
 * Lifecycle:
 *   - Immediately after disk read: use as 4-byte disk header scratch (copy target).
 *   - After header parse: reuse lo/hi as 16-bit total size (header + payload).
 */
.label rsrc_hdr_scratch             = $553b // 4-byte scratch buffer for on-disk header copy.
.label rsrc_total_bytes             = $553b // 16-bit total size in bytes (header + payload); aliases rsrc_hdr_scratch.
.label rsrc_data_ptr                = $553f // 16-bit pointer → resource base in RAM (header at +0, payload at +RSRC_*HDR_BYTES).

.label rsrc_raw_size                = $FD9C // 16-bit raw payload size (bytes); written into memory header size fields.
.label rsrc_resource_index          = $5676 // Resource index within its type (byte).
.label rsrc_resource_type           = $5677 // Resource type identifier (byte).

.label rsrc_ptr            = $4F    // Zero-page pointer to the resource base in RAM (lo/hi).
                                    // Used for indexed indirection: (rsrc_ptr),Y and via <rsrc_ptr/>rsrc_ptr.

.label evict_pri_cur       = $FE4B  // Current eviction priority selector (0..3).
                                    // Rotation order: ROOM → COSTUME → SOUND → SCRIPT; values ≥ PRIORITY_WRAP_AT wrap to 0 on INC.

.label evict_pri_start     = $FE4C  // Snapshot of starting priority for this pass.
                                    // Used to detect a full cycle with no successful eviction.

.label best_attr           = $FD9F  // Best-so-far LRU “age”/attr score when scanning rooms.
                                    // Non-zero means evictable; larger value = older/staler candidate.

.label best_room_idx       = $FD9E  // Index of the current best LRU room candidate.
                                    // 0 is used as a “no candidate” sentinel (scan runs MAX..1).

/*===========================================================
 * rsrc_load_from_disk: load resource, verify checksum, and stage header
 *
 * Summary:
 *   Pauses the game loop, seeks the disk stream to this resource,
 *   copies/validates the on-disk header, allocates memory for the
 *   full object, copies the payload, computes and verifies the
 *   checksum, then continues into rsrc_hdr_init on success.
 *
 * Arguments:
 *   rsrc_read_offset		Byte offset into the resource stream.
 *   rsrc_sector_idx 		Which sector in the physical chain.
 *   rsrc_total_bytes		16-bit total size in bytes (header + payload).
 *
 * State:
 *   rsrc_hdr_scratch		Destination buffer for the on-disk header copy.
 *
 * Returns:
 *   Flow:      BEQ on checksum match → falls through to rsrc_hdr_init.
 *   Globals:   rsrc_data_ptr set to allocated destination.
 *              rsrc_payload_ptr, rsrc_payload_len prepared.
 *              rsrc_chk_expected/rsrc_chk_accum updated.
 *              rsrc_retry_rem decremented on failures.
 *   Flags:     Clobbered by loads/stores and helper calls.
 *
 * Description:
 *   1) Pause game; seek to header start.
 *   2) Copy header into rsrc_hdr_scratch; validate header sentinel == 0.
 *      If not zero: run disk_id_check and retry from the top.
 *   3) Allocate memory for rsrc_total_bytes; publish result in rsrc_data_ptr.
 *   4) Seek stream to the payload start and copy rsrc_total_bytes bytes to rsrc_data_ptr.
 *   5) Prepare checksum:
 *        rsrc_hdr_ptr := rsrc_data_ptr
 *        rsrc_payload_len := rsrc_total_bytes − header size (RSRC_DHDR_BYTES)
 *        rsrc_payload_ptr := rsrc_data_ptr + header size (RSRC_DHDR_BYTES)
 *        rsrc_chk_expected := rsrc_hdr_ptr[RSRC_DHDR_OFF_CHECKSUM]
 *   6) Checksum: XOR-accumulate payload bytes into rsrc_chk_accum
 *		When length is 0, compare with rsrc_chk_expected. 
 *		If equal → BEQ rsrc_hdr_init; otherwise decrement rsrc_retry_rem.
 *      If retries remain → retry_copy payload; else show disk_error_msg and retry after user ack.
 *===========================================================
 */
* = $5541
rsrc_load_from_disk:
        // Pause the game loop while performing disk I/O to avoid tearing/state races.
        jsr pause_game

        /*
         * Read the first data sector for this resource:
         *   X = rsrc_read_offset            (byte offset into the resource stream)
         *   Y = rsrc_sector_idx     (which sector in the physical chain)
         * On return, the disk stream is positioned at the start of the header.
         */
        ldx rsrc_read_offset
        ldy rsrc_sector_idx
        jsr disk_seek_read

        /*
         * Copy the on-disk resource header into local scratch (rsrc_hdr_scratch).
         *   dest = rsrc_hdr_scratch
         *   count = RSRC_DHDR_BYTES bytes
         * After this, the stream pointer advances by header size.
         */
        ldx #<rsrc_hdr_scratch
        ldy #>rsrc_hdr_scratch
        lda RSRC_DHDR_BYTES
        sta disk_copy_count_lo
        lda #$00
        sta disk_copy_count_hi
        jsr disk_stream_copy

        /*
         * Validate the header’s sentinel (must be 0).
         *   BEQ allocate_payload → OK (Z=1 because A==0)
         *   else: run a disk ID/sync check and restart the load attempt.
         */
        lda rsrc_hdr_scratch + RSRC_DHDR_OFF_SENTINEL
        beq allocate_payload
        jsr disk_id_check
        jmp rsrc_load_from_disk

		/* ----------------------------------------
		 * Allocate space for the whole resource payload
		 * ----------------------------------------
		 */
allocate_payload:
        /*
         * Prepare allocator arguments:
         *   X/Y = rsrc_total_bytes
         */
        ldx <rsrc_total_bytes
        ldy >rsrc_total_bytes

        /*
         * Request a block large enough for the payload (+ header handled by allocator path).
         * On success: Z=0 and X/Y = allocated header pointer (lo/hi).
         * On failure: Z=1 (X/Y unspecified) and caller’s recovery path applies.
         */
        jsr mem_alloc

        /*
         * Publish the destination payload pointer for subsequent copies:
         *   rsrc_data_ptr := returned X/Y (lo/hi)
         */
        stx <rsrc_data_ptr
        sty >rsrc_data_ptr

        // Mark that we have at least one retry opportunity for this load sequence.
        lda #$01
        sta rsrc_retry_rem

copy_payload_from_disk:
        /*
         * Stream position: seek to the first sector that contains this resource data.
         *   X = rsrc_read_offset          (byte offset within the resource stream)
         *   Y = rsrc_sector_idx   (index in the physical sector chain)
         */
        jsr disk_seek_read

        /*
         * Copy the entire payload from the disk stream into the allocated destination.
         *   dest    = rsrc_data_ptr (lo/hi)
         *   count   = rsrc_total_bytes (lo/hi)
         *   effect  = advances both stream position and dest pointer by count bytes
         */
        ldx <rsrc_data_ptr
        ldy >rsrc_data_ptr
        lda <rsrc_total_bytes
        sta disk_copy_count_lo
        lda >rsrc_total_bytes
        sta disk_copy_count_hi
        jsr disk_stream_copy

		/*
		 * -------------------------------------------
		 * Prepare checksum calculation over the payload (exclude header)
		 * -------------------------------------------
		 * Seed rsrc_hdr_ptr with the start of the on-memory resource (header at +0).
		 *   rsrc_hdr_ptr := rsrc_data_ptr
		 */
        lda <rsrc_data_ptr
        sta <rsrc_hdr_ptr
        lda >rsrc_data_ptr
        sta >rsrc_hdr_ptr

        /*
         * Compute payload byte count (exclude header bytes):
         *   rsrc_payload_len = rsrc_total_bytes - RSRC_DHDR_BYTES
         * Little-endian subtract; C=1 before SBC means “no borrow” baseline.
         */
        sec
        lda <rsrc_total_bytes
        sbc #RSRC_DHDR_BYTES
        sta <rsrc_payload_len
        lda >rsrc_total_bytes
        sbc #$00
        sta >rsrc_payload_len

        /*
         * Point rsrc_payload_ptr at the first byte after the header:
         *   rsrc_payload_ptr = rsrc_data_ptr + RSRC_DHDR_BYTES
         * Little-endian add; C cleared so ADC adds carry-in=0.
         */
        clc
        lda <rsrc_data_ptr
        adc #RSRC_DHDR_BYTES
        sta <rsrc_payload_ptr
        lda >rsrc_data_ptr
        adc #$00
        sta >rsrc_payload_ptr

        /*
         * Load the expected checksum from the header:
         *   A = *(rsrc_hdr_ptr + RSRC_HDR_OFF_CHECKSUM)
         * Store for later comparison after summing the payload bytes.
         */
        ldy #RSRC_DHDR_OFF_CHECKSUM
        lda (rsrc_hdr_ptr),Y
        sta rsrc_chk_expected

		/*
		 * ----------------------------------------
		 * Initialize running checksum to 0 (XOR accumulator)
		 * ----------------------------------------
		 */
        lda #$00
        sta rsrc_chk_accum
rsrc_chk_step:
        ldy #$00                // always read current byte at rsrc_payload_ptr + 0
        lda rsrc_chk_accum
        /*
         * XOR current payload byte into the running checksum:
         *   checksum := checksum ⊕ *rsrc_payload_ptr
         */
        eor (rsrc_payload_ptr),Y
        sta rsrc_chk_accum

        // Advance source pointer: ++rsrc_payload_ptr (little-endian)
        inc <rsrc_payload_ptr
        bne dec_rem_count        		// no wrap → skip hi-byte increment
        inc >rsrc_payload_ptr           // wrapped lo → carry into hi

dec_rem_count:
        /*
         * Decrement remaining byte count (16-bit):
         *   if lo==0 then borrow from hi
         */
        lda <rsrc_payload_len
        bne dec_rem_lo
        dec >rsrc_payload_len
dec_rem_lo:
        dec <rsrc_payload_len

        /*
         * Loop if any bytes remain:
         *   test (lo | hi) != 0  → more data pending
         */
        lda <rsrc_payload_len
        ora >rsrc_payload_len
        bne rsrc_chk_step       // Z=0 → continue; Z=1 → done

		/*
		 * ----------------------------------------
		 * All bytes consumed — verify payload checksum against header’s expected value.
		 * ----------------------------------------
		 */
        lda rsrc_chk_accum
        cmp rsrc_chk_expected

        /*
         * Match? → proceed to write the resource header metadata.
         *   CMP sets Z=1 when A==expected → BEQ takes the “good” path.
         */
        beq rsrc_hdr_init

        /*
         * Mismatch — attempt a retry path:
         *   rsrc_retry_rem := rsrc_retry_rem - 1
         *   if rsrc_retry_rem > 0 → retry_copy the room data and re-check
         */
        dec rsrc_retry_rem
        bne retry_copy	          // Z=0 (counter not zero) → try again

        // No retries left — reset counter to 1 and prompt the user, then retry.
        lda #$01
        sta rsrc_retry_rem

        /*
         * Show disk error and wait for user to acknowledge (e.g., joystick button).
         */
        lda #<DISK_ERROR_MSG
        sta <print_msg_ptr
        lda #>DISK_ERROR_MSG
        sta >print_msg_ptr
        jsr print_message_wait_for_button

retry_copy:
        // Re-read the raw data from disk, then recompute and re-validate checksum.
        jmp copy_payload_from_disk

/*===========================================
 * rsrc_hdr_init (section): write resource header and return data pointer
 *
 * Summary:
 *   Initializes the resource header at rsrc_hdr_ptr with size (lo/hi),
 *   type, and index. Returns the payload pointer in X:Y, then resumes
 *   the game.
 *
 * Arguments (from previous section):
 *   rsrc_hdr_ptr           Base address for header
 *   rsrc_raw_size         	16-bit raw payload size in bytes.
 *   rsrc_resource_type    	Resource type identifier.
 *   rsrc_resource_index   	Resource index within the type.
 *   rsrc_data_ptr	        16-bit pointer to start of payload.
 *
 * Returns:
 *   X:Y                        rsrc_data_ptr (lo in X, hi in Y).
 *   Flags                      Clobbered by loads/stores and by unpause_game.
 *
 * Description:
 *   Writes header layout at (rsrc_hdr_ptr):
 *     +0..+1  size  (little-endian): <rsrc_raw_size, >rsrc_raw_size
 *     +2      type  (rsrc_resource_type)
 *     +3      index (rsrc_resource_index)
 *   Loads X:=<rsrc_data_ptr, Y:=>rsrc_data_ptr and calls unpause_game before RTS.
 *===========================================
 */
* = $5604
rsrc_hdr_init:
		// Store raw data size
        ldy #$00
        lda <rsrc_raw_size
        sta (rsrc_hdr_ptr),Y
        iny
        lda >rsrc_raw_size
        sta (rsrc_hdr_ptr),Y
		
		// Store resource type 
        iny
        lda rsrc_resource_type		
        sta (rsrc_hdr_ptr),Y
		
		// Store resource index
        iny
        lda rsrc_resource_index		
        sta (rsrc_hdr_ptr),Y
		
		// Return the resource data pointer via .X and .Y
        ldx <rsrc_data_ptr
        ldy >rsrc_data_ptr
		
		// Unpause game
        jsr unpause_game
        rts
		
		
/*===========================================
 * rsrc_update_ptr: publish rsrc_ptr into per-type tables (used after block relocation)
 *
 * Summary:
 *   Dispatches on resource type in Y and writes the resource pointer
 *   (lo/hi) to the matching pointer table at index X, then returns.
 *   Each test label (*test_type_…*) checks a specific type code and
 *   falls through to the next test on mismatch.
 *
 * Arguments:
 *   rsrc_ptr            Zero-page pointer to the resource base (lo/hi).
 *   Y = resource_type   One of:
 *                         RSRC_TYPE_OBJECT, RSRC_TYPE_COSTUME,
 *                         RSRC_TYPE_ROOM, RSRC_TYPE_ROOM_LAYERS,
 *                         RSRC_TYPE_SCRIPT, RSRC_TYPE_SOUND.
 *   X = resource_index  Table index to update.
 *
 * Updates:
 *   object_ptr_lo_tbl[X],  object_ptr_hi_tbl[X]
 *   costume_ptr_lo_tbl[X], costume_ptr_hi_tbl[X]
 *   room_ptr_lo_tbl[X],    room_ptr_hi_tbl[X]
 *   room_layers_ptr_lo_tbl[X], room_layers_ptr_hi_tbl[X]
 *   script_ptr_lo_tbl[X],  script_ptr_hi_tbl[X]
 *   sound_ptr_lo_tbl[X],   sound_ptr_hi_tbl[X]
 *   (Each receives <rsrc_ptr / >rsrc_ptr respectively.)
 *
 * Returns:
 *   Flow:                 RTS after storing into the matched table.
 *   Registers/Flags:      Modified by loads/stores and comparisons.
 *
 * Description:
 *   - Compare Y against known RSRC_TYPE_* codes in ascending checks.
 *   - On match, store <rsrc_ptr to the *_lo_tbl[X] and >rsrc_ptr to
 *     the *_hi_tbl[X], then RTS.
 *   - On mismatch, fall through to the next *test_type_* label.
 *   - NOTE (bug/placeholder): if none match, current code jumps to
 *     rsrc_evict_one_by_priority; this should trap/halt instead.
 *===========================================*/

* = $5A89

rsrc_update_ptr:
        // Dispatch by resource_type in Y. Each test label *checks* a value and falls through
        // to the next check when not equal. On match, write table[X] := rsrc_ptr and RTS.
        cpy #RSRC_TYPE_ROOM                      
        bne test_type_room_layers

        // type 3: room — publish pointer for room X
        lda <rsrc_ptr             // lo byte (little-endian)
        sta room_ptr_lo_tbl,x
        lda >rsrc_ptr             // hi byte
        sta room_ptr_hi_tbl,x
        rts

test_type_room_layers:
        cpy #RSRC_TYPE_ROOM_LAYERS                      
        bne test_type_costume

        // type 4: room scene layers — publish pointer for layer set X
        lda <rsrc_ptr
        sta room_layers_ptr_lo_tbl,x
        lda >rsrc_ptr
        sta room_layers_ptr_hi_tbl,x
        rts

test_type_costume:
        cpy #RSRC_TYPE_COSTUME                      
        bne test_type_object

        // type 2: costume — publish pointer for costume X
        lda <rsrc_ptr
        sta costume_ptr_lo_tbl,x
        lda >rsrc_ptr
        sta costume_ptr_hi_tbl,x
        rts

test_type_object:
        cpy #RSRC_TYPE_OBJECT                      
        bne test_type_script

        // type 1: object — publish pointer for object X
        lda <rsrc_ptr
        sta object_ptr_lo_tbl,x
        lda >rsrc_ptr
        sta object_ptr_hi_tbl,x
        rts

test_type_script:
        cpy #RSRC_TYPE_SCRIPT                      
        bne test_type_sound

        // type 5: script — publish pointer for script X
        lda <rsrc_ptr
        sta script_ptr_lo_tbl,x
        lda >rsrc_ptr
        sta script_ptr_hi_tbl,x
        rts

test_type_sound:
        cpy #RSRC_TYPE_SOUND                      
        bne rsrc_evict_one_by_priority // BUG - it shouldn't fall through but hang up instead

        // type 6: sound — publish pointer for sound X
        lda <rsrc_ptr
        sta sound_ptr_lo_tbl,x
        lda >rsrc_ptr
        sta sound_ptr_hi_tbl,x
        rts
/*===========================================
 * rsrc_evict_one_by_priority: try one eviction by rotating priority
 *
 * Summary:
 *   Attempts to evict exactly one resource per call, using a rotating
 *   priority: room → costume → sound → script. Starts from evict_pri_cur,
 *   tries that class’ eviction routine, then advances priority (with wrap).
 *   If any callee frees something, returns true; otherwise cycles priorities
 *   until it wraps back to the starting priority and returns false.
 *
 * Arguments:
 *   (none)
 *
 * Reads:
 *   evict_pri_cur              Current priority selector (0..3).
 *   rsrc_released_flag         Set ≠ 0 by callees when they free something.
 *
 * Updates:
 *   evict_pri_start            Snapshot of starting priority for wrap detection.
 *   evict_pri_cur              Incremented each attempt; may wrap ($FF→$00).
 *   rsrc_released_flag         Cleared on entry; may be set by callees.
 *
 * Returns:
 *   A = #$FF                   A resource was released (success).
 *   A = #$00                   Completed a full cycle with no release.
 *   Registers/Flags            Modified by dispatch and callee routines.
 *
 * Description:
 *   1) Clear rsrc_released_flag; save evict_pri_cur into evict_pri_start.
 *   2) Dispatch based on evict_pri_cur:
 *        0 → rsrc_release_evictable_room_lru
 *        1 → rsrc_release_one_evictable_costume
 *        2 → rsrc_release_evictable_sounds
 *        3 → rsrc_release_evictable_scripts
 *      If evict_pri_cur ≥ PRIORITY_WRAP_AT, set it to $FF so INC → $00.
 *   3) INC evict_pri_cur; if rsrc_released_flag ≠ 0 → return #$FF.
 *   4) If evict_pri_cur != evict_pri_start → continue dispatch loop.
 *      Else return #$00 (no release this cycle).
 *===========================================*/

* = $5AE3
rsrc_evict_one_by_priority:
        // Initialize: assume no release this pass (flag := #$00).
        // Policies below will set rsrc_released_flag ≠ 0 on success.
        lda #$00
        sta rsrc_released_flag

        // Snapshot the starting priority so we can detect a full wrap later.
        lda evict_pri_cur
        sta evict_pri_start

dispatch_by_priority:
        // Dispatch on evict_pri_cur (0..3 are valid, >=4 wraps to 0).
        lda evict_pri_cur
        cmp #PRIORITY_ROOM
        bne test_costume

        // case 0 → try releasing one room (LRU policy).
        jsr rsrc_release_evictable_room_lru
        jmp advance_priority

test_costume:
        cmp #PRIORITY_COSTUME
        bne test_sound

        // case 1 → try releasing one costume (not in current room; attrs==0).
        jsr rsrc_release_one_evictable_costume
        jmp advance_priority

test_sound:
        cmp #PRIORITY_SOUND
        bne test_script

        // case 2 → try releasing evictable sounds (skip protected indices).
        jsr rsrc_release_evictable_sounds
        jmp advance_priority

test_script:
        cmp #PRIORITY_SCRIPT
        bne test_wrap_threshold

        // case 3 → try releasing evictable scripts (attrs==0).
        jsr rsrc_release_evictable_scripts
        jmp advance_priority

test_wrap_threshold:
        // If priority >= 4, force wrap on next INC:
        // set to $FF so INC → $00 (wrap to case 0).
        cmp #PRIORITY_WRAP_AT
        bcc advance_priority
        lda #$FF
        sta evict_pri_cur

advance_priority:
        // Advance to next priority; INC $FF → $00 (wrap).
        inc evict_pri_cur

        // Did any callee release something? (flag set ≠ 0)
        lda rsrc_released_flag
        beq wrap_check_no_release

        // Yes → return True (#FF).
        lda #$FF
        rts

wrap_check_no_release:
        // No release yet. Have we wrapped back to the starting priority?
        // If not, continue dispatch loop.
        lda evict_pri_cur
        cmp evict_pri_start
        bne dispatch_by_priority

        // Completed a full cycle with no releases → return False (#00).
        lda #$00
        rts
/*===========================================
 * rsrc_release_evictable_room_lru: evict one LRU room
 *
 * Summary:
 *   Scans room slots from ROOM_MAX_INDEX down and selects the
 *   least-recently-used *evictable* room: it must be loaded
 *   (ptr.hi != 0), not locked (attr bit7 = 0), and have a
 *   non-zero “age/attr” score. The candidate with the largest
 *   attr value is evicted: table pointers are cleared and
 *   mem_release is called.
 *
 * Arguments:
 *   (none)
 *
 * Reads:
 *   ROOM_MAX_INDEX           Highest valid room index (inclusive).
 *   room_ptr_lo_tbl[X]       Room resource pointer (lo).
 *   room_ptr_hi_tbl[X]       Room resource pointer (hi); nonzero ⇒ loaded.
 *   room_mem_attrs[X]        Per-room attribute/age score; bit7=1 ⇒ locked.
 *
 * Updates:
 *   room_ptr_*_tbl[X]        Cleared for the evicted room to avoid dangling refs.
 *   room_mem_attrs[X]        Cleared for the evicted room.
 *   (Policy) release flags   May be updated by mem_release.
 *
 * Returns:
 *   Flow                      Evicts at most one room; returns immediately after free,
 *                             otherwise returns with no change if none qualified.
 *   Registers                 Clobbered by helper calls (mem_release) and table updates.
 *   Flags                     Modified by loads/stores/branches; no specific exit state.
 *
 * Description:
 *   - Iterate X := ROOM_MAX_INDEX … 1 (index 0 is not considered).
 *   - Skip if not loaded (ptr.hi == 0), or if locked (attr bit7 == 1),
 *     or if attr == 0 (not evictable/fresh).
 *   - Track the best candidate using the largest attr value (strictly greater).
 *   - If a candidate exists:
 *       Y := ptr.hi, X := ptr.lo; clear table refs & attrs; call mem_release.
 *===========================================*/
* = $5B38
rsrc_release_evictable_room_lru:
        lda #$00
        sta best_attr           // best-so-far “staleness” score (0 = none yet)
        sta best_room_idx           // best-so-far room index (0 used as “no candidate” sentinel)
        ldx #ROOM_MAX_INDEX           // start at highest index; loop scans X down to 1 (index 0 is not visited)

scan_room:
        // Must be loaded: hi byte non-zero ⇒ pointer is valid.
        // BEQ takes the “not loaded” path (Z=1 when hi==0) → skip.
        lda room_ptr_hi_tbl,x
        beq advance_room

        // Must not be locked: attr bit7 set (negative) ⇒ locked → skip.
        // BMI branches when N=1 (bit7 set).
        lda room_mem_attrs,x
        bmi advance_room

        // Evictable only if attr != 0. If attr == 0 → skip (in use / pinned / fresh).
        cmp #$00
        beq advance_room

        // LRU selection: prefer strictly larger attr (older/staler).
        // CMP sets C=0 on a<b → BCC skip; Z=1 on a==b → BEQ skip; only a>b selects.
        cmp best_attr
        bcc advance_room
        beq advance_room

        // New best candidate: remember its score and index.
        sta best_attr
        stx best_room_idx

advance_room:
        dex                             // next candidate (X := X-1)
        bne scan_room           		// continue while X != 0  (note: index 0 not checked)
                                        // fallthrough when X==0 → selection phase

        // If no candidate was found, best_room_idx is still 0 → return.
        lda best_room_idx
        bne evict_room_candidate        // non-zero ⇒ have a candidate
        rts

evict_room_candidate:
        ldx best_room_idx             // X := chosen room index

        // Form (X=ptr.lo, Y=ptr.hi) for mem_release.
        // Load hi first, move to Y; then load lo and stash on stack to TAX later.
        lda room_ptr_hi_tbl,x
        tay                             // Y := ptr.hi
        lda room_ptr_lo_tbl,x
        pha                             // save ptr.lo temporarily

        // Clear table reference before freeing to avoid dangling pointers.
        lda #$00
        sta room_ptr_hi_tbl,x
        sta room_ptr_lo_tbl,x

        // Clear room memory attributes now that it’s being evicted.
        lda #$00
        sta room_mem_attrs,x

        // Restore ptr.lo into X (mem_release expects X=lo, Y=hi).
        pla
        tax

        // Free the block at X:lo, Y:hi. Helper may clobber flags/regs.
        jsr mem_release
        rts
/*===========================================
 * rsrc_release_one_evictable_costume: free one eligible costume
 *
 * Summary:
 *   Scans costume slots from COSTUME_MAX_INDEX down and frees the first
 *   costume that is both loaded and not in the current room, provided
 *   its attributes mark it evictable (attrs == 0). Exits immediately
 *   after freeing one costume.
 *
 * Arguments:
 *   (none)
 *
 * Reads:
 *   COSTUME_MAX_INDEX           Highest valid costume index (inclusive).
 *   costume_ptr_lo_tbl[X]     Costume resource pointer (lo).
 *   costume_ptr_hi_tbl[X]     Costume resource pointer (hi); nonzero ⇒ loaded.
 *   costume_mem_attrs[X]      Per-costume attributes; 0 ⇒ evictable.
 *   room_for_character[X]     Owning room index for costume X.
 *   current_room              Index of the current room (active/in use).
 *
 * Updates:
 *   costume_ptr_*_tbl[X]      Cleared for the freed costume to avoid dangling refs.
 *   costume_mem_attrs[X]      Cleared for the freed costume.
 *   (Policy) release flags    May be updated by mem_release.
 *
 * Returns:
 *   Flow:                     Returns immediately after freeing one costume,
 *                             otherwise falls through with no frees.
 *   Registers                 Clobbered by helper calls (mem_release) and table updates.
 *   Flags                     Modified by loads/stores/branches; no specific exit condition.
 *
 * Description:
 *   - Iterate X := COSTUME_MAX_INDEX … 0.
 *   - Skip if not loaded (ptr.hi == 0) or if in current_room (in use).
 *   - Skip if attrs != 0 (not evictable); attrs == 0 ⇒ candidate.
 *   - Preserve loop index on stack; prepare mem_release calling convention:
 *       X := ptr.lo, Y := ptr.hi.
 *   - Clear table pointers and attributes *before* freeing to prevent stale uses.
 *   - Call mem_release and return.
 *===========================================*/
* = $5B84
rsrc_release_one_evictable_costume:
        ldx #COSTUME_MAX_INDEX        // start at highest costume index; scan downward (X := max…0)

scan_costume:
        // Must be loaded: hi byte non-zero ⇒ pointer is valid.
        // BEQ branches on Z=1 when hi==0 ⇒ not loaded → skip.
        lda costume_ptr_hi_tbl,x
        beq advance_costume

        // Must not belong to the current room: if room_for_character[X] == current_room,
        // the costume is in use. CMP sets Z=1 when equal; BEQ → skip.
        lda room_for_character,x
        cmp current_room
        beq advance_costume

        // Eviction policy: attrs == 0 ⇒ evictable. BNE (Z=0) means non-zero attrs → keep.
        lda costume_mem_attrs,x
        bne advance_costume

        // Prepare mem_release(X=ptr.lo, Y=ptr.hi); preserve loop index across the call.
        // Save X (index), load Y from ptr.hi, stash ptr.lo on stack to TAX later.
        txa
        pha                              // save index
        lda costume_ptr_hi_tbl,x
        tay                              // Y := ptr.hi (mem_release convention)
        lda costume_ptr_lo_tbl,x
        pha                              // save ptr.lo; will TAX after clearing refs

        // Clear table references *before* freeing to avoid dangling pointers.
        lda #$00
        sta costume_ptr_lo_tbl,x
        sta costume_ptr_hi_tbl,x

        // Restore ptr.lo into X for the free call.
        pla
        tax

        // Free the block at X:lo, Y:hi (helper may clobber flags/regs).
        jsr mem_release
        rts                               // done after freeing one costume

advance_costume:
        dex                               // next candidate (X := X-1)
        bne scan_costume             // Z=0 → more to scan; Z=1 → finished
        rts
/*===========================================
 * rsrc_release_evictable_sounds: free all evictable sounds
 *
 * Summary:
 *   Scans sound slots from SOUND_MAX_INDEX down to 0. For each slot,
 *   if its attributes indicate “evictable” (attrs == 0) and it is
 *   loaded (pointer hi-byte != 0), clears the table entry and calls
 *   mem_release to free the block. Protected slots are skipped.
 *
 * Arguments:
 *   (none)
 *
 * Reads:
 *   SOUND_MAX_INDEX            Highest valid sound index (inclusive).
 *   SOUND_PROTECT_1..3         Reserved, non-evictable sound indices.
 *   sound_mem_attrs[X]         Per-sound attributes; 0 ⇒ evictable.
 *   sound_ptr_lo_tbl[X]        Sound resource pointer (lo).
 *   sound_ptr_hi_tbl[X]        Sound resource pointer (hi); nonzero ⇒ loaded.
 *
 * Updates:
 *   sound_ptr_*_tbl[X]         Cleared to zero for each released sound.
 *   (Policy) release flags      May be updated by mem_release.
 *
 * Returns:
 *   Registers                  Clobbered by helper calls (mem_release) and table updates.
 *   Flags                      Modified by loads/stores/branches; no specific exit condition.
 *
 * Description:
 *   - Iterate X := SOUND_MAX_INDEX … 0.
 *   - Skip if attrs != 0 (not evictable) or ptr.hi == 0 (not loaded).
 *   - Skip protected indices SOUND_PROTECT_1, _2, _3.
 *   - Preserve X on stack; prepare mem_release calling convention:
 *       X := ptr.lo, Y := ptr.hi.
 *   - Clear table pointers *before* freeing to avoid dangling references.
 *   - Call mem_release; restore X (loop index) and continue.
 *===========================================*/
* = $5BB5
rsrc_release_evictable_sounds:
        ldx #SOUND_MAX_INDEX          // begin at highest sound index; scan downward

scan_sound:
        // Eviction policy: only sounds with attrs == 0 are evictable.
        // LDA sets Z=1 when value==0; BNE branches on Z=0 → non-zero attrs → keep.
        lda sound_mem_attrs,x
        bne advance_sound                // not evictable → skip

        // Must be loaded: pointer hi byte non-zero indicates a valid in-memory resource.
        // BEQ on Z=1 → not loaded → skip.
        lda sound_ptr_hi_tbl,x
        beq advance_sound                // nothing to release

        // Skip protected sounds (reserved/non-evictable content).
        // CPX compares X with the literal; BEQ when equal → skip.
        cpx #SOUND_PROTECT_1
        beq advance_sound
        cpx #SOUND_PROTECT_2
        beq advance_sound
        cpx #SOUND_PROTECT_3
        beq advance_sound

        // Prepare mem_release(X=ptr.lo, Y=ptr.hi) and preserve loop index across the call.
        txa                           // save current index
        pha
        lda sound_ptr_hi_tbl,x
        tay                           // Y := ptr.hi (mem_release calling convention)
        lda sound_ptr_lo_tbl,x
        pha                           // stash ptr.lo; will TAX after clearing table refs

        // Clear table references *before* freeing to avoid dangling pointers.
        lda #$00
        sta sound_ptr_lo_tbl,x
        sta sound_ptr_hi_tbl,x

        // Restore ptr.lo into X for the free call.
        pla
        tax

        // Free the block at X:lo, Y:hi (helper may clobber flags/regs).
        jsr mem_release

        // Restore loop index and continue scanning.
        pla
        tax

advance_sound:
        dex                          // next candidate (X := X-1)
        bne scan_sound		         // Z=0 → more to scan; Z=1 → finished
        rts
/*===========================================
 * rsrc_release_evictable_scripts: free all evictable scripts
 *
 * Summary:
 *   Scans all script slots from highest index down. For each script,
 *   if its memory attributes indicate “evictable” (attrs==0) and it is
 *   loaded (pointer hi-byte != 0), clears the table entry and calls
 *   mem_release to free the block.
 *
 * Arguments:
 *   (none)
 *
 * Reads:
 *   SCRIPT_MAX_INDEX        Highest valid script index (inclusive).
 *   script_memory_attrs[X]  Per-script memory attributes; 0 ⇒ evictable.
 *   script_ptr_lo_tbl[X]    Script resource pointer (lo).
 *   script_ptr_hi_tbl[X]    Script resource pointer (hi); nonzero ⇒ loaded.
 *
 * Updates:
 *   script_ptr_*_tbl[X]     Cleared to zero for each released script.
 *   rsrc_released_flag  	 Set by mem_release upon any successful free.
 *
 * Returns:
 *   Registers:  Clobbered by helper calls (mem_release) and table updates.
 *   Flags:      Modified by loads/stores and branches; no specific condition on exit.
 *
 * Description:
 *   - Iterate X := SCRIPT_MAX_INDEX .. 0.
 *   - Skip if attrs != 0 (not evictable) or pointer hi == 0 (not loaded).
 *   - Preserve X (index) on stack; form (X=ptr.lo, Y=ptr.hi) for mem_release.
 *   - Clear table pointers *before* freeing to avoid dangling references.
 *   - Restore index and continue scanning until all candidates processed.
 *===========================================*/

* = $5BEA
rsrc_release_evictable_scripts:
        ldx #SCRIPT_MAX_INDEX        // start at highest script index; scan downward

scan_script:
        // Eviction policy: only scripts with attrs == 0 are evictable.
        // LDA sets Z=1 iff value==0; BNE branches on Z=0 → non-zero attrs → keep.
        lda script_mem_attrs,x
        bne advance_script              // not evictable → skip

        // Must be loaded: hi byte of pointer is non-zero when present.
        // BEQ branches on Z=1 → not loaded → skip.
        lda script_ptr_hi_tbl,x
        beq advance_script              // no resource to release

        // Prepare mem_release(X=lo, Y=hi). Preserve loop index across the call.
        txa
        pha                          // save script index (X) on stack
        lda script_ptr_hi_tbl,x
        tay                          // Y := ptr.hi (mem_release convention)
        lda script_ptr_lo_tbl,x
        pha                          // save ptr.lo; will TAX after clearing refs

        // Drop table references before freeing to avoid stale/dangling pointers.
        lda #$00
        sta script_ptr_lo_tbl,x
        sta script_ptr_hi_tbl,x

        // Restore ptr.lo into X for the free call.
        pla
        tax

        // Free the block at X:lo, Y:hi (helper may clobber flags/regs).
        jsr mem_release

        // Restore loop index and continue scanning.
        pla
        tax

advance_script:
        dex                         // next candidate (X := X-1)
        bne scan_script         	// Z=0 → more to scan; Z=1 → finished
        rts
		