;===========================================
; Memory Manager — Free-List + Best-Fit + Local Compaction
;
; Overview:
;   - A singly-linked free list manages variable-size blocks. 
;	- Allocation uses a best-fit scan, optionally splitting the chosen free node. 
;	- A localized compaction step ("mem_bubble_used_left") bubbles USED blocks left over the leading
;   free region to grow contiguous free space. 
;	- Periodic coalescing merges adjacent free neighbors. 
;	An AO (address-order) pass can reorder the free list to improve coalescing and locality.
;
; Block header layout (all addresses little-endian):
;   FREE node:
;     +0..+1  block size   		(lo = tail bytes on last page, hi = full pages)
;     +2..+3  next free block   (pointer to next free node; $0000 = null)
;   USED block:
;     +0..+1  block size   (header + payload)
;     +2      resource_type   
;     +3      resource_index  
;
; Global invariants:
;   - Null pointer is detected via HI byte == $00 (no free blocks exist in page $00).
;   - mem_read_next_ptr returns X=next.lo, Y=next.hi and sets Z from Y.
;     STX/STY do not change flags; branches often rely on Z from mem_read_next_ptr.
;   - Size fields are (header+payload) for USED and “bytes-on-last-page/pages” encoding.
;
; Public routines:
;   mem_alloc_bestfit (X/Y = payload size):
;     - Computes mem_req_total = payload + 4 (free header size), seeds the scan,
;       and dispatches to best-fit. Returns X/Y = allocated header on success (Z=0).
;       On failure (free list empty or no fit), returns Z=1 (X/Y unspecified).
;		Used externally by alloc_data.
;
;   mem_coalesce_right:
;     - Merge right-adjacent free nodes in-place: if next == left+left.size,
;       link-out right and grow left by right.size. Cascades at same left node.
;		Used externally by the resource releaser.
;
;   mem_sort_free_ao (AO pass):
;     - For each anchor, finds the lowest-address node < anchor_next and swaps
;       it into anchor_next by pointer rewiring. In-place, O(n^2), non-stable.
;       Refresh mem_free_head from the stub on exit; update mem_free_tail.
;		Used externally by the resource releaser.
;
;   mem_compact_then_release
;     - Recovery path when mem_alloc_bestfit fails. Runs address-order sort
;       (mem_sort_free_ao), coalesces right-adjacent free nodes (mem_coalesce_right),
;       and compacts by bubbling USED blocks left into the leading FREE region
;       (mem_bubble_used_left). If still no fit, calls external
;       release_rsrcs_by_priority and refreshes pointers via update_rsrc_pointers,
;       then retries best-fit. Loops until allocation succeeds or nothing remains
;       to release. Returns X/Y = allocated header, Z=0 on success; Z=1 on failure.
;		Used externally by alloc_data.
;
; Private routines:
;	mem_copy_pages
;     - Copies a block as whole pages plus a tail: first moves total_pages
;       full 256-byte pages from mem_src→mem_dst, then copies mem_copy_tail
;       remaining bytes. Preserves intra-page order; clobbers A/X/Y.
;
;	mem_compact_leading_free
;     - Assumes the list starts with a FREE block. While a USED block directly
;       follows it, copies that USED (header+payload) down into the FREE region,
;       applies pointer fix-ups, advances mem_dst, and rebuilds the FREE header.
;       Stops at the first non-adjacent or FREE block. Updates mem_free_head/tail.
;
;	mem_read_next_ptr
;     - Utility: reads header +2..+3 (little-endian “next” pointer) from the
;       block whose header pointer is in mem_hdr_ptr. Returns X/Y = next (hi/lo)
;       and sets Z=1 if the next pointer is NULL.
;
;   mem_bestfit_select:
;     - Best-fit scan over free nodes (ties prefer later). On success sets
;       mem_free_cur to winner and calls mem_alloc_from_free.
;
;   mem_alloc_from_free:
;     - Split-or-consume the chosen free node. Split if remainder ≥ 4 bytes;
;       otherwise splice out the node. Maintains mem_free_tail.
;
;   mem_bubble_used_left:
;     - While USED blocks immediately follow the leading free node, copy each
;       USED (header+payload) into the FREE region (mem_dst<header), fix
;       external references, and advance pointers. Rebuild free header at end.
;
; Conventions:
;   - Registers: A/X/Y clobbered by most routines; branch docs specify Z/C meaning.
;   - Copy helper (mem_copy_pages):
;       mem_copy_page_cnt = full pages, mem_copy_tail = tail bytes on last page,
;       mem_read_ptr/mem_write_ptr = header addresses; direction safe for overlap.
;
;===========================================

;===============================================================================
; Variables
;===============================================================================

;Invariant: no block headers reside in page $00; null pointers are detected via hi-byte==0

;----------------------------------------
; mem_coalesce_right vars
;----------------------------------------
left_size           = $56c5    	; 16-bit size of current free block on the left side; shared temp reused elsewhere
mem_left            = $4f      	; zp pointer to current free block header (lo @ $4F, hi @ $50)
mem_right           = $59      	; pointer to block at (mem_left + left_size): immediate right neighbor


;----------------------------------------
; mem_sort_free_ao vars
;----------------------------------------
curr_block          = $4f      ; node under comparison during scan (alias of mem_free_cur storage)
anchor_block        = $51      ; pass anchor / predecessor for this sweep (often the head stub initially)
anchor_next         = $53      ; first candidate after anchor_block for this pass
min_offender        = $55      ; lowest-address node found with addr < anchor_next; $FFFF sentinel = none
min_prev            = $57      ; predecessor of min_offender (for relinking); shares ZP with `mem_src`
curr_prev           = $5746    ; ABS temp: rolling predecessor while scanning
curr_next           = $5748    ; ABS temp: cached anchor_next->next
min_next            = $574a    ; ABS temp: cached min_offender->next


;----------------------------------------
; mem_alloc_bestfit / mem_bestfit_select
;----------------------------------------
mem_free_cur        = $4f      ; zp pointer to current free block header (lo @ $4F, hi @ $50)
mem_req_payload     = $fd9c    ; requested payload size in bytes (header NOT included)
mem_best_blk        = $5813    ; pointer to best-fit free block found
mem_best_size       = $5815    ; 16-bit size of best-fit block (lo=tail, hi=pages)
mem_prev            = $5817    ; rolling predecessor of the current candidate during the scan
mem_req_total       = $5819    ; total bytes to carve = mem_req_payload + 4 (header + payload)
candidate_size      = $56c5    ; size of the candidate under inspection (reuses shared temp @ $56C5)


;----------------------------------------
; mem_compact_then_release
;----------------------------------------
mem_req_saved       = $58d3    ; preserved copy of mem_req_payload across compaction/retry attempts


;----------------------------------------
; mem_read_next_ptr
;----------------------------------------
mem_hdr_ptr         = $4f      ; input: pointer to a block header; routine returns next in X=lo, Y=hi


;----------------------------------------
; mem_alloc_from_free
;----------------------------------------
mem_split_rem     	= $56c5    ; remainder after carve: mem_best_size − mem_req_total (shared temp)
temp                = $56c7    ; 8-bit scratch used during pointer transfers
mem_split_tail      = $55      ; trailing free block created on split; in consume path holds predecessor for tail update
predecessor         = $51      ; predecessor pointer; may be the head stub


;----------------------------------------
; mem_bubble_used_left
;----------------------------------------
mem_src             = $57      ; pointer to USED block being moved (initially right of the leading FREE block); shares ZP with min_prev
mem_dst             = $4f      ; pointer to mem_dst (start of the leading FREE span)
mem_free_sz         = $59a2    ; size (bytes) of the current leading FREE block
mem_used_sz         = $59a4    ; size (bytes) of the USED block being relocated (header + payload)
mem_next_free       = $59a6    ; pointer to the next FREE block after the leading one
resource_type       = $59a8    ; resource type tag for pointer fix-ups
resource_index      = $59a9    ; resource index/id for pointer fix-ups


;----------------------------------------
; External routines / flags / tables
;----------------------------------------
update_rsrc_pointers     = $5a89    ; subroutine: retarget external pointers for a relocated resource (uses type/index)
release_rsrcs_by_priority = $5ae3   ; subroutine: release resources by priority tiers, then retry allocation
reload_snd_rsrc_ptrs     = $fe70    ; flag: non-zero => refresh sound resource pointers after relocation
sound_memory_attrs       = $7951    ; table: per-sound usage attributes, indexed by resource_index


;----------------------------------------
; Head and Tail of free block list
;----------------------------------------
mem_free_stub      	= $ff61    ; synthetic head: its header[+2..+3] holds mem_free_head
mem_free_head       = $ff63    ; free-list head pointer (lo @ $FF63, hi @ $FF64)
mem_free_tail       = $ff65    ; free-list tail pointer (lo @ $FF65, hi @ $FF66)


;----------------------------------------
; mem_copy_pages
;----------------------------------------
mem_copy_tail       = $5B      ; final-page byte count (1..255); set to 0 on non-final pages to copy a full 256 via Y-wrap
mem_copy_page_cnt   = $5C      ; number of full 256-byte pages to copy; decremented once per completed page
mem_read_ptr        = $5D      ; mem_src pointer (zp: lo @ $5D, hi @ $5E); hi increments after each full page
mem_write_ptr       = $5F      ; mem_dst pointer (zp: lo @ $5F, hi @ $60); hi increments after each full page


SOUND_RESOURCE  = $06
;------------------------------------------------------------
; Block header layout (4-byte header + payload)
;------------------------------------------------------------
MEM_HDR_SIZE_LO = $00   ; Low byte of block size (total, including header)
MEM_HDR_SIZE_HI = $01   ; High byte of block size
MEM_HDR_NEXT_LO = $02   ; Low byte of pointer to next block
MEM_HDR_NEXT_HI = $03   ; High byte of pointer to next block
MEM_HDR_LEN     = $04   ; Total header length (size + next = 4 bytes)
;------------------------------------------------------------
; Each allocated or free block begins with this header.
; Size fields store the block’s total length in bytes.
; “Next” fields link to the next block in memory order.
;------------------------------------------------------------


;===========================================
; Coalesce Adjacent Free Blocks (right-adjacency merge)
;
; Summary:
;   Walks the free list, merging pairs of free nodes that are laid out
;   back-to-back in memory (right node starts at left + left.size). Merges can
;   cascade at the same left node until its next is non-adjacent. Maintains
;   mem_free_tail if the right node was the tail.
;
; Arguments:
;   mem_free_head    	   	Head pointer of the free list. Seeds the walk.
;   mem_free_tail  	    	Tail pointer of the free list. May be updated
;                               when the rightmost participant of a merge was the tail.
;
; Effects / Returns:
;   In-place merges of adjacent nodes (sizes and next links updated).
;   mem_free_tail updated if the old tail got merged into its left neighbor.
;
;   Clobbers                      A, X, Y; condition flags. Calls mem_read_next_ptr.
;
; Preconditions / Invariants:
;   - Null = $0000 detected via HI byte == $00 (no headers reside in page $00).
;   - mem_read_next_ptr returns X=next.lo, Y=next.hi and sets Z from Y; STX/STY do not change flags.
;
; Description:
;
;   For each node:
;     1) Read its 16-bit size into mem_used_sz.
;
;     2) Compute mem_right = mem_left + mem_used_sz (the address that would
;        be immediately to the right if a neighbor is touching).
;
;     3) Read mem_next_free (current->next). If mem_next_free == mem_right, the two
;        blocks are **contiguous**:
;           • Link-out the right block: first_block.next ← second_block.next.
;           • Grow the left block: size(left) ← size(left) + size(right).
;           • If the right block was mem_free_tail, move the tail to the coalesced left.
;
;     4) If not contiguous, advance to mem_next_free and continue.
;
;   This pass eliminates internal gaps between free regions, creating larger
;   contiguous spans that improve the likelihood of satisfying big allocations.
;
;===========================================
* = $56C8

mem_coalesce_right:
		; Load the head of the free list into ‘mem_left’.
		LDA mem_free_head
		STA mem_left
		LDA mem_free_head + 1
		STA mem_left + 1

		; Empty list? If mem_free_head == $0000, nothing to merge → return.
		BEQ mem_exit_1

compute_adjacent:
		;----------------------------------------
		; Read the current free block’s size (on the left side):
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		LDA (mem_left),Y
		STA left_size
		INY
		LDA (mem_left),Y
		STA left_size + 1

		;----------------------------------------
		; Compute the address of the block that would be immediately to the right:
		;
		;   mem_right = mem_left + left_size
		;----------------------------------------
		CLC
		LDA left_size
		ADC mem_left
		STA mem_right
		LDA left_size + 1
		ADC mem_left + 1
		STA mem_right + 1

		; Fetch the pointer to the NEXT free block (into X/Y).
		JSR mem_read_next_ptr

		; If there is no next block (X:Y == $0000), we’re at the tail → done.
		BNE adjacency_check
		RTS

adjacency_check:
		;----------------------------------------
		; Test right-adjacency:
		;
		;   Are we exactly at mem_left + mem_used_sz ? (i.e., mem_right)
		;   Compare mem_next_free (X/Y) against mem_right (lo/hi).
		;----------------------------------------
		CPX mem_right
		BNE adjacency_check_hi
		CPY mem_right + 1
adjacency_check_hi:
		; Branch if NOT adjacent (X:Y ≠ mem_right).
		BNE next_block_is_not_adjacent

		;----------------------------------------
		; Adjacent detected
		;----------------------------------------
	   
		; Unnecessary code, as X/Y already have the desired values
		; Keeping here for consistency with original
		STX mem_right
		STY mem_right + 1

		;----------------------------------------
		; Splice out the second block from the free list:
		;   first_block.next ← second_block.next
		;
		; Copy header +2..+3 (next pointer) from mem_right → mem_left.
		;----------------------------------------
		LDY #MEM_HDR_NEXT_LO
		LDA (mem_right),Y
		STA (mem_left),Y
		INY
		LDA (mem_right),Y
		STA (mem_left),Y

		;----------------------------------------
		; Add the two block sizes to form the new, coalesced size in mem_left.
		;
		; Perform 16-bit addition: mem_left.size = left_size + mem_right.size
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		CLC
		LDA left_size
		ADC (mem_right),Y          ; add size.lo
		STA (mem_left),Y              ; write new size.lo
		INY
		LDA left_size + 1
		ADC (mem_right),Y          ; add size.hi (+ carry from low-byte add)
		STA (mem_left),Y              ; write new size.hi

		;----------------------------------------
		; If we just removed the TAIL node (mem_right == mem_free_tail),
		; then the newly coalesced first block becomes the new tail.
		;----------------------------------------
		LDA mem_right
		CMP mem_free_tail
		BNE continue
		LDA mem_right + 1
		CMP mem_free_tail + 1
		BNE continue
	   
		LDA mem_left
		STA mem_free_tail
		LDA mem_left + 1
		STA mem_free_tail + 1

continue:
		; Continue scanning: fall through to null-check using hi byte of mem_left.
		LDA mem_left + 1
		JMP is_block_null

next_block_is_not_adjacent:
		; Not adjacent → advance to the next free block and keep checking.
		JSR mem_read_next_ptr			;Redundant call - kept here to match the original
		STX mem_left
		STY mem_left + 1

;----------------------------------------
; Loop guard: if mem_left != $0000, process next candidate; else exit.
;----------------------------------------
is_block_null:
		BNE compute_adjacent
mem_exit_1:
		RTS
;===========================================
; Sort Free Blocks by Address (AO pass with localized swaps)
;
; Summary:
;   Reorders the singly linked free-list into ascending address order (AO).
;   For each anchor (anchor_block), find the lowest-address node < anchor_next
;   within the remainder of the list and swap it into anchor_next’s position
;   by pointer rewiring. Repeat until the end. O(n^2), in-place, non-stable.
;
; Arguments / Inputs:
;   mem_free_stub   Synthetic head; +2..+3 hold mem_free_head.
;   mem_free_head      Current head pointer (variable).
;   mem_free_tail       Tail pointer (variable).
;
; Effects / Outputs:
;   List becomes address-ordered. mem_free_tail updated to final node.
;   mem_free_head refreshed from the stub on exit.
;
;   Clobbers                       
;       A, X, Y; condition flags. Uses mem_read_next_ptr. Internal temps updated.
;
; Invariants:
;   - Null pointer detected via HI byte == $00 (no headers in page $00).
;   - mem_read_next_ptr: X=next.lo, Y=next.hi, Z from Y; STX/STY do not change flags.
;   - curr_block is the input pointer alias for mem_read_next_ptr.
;   - Tie-breaks don’t matter (addresses unique); algorithm is non-stable.
;
; Description:
;
;   The routine treats the free-list stub as a synthetic head whose +2..+3
;   mirror a normal block’s ‘next’ field. For each pass:
;
;     1) Set anchor_block to the current anchor (initially the stub).
;
;     2) Initialize min_offender ← $FFFF (sentinel meaning “none found”).
;
;     3) Walk the list window starting at anchor_next = anchor_block->next:
;          • For each curr_block, read mem_next_free = curr_block->next.
;          • Check Address Order (AO) against anchor_next:
;                (curr_block - anchor_next)  → C=0 means out of order.
;          • Track the lowest-address offender (strictly less than any prior).
;
;     4) If an offender was found, swap it with anchor_next by re-linking four
;        ‘next’ pointers:
;            anchor_block.next     	= min_offender
;            min_prev.next   		= anchor_next
;            min_offender.next 		= anchor_next.next
;            anchor_next.next      	= min_next
;        (min_prev/min_next snapshot offender’s neighbors.)
;
;     5) Advance the anchor:
;            anchor_block ← anchor_block->next
;        and repeat until anchor_block->next is null.
;
;   The effect is akin to performing an insertion step per anchor, moving
;   the lowest offending address up to its correct spot after anchor_block. After
;   all anchors advance, the free-list is sorted by address, which increases the
;   likelihood that adjacent free blocks become coalescible and can be merged
;   efficiently by mem_coalesce_right.
;
;	Note: the sorting is actually non-stable, but it doesn't matter as sorting keys (addresses)
; 	are always unique.
;===========================================
* = $574C
mem_sort_free_ao:
		;----------------------------------------
		; Initialize sorting by setting ‘anchor_block’ to point at the
		; free block stub. This structure mimics a normal block header,
		; holding the mem_free_head pointer at offsets +2..+3.
		;
		; The routine will use anchor_block as the list head during traversal,
		; allowing consistent pointer logic for block headers and the stub.
		;----------------------------------------
		LDA #<mem_free_stub
		STA anchor_block
		LDA #>mem_free_stub
		STA anchor_block + 1

setup_comparison:
		;----------------------------------------
		; Prepare pass state:
		;   min_offender ← $FFFF
		;     Sentinel meaning “no offender found yet”; any real block address
		;     will compare lower and replace it.
		;----------------------------------------
		LDA #$FF
		STA min_offender
		STA min_offender + 1

		;----------------------------------------
		; Initialize the sliding window at the list head:
		;   curr_prev ← anchor_block  (the stub)
		; We’ll advance to the first real entry next; keeping curr_prev aligned
		; with the node before the current candidate simplifies pointer swaps.
		;----------------------------------------
		LDA anchor_block
		STA curr_prev
		LDA anchor_block + 1
		STA curr_prev + 1

		;----------------------------------------
		; Prime the first candidate pair:
		;   anchor_next ← anchor_block->next        (read stub’s +2..+3)
		;   curr_block ← anchor_next            (mirrored in X/Y for speed)
		; If anchor_next == $0000, the list is empty/singleton → nothing to sort.
		;----------------------------------------
		LDY #MEM_HDR_NEXT_LO
		LDA (anchor_block),Y
		STA anchor_next
		TAX
		INY
		LDA (anchor_block),Y
		STA anchor_next + 1
		TAY
		BNE compare_order            ; nonzero: have a first candidate
		; No next block → nothing to sort
		JMP no_more_blocks

compare_order:
		;----------------------------------------
		; Establish the comparison window:
		;   curr_block ← X/Y     (candidate under test)
		;   curr_next    ← curr_block->next
		; We'll verify whether curr_block is in Address Order (AO)
		; relative to anchor_next and track any out-of-order offender.
		;----------------------------------------
		STX curr_block
		STY curr_block + 1

		; Read the singly-linked ‘next’ pointer from curr_block header.
		JSR mem_read_next_ptr
		STX curr_next
		STY curr_next + 1

		;----------------------------------------
		; Address Order (AO) check between curr_block and anchor_next.
		;
		; Compute (curr_block - anchor_next) as a 16-bit subtract:
		;   SEC; SBC low, then SBC high.
		; Interpretation:
		;   C=1 (no borrow)  → curr_block ≥ anchor_next  → AO OK → skip to next_comparison
		;   C=0 (borrow)     → curr_block <  anchor_next → AO violated → handle offender
		;----------------------------------------
		SEC
		LDA curr_block
		SBC anchor_next
		LDA curr_block + 1
		SBC anchor_next + 1
		BCS next_comparison            ; C=1 → in order; continue

		;----------------------------------------
		; AO violated → record the lowest-address offender seen so far.
		;
		; Compare (curr_block - min_offender):
		;   C=1 (no borrow)  → curr_block ≥ min_offender → not lower → keep existing
		;   C=0 (borrow)     → curr_block <  min_offender → NEW lowest offender
		;----------------------------------------
		SEC
		LDA curr_block
		SBC min_offender
		LDA curr_block + 1
		SBC min_offender + 1
		BCS next_comparison            ; C=1 → not lower than current lowest → skip

		;----------------------------------------
		; New lowest-address offender detected — snapshot offender and links:
		;
		;   min_offender ← curr_block       (the out-of-order node)
		;   min_prev   ← curr_prev          (node before offender)
		;   min_next   ← curr_next          (node after offender)
		; These will be used later to perform the pointer swap with anchor_next.
		;----------------------------------------
		LDA curr_block
		STA min_offender
		LDA curr_block + 1
		STA min_offender + 1

		LDA curr_prev
		STA min_prev
		LDA curr_prev + 1
		STA min_prev + 1

		LDA curr_next
		STA min_next
		LDA curr_next + 1
		STA min_next + 1

next_comparison:
		;----------------------------------------
		; Slide the comparison window forward:
		;   curr_prev    ← curr_block
		;   curr_block (X/Y) ← curr_next
		; Then continue while curr_block != $0000.
		;----------------------------------------
		LDA curr_block
		STA curr_prev
		LDA curr_block + 1
		STA curr_prev + 1

		LDX curr_next
		LDY curr_next + 1

		; If curr_block (X/Y) is null, we reached the end — proceed to swap check.
		BNE compare_order

		;----------------------------------------
		; End of this sweep: if an out-of-order (min_offender) was found,
		; perform a localized re-link to move it directly after anchor_block
		; (i.e., swap it into the position of anchor_next).
		;
		; min_offender == $FFFF → none found → skip swap.
		;----------------------------------------
		LDA min_offender + 1
		CMP #$FF
		BEQ next_start_block

		;----------------------------------------
		; Swap min_offender with anchor_next
		; Pointer surgery overview (all writes address header +2..+3 “next” fields):
		;   anchor_block.next     = min_offender
		;   min_prev.next   = anchor_next
		;   min_offender.next = anchor_next.next
		;   anchor_next.next      = min_next
		;----------------------------------------
		LDY #MEM_HDR_NEXT_LO

		; anchor_block->next = min_offender
		LDA min_offender
		STA (anchor_block),Y

		; min_prev->next = anchor_next
		LDA anchor_next
		STA (min_prev),Y

		; min_offender->next = anchor_next->next
		LDA (anchor_next),Y
		STA (min_offender),Y

		; anchor_next->next = min_next
		LDA min_next
		STA (anchor_next),Y

		; Repeat for the high bytes of the pointers
		INY
		LDA min_offender + 1
		STA (anchor_block),Y
		LDA anchor_next + 1
		STA (min_prev),Y
		LDA (anchor_next),Y
		STA (min_offender),Y
		LDA min_next + 1
		STA (anchor_next),Y

next_start_block:
		;----------------------------------------
		; Advance the outer pass anchor:
		;   anchor_block ← anchor_block->next
		; If no further nodes exist, finish by updating the tail pointer.
		;----------------------------------------
		LDA anchor_block
		STA curr_block
		LDA anchor_block + 1
		STA curr_block + 1
	   
		JSR mem_read_next_ptr               ; X/Y = anchor_block->next
		BEQ no_more_blocks               ; null → list exhausted
	   
		STX anchor_block                 ; step anchor forward
		STY anchor_block + 1
		JMP setup_comparison             ; begin next sweep from new anchor

no_more_blocks:
		;----------------------------------------
		; Sorting complete — refresh the recorded tail pointer to the final node.
		; (At this point anchor_block holds the last node in the list.)
		;----------------------------------------
		LDA anchor_block
		STA mem_free_tail
		LDA anchor_block + 1
		STA mem_free_tail + 1
		RTS
;===========================================
; Get Free Block
;
; Summary:
;   Entry point for the allocator.  
;   Given a requested payload size (in X/Y), this routine searches the free
;   list for a suitable memory block, calling the best-fit selector to
;   locate and allocate it.
;
; Arguments:
;   X / Y				Requested payload size (low/high).  
;       				Represents only the data portion; 
;						the routine adds 4 bytes for the internal block header.
;
; Vars/State:
;   mem_req_payload		Stores requested payload size from X/Y.  
;   mem_req_total		Computed as mem_req_payload + 4 (header included).  
;   mem_free_head		Head pointer of the free list.  
;   mem_free_cur        Updated to point to the current candidate free block.
;
; Returns:
;   X / Y               Pointer to allocated block header if successful.  
;   Z Flag              Z=0 on success (block allocated),  
;       				Z=1 if no free block exists (free list empty).           
;
; Preconditions / Notes:
;   - Null free pointer is detected via HI byte == $00 (no headers in page $00).
;   - BNE uses Z from the last LDA of mem_free_head+1; STA does not affect flags.
;   - Best-fit routine consumes mem_req_total and mem_req_payload and performs the split/consume.
;
; Description:
;   The allocator begins by recording the caller’s requested size and
;   calculating the total number of bytes needed, including the 4-byte
;   block header.  
;
;   It then initializes the search pointer to the start of the free list.
;
;   If no free blocks exist (list is empty), it returns immediately with Z=1.  
;
;   Otherwise, it chains into the mem_bestfit_select routine, which
;   performs a full best-fit scan to locate the optimal block and split
;   or consume it as needed.
;===========================================
* = $581B	
mem_alloc_bestfit:
		; Capture payload size for allocator (alias of raw_data_size used by mem_alloc_from_free).
		STX mem_req_payload
		STY mem_req_payload + 1

		; Compute total bytes required including the 4-byte block header:
		; Total requested bytes (HEADER + payload): mem_req_total = raw_data_size + MEM_HDR_LEN
		CLC
		LDA mem_req_payload
		ADC #MEM_HDR_LEN
		STA mem_req_total
		LDA mem_req_payload + 1
		ADC #$00
		STA mem_req_total + 1

		; Initialize the scan at the head of the free list.
		LDA mem_free_head
		STA mem_free_cur
		; LDA sets Z, BNE uses Z from LDA.
		LDA mem_free_head + 1
		STA mem_free_cur + 1

		; If the free list is empty (mem_free_cur == $0000), return with Z=1.
		; Otherwise, tail-call into the best-fit selector.
		BNE mem_bestfit_select
		RTS
;===========================================
; Select Best Free Block (Best-Fit)
;
; Summary:
;   Walk the free list and select the smallest block whose size ≥ mem_req_total.
;   On success, set mem_free_cur to the winner and invoke mem_alloc_from_free to carve it.
;
; Arguments:
;   mem_req_total           Requested allocation size in bytes (16-bit)
;
; Vars/State:
;   mem_free_cur     		Current free node under scan. 
;							On entry, points to the first REAL free node (not the stub).
;   mem_free_stub     		Synthetic predecessor for the list head (used to seed prev tracking).
;   mem_best_blk     		Address of the best candidate found so far (NULL when none).
;   mem_best_size     		Size of the best candidate. 
;   mem_prev     			Rolling predecessor of mem_free_cur during the scan.
;
; Returns:
;
;   Success: Z = 0, X = lo(mem_best_blk), Y = hi(mem_best_blk).
;   Failure: Z = 1 (no fit found). X/Y are unspecified.
;
; Clobbers:
;   A, X, Y; condition flags. Calls mem_alloc_from_free on success.
;
; Preconditions / Invariants:
;   - Null free pointer is detected via HI byte == $00 (no headers reside in page $00).
;   - mem_read_next_ptr leaves X=next.lo, Y=next.hi and Z derived from Y; BNE uses that Z.
;   - Tie-break: equal-size candidates prefer the later one encountered (non-stable).
;
; Description:
;   Initialize mem_best_size = $FFFF and mem_best_blk = $0000; set mem_prev = head stub.
;   For each node:
;     1) Read candidate size. If candidate < mem_req_total → skip.
;     2) If candidate ≤ mem_best_size → record candidate as the new best (size, block, predecessor).
;     3) Advance: mem_prev ← mem_free_cur; mem_free_cur ← mem_free_cur->next (via mem_read_next_ptr).
;   After the scan:
;     - If mem_best_blk == $0000 → return with Z = 1 (no fit).
;     - Else: mem_free_cur ← mem_best_blk; JSR mem_alloc_from_free; return X/Y = mem_best_blk.
;===========================================
mem_bestfit_select:
		;----------------------------------------
		; Initialize “current best” to: size = $FFFF (max) and block = $0000 (none).
		; Any real fitting candidate will be smaller than $FFFF and replace this.
		;----------------------------------------
		LDA #$FF
		STA mem_best_size
		STA mem_best_size + 1
		LDA #$00
		STA mem_best_blk
		LDA #$00
		STA mem_best_blk + 1

		;----------------------------------------
		; Seed mem_prev with the free-list stub head (acts as the predecessor
		; of the first real free block). As we walk the list, mem_prev will
		; track the node before ‘mem_free_cur’ for bookkeeping/surgery later.
		;----------------------------------------
		LDA #<mem_free_stub
		STA mem_prev
		LDA #>mem_free_stub
		STA mem_prev + 1

check_candidate:
		;----------------------------------------
		; Read the candidate’s size from its header:
		;   header +0 = size.lo (tail bytes), header +1 = size.hi (pages)
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		LDA (mem_free_cur),Y
		STA candidate_size
		INY
		LDA (mem_free_cur),Y
		STA candidate_size + 1

		;----------------------------------------
		; Fit test: does candidate >= mem_req_total ?
		;
		; Compute (candidate - mem_req_total). 
		; If a borrow occurs (BCC), then candidate < mem_req_total → it cannot fit → skip to next block.
		;----------------------------------------
		SEC
		LDA candidate_size
		SBC mem_req_total
		LDA candidate_size + 1
		SBC mem_req_total + 1
		BCC move_to_next_block            ; too small → examine next candidate

		;----------------------------------------
		; DATA FITS — check whether this candidate improves the current best-fit.
		;
		; Compare (mem_best_size - candidate_size):
		;
		;   If borrow (BCC) → candidate is LARGER than current best → not better.
		;   If no borrow    → candidate is <= current best         → accept later.
		;----------------------------------------
		SEC
		LDA mem_best_size
		SBC candidate_size
		LDA mem_best_size + 1
		SBC candidate_size + 1
		BCC move_to_next_block            ; candidate > best → skip, keep current best

		;----------------------------------------
		; NEW BEST-FIT FOUND
		;
		; This call to "mem_read_next_ptr" is not really needed, as X/Y will get overwritten
		; again by the following "mem_read_next_ptr" call later on - it doesn't harm either
		; Keeping it here as it's present in the original code
		;----------------------------------------
		JSR mem_read_next_ptr		

		; Record the new minimum (best) size.
		LDA candidate_size
		STA mem_best_size
		LDA candidate_size + 1
		STA mem_best_size + 1

		; Record the address of the new best-fit block.
		LDA mem_free_cur
		STA mem_best_blk
		LDA mem_free_cur + 1
		STA mem_best_blk + 1

		; Record the address of the predecessor
		LDA mem_prev
		STA predecessor
		LDA mem_prev + 1
		STA predecessor + 1

move_to_next_block:
		;----------------------------------------
		; Remember the current candidate as 'mem_prev' (predecessor tracking).
		; Useful for later list surgery, though not directly used in this pass.
		;----------------------------------------
		LDA mem_free_cur
		STA mem_prev
		LDA mem_free_cur + 1
		STA mem_prev + 1

		; Advance the scan to the next free block in the list.
		JSR mem_read_next_ptr
		STX mem_free_cur
		STY mem_free_cur + 1

		; Continue scanning while mem_free_cur != $0000.
		BNE check_candidate

		;----------------------------------------
		; END OF SCAN — commit best-fit (if any) and allocate
		; Load the winner back into mem_free_cur so mem_alloc_from_free operates on it.
		;----------------------------------------
		LDA mem_best_blk
		STA mem_free_cur
		LDA mem_best_blk + 1
		STA mem_free_cur + 1

		; If mem_best_blk == $0000 → no fitting block was found → return with Z=1.
		BNE free_block_found
		RTS

free_block_found:
		; Consume/split the chosen free block to satisfy the request.
		JSR mem_alloc_from_free

		; Return the allocated block pointer to caller:
		;   X = lo(mem_best_blk), Y = hi(mem_best_blk), Z=0 (success).
		LDX mem_best_blk
		LDY mem_best_blk + 1
		RTS
;===========================================
; Compact and Retry Allocation with Resource Release
;
; Summary:
;   Attempts to allocate a memory block of the requested size by first compacting
;   the free-list (pushing and merging free blocks toward the top of memory). If
;   that still fails, the routine progressively releases lower-priority resources
;   and retries the allocation until a block is obtained or no more resources can
;   be freed.
;
; Arguments:
;   mem_req_payload           	16-bit requested payload size in bytes.
;                                  Temporarily overwritten with $FFFF to mark
;                                  “allocation in progress” state.
;   mem_req_saved                   		Local copy of requested size used across retries.
;
; Uses:
;   mem_compact_leading_free       Defragments the free list by pushing free blocks
;                                  toward higher memory and coalescing contiguous
;                                  ones.
;   mem_alloc_bestfit                 Attempts to find a free block large enough for
;                                  ‘mem_req_saved’. Returns with Z=0 (BNE) on success.
;   release_rsrcs_by_priority      Frees memory by releasing lower-priority resources;
;                                  called if allocation failed, then retry loop repeats.
;
; Returns:
;   On success: free_block_found path is taken (BNE true), RTS with free block ready.
;   On failure: loops internally until resources are exhausted or block found.
;
; Description:
;   This routine is part of the allocator’s fallback mechanism when fragmentation
;   or resource exhaustion prevents allocation. It snapshots the caller’s requested
;   size into ‘mem_req_saved’ and “poisons” mem_req_payload ($FFFF) to signal an in-progress
;   allocation attempt. It then invokes mem_compact_leading_free to compact memory
;   so that smaller fragments merge into larger contiguous spaces. If mem_alloc_bestfit
;   fails to find a suitable block, the routine calls release_rsrcs_by_priority to
;   free noncritical assets (e.g., cached sound or graphics data) and loops back to
;   retry allocation. Once a valid free block is obtained, control exits normally.
;===========================================
* = $58D5

mem_compact_then_release:
		;----------------------------------------
		; Preserve caller’s requested payload size and force the allocator path:
		;   mem_req_saved ← mem_req_payload
		;   mem_req_payload ← $FFFF  (sentinel to indicate “recompute/use saved size”)
		; Rationale: some downstream paths consult mem_req_payload; poisoning it avoids
		; accidental reuse while we compact and possibly free resources.
		;----------------------------------------
		LDA mem_req_payload
		STA mem_req_saved
		LDA mem_req_payload + 1
		STA mem_req_saved + 1
		LDA #$FF
		STA mem_req_payload
		LDA #$FF
		STA mem_req_payload + 1

		;----------------------------------------
		; Phase 1: try to make a large contiguous free region by pushing leading
		; free space toward higher addresses and merging neighbors.
		;----------------------------------------
		JSR mem_compact_leading_free

		;----------------------------------------
		; Phase 2: attempt allocation with the preserved requested mem_req_saved.
		; On success, mem_alloc_bestfit returns with Z=0 → branch to exit.
		;----------------------------------------
		LDX mem_req_saved
		LDY mem_req_saved + 1
		JSR mem_alloc_bestfit
		BNE free_block_found_2

		;----------------------------------------
		; Phase 3 (fallback): still no block — release resources by priority and retry.
		; release_rsrcs_by_priority returns Z=0 when something was released; loop if so.
		;----------------------------------------
		JSR release_rsrcs_by_priority
		BNE mem_compact_then_release

free_block_found_2:
		RTS
;===========================================
; Get pointer to the next block
;
; Summary:
;   Reads the 16-bit “next” pointer from a block header and returns it in X/Y.
;
; Arguments:
;   mem_hdr_ptr				Pointer to current block header
;
; Returns:
;   X/Y                     Next block pointer (lo in X, hi in Y). 
;							If the stored pointer is $0000, X/Y will be $00/$00.
;
; Notes:
;   Block header layout (little-endian):
;     +0..+1 : size (bytes, includes header)
;     +2..+3 : next pointer (16-bit)
;===========================================
* = $58FF
mem_read_next_ptr:
		; Y = 2 → address the “next” field (low byte) in the block header
		LDY #MEM_HDR_NEXT_LO
		; Load low byte of next pointer → X
		LDA (mem_hdr_ptr),Y
		TAX
		; Load high byte of next pointer → Y
		INY
		LDA (mem_hdr_ptr),Y
		TAY
		RTS
;===========================================
; Allocate from a chosen free block (split or consume)
;
; Summary:
;   Carves the requested block (HEADER + payload) from the selected free node.
;   If the true remainder is ≥ 4 bytes (enough for a free header), splits and
;   creates a trailing free node; otherwise, consumes the whole free node.
;   Updates predecessor.next and keeps mem_free_tail correct.
;
; Arguments:
;   mem_free_cur			Pointer to the chosen free block header.
;   mem_best_size       16-bit size of the chosen free block
;							Note: this is an alias of mem_best_size.
;   mem_req_payload         16-bit requested payload size (header NOT included).
;   predecessor		        Predecessor pointer (may be the head stub); we write its +2/+3.
;
; Returns / Updates:
;   predecessor.next        → new trailing free (split) or original next (consume).
;   mem_split_tail          → trailing free (split) or predecessor (consume) for uniform tail update.
;   mem_split_rem    		→ (mem_best_size − mem_req_total), split path only.
;   mem_free_tail         	→ updated if the chosen block had been the tail.
;
;   Clobbers                A, X, Y; flags.
;
; Notes:
;   mem_req_total = mem_req_payload + 4 (payload + HEADER).
;   Using a split threshold of ≥ 4 permits header-only free nodes
;
; Description:
;   Computes mem_split_rem = mem_best_size - mem_req_payload
;
;   If mem_split_rem ≥ 4, the routine SPLITS:
;     • mem_split_tail = mem_free_cur + mem_req_payload
;     • predecessor.next ← mem_split_tail
;     • mem_split_tail.next  ← mem_free_cur.next
;     • mem_split_tail.size  ← mem_split_rem
;
;   Otherwise it CONSUMES the entire free block:
;     • predecessor.next ← mem_free_cur.next
;     • mem_req_payload ← mem_free_sz  (caller receives whole block)
;     • mem_split_tail ← predecessor (simplifies tail update)
;
;   Finally, if the chosen block was the list tail, mem_free_tail is updated to the
;   trailing free block (or stub) so the tail pointer remains correct.
;===========================================
* = $5909
mem_alloc_from_free:
		;----------------------------------------
		; Compute how much of the chosen free block will remain after carving out
		; the requested payload:
		;
		;   mem_split_rem = mem_best_size - mem_req_payload
		;----------------------------------------
		SEC
		LDA mem_best_size
		SBC mem_req_payload
		STA mem_split_rem
		LDA mem_best_size + 1
		SBC mem_req_payload + 1
		STA mem_split_rem + 1

		;----------------------------------------
		; Is there enough space left to create a NEW free block header?
		;
		; We need at least 4 bytes (size lo/hi + next lo/hi). 
		; Perform a 16-bit compare: (mem_split_rem - 4). 
		; If borrow occurs (BCC), there is NOT enough space to split → consume the whole block.
		; Note: C=1 & result==0 means remainder==4 ⇒ header-only free node
		;----------------------------------------
		SEC
		LDA mem_split_rem
		SBC #MEM_HDR_LEN                      ; subtract header size (low)
		LDA mem_split_rem + 1
		SBC #$00                      ; subtract header size (high)
		BCC consume_whole_block     ; C=0 → mem_split_rem < 4

		;--------------------------------
		; Split case
		;
		; Carve payload from the front of the chosen free block and create a NEW trailing 
		; free block with the leftover space.
		;
		;   mem_split_tail = mem_free_cur + mem_req_payload
		;----------------------------------------
		CLC
		LDA mem_free_cur
		ADC mem_req_payload
		STA mem_split_tail
		LDA mem_free_cur + 1
		ADC mem_req_payload + 1
		STA mem_split_tail + 1

		;----------------------------------------
		; Link the free list to the new trailing block
		;
		; Update the predecessor's next to point at mem_split_tail.
		;----------------------------------------
		LDY #MEM_HDR_NEXT_LO
		LDA mem_split_tail
		STA (predecessor),Y     ; write next.lo
		INY
		LDA mem_split_tail + 1
		STA (predecessor),Y     ; write next.hi


		;----------------------------------------
		; Chain the new trailing free block into the list
		;
		; Read mem_free_cur->next into X/Y, then store into mem_split_tail header (+2..+3).
		;
		;   mem_split_tail.next = mem_free_cur.next
		;----------------------------------------
		JSR mem_read_next_ptr
		STY temp                         ; stash hi byte temporarily
		LDY #MEM_HDR_NEXT_LO
		TXA
		STA (mem_split_tail),Y           ; header[2] = next.lo
		INY
		LDA temp
		STA (mem_split_tail),Y           ; header[3] = next.hi

		;----------------------------------------
		; Initialize the size of the new trailing free block
		;
		;   mem_split_tail.size = mem_split_rem  (header +0..+1)
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		LDA mem_split_rem
		STA (mem_split_tail),Y           ; header[0] = size.lo
		INY
		LDA mem_split_rem + 1
		STA (mem_split_tail),Y           ; header[1] = size.hi
		JMP tail_adjust_check  ; update tail pointer if needed

consume_whole_block:
		;----------------------------------------
		; Not enough room to hold a new free header (mem_split_rem < 4):
		; consume the ENTIRE chosen free block for this allocation.
		;
		; Splice the chosen block out of the free list:
		;   predecessor.next = mem_free_cur.next
		;----------------------------------------
		JSR mem_read_next_ptr
		STY temp
		LDY #MEM_HDR_NEXT_LO
		TXA
		STA (predecessor),Y      ; write next.lo
		INY
		LDA temp
		STA (predecessor),Y      ; write next.hi

		;----------------------------------------
		; Caller will use the whole block: set requested size to block size.
		;
		; (mem_req_payload now equals mem_best_size)
		;----------------------------------------
		LDA mem_best_size
		STA mem_req_payload
		LDA mem_best_size + 1
		STA mem_req_payload + 1

		;----------------------------------------
		; No trailing free block is created in this case.
		;
		; Set mem_split_tail to the list “table” placeholder so downstream
		; tail-adjustment logic can treat both paths uniformly.
		;----------------------------------------
		LDA predecessor
		STA mem_split_tail
		LDA predecessor + 1
		STA mem_split_tail + 1

tail_adjust_check:
		;----------------------------------------
		; Was the CHOSEN free block also the TAIL of the free list?
		;
		; If mem_free_tail == mem_free_cur, the block we just split/consumed was the tail.
		;----------------------------------------
		LDA mem_free_tail
		CMP mem_free_cur
		BNE tail_adjust_check_else
		LDA mem_free_tail + 1
		CMP mem_free_cur + 1
tail_adjust_check_else:
		;If it wasn't, nothing to adjust, exit
		BNE mem_exit

		;----------------------------------------
		; Yes: update the tail to the new trailing free block produced by this op.
		;
		; In the split path, mem_split_tail = (mem_free_cur + mem_req_payload).
		; In the “no split” path, mem_split_tail was set to the table placeholder
		; so downstream logic treats both cases uniformly.
		;----------------------------------------
		LDA mem_split_tail
		STA mem_free_tail                  
		LDA mem_split_tail + 1
		STA mem_free_tail + 1
mem_exit:
		RTS
;===========================================
; Compact all leading free space toward higher addresses
;
; Summary:
;   Iteratively “bubbles” used blocks to the right over the first free block,
;   pushing the free space toward higher addresses. After each bubble, merges
;   adjacent free blocks. Repeats until no more right-adjacent used blocks
;   remain or the free list is exhausted.
;
; Preconditions / Invariants:
;   - 'mem_free_cur' points to the header of the current leading free block.
;   - ZP aliasing: 'mem_free_cur' and 'mem_dst' share storage (mem_dst=free head).
;   - Null pointers are detected via hi byte == 0 (no headers live in page $00).
;
; Arguments / Inputs:
;   mem_free_head          Head pointer of the free list (lo/hi).
;   free header @ mem_free_cur  +0..+1 = size (lo=tail bytes, hi=pages)
;                             +2..+3 = next pointer
; Updates:
;   mem_free_head          May be updated by 'mem_bubble_used_left' to new free head.
;   Transient state           mem_src (mem_free_cur + size), mem_next_free (free->next),
;                             mem_free_sz. Aliases with vars used by callees.
; Calls:
;   mem_bubble_used_left              Slides USED run into the FREE region; updates head.
;   mem_coalesce_right      Merges adjacent FREE blocks.
;
; Description:
;
;   Loads the head of the free list into ‘mem_free_cur’, reads its size into
;   ‘mem_free_sz’, and computes ‘adjacent_block = mem_free_cur + mem_free_sz’.
;
;   It then fetches mem_free_cur->next:
;     • If no next free block exists, the routine returns (nothing to push).
;     • Otherwise, it:
;         1) Calls mem_bubble_used_left to relocate any used blocks that sit immediately
;            to the right of the leading free block, thereby sliding the free
;            region upward in memory (toward higher addresses).
;         2) Calls mem_coalesce_right to merge contiguous free neighbors that
;            may have become adjacent after the relocation.
;         3) Loops back to re-evaluate the (possibly updated) head until no more
;            movement is needed.
;
;   The end result is a more compacted free region near the top, improving the
;   chance of satisfying larger allocations.
;===========================================
* = $59AA

mem_compact_leading_free:
		;----------------------------------------
		; Load the head of the free list into the working pointer ‘mem_free_cur’.
		; This is the leading free region we try to push “to the back” (higher addrs).
		;----------------------------------------
		LDA mem_free_head
		STA mem_free_cur
		LDA mem_free_head + 1
		STA mem_free_cur + 1

		;----------------------------------------
		; Read the free block’s size from its header:
		;   header +0 = size lo (tail bytes on last page)
		;   header +1 = size hi (full 256-byte pages)
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		LDA (mem_free_cur),Y
		STA mem_free_sz
		INY
		LDA (mem_free_cur),Y
		STA mem_free_sz + 1


		;----------------------------------------
		; Compute the address of the block immediately to the right:
		;   mem_src = mem_free_cur + mem_free_sz
		; (size is stored as tail-bytes in lo and page count in hi)
		;----------------------------------------
		CLC
		LDA mem_free_cur
		ADC mem_free_sz
		STA mem_src
		LDA mem_free_cur + 1
		ADC mem_free_sz + 1
		STA mem_src + 1

		;----------------------------------------
		; Read the singly-linked list pointer:
		;
		; mem_read_next_ptr → X=next.lo, Y=next.hi, Z set from Y (TAY inside).
		; BNE means Y≠0 → non-null next (relies on “no headers in page $00” invariant).
		;----------------------------------------
		JSR mem_read_next_ptr
		BNE next_block_present
		RTS


next_block_present:
		STX mem_next_free
		STY mem_next_free + 1

		; Relocate any USED blocks that sit immediately to the right of the
		; leading free block so that the free region moves “back” (to higher
		; addresses). This may repeat internally until the next free block
		; is adjacent to ‘mem_dst’.
	   
		; 'mem_free_cur' (alias of 'mem_dst') already points at the FREE head.
		; 'mem_src' = mem_free_cur + size points at the first USED block to the right.
		; 'mem_next_free' is the next FREE block after the USED run.
		;
		; mem_bubble_used_left will:
		;   - Copy each USED block (header+payload) from mem_src → mem_dst,
		;   - Advance pointers until mem_src >= mem_next_free,
		;   - Rebuild the free header at the new mem_dst and update mem_free_head.	   
		JSR mem_bubble_used_left

		; The relocated free head may now be adjacent to the next free block; merge them.
		JSR mem_coalesce_right

		; Re-evaluate the (possibly changed) head; repeat until there is no next free block.		   
		JMP mem_compact_leading_free
;===========================================
; Move the first free block to the right (bubble used blocks left)
;
; Summary:
;   Copies each USED block immediately to the right of the leading FREE block
;   into the FREE region at 'mem_dst' (which is the leading FREE block),
;   thereby shifting USED blocks toward lower addresses and pushing FREE space
;   toward higher addresses.
;
;   Repeats while mem_src < mem_next_free. When mem_src reaches mem_next_free,
;   rebuilds the free header at the new 'mem_dst' and updates the list head.
;
;   After each move, external pointers are updated; a sound reload is requested
;   only if the moved block is an in-use sound.
;
; Preconditions:
;   mem_dst = address of the leading FREE block (start of the hole)
;   mem_src      = mem_dst + mem_free_sz (first USED block to the right)
;   mem_next_free  = pointer to the next FREE block after the current run of USED blocks
;
; Arguments (pointers/vars):
;   mem_src            USED block being moved (points to its HEADER)
;   mem_dst       FREE region to receive the move (points to where HEADER will be written)
;   mem_next_free        NEXT FREE block after the current run of USED blocks
;
; State:
;   mem_free_sz   Original size (bytes) of the leading FREE block
;   mem_used_sz        size of the used block being moved.
;
; Updates:
;   mem_src                        Advanced past the moved used block(s).
;   mem_dst                   Advanced to the end of the moved region.
;   mem_free_head              Updated to the rebuilt free block at the new mem_dst.
;   reload_snd_rsrc_ptrs          Cleared to #$00 before exit.
;
; Details:
;   This routine “bubbles” used blocks to the right into the leading free space, thereby
;   accumulating a larger contiguous free region. 
;
;	For each adjacent used block:
;
;     1) Read its size and metadata.
;     2) Optionally set reload_snd_rsrc_ptrs if a sound block is in-use.
;     3) Call mem_copy_pages to copy the entire block from ‘mem_src’ → ‘mem_dst’.
;     4) Call update_rsrc_pointers(metadata) so external references follow the move.
;     5) Advance both pointers (mem_src, mem_dst) by the moved size and loop while more used blocks remain
;        before ‘mem_next_free’.
;
;   Once the next free block is adjacent, the routine updates mem_free_head and writes
;   the free block header (size + next pointer) at ‘mem_dst’, completing the compaction step.
;
; Notes:
;   - Copy direction is forward and safe for overlap because mem_dst < mem_src.
;   - Sound resources: if in use, set 'reload_snd_rsrc_ptrs' during the move; it is consumed
;     by 'update_rsrc_pointers' and then cleared before the next iteration.
;===========================================
* = $59E5

; Conventions: mem_src=USED block (header addr), mem_dst=FREE hole (header addr); mem_next_free=next FREE block
mem_bubble_used_left:
		;----------------------------------------
		; Read size of the USED block from its header at 'mem_src':
		;
		;   +0 = bytes-on-last-page (0..255)
		;   +1 = full 256-byte pages
		;
		; size is stored little-endian across these two fields.
		; (size is the full block: header + payload)
		;----------------------------------------
		LDY #MEM_HDR_SIZE_LO
		LDA (mem_src),Y
		STA mem_used_sz             ; low byte: tail bytes
		INY
		LDA (mem_src),Y
		STA mem_used_sz + 1         ; high byte: page count
		INY                        ; Y now points at metadata

		;----------------------------------------
		; Read resource metadata used for pointer fix-ups after relocation:
		;
		;   +2 = resource_type  (e.g., sound, graphics, etc.)
		;   +3 = resource_index (which specific instance)
		;
		; For USED blocks, header +2/+3 carry (type,index). FREE blocks use +2/+3 as next.
		; We temporarily push 'resource_type' so we can load index into Y,
		; then restore A to hold the type for the comparison that follows.
		;----------------------------------------
		LDA (mem_src),Y
		STA resource_type
		PHA                        ; save A=resource_type
		INY
		LDA (mem_src),Y
		STA resource_index
		TAY                        ; Y = resource_index (for table lookups)
		PLA                        ; A = resource_type (restored)

		;----------------------------------------
		; If the moved block is a SOUND resource and marked “in use”,
		; set a flag so dependent pointers will be refreshed after relocation.
		;
		; A currently holds resource_type, Y holds resource_index.
		;----------------------------------------
		CMP #SOUND_RESOURCE         ; resource_type == sound?
		BNE copy_block_data         ; no → skip reload request

		LDA sound_memory_attrs,Y    ; load usage attribute for this sound instance
		; Indexing sound_memory_attrs by resource_index.
		BEQ copy_block_data         ; 0 → not in use → no reload needed

		LDA #$01
		STA reload_snd_rsrc_ptrs    ; mark: reload sound resource pointers later

copy_block_data:
		;----------------------------------------
		; Prepare argument block for mem_copy_pages:
		;
		;   mem_copy_tail = tail bytes on the last page      	(low  byte of mem_used_sz)
		;   mem_copy_page_cnt = full 256-byte pages to copy      			(high byte of mem_used_sz)
		;   mem_read_ptr = mem_src pointer (lo/hi)       					(address of the USED block HEADER)
		;   mem_write_ptr = mem_dst pointer (lo/hi)  				(address of FREE space to fill)
		;
		; Copy length equals mem_used_sz (header + payload).
		;
		; After setup, mem_copy_pages will copy (mem_copy_page_cnt * 256 + mem_copy_tail) bytes
		; from [mem_read_ptr] to [mem_write_ptr], handling page spans appropriately.
		;----------------------------------------
		LDA mem_used_sz
		STA mem_copy_tail
		LDA mem_used_sz + 1
		STA mem_copy_page_cnt

		LDA mem_src
		STA mem_read_ptr
		LDA mem_src + 1
		STA mem_read_ptr + 1

		LDA mem_dst
		STA mem_write_ptr
		LDA mem_dst + 1
		STA mem_write_ptr + 1

		; Perform the relocation copy (mem_src → mem_dst).
		; Copies (pages*256 + tail_bytes) as prepared above.
		JSR mem_copy_pages

		; Fix external references to the moved block.
		; API contract: X = resource_index, Y = resource_type.
		LDX resource_index
		LDY resource_type
		JSR update_rsrc_pointers

		; Clear the “reload sound pointers” request flag (consumed).
		LDA #$00
		STA reload_snd_rsrc_ptrs

		;----------------------------------------
		; Advance both pointers by the size of the moved USED block:
		;
		;   mem_dst ← mem_dst + mem_used_sz
		;   mem_src      ← mem_src      + mem_used_sz
		;
		; Invariant preserved: mem_dst < mem_src.
		; mem_dst now points to the start of the remaining FREE space (immediately after the moved USED block) 
		; mem_src points to the block that originally followed the moved USED block.
		;----------------------------------------
		; mem_dst += mem_used_sz
		CLC                               
		LDA mem_dst
		ADC mem_used_sz                   
		STA mem_dst
		LDA mem_dst + 1
		ADC mem_used_sz + 1                
		STA mem_dst + 1

		; mem_src += mem_used_sz
		CLC
		LDA mem_src
		ADC mem_used_sz                   
		STA mem_src
		LDA mem_src + 1
		ADC mem_used_sz +1                
		STA mem_src + 1


		;----------------------------------------
		; The next free block can either:
		; 	-reside immediately after the moved free block
		; 	-reside higher, with at least one block in between
		;
		; If it resides higher, then there's at least one used block in the middle and we can move it to the front.
		; If it resides immediately after, we now have two consecutive free blocks, which can be coalesced.
		;
		; Check if there are still USED blocks between our current 'mem_src'
		; and the next FREE block ('mem_next_free'). 
		;
		; We perform a 16-bit compare:
		;
		;   Compute (mem_src - mem_next_free) with SEC/SBC across low, then high.
		;   If result < 0 → borrow → C=0 → mem_next_free > mem_src → keep moving.
		;   If result ≥ 0 → C=1 → we've reached/passed mem_next_free → stop moving.
		;----------------------------------------
		SEC
		LDA mem_src
		SBC mem_next_free            ; low-byte: mem_src - mem_next_free
		LDA mem_src + 1
		SBC mem_next_free + 1            ; high-byte with borrow propagation
		; C clear → mem_next_free > mem_src → more used blocks remain to relocate, loop
		BCC mem_bubble_used_left

		;----------------------------------------
		; Adjacent FREE block now follows the relocated region:
		;
		;   Update the free-list HEAD to point at 'mem_dst',
		;   then rebuild the FREE block header at 'mem_dst':
		;     +0..+1 = size  (mem_free_sz)
		;     +2..+3 = next  (mem_next_free)
		;----------------------------------------
		; FREE header written at new mem_dst; list head points here.
		LDA mem_dst
		STA mem_free_head
		LDA mem_dst + 1
		STA mem_free_head + 1

		LDY #MEM_HDR_SIZE_LO
		LDA mem_free_sz
		STA (mem_dst),Y              ; header[0] = size lo
		INY
		LDA mem_free_sz + 1
		STA (mem_dst),Y              ; header[1] = size hi
		INY
		LDA mem_next_free
		STA (mem_dst),Y              ; header[2] = next lo
		INY
		LDA mem_next_free + 1
		STA (mem_dst),Y              ; header[3] = next hi
		RTS
;===========================================
; Copy memory: (mem_copy_page_cnt * 256 + mem_copy_tail) bytes
;
; Arguments:
;   mem_copy_tail        Bytes to copy on the final (last) page (0..255)
;   mem_copy_page_cnt               Number of full 256-byte pages to copy
;   mem_read_ptr             Source pointer (lo/hi)
;   mem_write_ptr            Destination pointer (lo/hi)
;
; Returns:
;   Copies (mem_copy_page_cnt * 256 + mem_copy_tail) bytes from mem_read_ptr to mem_write_ptr.
;   On exit, mem_read_ptr/mem_write_ptr have been advanced by the total copied size.
;
; Notes:
;   - If mem_copy_page_cnt == 0 and mem_copy_tail == 0 → nothing to do.
;   - The routine copies page-by-page; for non-final pages it copies 256 bytes
;     by temporarily setting mem_copy_tail = $00 (meaning 256 via CPY logic).
;===========================================
* = $5C13
mem_copy_pages:
		; Save final-page byte count on the stack
		LDA mem_copy_tail
		PHA
		LDX mem_copy_page_cnt

next_page:
		BEQ last_page                   ; X==0 → process the final partial page

		; Not the last page: copy a full 256-byte page
		LDA #$00
		STA mem_copy_tail
		JMP copy_page

; It's the last page: restore the true final-byte count
last_page:
		PLA
		STA mem_copy_tail
		BNE copy_page                   ; 0 means nothing left to copy → RTS
		RTS

copy_page:
		LDY #$00
; Copy from mem_read_ptr to mem_write_ptr until Y reaches mem_copy_tail
copy_loop:
		LDA (mem_read_ptr),Y
		STA (mem_write_ptr),Y
		INY
		CPY mem_copy_tail
		BNE copy_loop

		; Finished this page: advance mem_src/dest to next page, decrement page count
		INC mem_read_ptr + 1
		INC mem_write_ptr + 1
		DEX
		BPL next_page
		RTS

;===========================================
; mem_sort_free_ao pseudo_code
;===========================================
; // Reorders the FREE list by ascending address using an anchor-and-swap sweep.
;
; function mem_sort_free_ao():
;     ; anchor_block ← head stub (mem_free_stub)
;     anchor_block ← mem_free_stub
;
;     loop_pass:
;         ; Reset per-pass state
;         min_offender ← 0xFFFF        ; sentinel: “no offender found yet”
;         curr_prev    ← anchor_block  ; rolling predecessor in the scan window
;
;         ; Prime first candidate pair
;         anchor_next  ← next(anchor_block)       ; stub->next (first real node)
;         if anchor_next == NULL then
;             goto no_more_blocks                 ; empty/singleton — sorting done
;         end if
;
;         ; Scan forward from anchor_next to end, tracking lowest-address offender
;         curr_block ← anchor_next
;         while curr_block != NULL do
;             curr_next ← next(curr_block)
;
;             ; AO check: offender iff curr_block < anchor_next
;             if addr_lt(curr_block, anchor_next) then
;                 ; Keep the lowest-address offender seen so far
;                 if addr_lt(curr_block, min_offender) then
;                     min_offender ← curr_block
;                     min_prev     ← curr_prev
;                     min_next     ← curr_next
;                 end if
;             end if
;
;             ; Slide the window
;             curr_prev  ← curr_block
;             curr_block ← curr_next
;         end while
;
;         ; Swap phase: if an offender was found, swap it into anchor_next’s slot
;         ; NOTE (as coded): checks only high byte of min_offender against $FF
;         if high(min_offender) == 0xFF then
;             goto next_start_block                 ; treat as “no offender found”
;         end if
;
;         ; Pointer rewiring (all at header +2..+3: the ‘next’ field)
;         ;   anchor_block.next     = min_offender
;         ;   min_prev.next         = anchor_next
;         ;   min_offender.next     = anchor_next.next
;         ;   anchor_next.next      = min_next
;         tmp_next ← next(anchor_next)              ; preserve before overwrite
;         set_next(anchor_block,  min_offender)
;         set_next(min_prev,      anchor_next)
;         set_next(min_offender,  tmp_next)
;         set_next(anchor_next,   min_next)
;
;         ; Advance the outer pass anchor and repeat
;         next_start_block:
;         anchor_block ← next(anchor_block)
;         if anchor_block == NULL then
;             goto no_more_blocks                   ; exhausted list
;         else
;             goto loop_pass
;         end if
;
;     no_more_blocks:
;         ; Record final tail as the current anchor_block
;         mem_free_tail ← anchor_block
;         return
; end function
;

;===========================================
; mem_bestfit_select pseudo_code
;===========================================
; Select the smallest FREE block with size ≥ mem_req_total, then allocate from it.
; Returns: pointer to the allocated block header, or NULL if no fit.
;
; Inputs (globals / parameters)
; mem_req_total            ; u16 total requested size = payload
; mem_free_cur             ; u16 pointer to first REAL FREE node (not the stub)
; mem_free_stub    ; u16 synthetic predecessor of the head
;
;  Outputs / side effects
;  - On success: mem_free_cur is set to the winning node before allocation.
;  - predecessor is set to the winning node’s predecessor for list surgery.
;  - mem_best_size is set to the winning size (consumed by mem_alloc_from_free).
;  - Calls mem_alloc_from_free(); returns pointer to the allocated header.
;  - On failure: returns NULL (no allocation performed).
;
; function mem_bestfit_select(mem_req_total: u16) -> u16 | NULL
;     ; Initialize “best so far” to sentinel values
;     mem_best_size  ← 0xFFFF
;     mem_best_blk ← NULL
;     predecessor     ← mem_free_stub  ; placeholder; will be replaced on first best
;     scan_prev       ← mem_free_stub  ; rolling predecessor during the walk
;
;     ; Walk the FREE list starting at mem_free_cur
;     while mem_free_cur != NULL do
;         cand_size ← mem_free_cur->size
;
;         ; Fit test: skip if candidate is too small
;         if cand_size < mem_req_total then
;             goto advance
;         end if
;
;         ; Best-fit compare: accept if candidate ≤ current best
;         if cand_size ≤ mem_best_size then
;             mem_best_size  ← cand_size
;             mem_best_blk ← mem_free_cur
;             predecessor     ← scan_prev           ; remember predecessor of the BEST
;         end if
;
;         ; Step to next node
;         advance:
;             scan_prev  ← mem_free_cur
;             mem_free_cur ← mem_free_cur->next  ; NULL if hi/lo are both zero
;     end while
;
;     ; Commit or fail
;     if mem_best_blk == NULL then
;         return NULL
;     end if
;
;     ; Prepare for allocation: operate on the winning node
;     mem_free_cur ← mem_best_blk
;     ; predecessor already set for this winner
;     ; mem_best_size carries the chosen block size
;
;     ; Perform the split/consume; allocator updates links and tail
;     mem_alloc_from_free()   ; uses (mem_free_cur, mem_best_size, mem_req_total/raw_data_size, predecessor)
;
;     ; Return pointer to the allocated block header (the original winning address)
;     return mem_best_blk
; end function
;

;===========================================
; mem_alloc_from_free pseudo_code
;===========================================
;  Inputs
;    mem_free_cur         : u16  (addr of chosen FREE header)
;    mem_best_size  : u16  (size of FREE block = header+payload)
;    mem_req_payload      : u16  (requested payload; will be overwritten on consume)
;    predecessor        : u16  (addr of predecessor header or head-stub; write its +2/+3)
; 
;  Globals
;    mem_free_tail    : u16
; 
; procedure mem_alloc_from_free()
;     remaining := mem_best_size - mem_req_payload       // 16-bit unsigned subtract
; 
;     if remaining < 4 then
;         // ---- CONSUME (no split)
;         predecessor->next := mem_free_cur->next                 
; 
;         // Overwrites the payload request with block size (header+payload)
;         mem_req_payload := mem_best_size
; 
;         // For uniform tail update use the predecessor as the placeholder
;         mem_split_tail := predecessor
;     else
;         // ---- SPLIT (payload-only carve)
;         mem_split_tail := mem_free_cur + mem_req_payload     // NOTE: payload offset (no +4)

;         predecessor->next = mem_split_tail
;         mem_split_tail->next = mem_free_cur->next

;         mem_split_tail->size = remaining
;     end if
; 
;     // ---- Tail maintenance
;     if mem_free_tail == mem_free_cur then
;         mem_free_tail := mem_split_tail
;     end if
; end procedure

;===========================================
; mem_bubble_used_left pseudo_code
;===========================================
;
;  Relocate the USED blocks immediately to the right of the leading FREE block
;  into the FREE region at `mem_dst`, sliding the hole right. When `mem_src`
;  reaches `mem_next_free`, rebuild the FREE header at `mem_dst` and update the head.
;
;  Preconditions:
;    mem_dst = address of the leading FREE block (start of the hole)
;    mem_src      = mem_dst + mem_free_sz
;    mem_next_free  = address of the next FREE block after the run of USED blocks
;
;  Invariant (maintained by the loop):
;    mem_dst < mem_src ≤ mem_next_free
;    (mem_src - mem_dst) == mem_free_sz
;
; procedure mem_bubble_used_left(mem_dst: u16, mem_src: u16, mem_next_free: u16, mem_free_sz: u16)
;     while mem_src < mem_next_free do
;         // ---- Read USED block header at `mem_src`
;         size_lo      ← [mem_src + 0]                 // tail bytes on last page
;         size_hi      ← [mem_src + 1]                 // count of full 256-byte pages
;         mem_used_sz   ← (size_hi << 8) + size_lo    // total bytes: header + payload
;
;         // ---- Copy the entire USED block forward into the FREE region
;         mem_copy_pages(
;             mem_copy_tail = size_lo,
;             mem_copy_page_cnt        = size_hi_pg,
;             mem_read_ptr           = mem_src,       // points at USED block HEADER
;             mem_write_ptr          = mem_dst   // start of FREE region
;         )
;
;         // ---- Advance both pointers by the moved size
;         mem_dst ← mem_dst + mem_used_sz
;         mem_src      ← mem_src      + mem_used_sz
;         // Invariant still holds; (mem_src - mem_dst) == mem_free_sz
;     end while
;
;     // ---- Finalize: rebuild FREE header at new mem_dst, update list head
;     mem_free_head ← mem_dst
;     [mem_dst + 0] ← low(mem_free_sz)   // size lo
;     [mem_dst + 1] ← high(mem_free_sz)  // size hi
;     [mem_dst + 2] ← low(mem_next_free)        // next lo
;     [mem_dst + 3] ← high(mem_next_free)       // next hi
; end procedure
;
;


;===========================================
; mem_compact_leading_free pseudo_code
;===========================================
; Slide the leading FREE block (“the hole”) to higher addresses by moving the
; USED run on its right into the hole, then merge adjacent FREE blocks.
; Repeats until there is no FREE block to the right of the head.
;
; procedure mem_compact_leading_free()
;     loop:
;         // 1) Seed from the FREE-list head
;         mem_free_cur ← mem_free_head
;         if mem_free_cur == NULL then
;             return
;         end if
;
;         // 2) Read head FREE size and compute the first USED on its right
;         mem_free_sz ← read_block_size_bytes(mem_free_cur)   // header + payload bytes
;         mem_src          ← mem_free_cur + mem_free_sz        // first USED block header
;
;         // 3) Fetch the next FREE block after this run
;         next_free ← read_free_next(mem_free_cur)                ; NULL if none
;         if next_free == NULL then
;             return                                           ; nothing to push rightward
;         end if
;
;         // 4) Bubble USED blocks into the hole until mem_src reaches next_free
;         mem_bubble_used_left(
;             mem_dst      = mem_free_cur,        // start of the hole
;             mem_src           = mem_src,            // first USED block header
;             mem_next_free       = next_free,         // following FREE block
;             mem_free_sz  = mem_free_sz    // size of the hole (bytes)
;         )
;
;         // 5) After the hole slides right, adjacent FREEs may now touch — merge them
;         mem_coalesce_right()
;
;         // 6) Re-evaluate the (possibly changed) head and repeat
;         goto loop
; end procedure
;
;

;===========================================
; mem_alloc_from_free (graphical explanation)
;===========================================
; 
; Addresses grow → right
;
; Names:
;   pred             = predecessor node (may be the head stub); we write pred.+2..+3
;   mem_free_cur       = chosen FREE block to satisfy the request
;   old_next         = mem_free_cur.next (the next FREE block in the list)
;   mem_best_size= size(mem_free_cur)   ; 16-bit
;
; ----------------------------------------------------------------------
; initial state (before allocation)
;
; Free blocks linked list:
;
;    pred  ──►  mem_free_cur  ──►  old_next  ──►  ...
;
; Memory:
;    [ FREE: mem_best_size @ mem_free_cur ]  [ ... ]
;
; Goal: carve space for the request from mem_free_cur, then fix links and tail.
;
; ----------------------------------------------------------------------
; A) Split path
;
; Free blocks linked list:
;
; Before 
;   pred ──► mem_free_cur ──► old_next
;            [ FREE: mem_best_size ]
;
; After split (payload carve)
;   pred ──► mem_split_tail ──► old_next
;            [ ALLOCATED ] [ FREE: mem_split_rem ]
;                ^ at mem_free_cur       ^ at mem_free_cur+mem_req_payload
;
;   (Note: trailing FREE header sits after payload bytes, not after header+payload.)
;
; ----------------------------------------------------------------------
; B) Consume path
;
; Free blocks linked list:
;
; Before
;   pred ──► mem_free_cur ──► old_next
;            [ FREE: mem_best_size ]
;
; After consume
;   pred ──► old_next
;   [ ALLOCATED takes entire mem_free_cur region ]
;
; ----------------------------------------------------------------------

;===========================================
; mem_bubble_used_left (graphical explanation)
;===========================================
; Legend
;
; [FREE:F]     free block of constant size F  (header+payload bytes)
; [U# :S#]     used block # with size S#      (header+payload bytes)
; dest         = mem_dst (start of FREE region / “hole”)
; src          = mem_src      (start of first USED block header to the right)
; next_free    = next FREE block after the run of USED blocks
;
; Invariant: dest < src ≤ next_free, and (src - dest) == F at loop entry.

; addresses → increasing
; ┌──────────────────────────────────────────────────────────────────────────┐
; │  dest = A                                                                │
; │  src  = A + F                                                            │
; │                                                                          │
; │  A: [FREE:F] [U1:S1] [U2:S2] … [Uk:Sk] [FREE: G (next_free)]             │
; │       ^dest     ^src                              ^next_free             │
; └──────────────────────────────────────────────────────────────────────────┘
;
; dest := dest + Si
; src  := src  + Si
;
; Loop condition:
;   if src < next_free  → more USED blocks remain → repeat
;   if src ≥ next_free  → run finished → finalize
;  
; One iteration
; -------------
; BEFORE:
;    … [FREE:F] [Ui:Si] [Ui+1:Si+1] … [FREE:G] …
;        ^dest    ^src
;
; AFTER COPY + ADVANCE:
;    … [Ui:Si] [FREE:F] [Ui+1:Si+1] … [FREE:G] …
;                 ^dest     ^src
;
; Note: the hole of size F “slides right” by Si bytes. Distance (src - dest) stays == F.
;
;
; Multiple iterations
; -------------------
; Step 0:
;   A: [FREE:F] [U1:S1] [U2:S2] [U3:S3] [FREE:G]
;         ^dest    ^src                     ^next_free
;
; Step 1 (move U1):
;   A: [U1:S1] [FREE:F] [U2:S2] [U3:S3] [FREE:G]
;                 ^dest    ^src
;
; Step 2 (move U2):
;   A: [U1:S1] [U2:S2] [FREE:F] [U3:S3] [FREE:G]
;                         ^dest    ^src
;
; Step 3 (move U3):
;   A: [U1:S1] [U2:S2] [U3:S3] [FREE:F] [FREE:G]
;                                 ^dest   ^src == next_free
; Done moving: src >= next_free
  