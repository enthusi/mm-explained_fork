;===============================================================================
; Variables
;===============================================================================

;Invariant: no block headers reside in page $00; null pointers are detected via hi-byte==0

;----------------------------------------
; coalesce_free_blocks vars
;----------------------------------------
left_size             = $56c5    	; 16-bit size of current FREE block on the left side; shared temp reused elsewhere
left_block            = $4f      	; zp pointer to current FREE block header (lo @ $4F, hi @ $50)
right_block           = $59      	; pointer to block at (left_block + left_size): immediate right neighbor


;----------------------------------------
; sort_free_blocks vars
;----------------------------------------
curr_block            = $4f      ; node under comparison during scan (alias of free_block storage)
anchor_block              = $51      ; pass anchor / predecessor for this sweep (often the head stub initially)
anchor_next               = $53      ; first candidate after anchor_block for this pass
min_offender          = $55      ; lowest-address node found with addr < anchor_next; $FFFF sentinel = none
min_prev            = $57      ; predecessor of min_offender (for relinking); shares ZP with `source`
curr_prev                = $5746    ; ABS temp: rolling predecessor while scanning
curr_next           = $5748    ; ABS temp: cached anchor_next->next
min_next            = $574a    ; ABS temp: cached min_offender->next


;----------------------------------------
; get_free_block / find_best_free_block
;----------------------------------------
free_block               = $4f      ; zp pointer to current FREE block header (lo @ $4F, hi @ $50)
payload_size             = $fd9c    ; requested payload size in bytes (header NOT included)
best_free_block          = $5813    ; pointer to best-fit FREE block found
best_free_size           = $5815    ; 16-bit size of best-fit block (lo=tail, hi=pages); alias of chosen_block_size
prev_block               = $5817    ; rolling predecessor of the current candidate during the scan
size_needed              = $5819    ; total bytes to carve = payload_size + 4 (header + payload)
candidate_size           = $56c5    ; size of the candidate under inspection (reuses shared temp @ $56C5)


;----------------------------------------
; compact_and_release
;----------------------------------------
saved_request_size       = $58d3    ; preserved copy of payload_size across compaction/retry attempts


;----------------------------------------
; get_next_block
;----------------------------------------
block_ptr                = $4f      ; input: pointer to a block header; routine returns next in X=lo, Y=hi


;----------------------------------------
; allocate_block
;----------------------------------------
chosen_block_size        = $5815    ; size of the selected FREE block (alias of best_free_size)
remaining_free_space     = $56c5    ; remainder after carve: chosen_block_size − size_needed (shared temp)
temp                     = $56c7    ; 8-bit scratch used during pointer transfers
new_free_block           = $55      ; trailing FREE block created on split; in consume path holds predecessor for tail update
predecessor              = $51      ; predecessor pointer; may be the head stub


;----------------------------------------
; move_to_back
;----------------------------------------
source                   = $57      ; pointer to USED block being moved (initially right of the leading FREE block); shares ZP with min_prev
destination              = $4f      ; pointer to destination (start of the leading FREE span)
free_block_size          = $59a2    ; size (bytes) of the current leading FREE block
block_size               = $59a4    ; size (bytes) of the USED block being relocated (header + payload)
next_block               = $59a6    ; pointer to the next FREE block after the leading one
resource_type            = $59a8    ; resource type tag for pointer fix-ups
resource_index           = $59a9    ; resource index/id for pointer fix-ups


;----------------------------------------
; routines / flags / tables
;----------------------------------------
update_rsrc_pointers     = $5a89    ; subroutine: retarget external pointers for a relocated resource (uses type/index)
release_rsrcs_by_priority = $5ae3   ; subroutine: release resources by priority tiers, then retry allocation
reload_snd_rsrc_ptrs     = $fe70    ; flag: non-zero => refresh sound resource pointers after relocation
sound_memory_attrs       = $7951    ; table: per-sound usage attributes, indexed by resource_index


;----------------------------------------
; Head and Tail of free block list
;----------------------------------------
free_list_head_stub      = $ff61    ; synthetic head: its header[+2..+3] holds first_free_block
first_free_block         = $ff63    ; FREE-list head pointer (lo @ $FF63, hi @ $FF64)
last_free_block          = $ff65    ; FREE-list tail pointer (lo @ $FF65, hi @ $FF66)


;----------------------------------------
; mem_copy_memory
;----------------------------------------
page_bytes_to_copy       = $5B      ; final-page byte count (1..255); set to 0 on non-final pages to copy a full 256 via Y-wrap
total_pages              = $5C      ; number of full 256-byte pages to copy; decremented once per completed page
read_ptr                 = $5D      ; source pointer (zp: lo @ $5D, hi @ $5E); hi increments after each full page
write_ptr                = $5F      ; destination pointer (zp: lo @ $5F, hi @ $60); hi increments after each full page


SOUND_RESOURCE = $06

* = $56C8

;===========================================
; Coalesce Adjacent Free Blocks (right-adjacency merge)
;
; Summary:
;   Walks the FREE list, merging pairs of FREE nodes that are laid out
;   back-to-back in memory (right node starts at left + left.size). Merges can
;   cascade at the same left node until its next is non-adjacent. Maintains
;   last_free_block if the right node was the tail.
;
; Arguments:
;   first_free_block    	   	Head pointer of the FREE list. Seeds the walk.
;   last_free_block  	    	Tail pointer of the FREE list. May be updated
;                               when the rightmost participant of a merge was the tail.
;
; Effects / Returns:
;   In-place merges of adjacent nodes (sizes and next links updated).
;   last_free_block updated if the old tail got merged into its left neighbor.
;
;   Clobbers                      A, X, Y; condition flags. Calls get_next_block.
;
; Preconditions / Invariants:
;   - Null = $0000 detected via HI byte == $00 (no headers reside in page $00).
;   - get_next_block returns X=next.lo, Y=next.hi and sets Z from Y; STX/STY do not change flags.
;
; Description:
;
;   For each node:
;     1) Read its 16-bit size into block_size.
;
;     2) Compute right_block = left_block + block_size (the address that would
;        be immediately to the right if a neighbor is touching).
;
;     3) Read next_block (current->next). If next_block == right_block, the two
;        blocks are **contiguous**:
;           • Link-out the right block: first_block.next ← second_block.next.
;           • Grow the left block: size(left) ← size(left) + size(right).
;           • If the right block was last_free_block, move the tail to the coalesced left.
;
;     4) If not contiguous, advance to next_block and continue.
;
;   This pass eliminates internal gaps between free regions, creating larger
;   contiguous spans that improve the likelihood of satisfying big allocations.
;
;===========================================
coalesce_free_blocks:
		; Load the head of the FREE list into ‘left_block’.
		LDA first_free_block
		STA left_block
		LDA first_free_block + 1
		STA left_block + 1

		; Empty list? If first_free_block == $0000, nothing to merge → return.
		BEQ exit_1

compute_adjacent:
		;----------------------------------------
		; Read the current FREE block’s size (on the left side):
		;----------------------------------------
		LDY #$00
		LDA (left_block),Y
		STA left_size
		INY
		LDA (left_block),Y
		STA left_size + 1

		;----------------------------------------
		; Compute the address of the block that would be immediately to the right:
		;
		;   right_block = left_block + left_size
		;----------------------------------------
		CLC
		LDA left_size
		ADC left_block
		STA right_block
		LDA left_size + 1
		ADC left_block + 1
		STA right_block + 1

		; Fetch the pointer to the NEXT free block (into X/Y).
		JSR get_next_block

		; If there is no next block (X:Y == $0000), we’re at the tail → done.
		BNE adjacency_check
		RTS

adjacency_check:
		;----------------------------------------
		; Test right-adjacency:
		;
		;   Are we exactly at left_block + block_size ? (i.e., right_block)
		;   Compare next_block (X/Y) against right_block (lo/hi).
		;----------------------------------------
		CPX right_block
		BNE adjacency_check_2
		CPY right_block + 1
adjacency_check_2:
		; Branch if NOT adjacent (X:Y ≠ right_block).
		BNE next_block_is_not_adjacent

		;----------------------------------------
		; Adjacent detected
		;----------------------------------------
	   
		; Unnecessary code, as X/Y already have the desired values
		; Keeping here for consistency with original
		STX right_block
		STY right_block + 1

		;----------------------------------------
		; Splice out the second block from the FREE list:
		;   first_block.next ← second_block.next
		;
		; Copy header +2..+3 (next pointer) from right_block → left_block.
		;----------------------------------------
		LDY #$02
		LDA (right_block),Y
		STA (left_block),Y
		INY
		LDA (right_block),Y
		STA (left_block),Y

		;----------------------------------------
		; Add the two block sizes to form the new, coalesced size in left_block.
		;
		; Perform 16-bit addition: left_block.size = left_size + right_block.size
		;----------------------------------------
		LDY #$00
		CLC
		LDA left_size
		ADC (right_block),Y          ; add size.lo
		STA (left_block),Y              ; write new size.lo
		INY
		LDA left_size + 1
		ADC (right_block),Y          ; add size.hi (+ carry from low-byte add)
		STA (left_block),Y              ; write new size.hi

		;----------------------------------------
		; If we just removed the TAIL node (right_block == last_free_block),
		; then the newly coalesced first block becomes the new tail.
		;----------------------------------------
		LDA right_block
		CMP last_free_block
		BNE continue
		LDA right_block + 1
		CMP last_free_block + 1
		BNE continue
	   
		LDA left_block
		STA last_free_block
		LDA left_block + 1
		STA last_free_block + 1

continue:
		; Continue scanning: fall through to null-check using hi byte of left_block.
		LDA left_block + 1
		JMP is_block_null

next_block_is_not_adjacent:
		; Not adjacent → advance to the next FREE block and keep checking.
		JSR get_next_block			;Redundant call - kept here to match the original
		STX left_block
		STY left_block + 1

;----------------------------------------
; Loop guard: if left_block != $0000, process next candidate; else exit.
;----------------------------------------
is_block_null:
		BNE compute_adjacent
exit_1:
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
;   free_list_head_stub   Synthetic head; +2..+3 hold first_free_block.
;   first_free_block      Current head pointer (variable).
;   last_free_block       Tail pointer (variable).
;
; Effects / Outputs:
;   List becomes address-ordered. last_free_block updated to final node.
;   first_free_block refreshed from the stub on exit.
;
;   Clobbers                       
;       A, X, Y; condition flags. Uses get_next_block. Internal temps updated.
;
; Invariants:
;   - Null pointer detected via HI byte == $00 (no headers in page $00).
;   - get_next_block: X=next.lo, Y=next.hi, Z from Y; STX/STY do not change flags.
;   - curr_block is the input pointer alias for get_next_block.
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
;          • For each curr_block, read next_block = curr_block->next.
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
;   efficiently by coalesce_free_blocks.
;
;	Note: the sorting is actually non-stable, but it doesn't matter as sorting keys (addresses)
; 	are always unique.
;===========================================
* = $574C
sort_free_blocks:
		;----------------------------------------
		; Initialize sorting by setting ‘anchor_block’ to point at the
		; free block stub. This structure mimics a normal block header,
		; holding the first_free_block pointer at offsets +2..+3.
		;
		; The routine will use anchor_block as the list head during traversal,
		; allowing consistent pointer logic for block headers and the stub.
		;----------------------------------------
		LDA #<free_list_head_stub
		STA anchor_block
		LDA #>free_list_head_stub
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
		LDY #$02
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
		JSR get_next_block
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
		LDY #$02

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
	   
		JSR get_next_block               ; X/Y = anchor_block->next
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
		STA last_free_block
		LDA anchor_block + 1
		STA last_free_block + 1
		RTS
;===========================================
; Get Free Block (Allocator Entry Point)
;
; Summary:
;   Entry point for the allocator.  
;   Given a requested payload size (in X/Y), this routine searches the free
;   list for a suitable memory block, calling the **best-fit** selector to
;   locate and allocate it.
;
; Arguments:
;   X / Y				Requested payload size (low/high).  
;       				Represents only the data portion; 
;						the routine adds 4 bytes for the internal block header.
;
; Vars/State:
;   payload_size		Stores requested payload size from X/Y.  
;   size_needed			Computed as payload_size + 4 (header included).  
;   first_free_block	Head pointer of the free list.  
;   free_block          Updated to point to the current candidate free block.
;
; Returns:
;   X / Y               Pointer to allocated block header if successful.  
;   Z Flag                         
;       				Z=0 on success (block allocated),  
;       				Z=1 if no free block exists (free list empty).
;
; Preconditions / Notes:
;   - Null FREE pointer is detected via HI byte == $00 (no headers in page $00).
;   - BNE uses Z from the last LDA of first_free_block+1; STA does not affect flags.
;   - Best-fit routine consumes size_needed and payload_size and performs the split/consume.
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
;   Otherwise, it chains into the find_best_free_block routine, which
;   performs a full best-fit scan to locate the optimal block and split
;   or consume it as needed.
;===========================================
* = $581B	
get_free_block:
		; Capture payload size for allocator (alias of raw_data_size used by allocate_block).
		STX payload_size
		STY payload_size + 1

		; Compute total bytes required including the 4-byte block header:
		; Total requested bytes (HEADER + payload): size_needed = raw_data_size + 4
		CLC
		LDA payload_size
		ADC #$04
		STA size_needed
		LDA payload_size + 1
		ADC #$00
		STA size_needed + 1

		; Initialize the scan at the head of the FREE list.
		; Check head.hi (null if $00). LDA sets Z, STA does not; BNE uses Z from LDA.
		LDA first_free_block
		STA free_block
		LDA first_free_block + 1
		STA free_block + 1

		; If the FREE list is empty (free_block == $0000), return with Z=1.
		; Otherwise, tail-call into the best-fit selector.
		BNE find_best_free_block
		RTS
;===========================================
; Find Best Free Block (Best-Fit Allocator)
;
; Summary:
;   Walk the FREE list and select the smallest block whose size ≥ size_needed.
;   On success, set free_block to the winner and invoke allocate_block to carve it.
;
; Arguments:
;   size_needed           Requested allocation size in bytes (16-bit)
;
; Vars/State:
;   free_block     			Current FREE node under scan. 
;							On entry, points to the first REAL FREE node (not the stub).
;   free_list_head_stub     Synthetic predecessor for the list head (used to seed prev tracking).
;   best_free_block     	Address of the best candidate found so far (NULL when none).
;   best_free_size     		Size of the best candidate. 
;							Also consumed later by allocate_block as chosen_block_size.
;   prev_block     			Rolling predecessor of free_block during the scan.
;
; Returns:
;
;   Success: X = lo(best_free_block), Y = hi(best_free_block), Z = 0.
;   Failure: Z = 1 (no fit found). X/Y are unspecified unless caller requires zeroing.
;
; Clobbers:
;   A, X, Y; condition flags. Calls allocate_block on success.
;
; Preconditions / Invariants:
;   - Null FREE pointer is detected via HI byte == $00 (no headers reside in page $00).
;   - get_next_block leaves X=next.lo, Y=next.hi and Z derived from Y; BNE uses that Z.
;   - Tie-break: equal-size candidates prefer the later one encountered (non-stable).
;
; Description:
;   Initialize best_free_size = $FFFF and best_free_block = $0000; set prev_block = head stub.
;   For each node:
;     1) Read candidate size. If candidate < size_needed → skip.
;     2) If candidate ≤ best_free_size → record candidate as the new best (size, block, predecessor).
;     3) Advance: prev_block ← free_block; free_block ← free_block->next (via get_next_block).
;   After the scan:
;     - If best_free_block == $0000 → return with Z = 1 (no fit).
;     - Else: free_block ← best_free_block; JSR allocate_block; return X/Y = best_free_block.
;===========================================
find_best_free_block:
		;----------------------------------------
		; Initialize “current best” to: size = $FFFF (max) and block = $0000 (none).
		; Any real fitting candidate will be smaller than $FFFF and replace this.
		;----------------------------------------
		LDA #$FF
		STA best_free_size
		STA best_free_size + 1
		LDA #$00
		STA best_free_block
		LDA #$00
		STA best_free_block + 1

		;----------------------------------------
		; Seed prev_block with the free-list stub head (acts as the predecessor
		; of the first real free block). As we walk the list, prev_block will
		; track the node before ‘free_block’ for bookkeeping/surgery later.
		;----------------------------------------
		LDA #<free_list_head_stub
		STA prev_block
		LDA #>free_list_head_stub
		STA prev_block + 1

check_candidate:
		;----------------------------------------
		; Read the candidate FREE block’s size from its header:
		;   header +0 = size.lo (tail bytes), header +1 = size.hi (pages)
		;----------------------------------------
		LDY #$00
		LDA (free_block),Y
		STA candidate_size
		INY
		LDA (free_block),Y
		STA candidate_size + 1

		;----------------------------------------
		; Fit test: does candidate >= size_needed ?
		;
		; Compute (candidate - size_needed). 
		; If a borrow occurs (BCC), then candidate < size_needed → it cannot fit → skip to next block.
		;----------------------------------------
		SEC
		LDA candidate_size
		SBC size_needed
		LDA candidate_size + 1
		SBC size_needed + 1
		BCC move_to_next_block            ; too small → examine next candidate

		;----------------------------------------
		; DATA FITS — check whether this candidate improves the current best-fit.
		;
		; Compare (best_free_size - candidate_size):
		;
		;   If borrow (BCC) → candidate is LARGER than current best → not better.
		;   If no borrow    → candidate is <= current best         → accept later.
		;----------------------------------------
		SEC
		LDA best_free_size
		SBC candidate_size
		LDA best_free_size + 1
		SBC candidate_size + 1
		BCC move_to_next_block            ; candidate > best → skip, keep current best

		;----------------------------------------
		; NEW BEST-FIT FOUND
		;
		; This call to "get_next_block" is not really needed, as X/Y will get overwritten
		; again by the following "get_next_block" call later on - it doesn't harm either
		; Keeping it here as it's present in the original code
		;----------------------------------------
		JSR get_next_block		

		; Record the new minimum (best) size.
		LDA candidate_size
		STA best_free_size
		LDA candidate_size + 1
		STA best_free_size + 1

		; Record the address of the new best-fit block.
		LDA free_block
		STA best_free_block
		LDA free_block + 1
		STA best_free_block + 1

		; Record the address of the predecessor
		LDA prev_block
		STA predecessor
		LDA prev_block + 1
		STA predecessor + 1

move_to_next_block:
		;----------------------------------------
		; Remember the current candidate as 'prev_block' (predecessor tracking).
		; Useful for later list surgery, though not directly used in this pass.
		;----------------------------------------
		LDA free_block
		STA prev_block
		LDA free_block + 1
		STA prev_block + 1

		; Advance the scan to the next FREE block in the list.
		JSR get_next_block
		STX free_block
		STY free_block + 1

		; Continue scanning while free_block != $0000.
		BNE check_candidate

		;----------------------------------------
		; END OF SCAN — commit best-fit (if any) and allocate
		; Load the winner back into free_block so allocate_block operates on it.
		;----------------------------------------
		LDA best_free_block
		STA free_block
		LDA best_free_block + 1
		STA free_block + 1

		; If best_free_block == $0000 → no fitting block was found → return with Z=1.
		BNE free_block_found
		RTS

free_block_found:
		; Consume/split the chosen FREE block to satisfy the request.
		JSR allocate_block

		; Return the allocated block pointer to caller:
		;   X = lo(best_free_block), Y = hi(best_free_block), Z=0 (success).
		LDX best_free_block
		LDY best_free_block + 1
		RTS
;===========================================
; Compact and Retry Allocation with Resource Release
;
; Summary:
;   Attempts to allocate a memory block of the requested size by first compacting
;   the FREE-list (pushing and merging free blocks toward the top of memory). If
;   that still fails, the routine progressively releases lower-priority resources
;   and retries the allocation until a block is obtained or no more resources can
;   be freed.
;
; Arguments:
;   payload_size           	16-bit requested payload size in bytes.
;                                  Temporarily overwritten with $FFFF to mark
;                                  “allocation in progress” state.
;   saved_request_size                   		Local copy of requested size used across retries.
;
; Uses:
;   move_free_blocks_to_back       Defragments the free list by pushing free blocks
;                                  toward higher memory and coalescing contiguous
;                                  ones.
;   get_free_block                 Attempts to find a free block large enough for
;                                  ‘saved_request_size’. Returns with Z=0 (BNE) on success.
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
;   size into ‘saved_request_size’ and “poisons” payload_size ($FFFF) to signal an in-progress
;   allocation attempt. It then invokes move_free_blocks_to_back to compact memory
;   so that smaller fragments merge into larger contiguous spaces. If get_free_block
;   fails to find a suitable block, the routine calls release_rsrcs_by_priority to
;   free noncritical assets (e.g., cached sound or graphics data) and loops back to
;   retry allocation. Once a valid free block is obtained, control exits normally.
;===========================================
* = $58D5

compact_and_release:
		;----------------------------------------
		; Preserve caller’s requested payload size and force the allocator path:
		;   saved_request_size ← payload_size
		;   payload_size ← $FFFF  (sentinel to indicate “recompute/use saved size”)
		; Rationale: some downstream paths consult payload_size; poisoning it avoids
		; accidental reuse while we compact and possibly free resources.
		;----------------------------------------
		LDA payload_size
		STA saved_request_size
		LDA payload_size + 1
		STA saved_request_size + 1
		LDA #$FF
		STA payload_size
		LDA #$FF
		STA payload_size + 1

		;----------------------------------------
		; Phase 1: try to make a large contiguous FREE region by pushing leading
		; free space toward higher addresses and merging neighbors.
		;----------------------------------------
		JSR move_free_blocks_to_back

		;----------------------------------------
		; Phase 2: attempt allocation with the preserved requested saved_request_size.
		; On success, get_free_block returns with Z=0 → branch to exit.
		;----------------------------------------
		LDX saved_request_size
		LDY saved_request_size + 1
		JSR get_free_block
		BNE free_block_found_2

		;----------------------------------------
		; Phase 3 (fallback): still no block — release resources by priority and retry.
		; release_rsrcs_by_priority returns Z=0 when something was released; loop if so.
		;----------------------------------------
		JSR release_rsrcs_by_priority
		BNE compact_and_release

free_block_found_2:
		RTS
;===========================================
; Get pointer to the next block
;
; Summary:
;   Reads the 16-bit “next” pointer from a block header and returns it in X/Y.
;
; Arguments:
;   block_ptr                 Pointer to current block header
;
; Returns:
;   X/Y                       Next block pointer (lo in X, hi in Y). 
;								If the stored pointer is $0000, X/Y will be $00/$00.
;
; Notes:
;   Block header layout (little-endian):
;     +0..+1 : size (bytes, includes header)
;     +2..+3 : next pointer (16-bit)
;===========================================
* = $58FF
get_next_block:
		; Y = 2 → address the “next” field (low byte) in the block header
		LDY #$02
		; Load low byte of next pointer → X
		LDA (block_ptr),Y
		TAX
		; Load high byte of next pointer → Y
		INY
		LDA (block_ptr),Y
		TAY
		RTS
;===========================================
; Allocate from a chosen FREE block (split or consume)
;
; Summary:
;   Carves the requested block (HEADER + payload) from the selected FREE node.
;   If the true remainder is ≥ 4 bytes (enough for a FREE header), splits and
;   creates a trailing FREE node; otherwise, consumes the whole FREE node.
;   Updates predecessor.next and keeps last_free_block correct.
;
; Arguments:
;   free_block              Pointer to the chosen FREE block header.
;   chosen_block_size       16-bit size of the chosen FREE block
;							Note: this is an alias of best_free_size.
;   payload_size           16-bit requested payload size (header NOT included).
;   predecessor		        Predecessor pointer (may be the head stub); we write its +2/+3.
;
; Returns / Updates:
;   predecessor.next        → new trailing FREE (split) or original next (consume).
;   new_free_block          → trailing FREE (split) or predecessor (consume) for uniform tail update.
;   remaining_free_space    → (chosen_block_size − size_needed), split path only.
;   last_free_block         → updated if the chosen block had been the tail.
;   Clobbers                A, X, Y; flags.
;
; Notes:
;   size_needed = payload_size + 4 (payload + HEADER).
;   Using a split threshold of ≥ 4 permits header-only FREE nodes
;
; Description:
;   Computes remaining_free_space = chosen_block_size - payload_size
;
;   If remaining_free_space ≥ 4, the routine SPLITS:
;     • new_free_block = free_block + payload_size
;     • predecessor.next ← new_free_block
;     • new_free_block.next  ← free_block.next
;     • new_free_block.size  ← remaining_free_space
;
;   Otherwise it CONSUMES the entire FREE block:
;     • predecessor.next ← free_block.next
;     • payload_size ← free_block_size  (caller receives whole block)
;     • new_free_block ← predecessor (simplifies tail update)
;
;   Finally, if the chosen block was the list tail, last_free_block is updated to the
;   trailing FREE block (or stub) so the tail pointer remains correct.
;===========================================

* = $5909
allocate_block:
		;----------------------------------------
		; Compute how much of the chosen FREE block will remain after carving out
		; the requested payload:
		;
		;   remaining_free_space = chosen_block_size - payload_size
		;
		;----------------------------------------
		SEC
		LDA chosen_block_size
		SBC payload_size
		STA remaining_free_space
		LDA chosen_block_size + 1
		SBC payload_size + 1
		STA remaining_free_space + 1

		;----------------------------------------
		; Is there enough space left to create a NEW free block header?
		;
		; We need at least 4 bytes (size lo/hi + next lo/hi). 
		; Perform a 16-bit compare: (remaining_free_space - 4). 
		; If borrow occurs (BCC), there is NOT enough space to split → consume the whole block.
		; Note: C=1 & result==0 means remainder==4 ⇒ header-only FREE node
		;----------------------------------------
		SEC
		LDA remaining_free_space
		SBC #$04                      ; subtract header size (low)
		LDA remaining_free_space + 1
		SBC #$00                      ; subtract header size (high)
		BCC consume_whole_block     ; C=0 → remaining_free_space < 4

		;--------------------------------
		; Split case
		;
		; Carve payload from the front of the chosen FREE block and create a NEW trailing 
		; FREE block with the leftover space.
		;
		;   new_free_block = free_block + payload_size
		;----------------------------------------
		CLC
		LDA free_block
		ADC payload_size
		STA new_free_block
		LDA free_block + 1
		ADC payload_size + 1
		STA new_free_block + 1

		;----------------------------------------
		; Link the FREE list to the new trailing block
		;
		; Update the predecessor's next to point at new_free_block.
		;----------------------------------------
		LDY #$02
		LDA new_free_block
		STA (predecessor),Y     ; write next.lo
		INY
		LDA new_free_block + 1
		STA (predecessor),Y     ; write next.hi


		;----------------------------------------
		; Chain the new trailing FREE block into the list
		;
		; Read free_block->next into X/Y, then store into new_free_block header (+2..+3).
		;
		;   new_free_block.next = free_block.next
		;----------------------------------------
		JSR get_next_block
		STY temp                         ; stash hi byte temporarily
		LDY #$02
		TXA
		STA (new_free_block),Y           ; header[2] = next.lo
		INY
		LDA temp
		STA (new_free_block),Y           ; header[3] = next.hi

		;----------------------------------------
		; Initialize the size of the new trailing FREE block
		;
		;   new_free_block.size = remaining_free_space  (header +0..+1)
		;----------------------------------------
		LDY #$00
		LDA remaining_free_space
		STA (new_free_block),Y           ; header[0] = size.lo
		INY
		LDA remaining_free_space + 1
		STA (new_free_block),Y           ; header[1] = size.hi
		JMP tail_adjust_check  ; update tail pointer if needed

consume_whole_block:
		;----------------------------------------
		; Not enough room to hold a new FREE header (remaining_free_space < 4):
		; consume the ENTIRE chosen FREE block for this allocation.
		;
		; Splice the chosen block out of the FREE list:
		;   predecessor.next = free_block.next
		;----------------------------------------
		JSR get_next_block
		STY temp
		LDY #$02
		TXA
		STA (predecessor),Y      ; write next.lo
		INY
		LDA temp
		STA (predecessor),Y      ; write next.hi

		;----------------------------------------
		; Caller will use the whole block: set requested size to block size.
		;
		; (payload_size now equals chosen_block_size)
		;----------------------------------------
		LDA chosen_block_size
		STA payload_size
		LDA chosen_block_size + 1
		STA payload_size + 1

		;----------------------------------------
		; No trailing FREE block is created in this case.
		;
		; Set new_free_block to the list “table” placeholder so downstream
		; tail-adjustment logic can treat both paths uniformly.
		;----------------------------------------
		LDA predecessor
		STA new_free_block
		LDA predecessor + 1
		STA new_free_block + 1

tail_adjust_check:
		;----------------------------------------
		; Was the CHOSEN free block also the TAIL of the free list?
		;
		; If last_free_block == free_block, the block we just split/consumed was the tail.
		;----------------------------------------
		LDA last_free_block
		CMP free_block
		BNE tail_adjust_check_2
		LDA last_free_block + 1
		CMP free_block + 1
tail_adjust_check_2:
		;If it wasn't, nothing to adjust, exit
		BNE exit

		;----------------------------------------
		; Yes: update the tail to the new trailing FREE block produced by this op.
		;
		; In the split path, new_free_block = (free_block + payload_size).
		; In the “no split” path, new_free_block was set to the table placeholder
		; so downstream logic treats both cases uniformly.
		;----------------------------------------
		LDA new_free_block
		STA last_free_block                  
		LDA new_free_block + 1
		STA last_free_block + 1
exit:
		RTS
;===========================================
; Move all leading free space toward higher addresses
;
; Summary:
;   Iteratively “bubbles” used blocks to the right over the first free block,
;   pushing the free space toward higher addresses. After each bubble, merges
;   adjacent free blocks. Repeats until no more right-adjacent used blocks
;   remain or the free list is exhausted.
;
; Preconditions / Invariants:
;   - 'free_block' points to the header of the current leading FREE block.
;   - ZP aliasing: 'free_block' and 'destination' share storage (destination=free head).
;   - Null pointers are detected via hi byte == 0 (no headers live in page $00).
;
; Arguments / Inputs:
;   first_free_block          Head pointer of the FREE list (lo/hi).
;   FREE header @ free_block  +0..+1 = size (lo=tail bytes, hi=pages)
;                             +2..+3 = next pointer
; Returns:
;   first_free_block          May be updated by 'move_to_back' to new FREE head.
;   Transient state           source (free_block + size), next_block (free->next),
;                             free_block_size. Aliases with vars used by callees.
; Calls:
;   move_to_back              Slides USED run into the FREE region; updates head.
;   coalesce_free_blocks      Merges adjacent FREE blocks.
;
; Description:
;
;   Loads the head of the free list into ‘free_block’, reads its size into
;   ‘free_block_size’, and computes ‘adjacent_block = free_block + free_block_size’.
;
;   It then fetches free_block->next:
;     • If no next free block exists, the routine returns (nothing to push).
;     • Otherwise, it:
;         1) Calls move_to_back to relocate any used blocks that sit immediately
;            to the right of the leading free block, thereby sliding the free
;            region upward in memory (toward higher addresses).
;         2) Calls coalesce_free_blocks to merge contiguous free neighbors that
;            may have become adjacent after the relocation.
;         3) Loops back to re-evaluate the (possibly updated) head until no more
;            movement is needed.
;
;   The end result is a more compacted free region near the top, improving the
;   chance of satisfying larger allocations.
;===========================================
* = $59AA

move_free_blocks_to_back:
		;----------------------------------------
		; Load the head of the free list into the working pointer ‘free_block’.
		; This is the leading FREE region we try to push “to the back” (higher addrs).
		;----------------------------------------
		LDA first_free_block
		STA free_block
		LDA first_free_block + 1
		STA free_block + 1

		;----------------------------------------
		; Read the FREE block’s size from its header:
		;   header +0 = size lo (tail bytes on last page)
		;   header +1 = size hi (full 256-byte pages)
		;----------------------------------------
		LDY #$00
		LDA (free_block),Y
		STA free_block_size
		INY
		LDA (free_block),Y
		STA free_block_size + 1


		;----------------------------------------
		; Compute the address of the block immediately to the right:
		;   source = free_block + free_block_size
		; (size is stored as tail-bytes in lo and page count in hi)
		;----------------------------------------
		CLC
		LDA free_block
		ADC free_block_size
		STA source
		LDA free_block + 1
		ADC free_block_size + 1
		STA source + 1

		;----------------------------------------
		; Read the singly-linked list pointer:
		;
		; get_next_block → X=next.lo, Y=next.hi, Z set from Y (TAY inside).
		; BNE means Y≠0 → non-null next (relies on “no headers in page $00” invariant).
		;----------------------------------------
		JSR get_next_block
		BNE next_block_present
		RTS


next_block_present:
		STX next_block
		STY next_block + 1

		; Relocate any USED blocks that sit immediately to the right of the
		; leading FREE block so that the FREE region moves “back” (to higher
		; addresses). This may repeat internally until the next FREE block
		; is adjacent to ‘destination’.
	   
		; 'free_block' (alias of 'destination') already points at the FREE head.
		; 'source' = free_block + size points at the first USED block to the right.
		; 'next_block' is the next FREE block after the USED run.
		;
		; move_to_back will:
		;   - Copy each USED block (header+payload) from source → destination,
		;   - Advance pointers until source >= next_block,
		;   - Rebuild the FREE header at the new destination and update first_free_block.	   
		JSR move_to_back

		; The relocated FREE head may now be adjacent to the next FREE block; merge them.
		JSR coalesce_free_blocks

		; Re-evaluate the (possibly changed) head; repeat until there is no next FREE block.		   
		JMP move_free_blocks_to_back
;===========================================
; Move the first free block “to the back” (bubble used blocks left)
;
; Summary:
;   Copies each USED block immediately to the right of the leading FREE block
;   into the FREE region at 'destination' (which is the leading FREE block),
;   thereby shifting USED blocks toward lower addresses and pushing FREE space
;   toward higher addresses.
;
;   Repeats while source < next_block. When source reaches next_block,
;   rebuilds the FREE header at the new 'destination' and updates the list head.
;
;   After each move, external pointers are updated; a sound reload is requested
;   only if the moved block is an in-use sound.
;
; Preconditions:
;   destination = address of the leading FREE block (start of the hole)
;   source      = destination + free_block_size (first USED block to the right)
;   next_block  = pointer to the next FREE block after the current run of USED blocks
;
; Arguments (pointers/vars):
;   source            USED block being moved (points to its HEADER)
;   destination       FREE region to receive the move (points to where HEADER will be written)
;   next_block        NEXT FREE block after the current run of USED blocks
;   free_block_size   Original size (bytes) of the leading FREE block
;   block_size        size of the used block being moved.
;   resource_type / resource_index
;                                 Metadata from the used block; passed to
;                                 update_rsrc_pointers to fix external references.
;
;   sound_memory_attrs            Table consulted when resource_type indicates “sound” to
;                                 decide whether to set reload_snd_rsrc_ptrs.
; Updates:
;   source                        Advanced past the moved used block(s).
;   destination                   Advanced to the end of the moved region.
;   first_free_block              Updated to the rebuilt free block at the new destination.
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
;     3) Call mem_copy_memory to copy the entire block from ‘source’ → ‘destination’.
;     4) Call update_rsrc_pointers(metadata) so external references follow the move.
;     5) Advance both pointers (source, destination) by the moved size and loop while more used blocks remain
;        before ‘next_block’.
;
;   Once the next free block is adjacent, the routine updates first_free_block and writes
;   the free block header (size + next pointer) at ‘destination’, completing the compaction step.
;
; Notes:
;   - Copy direction is forward and safe for overlap because destination < source.
;   - Sound resources: if in use, set 'reload_snd_rsrc_ptrs' during the move; it is consumed
;     by 'update_rsrc_pointers' and then cleared before the next iteration.
;===========================================
* = $59E5

; Conventions: source=USED block (header addr), destination=FREE hole (header addr); next_block=next FREE block
move_to_back:
		;----------------------------------------
		; Read size of the USED block from its header at 'source':
		;
		;   +0 = bytes-on-last-page (0..255)
		;   +1 = full 256-byte pages
		;
		; size is stored little-endian across these two fields.
		; (size is the full block: header + payload)
		;----------------------------------------
		LDY #$00
		LDA (source),Y
		STA block_size             ; low byte: tail bytes
		INY
		LDA (source),Y
		STA block_size + 1         ; high byte: page count
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
		LDA (source),Y
		STA resource_type
		PHA                        ; save A=resource_type
		INY
		LDA (source),Y
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
		; Prepare argument block for mem_copy_memory:
		;
		;   page_bytes_to_copy = tail bytes on the last page      	(low  byte of block_size)
		;   total_pages = full 256-byte pages to copy      			(high byte of block_size)
		;   read_ptr = source pointer (lo/hi)       					(address of the USED block HEADER)
		;   write_ptr = destination pointer (lo/hi)  				(address of FREE space to fill)
		;
		; Copy length equals block_size (header + payload).
		;
		; After setup, mem_copy_memory will copy (total_pages * 256 + page_bytes_to_copy) bytes
		; from [read_ptr] to [write_ptr], handling page spans appropriately.
		;----------------------------------------
		LDA block_size
		STA page_bytes_to_copy
		LDA block_size + 1
		STA total_pages

		LDA source
		STA read_ptr
		LDA source + 1
		STA read_ptr + 1

		LDA destination
		STA write_ptr
		LDA destination + 1
		STA write_ptr + 1

		; Perform the relocation copy (source → destination).
		; Copies (pages*256 + tail_bytes) as prepared above.
		JSR mem_copy_memory

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
		;   destination ← destination + block_size
		;   source      ← source      + block_size
		;
		; Invariant preserved: destination < source.
		; destination now points to the start of the remaining FREE space (immediately after the moved USED block) 
		; source points to the block that originally followed the moved USED block.
		;----------------------------------------
		; destination += block_size
		CLC                               
		LDA destination
		ADC block_size                   
		STA destination
		LDA destination + 1
		ADC block_size + 1                
		STA destination + 1

		; source += block_size
		CLC
		LDA source
		ADC block_size                   
		STA source
		LDA source + 1
		ADC block_size +1                
		STA source + 1


		;----------------------------------------
		; The next free block can either:
		; 	-reside immediately after the moved free block
		; 	-reside higher, with at least one block in between
		;
		; If it resides higher, then there's at least one used block in the middle and we can move it to the front.
		; If it resides immediately after, we now have two consecutive free blocks, which can be coalesced.
		;
		; Check if there are still USED blocks between our current 'source'
		; and the next FREE block ('next_block'). 
		;
		; We perform a 16-bit compare:
		;
		;   Compute (source - next_block) with SEC/SBC across low, then high.
		;   If result < 0 → borrow → C=0 → next_block > source → keep moving.
		;   If result ≥ 0 → C=1 → we've reached/passed next_block → stop moving.
		;----------------------------------------
		SEC
		LDA source
		SBC next_block            ; low-byte: source - next_block
		LDA source + 1
		SBC next_block + 1            ; high-byte with borrow propagation
		; C clear → next_block > source → more used blocks remain to relocate, loop
		BCC move_to_back

		;----------------------------------------
		; Adjacent FREE block now follows the relocated region:
		;
		;   Update the free-list HEAD to point at 'destination',
		;   then rebuild the FREE block header at 'destination':
		;     +0..+1 = size  (free_block_size)
		;     +2..+3 = next  (next_block)
		;----------------------------------------
		; FREE header written at new destination; list head points here.
		LDA destination
		STA first_free_block
		LDA destination + 1
		STA first_free_block + 1

		LDY #$00
		LDA free_block_size
		STA (destination),Y              ; header[0] = size lo
		INY
		LDA free_block_size + 1
		STA (destination),Y              ; header[1] = size hi
		INY
		LDA next_block
		STA (destination),Y              ; header[2] = next lo
		INY
		LDA next_block + 1
		STA (destination),Y              ; header[3] = next hi
		RTS
;===========================================
; Copy memory: (total_pages * 256 + page_bytes_to_copy) bytes
;
; Arguments:
;   page_bytes_to_copy   ($5b)     Bytes to copy on the final (last) page (0..255)
;   total_pages          ($5c)     Number of full 256-byte pages to copy
;   read_ptr             ($5d/$5e) Source pointer (lo/hi)
;   write_ptr            ($5f/$60) Destination pointer (lo/hi)
;
; Returns:
;   Copies (total_pages * 256 + page_bytes_to_copy) bytes from read_ptr to write_ptr.
;   On exit, read_ptr/write_ptr have been advanced by the total copied size.
;
; Notes:
;   - If total_pages == 0 and page_bytes_to_copy == 0 → nothing to do.
;   - The routine copies page-by-page; for non-final pages it copies 256 bytes
;     by temporarily setting page_bytes_to_copy = $00 (meaning 256 via CPY logic).
;===========================================
* = $5C13
mem_copy_memory:
		; Save final-page byte count on the stack
		LDA page_bytes_to_copy
		PHA
		LDX total_pages

next_page:
		BEQ last_page                   ; X==0 → process the final partial page

		; Not the last page: copy a full 256-byte page
		LDA #$00
		STA page_bytes_to_copy
		JMP copy_page

; It's the last page: restore the true final-byte count
last_page:
		PLA
		STA page_bytes_to_copy
		BNE copy_page                   ; 0 means nothing left to copy → RTS
		RTS

copy_page:
		LDY #$00
; Copy from read_ptr to write_ptr until Y reaches page_bytes_to_copy
copy_loop:
		LDA (read_ptr),Y
		STA (write_ptr),Y
		INY
		CPY page_bytes_to_copy
		BNE copy_loop

		; Finished this page: advance source/dest to next page, decrement page count
		INC read_ptr + 1
		INC write_ptr + 1
		DEX
		BPL next_page
		RTS

;===========================================
; sort_free_blocks pseudo_code
;===========================================
; // Reorders the FREE list by ascending address using an anchor-and-swap sweep.
;
; function sort_free_blocks():
;     ; anchor_block ← head stub (free_list_head_stub)
;     anchor_block ← free_list_head_stub
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
;         last_free_block ← anchor_block
;         return
; end function
;

;===========================================
; find_best_free_block pseudo_code
;===========================================
; Select the smallest FREE block with size ≥ size_needed, then allocate from it.
; Returns: pointer to the allocated block header, or NULL if no fit.
;
; Inputs (globals / parameters)
; size_needed            ; u16 total requested size = payload
; free_block             ; u16 pointer to first REAL FREE node (not the stub)
; free_list_head_stub    ; u16 synthetic predecessor of the head
;
;  Outputs / side effects
;  - On success: free_block is set to the winning node before allocation.
;  - predecessor is set to the winning node’s predecessor for list surgery.
;  - best_free_size is set to the winning size (consumed by allocate_block).
;  - Calls allocate_block(); returns pointer to the allocated header.
;  - On failure: returns NULL (no allocation performed).
;
; function find_best_free_block(size_needed: u16) -> u16 | NULL
;     ; Initialize “best so far” to sentinel values
;     best_free_size  ← 0xFFFF
;     best_free_block ← NULL
;     predecessor     ← free_list_head_stub  ; placeholder; will be replaced on first best
;     scan_prev       ← free_list_head_stub  ; rolling predecessor during the walk
;
;     ; Walk the FREE list starting at free_block
;     while free_block != NULL do
;         cand_size ← free_block->size
;
;         ; Fit test: skip if candidate is too small
;         if cand_size < size_needed then
;             goto advance
;         end if
;
;         ; Best-fit compare: accept if candidate ≤ current best
;         if cand_size ≤ best_free_size then
;             best_free_size  ← cand_size
;             best_free_block ← free_block
;             predecessor     ← scan_prev           ; remember predecessor of the BEST
;         end if
;
;         ; Step to next node
;         advance:
;             scan_prev  ← free_block
;             free_block ← free_block->next  ; NULL if hi/lo are both zero
;     end while
;
;     ; Commit or fail
;     if best_free_block == NULL then
;         return NULL
;     end if
;
;     ; Prepare for allocation: operate on the winning node
;     free_block ← best_free_block
;     ; predecessor already set for this winner
;     ; best_free_size carries the chosen block size
;
;     ; Perform the split/consume; allocator updates links and tail
;     allocate_block()   ; uses (free_block, best_free_size, size_needed/raw_data_size, predecessor)
;
;     ; Return pointer to the allocated block header (the original winning address)
;     return best_free_block
; end function
;

;===========================================
; allocate_block pseudo_code
;===========================================
;  Inputs
;    free_block         : u16  (addr of chosen FREE header)
;    chosen_block_size  : u16  (size of FREE block = header+payload)
;    payload_size      : u16  (requested payload; will be overwritten on consume)
;    predecessor        : u16  (addr of predecessor header or head-stub; write its +2/+3)
; 
;  Globals
;    last_free_block    : u16
; 
; procedure allocate_block()
;     remaining := chosen_block_size - payload_size       // 16-bit unsigned subtract
; 
;     if remaining < 4 then
;         // ---- CONSUME (no split)
;         predecessor->next := free_block->next                 
; 
;         // Overwrites the payload request with block size (header+payload)
;         payload_size := chosen_block_size
; 
;         // For uniform tail update use the predecessor as the placeholder
;         new_free_block := predecessor
;     else
;         // ---- SPLIT (payload-only carve)
;         new_free_block := free_block + payload_size     // NOTE: payload offset (no +4)

;         predecessor->next = new_free_block
;         new_free_block->next = free_block->next

;         new_free_block->size = remaining
;     end if
; 
;     // ---- Tail maintenance
;     if last_free_block == free_block then
;         last_free_block := new_free_block
;     end if
; end procedure

;===========================================
; move_to_back pseudo_code
;===========================================
;
;  Relocate the USED blocks immediately to the right of the leading FREE block
;  into the FREE region at `destination`, sliding the hole right. When `source`
;  reaches `next_block`, rebuild the FREE header at `destination` and update the head.
;
;  Preconditions:
;    destination = address of the leading FREE block (start of the hole)
;    source      = destination + free_block_size
;    next_block  = address of the next FREE block after the run of USED blocks
;
;  Invariant (maintained by the loop):
;    destination < source ≤ next_block
;    (source - destination) == free_block_size
;
; procedure move_to_back(destination: u16, source: u16, next_block: u16, free_block_size: u16)
;     while source < next_block do
;         // ---- Read USED block header at `source`
;         size_lo      ← [source + 0]                 // tail bytes on last page
;         size_hi      ← [source + 1]                 // count of full 256-byte pages
;         block_size   ← (size_hi << 8) + size_lo    // total bytes: header + payload
;
;         // ---- Copy the entire USED block forward into the FREE region
;         mem_copy_memory(
;             page_bytes_to_copy = size_lo,
;             total_pages        = size_hi_pg,
;             read_ptr           = source,       // points at USED block HEADER
;             write_ptr          = destination   // start of FREE region
;         )
;
;         // ---- Advance both pointers by the moved size
;         destination ← destination + block_size
;         source      ← source      + block_size
;         // Invariant still holds; (source - destination) == free_block_size
;     end while
;
;     // ---- Finalize: rebuild FREE header at new destination, update list head
;     first_free_block ← destination
;     [destination + 0] ← low(free_block_size)   // size lo
;     [destination + 1] ← high(free_block_size)  // size hi
;     [destination + 2] ← low(next_block)        // next lo
;     [destination + 3] ← high(next_block)       // next hi
; end procedure
;
;


;===========================================
; move_free_blocks_to_back pseudo_code
;===========================================
; Slide the leading FREE block (“the hole”) to higher addresses by moving the
; USED run on its right into the hole, then merge adjacent FREE blocks.
; Repeats until there is no FREE block to the right of the head.
;
; procedure move_free_blocks_to_back()
;     loop:
;         // 1) Seed from the FREE-list head
;         free_block ← first_free_block
;         if free_block == NULL then
;             return
;         end if
;
;         // 2) Read head FREE size and compute the first USED on its right
;         free_block_size ← read_block_size_bytes(free_block)   // header + payload bytes
;         source          ← free_block + free_block_size        // first USED block header
;
;         // 3) Fetch the next FREE block after this run
;         next_free ← read_free_next(free_block)                ; NULL if none
;         if next_free == NULL then
;             return                                           ; nothing to push rightward
;         end if
;
;         // 4) Bubble USED blocks into the hole until source reaches next_free
;         move_to_back(
;             destination      = free_block,        // start of the hole
;             source           = source,            // first USED block header
;             next_block       = next_free,         // following FREE block
;             free_block_size  = free_block_size    // size of the hole (bytes)
;         )
;
;         // 5) After the hole slides right, adjacent FREEs may now touch — merge them
;         coalesce_free_blocks()
;
;         // 6) Re-evaluate the (possibly changed) head and repeat
;         goto loop
; end procedure
;
;

;===========================================
; allocate_block (graphical explanation)
;===========================================
; 
; Addresses grow → right
;
; Names:
;   pred             = predecessor node (may be the head stub); we write pred.+2..+3
;   free_block       = chosen FREE block to satisfy the request
;   old_next         = free_block.next (the next FREE block in the list)
;   chosen_block_size= size(free_block)   ; 16-bit
;
; ----------------------------------------------------------------------
; initial state (before allocation)
;
; Free blocks linked list:
;
;    pred  ──►  free_block  ──►  old_next  ──►  ...
;
; Memory:
;    [ FREE: chosen_block_size @ free_block ]  [ ... ]
;
; Goal: carve space for the request from free_block, then fix links and tail.
;
; ----------------------------------------------------------------------
; A) Split path
;
; Free blocks linked list:
;
; Before 
;   pred ──► free_block ──► old_next
;            [ FREE: chosen_block_size ]
;
; After split (payload carve)
;   pred ──► new_free_block ──► old_next
;            [ ALLOCATED ] [ FREE: remaining_free_space ]
;                ^ at free_block       ^ at free_block+payload_size
;
;   (Note: trailing FREE header sits after payload bytes, not after header+payload.)
;
; ----------------------------------------------------------------------
; B) Consume path
;
; Free blocks linked list:
;
; Before
;   pred ──► free_block ──► old_next
;            [ FREE: chosen_block_size ]
;
; After consume
;   pred ──► old_next
;   [ ALLOCATED takes entire free_block region ]
;
; ----------------------------------------------------------------------

;===========================================
; move_to_back (graphical explanation)
;===========================================
; Legend
;
; [FREE:F]     free block of constant size F  (header+payload bytes)
; [U# :S#]     used block # with size S#      (header+payload bytes)
; dest         = destination (start of FREE region / “hole”)
; src          = source      (start of first USED block header to the right)
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
  