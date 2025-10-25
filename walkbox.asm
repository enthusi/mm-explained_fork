#importonce
#import "globals.inc"
#import "constants.inc"

// -----------------------------------------------------------------------------
// Walkbox data pointers and indices
// -----------------------------------------------------------------------------
.label box_ptr                   = $0017  // ZP: walkbox table base (lo at $17, hi at $18)
.label walkbox_room              = $FE74  // Active room id for walkbox tables
.label wbox_idx                  = $FC3F  // Scratch: original walkbox index under test

// -----------------------------------------------------------------------------
// Box geometry (current candidate)
// -----------------------------------------------------------------------------
.label box_left_edge             = $1A64  // Box left edge (pixels)
.label box_right_edge            = $1A65  // Box right edge (pixels)
.label box_top_edge              = $1A66  // Box top edge (pixels)
.label box_bottom_edge           = $1A67  // Box bottom edge (pixels)

// -----------------------------------------------------------------------------
// Nearest-point computation (to test point)
// -----------------------------------------------------------------------------
.label nearest_x                 = $1A60  // Nearest X on or inside box
.label nearest_y                 = $1A61  // Nearest Y on or inside box
.label scaled_x_dist             = $1A5C  // Scaled |dx| = 2*|x - nearest_x|
.label scaled_y_dist             = $1A5D  // Scaled |dy| = |y - nearest_y|/4

// -----------------------------------------------------------------------------
// Test point and selection state
// -----------------------------------------------------------------------------
.label test_x                    = $FE72  // Input test X (pixels)
.label test_y                    = $FE73  // Input test Y (pixels)
.label box_index                = $1A5B  // Running box index (starts at -1)
.label min_distance             = $1A5E  // Best scaled distance so far
.label min_nearest_x            = $1A62  // Nearest X for best candidate
.label min_nearest_y            = $1A63  // Nearest Y for best candidate
.label closest_box_index        = $FDAC  // Output: best box index

// -----------------------------------------------------------------------------
// Packed list iteration (generic element list)
// -----------------------------------------------------------------------------
.label elem_list_ptr             = $007C  // ZP: packed list base pointer
.label element_count             = $FC3D  // Running count of elements visited
.label target_index              = $CAD3  // Requested element index (0-based)
.label target_ofs                = $CAD7  // Output: byte offset to element start

// -----------------------------------------------------------------------------
// Adjacency list traversal
// -----------------------------------------------------------------------------
.label adj_list_ptr              = $007C  // ZP: adjacency list base pointer
.label dest_box_index            = $CAD1  // Target box index to find
.label list_start_ofs            = $CAD7  // Start offset into adjacency sublist

// -----------------------------------------------------------------------------
// Current/active box id (aliases share $CAD3)
// -----------------------------------------------------------------------------
.label current_box               = $CAD3  // Box id under test

// -----------------------------------------------------------------------------
// Depth-first search state
// -----------------------------------------------------------------------------
.label active_depth_level        = $CAD8  // DFS/BFS depth (1-based when active)
.label actor_discovered_boxes    = $CAD9  // Discovered boxes by depth (index 1..N)
.label scan_limit_index          = $FC3D  // Exclusive upper bound for scans (depth+1)

// -----------------------------------------------------------------------------
// Adjacency scan offsets and resume indices
// -----------------------------------------------------------------------------
.label target_offset             = $CAD7  // Base index for selected adjacency sublist
.label list_offset               = $FC3B  // Working Y-offset into (adj_list_ptr),Y
.label resume_index              = $CAD2  // Last-consumed index for this sublist (resume anchor)
.label resume_index_tbl          = $CB29  // Per-depth resume index table, indexed by X

// -----------------------------------------------------------------------------
// Destination and routine return
// -----------------------------------------------------------------------------
.label destination_box_id        = $CAD1  // Destination walkbox id
.label return_value              = $CAD0  // Routine result code

// -----------------------------------------------------------------------------
// Search depth and advance/backtrack results
// -----------------------------------------------------------------------------
.const MAX_SEARCH_DEPTH                = $10    // Hard cap for DFS depth (1..$10)
.const MIN_SEARCH_DEPTH                = $01    // Minimum valid depth
.const ADVANCE_OK                      = $00    // increment_search_depth: advanced
.const ADVANCE_BLOCKED                 = $FF    // increment_search_depth: at cap
.const BACKTRACK_BLOCKED               = $FF    // decrement_search_depth: at min

// -----------------------------------------------------------------------------
// Per-depth scan bounds and list conventions
// -----------------------------------------------------------------------------
.const BOX_LIST_FIRST                  = $01    // First valid depth index (0 unused)
.const ELEM_TERM                       = $FF    // Terminator byte in adjacency sublists
.const ADJ_PTR_OFFSET_BIAS             = $02    // Bias: box_ptr-2 holds relative ofs

// -----------------------------------------------------------------------------
// Walkbox table layout and markers
// -----------------------------------------------------------------------------
.const OFS_WALKBOX                     = $15    // Room header byte: 8-bit ofs to walkboxes
.const WALKBOX_STRIDE                  = $05    // Bytes per walkbox record
.const WALKBOX_SENTINEL                = $FF    // End-of-table marker in walkbox lists
.const WALKBOX_NOT_RESIDENT            = $FF    // Flag: walkbox data not in memory

// -----------------------------------------------------------------------------
// Geometry tests and distance scaling
// -----------------------------------------------------------------------------
.const INSIDE_BOX                      = $00    // Point classified inside/on box
.const OUTSIDE_BOX                     = $FF    // Point classified outside box
.const INIT_MAX_DIST                   = $FF    // Initial “worst” scaled distance

// -----------------------------------------------------------------------------
// Destination adjacency checks
// -----------------------------------------------------------------------------
.const DEST_IS_CONTAINED               = $00    // Destination found in adjacency set
.const DEST_IS_NOT_CONTAINED           = $FF    // Destination absent from adjacency set

// -----------------------------------------------------------------------------
// Adjacency exploration results (per neighbor)
// -----------------------------------------------------------------------------
.const ONE_UNKNOWN              	   = $00    // At least one neighbor unseen
.const ALL_KNOWN                	   = $FF    // All neighbors already seen

// -----------------------------------------------------------------------------
// Seen/Not-seen test results for a single box
// -----------------------------------------------------------------------------
.const BOX_SEEN                        = $00    // Box present in discovered set
.const BOX_NOT_SEEN                    = $FF    // Box not present in discovered set

// -----------------------------------------------------------------------------
// Path building outcomes and return sentinel
// -----------------------------------------------------------------------------
.const RESULT_PATH_OK                  = $00    // Path found and copied
.const RESULT_PATH_GRAPH_EXHAUSTED     = $01    // Search exhausted via backtrack block
.const RESULT_PATH_MAX_DEPTH           = $02    // Search stopped at depth cap
.const INIT_RETURN_SENTINEL            = $FF    // Initial value for return_value

// -----------------------------------------------------------------------------
// Per-actor path buffer sizing
// -----------------------------------------------------------------------------
.const ACTOR_PATH_BUF_SIZE             = $10    // Bytes per actor path segment


/*
================================================================================
build_walkbox_path
================================================================================

Summary
    Constructs a path of connected walkboxes between an actor’s current and
    destination boxes using a bounded depth-first search with backtracking.

Arguments
    current_box_for_actor
    destination_box_for_actor

Global Inputs
    actor                            current actor index
    box_ptr                          pointer to actor’s walkbox boundary data

Global Outputs
    adj_list_ptr                     pointer to adjacency list for current actor
    current_box                    current walkbox being processed
    destination_box_id               goal walkbox id
    active_depth_level               1-based recursion depth
    return_value                     result code for search outcome

Returns
    .A  	RESULT_PATH_OK ($00)              path found and copied
			RESULT_PATH_GRAPH_EXHAUSTED ($01) full graph explored, no path
			RESULT_PATH_MAX_DEPTH ($02)       search capped by max depth

Description
    - Retrieves the actor’s walkbox data and computes the base address of the
      adjacency list from a relative offset.
    - Resets search depth and state variables, then loads the actor’s current
      and destination box ids.
    - Executes iterative depth-first traversal:
        • Calls increment_search_depth (bounded to MAX_SEARCH_DEPTH).
        • If destination box is adjacent → write_actor_path_buffer and return PATH_OK.
        • Otherwise checks adjacency lists:
              - If all adjacents known → attempt decrement_search_depth.
                    • If backtrack fails → PATH_GRAPH_EXHAUSTED.
                    • Else continue at previous depth.
              - If not all known → continue scanning.
        • Terminates with PATH_MAX_DEPTH if depth limit reached.
    - The loop continues while .A == $00; each nonzero result exits.
================================================================================
*/
* = $18EB
build_walkbox_path:
        // ------------------------------------------------------------
        // Initialize adjacency pointer for the actor.
        // ------------------------------------------------------------
        ldx     actor
        jsr     get_walkboxes_for_costume

        // Point adj_list_ptr two bytes before walkbox list start
        lda     >box_ptr
        sta     >adj_list_ptr
        lda     <box_ptr
        sec
        sbc     #ADJ_PTR_OFFSET_BIAS
        sta     <adj_list_ptr
        bcs     adjust_adj_list_ptr
        dec     >adj_list_ptr

adjust_adj_list_ptr:
        // ------------------------------------------------------------
        // Read relative offset and apply it to adj_list_ptr.
        // ------------------------------------------------------------
        ldy     #$00
        clc
        adc     (adj_list_ptr),y
        sta     <adj_list_ptr
        bcc     init_search_state
        inc     >adj_list_ptr

init_search_state:
        // ------------------------------------------------------------
        // Reset search state and load actor-specific boxes.
        // ------------------------------------------------------------
        lda     #$00
        sta     active_depth_level
        lda     #INIT_RETURN_SENTINEL
        sta     return_value

        lda     current_box_for_actor,x
        sta     current_box

        lda     destination_box_for_actor,x
        sta     destination_box_id

search_loop:
        // ------------------------------------------------------------
        // Advance search depth; exit if max depth reached.
        // ------------------------------------------------------------
        jsr     increment_search_depth
        cmp     #ADVANCE_OK
        bne     hit_max_depth_exit

        // ------------------------------------------------------------
        // Check adjacency for destination; if found → copy path.
        // ------------------------------------------------------------
        jsr     adjacency_contains_destination
        cmp     #DEST_IS_CONTAINED
        bne     explore_or_backtrack
        jsr     write_actor_path_buffer
        lda     #RESULT_PATH_OK
        sta     return_value
        rts

explore_or_backtrack:
        // ------------------------------------------------------------
        // Process adjacency exploration or backtrack if exhausted.
        // ------------------------------------------------------------
        jsr     all_adjacent_boxes_known
        cmp     #ALL_KNOWN
        bne     loop_if_continue

        // All adjacents known → attempt backtrack.
        jsr     decrement_search_depth
        cmp     #BACKTRACK_BLOCKED
        bne     backtrack_continue

        // Backtrack failed → search exhausted.
        lda     #RESULT_PATH_GRAPH_EXHAUSTED
        sta     return_value
        rts

backtrack_continue:
        lda     #$00

loop_if_continue:
        // ------------------------------------------------------------
        // Force re-evaluation of adjacents or proceed scanning.
        // ------------------------------------------------------------
        beq     explore_or_backtrack

        lda     #$00
        jmp     test_continue_and_dispatch

hit_max_depth_exit:
        // ------------------------------------------------------------
        // Reached max depth limit → terminate search.
        // ------------------------------------------------------------
        lda     #RESULT_PATH_MAX_DEPTH
        sta     return_value

test_continue_and_dispatch:
        // ------------------------------------------------------------
        // Continue loop if A == $00, else return.
        // ------------------------------------------------------------
        beq     search_loop
        rts
/*
================================================================================
  increment_search_depth
================================================================================
Summary
	Advance the search depth by one step up to the maximum allowed level.

Arguments
	None

Global Inputs
	active_depth_level            1-based current search depth
	current_box                 box id to record at the new depth

Global Outputs
	active_depth_level            incremented on success
	actor_discovered_boxes[...]   updated at index = active_depth_level
	resume_index_tbl[...]         primed to $FF at index = active_depth_level

Returns
	.A  		ADVANCE_BLOCKED ($FF)     depth already at MAX_SEARCH_DEPTH
				ADVANCE_OK ($00)          depth incremented and state updated

Description
	- If active_depth_level equals MAX_SEARCH_DEPTH, do not modify state and
	return ADVANCE_BLOCKED.
	- Otherwise:
		• Increment active_depth_level.
		• Store current_box to actor_discovered_boxes[active_depth_level].
		• Set resume_index_tbl[active_depth_level] := $FF so the next scan
		starts at index $00.
		• Return ADVANCE_OK.
================================================================================
*/

* = $195F
increment_search_depth:
        // ------------------------------------------------------------
        // Guard: if depth at maximum → return ADVANCE_BLOCKED.
        // ------------------------------------------------------------
        lda     active_depth_level
        cmp     #MAX_SEARCH_DEPTH
        bne     perform_depth_increment
        lda     #ADVANCE_BLOCKED
        rts

perform_depth_increment:
        // ------------------------------------------------------------
        // Increase depth, record box at this depth, prime resume index.
        // ------------------------------------------------------------
        inc     active_depth_level
        ldy     active_depth_level

        // Record the box discovered at this depth.
        lda     current_box
        sta     actor_discovered_boxes,y

        // Prime resume index to $FF so first INC yields $00 on next scan.
        lda     #$FF
        sta     resume_index_tbl,y

        // Success.
        lda     #ADVANCE_OK
        rts

/*
================================================================================
decrement_search_depth
================================================================================

Summary
    Perform one backtrack step on the search depth with underflow guard.

Arguments
    None

Global Inputs
    active_depth_level            ; 1-based current search depth

Global Outputs
    active_depth_level            ; decremented on success

Returns
    .A  BACKTRACK_BLOCKED ($FF)   ; if depth == MIN_SEARCH_DEPTH

Description
    - If active_depth_level equals MIN_SEARCH_DEPTH, do not modify state and
      return BACKTRACK_BLOCKED.
    - Otherwise decrement active_depth_level by one.
================================================================================
*/
* = $197D
decrement_search_depth:
        // ------------------------------------------------------------
        // Prevent underflow: if depth == 1 → return BACKTRACK_BLOCKED.
        // ------------------------------------------------------------
        lda     active_depth_level
        cmp     #MIN_SEARCH_DEPTH
        bne     perform_depth_decrement
        lda     #BACKTRACK_BLOCKED
        rts

perform_depth_decrement:
        // ------------------------------------------------------------
        // Decrement active_depth_level and return new depth.
        // ------------------------------------------------------------
        dec     active_depth_level
        rts
/*
================================================================================
setup_adjacency_list
================================================================================

Summary
    Initialize per-depth adjacency scanning so a graph/search walk can resume
    exactly where it left off. Selects the target element (neighbor list) for
    this depth and computes its start offset, honoring prior progress.

Arguments
    active_depth_level    Depth index into per-depth state.

Returns
    resume_index   Resume index within the selected element.
    list_start_ofs                  Byte offset to that element’s start.

Global Inputs
    actor_discovered_boxes[]   Per-depth selected element index.
    resume_index_tbl[]  Per-depth resume index within that element.

Global Outputs
    current_box           Element index to locate in the list.
    resume_index   Starting index inside that element.
    list_start_ofs                  Computed byte offset to element start.

Description
    • Look up the element index chosen for this depth and store it as
      current_box.
    • Load the last processed index for this depth and use it as the starting
      position when resuming inside that element (supports backtracking).
    • Call get_list_element_offset to translate the element index into a byte
      offset within the $FF-delimited list and store it in list_start_ofs.
================================================================================
*/
* = $198B
setup_adjacency_list:
        // Select element index for this depth and record resume index
        ldy     active_depth_level                // Y := depth
        lda     actor_discovered_boxes,y
        sta     current_box                // element to locate in list

        lda     resume_index_tbl,y
        sta     resume_index        // resume position within element

        // Translate element index → byte offset (writes list_start_ofs)
        jsr     get_list_element_offset
        rts

/*
================================================================================
  adjacency_contains_destination
================================================================================
Summary
	Check whether the destination box index appears in the current box’s
	adjacency sublist. Linear scan to sentinel.

Arguments
	(via setup_adjacency_list)
	list_start_ofs      Start offset within the adjacency sublist.

Returns
	A                   #DEST_IS_CONTAINED if found;
						#DEST_IS_NOT_CONTAINED if not found (hit terminator).
	Y                   Offset advanced to the byte after the last examined one.

Global Inputs
	adj_list_ptr        ZP pointer to adjacency list base.
	dest_box_index      Target box index to search.
	list_start_ofs      Sublist start offset (produced earlier).

Description
	• Initialize list context, then set Y := list_start_ofs.
	• Loop: read candidate = (adj_list_ptr),Y.
		– If candidate == dest_box_index → return #DEST_IS_CONTAINED.
		– Else increment Y; if candidate == ELEM_TERM → return #DEST_IS_NOT_CONTAINED.
		– Else continue.
================================================================================
*/
* = $199E
adjacency_contains_destination:
        // Initialize adjacency context and load actor (X unused here)
        jsr     setup_adjacency_list
        ldx     actor

        // Y := start of this box’s adjacency sublist
        ldy     list_start_ofs

scan_candidate:
        // Read candidate adjacent box index; compare to destination
        lda     (adj_list_ptr),y
        cmp     dest_box_index
        bne     advance_and_check_eol

        // Match → success
        lda     #DEST_IS_CONTAINED
        rts

advance_and_check_eol:
        iny                                 // advance to next candidate
        cmp     #ELEM_TERM                  // was previous byte the terminator?
        bne     scan_candidate              // no → keep scanning

        // Reached end-of-list without a match → A is #$FF from last CMP, A = DEST_IS_NOT_CONTAINED
        rts

/*
================================================================================
all_adjacent_boxes_known
================================================================================

Summary
    Determines whether every adjacent box for the current list element has
    already been discovered by the actor.

Arguments
    element_index                  selects which adjacency sublist to scan

Returns
    .A  ONE_UNKNOWN ($00)   at least one adjacent box not yet known
        ALL_KNOWN ($FF)     all adjacent boxes already known

Description
    - Calls setup_adjacency_list with X = element_index to load adj_list_ptr
      and sublist_base_ofs.
    - Resumes scanning at iter_ofs := sublist_base_ofs + resume_index.
    - For each box id:
        * Reads (adj_list_ptr),Y.
        * If byte == ELEM_TERM → return ALL_KNOWN.
        * Otherwise sets current_box and calls is_box_seen_by_actor.
          Returns ONE_UNKNOWN on first unseen box, else continues.
    - Updates resume_index_tbl,X before each fetch to maintain continuation
      state across calls.
================================================================================
*/

* = $19B6
all_adjacent_boxes_known:
        // ------------------------------------------------------------
        // Prepare adjacency context for X = active_depth_level.
        // Loads adj_list_ptr and target_offset for element X.
        // ------------------------------------------------------------
        ldx     active_depth_level
        jsr     setup_adjacency_list

        // ------------------------------------------------------------
        // Compute list_offset := target_offset + resume_index; then Y := list_offset.
        // This resumes scanning at the element’s previously consumed position.
        // ------------------------------------------------------------
        lda     resume_index
        clc
        adc     target_offset
        tay
        sty     list_offset

scan_next_adjacent:
        // ------------------------------------------------------------
        // Advance resume index for this element, then step list_offset.
        // ------------------------------------------------------------
        inc     resume_index_tbl,x
        inc     list_offset

        // ------------------------------------------------------------
        // Read next adjacent box id from (adj_list_ptr),Y.
        // ------------------------------------------------------------
        ldy     list_offset
        lda     (adj_list_ptr),y

        // ------------------------------------------------------------
        // End-of-list sentinel? If yes → all known.
        // ------------------------------------------------------------
        cmp     #ELEM_TERM
        bne     check_adjacent_seen_state
        lda     #ALL_KNOWN
        rts

check_adjacent_seen_state:
        // ------------------------------------------------------------
        // Test this adjacent: if unseen → return ONE_UNKNOWN.
        // Otherwise continue scanning.
        // ------------------------------------------------------------
        sta     current_box
        jsr     is_box_seen_by_actor          // returns A=$00 if seen, A=$FF if not
        cmp     #BOX_NOT_SEEN                 // Z=1 when unseen
        bne     loop_if_box_seen           // A=$00 (seen) → keep scanning
        lda     #ONE_UNKNOWN
        rts

loop_if_box_seen:
        // ------------------------------------------------------------
        // Seen → loop.
        // ------------------------------------------------------------
        bne     scan_next_adjacent            // always taken here (Z=0 after CMP)
        rts                                   // safety (not reached)

/*
================================================================================
is_box_seen_by_actor
================================================================================

Summary
    Linear membership test for a box id within the actor’s discovered list.

Arguments
    current_box					box id under test
    active_depth_level		current DFS/BFS depth for this actor

Returns
    .A  		BOX_SEEN ($00) if found
				BOX_NOT_SEEN ($FF) if not found

Description
    - Sets scan limit to active_depth_level + 1 (exclusive upper bound).
    - Scans actor_discovered_boxes from index 1 up to and including depth.
    - Early exit on first match; otherwise returns BOX_NOT_SEEN.
================================================================================
*/
* = $19EB
is_box_seen_by_actor:
        // ------------------------------------------------------------
        // Initialize scan limit: scan_limit_index := active_depth_level + 1
        // Y will serve as the loop counter (1..active_depth_level).
        // ------------------------------------------------------------
        ldy     active_depth_level
        iny
        sty     scan_limit_index

        // ------------------------------------------------------------
        // Start linear scan at index 1; index 0 is intentionally skipped.
        // ------------------------------------------------------------
        ldy     #BOX_LIST_FIRST

scan_discovered_boxes:
        // ------------------------------------------------------------
        // Compare list[Y] with current_box; match → return BOX_SEEN.
        // ------------------------------------------------------------
        lda     actor_discovered_boxes,y
        cmp     current_box
        bne     step_and_check_limit
        lda     #BOX_SEEN
        rts

step_and_check_limit:
        // ------------------------------------------------------------
        // Advance Y; if Y != scan_limit_index keep scanning. Otherwise not found.
        // ------------------------------------------------------------
        iny
        cpy     scan_limit_index
        bne     scan_discovered_boxes

        // ------------------------------------------------------------
        // No match in 1..active_depth_level → return BOX_NOT_SEEN.
        // ------------------------------------------------------------
        lda     #BOX_NOT_SEEN
        rts

/*
================================================================================
write_actor_path_buffer
================================================================================

Summary
    Serialize the found walkbox route for the given actor into the per-actor
    path buffer, starting with the destination box and then the reverse chain
    from the current depth down to depth 1.

Arguments
    actor

Global Inputs
    active_depth_level             1-based depth of the discovered route
    destination_box_id             goal walkbox id
    actor_discovered_boxes[...]    source stack indexed by depth (1..N)
    ACTOR_PATH_BUF_SIZE            stride for each actor’s output segment
    MIN_SEARCH_DEPTH               minimal valid depth (usually $01)

Global Outputs
    actor_search_depth[...]        per-actor path length in steps (depth-1)
    actor_discovered_boxes[...]    per-actor output segment filled with
                                   {destination, depth..2 entries}

Returns
    None

Description
    - Compute Y := 16 * (actor + 1) to address the actor’s output segment.
    - Store path length as (active_depth_level - 1) into actor_search_depth[X].
    - Write destination_box_id at the start of the actor’s segment.
    - If depth == MIN_SEARCH_DEPTH, exit.
    - Otherwise copy entries from actor_discovered_boxes[depth..2] into the
      actor’s segment in reverse order to reconstruct the path.
================================================================================
*/
* = $1A08
write_actor_path_buffer:
        // ------------------------------------------------------------
        // Y := 16 * (actor + 1) → select actor’s output segment base.
        // ------------------------------------------------------------
        ldx     actor
        txa
        asl     
        asl     
        asl     
        asl     
        clc
        adc     #ACTOR_PATH_BUF_SIZE
        tay

        // ------------------------------------------------------------
        // Save path length in steps: depth - 1.
        // ------------------------------------------------------------
        lda     active_depth_level
        sta     actor_search_depth,x
        dec     actor_search_depth,x

        // ------------------------------------------------------------
        // Write destination box as first element of the actor’s path.
        // ------------------------------------------------------------
        lda     destination_box_id
        sta     actor_discovered_boxes,y
        iny

        // ------------------------------------------------------------
        // If depth == 1, we are already at destination → done.
        // ------------------------------------------------------------
        ldx     active_depth_level
        cpx     #MIN_SEARCH_DEPTH
        beq     end_copy_path

copy_path_reverse_loop:
        // ------------------------------------------------------------
        // Copy source stack in reverse (depth..2) into actor buffer.
        // ------------------------------------------------------------
        lda     actor_discovered_boxes,x
        sta     actor_discovered_boxes,y
        iny
        dex
        cpx     #MIN_SEARCH_DEPTH
        bne     copy_path_reverse_loop

end_copy_path:
        rts

/*
================================================================================
 get_list_element_offset
================================================================================
Summary
	Return the byte offset (from list start) of the target element in a packed
	list where each element is terminated by $FF.

Arguments
	target_index        0-based index of the desired element.

Returns
	target_ofs          Offset (0..255) to the first byte of the target element.

Global Inputs
	elem_list_ptr       ZP pointer to list base (lo=$7C, hi=$7D).
	
Global Outputs
	target_ofs

Description
	• If target_index == 0, the offset is 0.
	• Otherwise scan bytes starting at (elem_list_ptr)+Y. Each $FF ends one
	element and increments element_count. When element_count equals
	target_index, Y points at the start of the target element; store Y.
	• No bounds check is performed for indexes past the end of the list.
================================================================================
*/
* = $1A37
get_list_element_offset:
        // ------------------------------------------------------------
        // Initialize: Y=0 scan offset; element_count=0
        // ------------------------------------------------------------
        ldy     #$00
        sty     element_count

        // ------------------------------------------------------------
        // Fast path: index 0 → offset 0
        // ------------------------------------------------------------
        lda     target_index
        bne     scan_next_byte
        sta     target_ofs                 // A holds 0 here
        rts

scan_next_byte:
        // ------------------------------------------------------------
        // Read next byte; advance Y
        // ------------------------------------------------------------
        lda     (elem_list_ptr),y
        iny

        // ------------------------------------------------------------
        // If byte != terminator, still inside current element → keep scanning
        // If byte == terminator, we just ended one element → bump count
        // ------------------------------------------------------------
        cmp     #ELEM_TERM
        bne     scan_next_byte

        inc     element_count                 // ended one element

        // ------------------------------------------------------------
        // Reached the requested element count?
        //   When equal: Y now points to start of target element
        // ------------------------------------------------------------
        lda     element_count
        cmp     target_index
        bne     scan_next_byte

        sty     target_ofs                 // commit offset of target element
        rts
/*
================================================================================
  get_closest_box
================================================================================
Summary
	Iterate through all walkboxes of a room and determine which one is closest
	to a given coordinate pair (box_check_x, box_check_y). Updates global
	variables with the index of the nearest box, the nearest point on its edges,
	and the minimum computed distance. Returns failure if no boxes are present.

Arguments
	walkbox_room        Room index whose walkboxes are being scanned.
	box_check_x         X coordinate of the test point.
	box_check_y         Y coordinate of the test point.

Returns
	On success:
		closest_box_index   → index of nearest walkbox
		min_nearest_x       → nearest X on or inside that box
		min_nearest_y       → nearest Y on or inside that box
		min_distance        → anisotropic distance value
		A                   → undefined (not guaranteed to contain min_distance)
	On failure (no room data loaded):
		closest_box_index   = #$FF
		A                   = #$FF

Global Inputs
	box_ptr                 Zero-page pointer to the active walkbox table.
	walkbox_room, box_check_x, box_check_y.
	get_walkboxes_for_room  (initializes box_ptr for this room).
	get_distance_to_box     (computes distance and nearest point).

Description
	• Calls get_walkboxes_for_room to retrieve the base pointer to the room’s
	walkbox list; aborts if the room is not resident.
	• Each walkbox entry consists of 6 bytes:
	[left, right, top, bottom, byte4, byte5]
	and the list terminates with #$FF at the next entry’s start.
	• For every box:
		– Loads its edges into box_left_edge..box_bottom_edge.
		– Calls get_distance_to_box to compute both the distance (A) and
		the nearest point (nearest_x, nearest_y).
		– If the distance is less than min_distance, records this box as
	the new closest.
	• Continues until a #$FF sentinel is encountered.
================================================================================
*/
* = $1AC1
get_closest_box:
        // ------------------------------------------------------------
        // Acquire walkbox table pointer for the requested room
        //   A = #$FF on failure (room not resident)
        // ------------------------------------------------------------
        jsr     get_walkboxes_for_room
        cmp     #WALKBOX_SENTINEL
        bne     init_scan
        sta     closest_box_index            // propagate #$FF to output index
        rts

init_scan:
        // ------------------------------------------------------------
        // Initialize search state
        //   min_distance := $FF (worst), box_index := $FF then pre-increment
        // ------------------------------------------------------------
        lda     #INIT_MAX_DIST
        sta     min_distance
        lda     #$ff
        sta     box_index

        ldy     #$00                         // Y := table offset
scan_next_box:
        inc     box_index                    // next logical record index

        // ------------------------------------------------------------
        // End-of-list check: leading byte == $FF means no more records
        // ------------------------------------------------------------
        lda     (box_ptr),y
        cmp     #WALKBOX_SENTINEL
        bne     load_current_box
        rts

load_current_box:
        // ------------------------------------------------------------
        // Load current box edges: [left, right, top, bottom]
        // Then skip the next two to land at next record start
        // ------------------------------------------------------------
        lda     (box_ptr),y
        sta     box_left_edge
        iny
        lda     (box_ptr),y
        sta     box_right_edge
        iny
        lda     (box_ptr),y
        sta     box_top_edge
        iny
        lda     (box_ptr),y
        sta     box_bottom_edge
        iny
        iny                                   // skip two extra bytes (stride = 6)

        // ------------------------------------------------------------
        // Compute distance to this box and the nearest point on it
        //   On return: A = distance, nearest_x/nearest_y filled
        // ------------------------------------------------------------
        jsr     get_distance_to_box

        // ------------------------------------------------------------
        // If this distance is strictly smaller, capture as new minimum
        // ------------------------------------------------------------
        cmp     min_distance
        bcs     continue_scan                      // A >= min_distance → keep current best

        sta     min_distance
        lda     box_index
        sta     closest_box_index
        lda     nearest_x
        sta     min_nearest_x
        lda     nearest_y
        sta     min_nearest_y

continue_scan:
        jmp     scan_next_box

/*
================================================================================
  get_distance_to_box
================================================================================
Summary
	Compute an anisotropic distance from a test point to an axis-aligned box and
	record the nearest point on or in that box. Inside the box returns zero.

Arguments
	test_x                  Test X coordinate.
	test_y                  Test Y coordinate.
	box_left_edge           Box left edge.
	box_right_edge          Box right edge.
	box_top_edge            Box top edge.
	box_bottom_edge         Box bottom edge.

Returns
	A                       Distance byte.
	nearest_x               Nearest X on/inside the box.
	nearest_y               Nearest Y on/inside the box.

Vars/State
	scaled_x_dist           2*|test_x − nearest_x|.
	scaled_y_dist           |test_y − nearest_y|/4.

Description
	• Clamp test_x to [box_left_edge, box_right_edge] → nearest_x.
	• Clamp test_y to [box_top_edge, box_bottom_edge] → nearest_y.
	• Compute |dx| and |dy|, then scale: X by ×2, Y by ÷4.
	• If scaled_x_dist > scaled_y_dist: return scaled_x_dist/2 + scaled_y_dist;
	otherwise return scaled_y_dist/2 + scaled_x_dist.
================================================================================

Distance approximation formula

This distance formulation is a fast integer approximation of a Euclidean metric.

A true Euclidean distance requires a square root and multiplications:
	d = sqrt(dx² + dy²) which are costly on 6502 hardware.

Instead, this routine blends the absolute deltas with a fixed ratio:


if |dx| > |dy|:
    d ≈ |dx|/2 + |dy|
else:
    d ≈ |dy|/2 + |dx|


After axis scaling (×2 for X, ÷4 for Y) this produces:
	d ≈ 0.5*max(|dx|,|dy|) + min(|dx|,|dy|)

This hybrid between the Manhattan (L1) and Chebyshev (L∞) metrics forms an
'octagonal' distance contour, closely approximating circular Euclidean ranges.

The blend 0.5*max + min is a midpoint between L1 and L∞ and an inexpensive way 
to get contours roughly circular without using square roots or multiplications.
==================================================================
*/

* = $1B1B
get_distance_to_box:
        // ------------------------------------------------------------
        // Resolve nearest X coordinate relative to box edges
        //   • If test_x > right  → clamp to right edge.
        //   • If test_x < left   → clamp to left edge.
        //   • Otherwise          → point is inside horizontally.
        // ------------------------------------------------------------
        lda     test_x                        // load X coordinate to test
        cmp     box_right_edge                // compare to right edge
        bcc     x_within_or_left_of_right     // if ≤ right, continue testing
        beq     x_within_or_left_of_right
        lda     box_right_edge                // test_x > right → clamp to right
        jmp     commit_nearest_x              // done with X side

x_within_or_left_of_right:
        cmp     box_left_edge                 // compare to left edge
        bcs     commit_nearest_x              // if ≥ left → inside; keep original
        lda     box_left_edge                 // test_x < left → clamp to left

commit_nearest_x:
        sta     nearest_x                     // store resolved nearest X

        // ------------------------------------------------------------
        // Resolve nearest Y relative to box edges
        //   If test_y > bottom → clamp to bottom
        //   If test_y < top    → clamp to top
        //   Else                → inside vertically, keep test_y
        // ------------------------------------------------------------
        lda     test_y                        // A := test_y
        cmp     box_bottom_edge               // A ? bottom
        bcc     y_within_or_above_bottom      // A <  bottom → maybe above; check top
        beq     y_within_or_above_bottom      // A == bottom → inside so far
        lda     box_bottom_edge               // A > bottom → clamp to bottom
        jmp     commit_nearest_y

y_within_or_above_bottom:
        cmp     box_top_edge                  // A ? top
        bcs     commit_nearest_y              // A >= top → inside vertically
        lda     box_top_edge                  // A <  top → clamp to top

commit_nearest_y:
        sta     nearest_y                     // nearest_y := resolved Y

		// ------------------------------------------------------------
        // Compute |dx| then store scaled_x_dist = 2*|dx|
		// ------------------------------------------------------------
        lda     test_x                        // A := test_x
        sec                                   // prepare subtraction
        sbc     nearest_x                     // A := test_x - nearest_x
        bcs     set_x_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two’s complement negate
        clc
        adc     #$01

set_x_distance:
        asl                                   // A := 2*|dx|
        sta     scaled_x_dist                 // save scaled X distance

		// ------------------------------------------------------------
		// Compute |dy| then scale to scaled_y_dist = |dy|/4
		// ------------------------------------------------------------
        lda     test_y                        // A := test_y
        sec                                   // prepare subtraction
        sbc     nearest_y                     // A := test_y - nearest_y
        bcs     set_y_distance                // if A ≥ 0 keep magnitude
        eor     #$ff                          // two’s complement negate
        clc
        adc     #$01

set_y_distance:
        lsr                                   // ÷2
        lsr                                   // ÷4 total
        sta     scaled_y_dist                  // save scaled Y distance

		// ------------------------------------------------------------
		// Combine scaled components
		//   If scaled_x_dist > scaled_y_dist:
		//       return scaled_x_dist/2 + scaled_y_dist
		//   else:
		//       return scaled_y_dist/2 + scaled_x_dist
		// ------------------------------------------------------------
        cmp     scaled_x_dist                 // compare scaled_y_dist (A) vs scaled_x_dist
        bcc     y_dist_le_x_dist              // if y < x  → use y/2 + x
        beq     y_dist_le_x_dist              // if y == x → same path (y/2 + x)

        lda     scaled_x_dist                 // y > x → use x/2 + y
        lsr                                   // x/2
        clc
        adc     scaled_y_dist				  // + y
        jmp     return_distance

y_dist_le_x_dist:
        lda     scaled_y_dist                 // y ≤ x → use y/2 + x
        lsr                                   // y/2
        clc
        adc     scaled_x_dist				  // + x

return_distance:
		rts
/*
==============================================================================
  get_walkboxes_for_room
==============================================================================
Summary
	Return a pointer to the room’s walkbox table for a given room index. If the
	room resource is not resident, return a failure code.

Arguments
	walkbox_room       		Room index to query.

Returns
	On success: A = #$00; box_ptr → walkbox table
	On failure: A = #WALKBOX_NOT_RESIDENT (#$FF)

Global Inputs
	room_ptr_lo_tbl[]      Base pointer LO bytes for rooms.
	room_ptr_hi_tbl[]      Base pointer HI bytes for rooms.

Global Outputs
	box_ptr           Pointer to walkbox table (set by callee on success).

Description
	• Test room residency by checking room_ptr_hi_tbl[walkbox_room].
	• If not resident, return failure immediately.
	• If resident, tail-call get_walkboxes_ptr to compute box_ptr from the
	room base plus the walkbox offset stored in the room block.
================================================================================
*/
* = $2DA3
get_walkboxes_for_room:
        ldy     walkbox_room                // Y := target room index to check
        lda     room_ptr_hi_tbl,y           // load high byte of room’s base pointer
        bne     ptr_present                 // if nonzero → room resource is loaded
        lda     #WALKBOX_NOT_RESIDENT       // else mark failure: room not in memory
        rts                                 
ptr_present:
        jmp     get_walkboxes_ptr           // jump to common resolver to compute pointer
/*
================================================================================
 get_walkboxes_for_costume
================================================================================
Summary
	Return a pointer to the walkbox table for the room currently occupied by the
	active costume’s actor. If that room’s resource is not resident, return a
	failure code.

Arguments
	active_costume       Costume index for the actor in focus.

Returns
	On success: A = #$00; box_ptr → walkbox table
	On failure: A = #WALKBOX_NOT_RESIDENT (#$FF)

Global Inputs
	costume_room_idx[]       Map: costume index → current room index.
	room_ptr_lo_tbl[]        Base pointer LO bytes for rooms.
	room_ptr_hi_tbl[]        Base pointer HI bytes for rooms.

Global Outputs
	box_ptr              Pointer to walkbox table (set by callee on success).

Description
	• Retrieve the actor’s current room index from costume_room_idx[active_costume].
	• Check if the room resource is resident via its high-byte entry.
	• If not resident, return failure immediately.
	• If resident, call get_walkboxes_ptr to compute box_ptr for that room.

================================================================================
*/
* = $2DB1
get_walkboxes_for_costume:
        ldy     active_costume              // Y := active costume index
        lda     costume_room_idx,y          // A := room index assigned to this costume
        tay                                 // Y := actor’s current room index
        lda     room_ptr_hi_tbl,y           // load high byte of room base pointer
        bne     get_walkboxes_ptr           // if nonzero → room resource is resident
        lda     #WALKBOX_NOT_RESIDENT       // else return failure
        rts                                 
/*
==============================================================================
  get_walkboxes_ptr
==============================================================================

Summary
	Compute box_ptr to the room’s walkbox table using the room’s loaded
	base address and the block-relative walkbox offset.

Arguments
	Y    Room index.
	A    Room base high byte (room_ptr_hi_tbl[Y]).

Returns
	A = #$00; box_ptr → walkbox table

Global Inputs
	room_ptr_lo_tbl[]    Low bytes of room base pointers.
	OFS_WALKBOX          Byte offset within the room block of the walkbox pointer.

Global Outputs
	box_ptr         ZP pointer to walkbox table (lo/hi).

Description
	• Seed box_ptr with the room base (HI from A, LO from room_ptr_lo_tbl[Y]).
	• Read the 8-bit walkbox offset at base + OFS_WALKBOX ($15).
	• Add the offset to the base pointer; on carry, increment the high byte.
==============================================================================
*/
* = $2DC0
get_walkboxes_ptr:
        sta     >box_ptr                // Set HI of box_ptr from A (room base HI; Y=room index)
        lda     room_ptr_lo_tbl,y            // Load LO byte of room base from table[Y]
        sta     <box_ptr                // Set LO of box_ptr to complete base pointer

        ldy     #OFS_WALKBOX              // Y := offset-of-walkbox byte within block ($15)
        lda     (box_ptr),y          // A := 8-bit walkbox offset at base+$15
        clc                               // clear carry for 8-bit addition
        adc     <box_ptr             // add offset to LO; C=1 if wrap occurred
        sta     <box_ptr             // commit updated LO
        bcc     exit                      // no carry → HI unchanged
        inc     >box_ptr             // carry → increment HI to complete pointer
exit:
        lda     #$00                      // success sentinel in A
        rts                               // return to caller with box_ptr valid


/*
================================================================================
  is_actor_pos_inside_box
================================================================================
Summary
	Determine whether an actor’s position lies within a rectangular box defined
	in the active room’s box table. The box is described by four consecutive
	bytes: [left, right, top, bottom]. All boundaries are inclusive.

Arguments
	X   			Actor index.
	Y   			Offset to box data (relative to box_ptr).
	box_ptr  		pointer to the start of the box table.

Returns
	A   #$00 if the actor is inside the box.
		#$FF if the actor is outside the box.

Global Inputs
	box_ptr                 Pointer to the active room’s box table.
	position_x_for_actor[]  Actor horizontal positions.
	position_y_for_actor[]  Actor vertical positions.

Description
	• Compares the actor’s X coordinate against the left and right box edges.
	Fails immediately if outside those limits.
	• Compares the actor’s Y coordinate against the top and bottom edges.
	Fails if outside the vertical bounds.
	• If all comparisons pass, returns success (A = #$00).
================================================================================
*/
* = $30ad
is_actor_pos_inside_box:
        /*
		------------------------------------------------------------
         X-range check: ensure left ≤ pos_x ≤ right
        
		   Reads left at (box_ptr)+Y, then right at (box_ptr)+Y+1.
           Early-out if outside on either edge test.
        ------------------------------------------------------------
		*/
        lda     position_x_for_actor,x
        cmp     (box_ptr),y                  // pos_x ? left
        bcc     return_outside               // pos_x < left → outside
        iny
        cmp     (box_ptr),y                  // pos_x ? right
        beq     check_y                      // equal is inside so far
        bcs     return_outside               // pos_x > right → outside

check_y:
        /* 
		------------------------------------------------------------
         Y-range check: ensure top ≤ pos_y ≤ bottom
		
           Reads top at (box_ptr)+Y+1, then bottom at (box_ptr)+Y+2.
           Early-out if outside
        ------------------------------------------------------------
		*/
        iny
        lda     position_y_for_actor,x
        cmp     (box_ptr),y                  // pos_y ? top
        bcc     return_outside               // pos_y < top → outside
        iny
        cmp     (box_ptr),y                  // pos_y ? bottom
        beq     return_inside                // equal counts as inside
        bcs     return_outside               // pos_y > bottom → outside

return_inside:
        lda     #INSIDE_BOX
        rts

return_outside:
        lda     #OUTSIDE_BOX
        rts
/*
================================================================================
  get_walkbox_offset
================================================================================
Summary
	Compute the byte offset into the walkbox table for a given walkbox index.
	Each walkbox entry occupies 5 bytes, so the offset = index * 5 (low byte).

Arguments
	A   			Walkbox index

Returns
	Y   			(index * 5)
	A   			Same as Y on return

Vars/State
	wbox_idx     	Temporary storage for original index

Description
	• Saves the input index to wbox_idx for reuse.
	• Multiplies A by 4 using two left shifts.
	• Adds the saved original index to form (index*4 + index).
	• Transfers the low byte of the result to Y for table addressing.
	• Handles only 8-bit arithmetic; high byte is discarded.
================================================================================
*/
* = $30D0
get_walkbox_offset:
        sta     wbox_idx                        // store original walkbox index for reuse
        asl                                     // shift left once → multiply by 2
        asl                                     // shift left again → multiply by 4 total
        clc                                     
        adc     wbox_idx                        // add original index → index*5 (low byte only)
        tay                                     // copy result to Y for table offset
        rts                                     // return to caller; Y holds (index * 5)


