#importonce
#import "globals.inc"
#import "constants.inc"
#import "walkbox.asm"

/*
================================================================================
Waypoint system overview
================================================================================

The waypoint logic ensures that an actor walks naturally between “walkbox” zones
without cutting through walls or getting stuck on edges.

1. Finding where the actor can go

   * When the actor gets a new destination, the code checks which walkbox
     contains that spot.
   * If the spot is outside all boxes, it finds the nearest box and snaps
     the destination there.
   * If no valid box exists, the operation stops.

2. Building a path

   * The engine keeps a list of connected boxes from a prior search.
   * The last box in that list becomes the *target box*.
   * The actor’s current box is treated as the *source box*.
   * Each box has four edges: left, right, top, and bottom.

3. Figuring out their relationship

   * The code compares the two boxes and generates a coordinate relationship result:
     REL_ABOVE, REL_BELOW, REL_LEFT, REL_RIGHT, or REL_OVERLAP.
   * This defines whether the target box is above, below, left, right, or
     overlapping the current box.

4. Checking if the move is straight or needs a turn

   * Depending on how the boxes line up, the actor might walk straight or need
     to turn a corner.
   * It checks if the actor’s coordinate (X or Y, depending on direction) is
     inside the target box’s range.
   * If it’s inside, it can move straight; otherwise, a corner turn is required.

5. Building a “narrow hallway”

   * When boxes don’t align perfectly, the shared overlap acts as a hallway.
   * The code calculates that by taking:
     • The higher of the two top/left edges.
     • The lower of the two bottom/right edges.
   * If the destination isn’t strictly inside this hallway, a corner is needed.

6. Checking along the movement direction

   * It then checks the other coordinate to ensure the destination won’t cross
     the current box edge too early.
   * This ensures the actor stays inside valid areas during movement.

7. Picking the right waypoint

   * If the path is clear, the waypoint is simply the destination.
   * If not, it picks a *corner point* where the actor should turn before
     continuing toward the goal.
     • Moving up/down → adjust Y, possibly keep X.
     • Moving left/right → adjust X, possibly keep Y.

8. Result

   * Valid waypoint → stored in the actor’s table.
   * Failed computation (overlap/invalid) → use destination directly.

In short:

	* Snap the destination to the map.
	* See how the boxes connect.
	* If a straight line is safe, go straight.
	* If not, pick the best corner.
	* Store that point as the waypoint so the actor walks smoothly and stays within
	  valid walkboxes.
================================================================================

Detailed explanation

	The pathing code emits one “next waypoint” for an actor given a desired
	destination and a walkbox graph. It guarantees the waypoint is reachable without
	crossing solid edges, and it encodes when a corner turn is required.

Inputs and state

	* Destination: (actor_x_dest, actor_y_dest).
	* Current walkbox index and base pointer: actor_current_box, current_box_ptr.
	* Discovered path: actor_discovered_boxes with per-actor stride =
	  DISCOVERED_LIST_STRIDE; terminal depth in search_depth_tbl.
	* Relation codes: REL_* from classify_box_relation (bit1=axis, bit0=near-origin).
	* Axis-class result: AXIS_OUTSIDE_LOW/HIGH or AXIS_INSIDE from
	  classify_actor_axis_vs_box (orthogonal to relation axis).

High-level flow

	1. Snap destination to a valid walkbox (nearest_x_candidate, nearest_y_candidate,
	   tgt_box_idx). If none (NO_BOX_FOUND), abort higher up.
	2. Select the target box to aim for:

	   * Use final BFS depth (depth-1) to read the last box id from the discovered
		 list. Compute target_box_ptr = base + (tgt_box_idx * BOX_RECORD_SIZE).
	   * Compute current_box_ptr = base + (actor_current_box * BOX_RECORD_SIZE).
	3. Classify current vs target boxes with classify_box_relation:

	   * REL_BELOW/REL_ABOVE/REL_LEFT/REL_RIGHT (bit1 denotes axis).
	   * REL_OVERLAP_TOUCH → early exit: stop motion, reset depth, no waypoint set.

Straight vs corner test (orthogonal axis first)
	4) Classify actor vs target on the orthogonal axis with classify_actor_axis_vs_box:

	* Returns AXIS_INSIDE if actor’s coordinate lies within target’s orthogonal
	  range (inclusive), else LOW/HIGH.

	5. If not AXIS_INSIDE → straight line would clip; corner is required.

“Narrowest hall” filter (strict containment)
	6) Build the tightest safe corridor on the orthogonal axis by fusing current and
	target edges:

	* hall_lower_bound  = max(first edges)   ; farther from origin (top/left side)
	* hall_upper_bound  = min(second edges)  ; closer to origin (bottom/right side)
	  The destination must satisfy strict interior:
	  hall_lower_bound < dest_coord < hall_upper_bound
	  Touching either bound implies an immediate adjustment requirement. This removes
	  false “straight” paths that would scrape a solid edge as the actor transitions
	  between boxes.

Along-relation consistency check (other axis)
	7) Check the destination’s coordinate on the relation axis against the current
	box edge nearest the origin, using rel bit0:

	* If REL_NEAR_ORIGIN_BIT=0 (current is farther: down/right): require
	  dest_other_axis < current.first_edge.
	* If REL_NEAR_ORIGIN_BIT=1 (current is nearer: up/left): require
	  dest_other_axis ≥ current.first_edge.
	  This prevents the straight segment from crossing the current box boundary on
	  the relation axis before reaching the target corridor.

Corner waypoint construction
	8) If either orthogonal strict test or along-relation test fails, compute a
	corner waypoint aligned to box edges:

	* For vertical relation (REL_ABOVE/REL_BELOW):

	  * X: keep actor X if AXIS_INSIDE, else choose target LEFT/RIGHT nearest to
		actor (offset 0/1).
	  * Y: pick TOP/BOTTOM directly from relation (offset 2/3).
	* For horizontal relation (REL_LEFT/REL_RIGHT):

	  * Y: keep actor Y if AXIS_INSIDE, else choose target TOP/BOTTOM (offset 2/3).
	  * X: pick LEFT/RIGHT directly from relation (offset 0/1).
		The corner is guaranteed to sit at a valid junction where a turn can be made
		without leaving any walkbox.

Return codes and side effects

	* compute_next_waypoint returns:
		  * $00 when a waypoint is emitted (either straight or via corner).
		  * $FF only on overlap/abort; caller may fall back to raw destination.
	* update_actor_waypoint_if_needed clears the per-actor update flag, invokes
	  compute_next_waypoint, and falls back to (dest_x, dest_y) when $FF is returned.

Equality policy

	* Box relation: equality counts as overlap.
	* Axis-class (orthogonal) inside test: inclusive against target’s own edges.
	* Narrowest-hall containment: strict; touching bounds forces a corner.

Complexity and invariants

	* All operations are O(1) per update. Box records are 5 bytes laid out as
	  {left, right, top, bottom}. Bit1 of REL_* is the axis selector; bit0 encodes
	  which box is nearer the origin. The logic preserves X when required; A, Y are
	  clobbered by the tests and construction steps.

Caveats

	* The reference implementation includes a DEY/BNE branch that makes one “other
	  axis” path unreachable; the correct branch should test Y without modifying it.
================================================================================
 Box overlap

 To check for a vertical overlap between two boxes, we compare their top and bottom edges.
 The comparison determines whether the boxes share any vertical space on screen.

 Remember: on screen, Y increases as you move downward.
 Therefore, a higher position visually corresponds to a smaller Y value.

 ------------------------------------------------------------
 Scenario 1 — Vertical overlap
 ------------------------------------------------------------
 Condition:
     B_top < A_bottom
 Meaning:
     Box B starts higher on the screen than the bottom of box A,
     so their vertical ranges overlap.

 Diagram (Y increases ↓):

         |-------|  ← A_top
         |   A   |
         |       |       |-------| ← B_top
 A_bottom|-------|       |   B   |
                         |-------| ← B_bottom

 Result:
     Boxes A and B vertically overlap.


 ------------------------------------------------------------
 Scenario 2 — No vertical overlap
 ------------------------------------------------------------
 Condition:
     B_top > A_bottom
 Meaning:
     Box B begins *below* the bottom of box A,
     so there is an empty gap between them.

 Diagram (Y increases ↓):

 A_top   |-------|
         |   A   |
 A_bottom|-------|
                 |-------| ← B_top
                 |   B   |
                 |-------| ← B_bottom

 Result:
     Boxes A and B do not overlap vertically.
     B is relatively *below* A.


 ------------------------------------------------------------
 For horizontal comparisons:
 ------------------------------------------------------------
 Apply the same logic, but use the left/right edges instead of top/bottom.
 Example: if B_left < A_right → horizontal overlap.
 Otherwise, if B_left > A_right → B is to the right of A.
 
================================================================================
 Corner waypoints
 
 When determining whether the actor must “turn a corner,” the axis being tested
 depends on the relative position between the current and target boxes.

 If the relation is horizontal (left/right), the routine checks the vertical
 axis (top/bottom).
 
 If the relation is vertical (above/below), the routine checks the horizontal
 axis (left/right).

 This cross-axis test allows the engine to detect whether a straight-line path
 between the actor’s position and the target box would hit a wall, requiring a
 detour via a box corner.

 ------------------------------------------------------------
 Scenario 1 — Perfectly aligned boxes
 ------------------------------------------------------------
 Both boxes are side by side and vertically aligned.  
 The actor can move horizontally with no obstruction.

 Diagram (horizontal relation → test vertical axis):

        |--------|=========|
        |   X    | current |		X: target
        | target |    A    |		A: actor
        |--------|=========|

 Result:
     The actor’s vertical coordinate lies fully inside the overlap range.
     No corner turn needed — the actor can move straight left or right.


 ------------------------------------------------------------
 Scenario 2 — Mismatched box heights
 ------------------------------------------------------------
 The current and target boxes do not share identical vertical spans.
 The actor may need to “turn a corner” if the destination row lies
 outside the vertical overlap between both boxes.

 Diagram (Y increases ↓):

 Row 0   |--------|---------|
 Row 1   | target | current |
 Row 2   |        |    A    | ← actor inside current box
 Row 3   |        |---------|
 Row 4   |--------|          ← target box extends lower

 If the destination is near Row 4, the actor cannot move left in a straight line,
 since the current box ends at Row 3. It must first travel down to the corner
 before entering the target box.

 Path visualization:

 Row 0   |--------|---------|
 Row 1   | target | current |
 Row 2   |        |         |
 Row 3   |     ...C....A----| ← C = corner where path turns
 Row 4   |---X..--|          ← X = destination

 A = actor’s position  
 X = destination point  
 C = corner point (path intersection)  
 . = trajectory

 Result:
     The actor’s Y coordinate is below the target box’s bottom edge.
     The movement requires hitting the corner C before continuing
     into the target box.

 Another example:

 			|--------|          	
 			| X      |          	
 			| target |---------|	
 			|--------| current |	
				     |    A    |	
				     |---------|	
		 	          				
	Here, the actor’s vertical position is lower than the bottom edge of the target box.
	As a result, a direct path would intersect the box boundary, so the actor must first 
	reach the current box’s top-left corner before continuing into the target box.


 ------------------------------------------------------------
 Scenario 3 — Vertical relation
 ------------------------------------------------------------
 For vertical relations (target above or below), the logic is inverted:
 the routine checks the horizontal axis instead.

 Example: target box lies below current box,
 but the actor’s X position is outside the overlap horizontally.
 The routine detects that a straight-down move would cross a wall
 and sets the corner waypoint instead.
*/
.label current_box_ptr          = $0017  // zp ptr → current walkbox edge table {left,right,top,bottom}
.label target_box_ptr           = $0019  // zp ptr → target  walkbox edge table {left,right,top,bottom}
.label nearest_x_candidate      = $1A62  // snapped X (pixels) nearest to destination within chosen walkbox
.label nearest_y_candidate      = $1A63  // snapped Y (pixels) nearest to destination within chosen walkbox
.label hall_lower_bound         = $1E04  // tight corridor lower bound on orthogonal axis (max of first edges)
.label hall_upper_bound         = $1E05  // tight corridor upper bound on orthogonal axis (min of second edges)
.label saved_ofs_y              = $1E8F  // scratch: Y-offset into discovered_boxes_tbl for this actor+depth
.label prev_box_dbg             = $CAD4  // debug: penultimate discovered box id (for inspection/logging)
.label rel_axis_coord           = $CAD5  // scratch: actor coordinate on the axis being evaluated (X or Y)
.label box_relpos_code          = $CAD6  // REL_* code from classify_box_relation (bit1=axis, bit0=near-origin)
.label tgt_box_idx              = $FC3B  // walkbox index selected for target at final BFS depth
.label actor_axis_class         = $FC3C  // axis test vs target: $00 below/left, $01 above/right, $02 inside
.label dest_other_axis          = $FC3F  // temp: destination coordinate on the axis opposite to the hall axis
.label closest_box_index             = $FDAC    // current walkbox index
.label candidate_x                   = $FE72    // candidate X
.label candidate_y                   = $FE73    // candidate Y
.label diag_slope_mode                 = $FC3D    // decoded slope selector
.label diag_clamp_applied             = $CB39    // 00=no adjust, 01=adjusted

.const WALKBOX_ATTR_OFS              = $04      // record: attribute byte
.const DIAG_ENABLE_BIT           = $80    // attribute bit7: diagonal present
.const DIAG_MASK                 = $7C    // attribute bits6..2 mask for slope code
.const DIAG_CODE_UP_LEFT         = $08    // bits3..2=10b → up-left slope
.const DIAG_CODE_DOWN_RIGHT      = $0C    // bits3..2=11b → down-right slope
.const DIAG_MODE_UP_LEFT     = $01    // stored in box_attribute for up-left
.const DIAG_MODE_DOWN_RIGHT  = $02    // stored in box_attribute for down-right
.const RET_NO_ADJUST             = $00    // function return: no clamp applied
.const RET_ADJUSTED              = $01    // function return: clamp applied
.const ROOM_X_MAX                = $A0    // upper bound used in up-left path
.const NO_BOX_FOUND              = $FF    // sentinel: no valid walkbox located
.const DISCOVERED_LIST_STRIDE    = $10    // bytes reserved per actor in discovered_boxes_tbl
.const PATH_UPDATE_REQUIRED      = $01    // flag value to trigger path recomputation
.const REL_BELOW                 = $00    // box relation: current below target
.const REL_ABOVE                 = $01    // box relation: current above target
.const REL_RIGHT                 = $02    // box relation: current to the right of target
.const REL_LEFT                  = $03    // box relation: current to the left of target
.const REL_OVERLAP_TOUCH         = $FF    // boxes overlap or share an edge
.const REL_AXIS_HORIZONTAL_BIT   = $02    // bit1 set   → horizontal comparison (left/right)
.const REL_NEAR_ORIGIN_BIT       = $01    // bit0 set   → current box nearer origin (up/left)
.const AXIS_OUTSIDE_LOW          = $00    // actor coordinate below lower edge → corner down/right
.const AXIS_OUTSIDE_HIGH         = $01    // actor coordinate above upper edge → corner up/left
.const AXIS_INSIDE               = $02    // actor coordinate inside target range
.const PATH_ADJUST_REQUIRED      = $FF    // path recalculation needed (return code)
.const PATH_NO_ADJUST            = $00    // straight path valid (return code)

/*
================================================================================
Diagonal edge horizontal offset lookup table
================================================================================

Summary
    Provides the per-scanline ΔX (horizontal shift) used when clamping
    coordinates to diagonal walkbox edges.

Description
    Each entry corresponds to one vertical pixel row below the walkbox’s top
    edge. The value gives the number of pixels to shift horizontally from the
    straight boundary to follow the diagonal slope.

    Used by clamp_walkbox_diagonal_edge to determine how far the visible
    wall edge moves horizontally as Y increases.

Notes
    - Indexed by (candidate_y − top_edge).
    - Values plateau briefly to approximate shallower segments of the wall.
    - Table length = 22 entries.

================================================================================
*/
* = $1C09
diag_dx_lut:
        .byte $00,$01,$02,$03,$03
        .byte $04,$05,$06,$06,$07
        .byte $08,$09,$09,$0A,$0B
        .byte $0C,$0C,$0D,$0E,$0F
        .byte $10,$10

/*
================================================================================
adjust_actor_destination
================================================================================

Summary
    Snap an actor’s requested destination to the nearest valid walkbox point for
    the actor’s current room. Leave destination unchanged if no walkbox exists.

Arguments
    None

Global Inputs
    actor                            index used to read current destination
    active_costume                   selects costume → room mapping
    costume_room_idx[active_costume] room id for walkbox lookup
    actor_x/y_dest[actor]  			 requested X/Y destination

Global Outputs
    actor_x/y_dest[...]     	 	 updated when a walkbox is found

Returns
    .A  nearest_box_idx            	 NO_BOX_FOUND if no walkbox intersects

Description
    - Load the actor’s requested destination into test_x/test_y.
    - Set walkbox_room from active_costume via costume_room_idx.
    - Call snap_coords_to_walkbox to compute nearest_box_idx and possibly
      refine test_x/test_y to a valid walkbox boundary point.
    - If nearest_box_idx != NO_BOX_FOUND, write test_x/test_y back to the
      owning actor’s destination slots.
================================================================================
*/
* = $1A68
adjust_actor_destination:
        // 
        // Seed snap inputs from the actor’s requested destination.
        // 
        ldx     actor
        lda     actor_x_dest,x
        sta     test_x
        lda     actor_y_dest,x
        sta     test_y

        // 
        // Select walkbox room from active costume.
        // 
        ldx     active_costume
        lda     costume_room_idx,x
        sta     walkbox_room

        // 
        // Snap to nearest valid walkbox point.
        // 
        jsr     snap_coords_to_walkbox

        // 
        // No walkbox? Leave destination unchanged.
        // 
        cmp     #NO_BOX_FOUND
        bne     apply_snapped_destination
        rts

apply_snapped_destination:
        // 
        // Write adjusted coordinates back to the owning actor’s dest.
        // 
        ldx     active_costume
        lda     actor_for_costume,x
        tax
        lda     test_x
        sta     actor_x_dest,x
        lda     test_y
        sta     actor_y_dest,x
        rts
/*
================================================================================
snap_coords_to_walkbox
================================================================================

Summary
    Snap input coordinates to the nearest valid point constrained by walkboxes
    and return the index of the nearest walkbox.

Arguments
    None

Global Inputs
    nearest_x/y_candidate      nearest X/Y before slope adjustment

Global Outputs
    test_x                     adjusted X
    test_y                     adjusted Y

Returns
    A  nearest_box_idx        $FF if no walkbox found

Description
    - Preserve X and Y.
    - Call get_nearest_box to compute nearest_box_idx and nearest_{x,y}_candidate.
    - If nearest_box_idx == NO_BOX_FOUND:
        • Restore X and Y, return with A=$FF (current code has a stack bug).
    - Else:
        • Write nearest_{x,y}_candidate to test_{x,y}.
        • Call adjust_diagonal_boundary to refine along sloped edges.
        • Restore X and Y and return nearest_box_idx in A.
================================================================================
*/
* = $1A9B
snap_coords_to_walkbox:
        // 
        // Preserve caller registers X,Y.
        // 
        txa
        pha
        tya
        pha

        // 
        // Find nearest walkbox (sets nearest_box_idx and nearest_{x,y}_candidate).
        // 
        jsr     get_nearest_box

        // 
        // If no box found → return $FF after restoring regs.
        // 
        lda     nearest_box_idx
        cmp     #NO_BOX_FOUND
        bne     write_nearest_point
		//BUG - original return address not in stack, so it will crash on return
		rts

        // 
        // Write nearest point and refine along diagonal boundaries.
        // 
write_nearest_point:
        lda     nearest_x_candidate
        sta     test_x
        lda     nearest_y_candidate
        sta     test_y
        jsr     adjust_diagonal_boundary

        // 
        // Restore Y,X and return nearest_box_idx in A.
        // 
        pla
        tay
        pla
        tax
        lda     nearest_box_idx
        rts
/*
================================================================================
  snap_and_stage_path_update
================================================================================

Summary
	Update an actor’s navigation target: snap the requested destination to the
	nearest walkbox, decide whether a path rebuild is needed, optionally build
	the path, and record bookkeeping flags and snapshots.

Arguments
	None

Vars/State
	actor                         current actor index (0..N), read
	nearest_box_idx               box index selected by snap step, read
	actor_current_box             table [X] → actor’s current walkbox, read
	actor_destination_box         table [X] → actor’s destination walkbox, write
	actor_discovered_boxes        per-actor BFS scratch (stride = 16), write
	actor_path_update_flag        table [X] → path-needs-update flag, write
	actor_last_box                table [X] → last-known box snapshot, write

Global Outputs
	actor_destination_box[X]             updated to snapped target box (if any)
	actor_discovered_boxes[base+0]       cleared to NO_BOX_FOUND when skipping build
	actor_path_update_flag[X]            set to PATH_UPDATE_REQUIRED
	actor_last_box[X]                    snapshot of actor_current_box[X]

Returns
	None

Description

	* Calls snap routine to map the desired destination to a valid walkbox.
	* If no box is found, exits early and leaves state unchanged.
	* If snapped box equals current box:
	  • Skips path construction.
	  • Clears this actor’s discovered-box slot to NO_BOX_FOUND.
	* Otherwise:
	  • Invokes full path build from current to destination box.
	* In all non-early-exit cases:
	  • Sets PATH_UPDATE_REQUIRED for the actor.
	  • Snapshots actor_current_box into actor_last_box.

Notes
	* Calls: snap_coords_to_walkbox, build_walkbox_path.
	* Clobbers A, X, Y; flags per last writes (no carry/overflow guarantees).
	* Assumes actor holds a valid index before invocation.
================================================================================
*/
* = $1C4A
snap_and_stage_path_update:
        // 
        // Adjust destination to nearest walkbox; return if none found.
        // 
        jsr     snap_coords_to_walkbox
        ldx     actor
        lda     nearest_box_idx
        cmp     #NO_BOX_FOUND
        bne     save_destination_box
        rts

save_destination_box:
        // 
        // Save closest box as destination for this actor.
        // 
        sta     actor_destination_box,x

        // 
        // If destination box equals current box → skip path build.
        // 
        cmp     actor_current_box,x
        beq     clear_path_buffer

        // 
        // Different destination → build full path.
        // 
        jsr     build_walkbox_path
        ldx     actor
        jmp     mark_path_update_and_snapshot

clear_path_buffer:
        // 
        // Clear actor’s discovered-box list (stride = 16 bytes).
        // 
        txa
        asl
        asl
        asl
        asl
        clc
        adc     #DISCOVERED_LIST_STRIDE
        tay
        lda     #NO_BOX_FOUND
        sta     actor_discovered_boxes,y

mark_path_update_and_snapshot:
        // 
        // Flag path update and record last box position.
        // 
        lda     #PATH_UPDATE_REQUIRED
        sta     actor_path_update_flag,x
        lda     actor_current_box,x
        sta     actor_last_box,x
        rts
/*
================================================================================
  apply_waypoint_update_if_needed
================================================================================

Summary
	If a waypoint update is pending for the actor, rebuild the next waypoint
	via walkboxes; otherwise return immediately. On overlap/fail, fall back to
	the raw destination coordinates.

Arguments
	X  actor index

Global Inputs
	actor                          current actor index (mirrored in X)
	actor_path_update_flag[]       nonzero → waypoint must be recomputed
	actor_x/y_dest[]			   actor’s target destination

Global Outputs
	actor_tgt_waypoint_x/y[]       waypoint X/Y written on success or fallback
	actor_path_update_flag[]       cleared on entry when work is performed

Returns
	None

Description

	* Read actor_path_update_flag[X]; if zero, no work.
	* Clear the flag and call build_waypoint_from_path.
	* If build_waypoint_from_path returned $FF (overlap/no path), copy
	  (actor_x_dest, actor_y_dest) to (actor_tgt_waypoint_x, actor_tgt_waypoint_y).
	* Otherwise the callee has already written the waypoint.

Notes

	* Clobbers A, X. Y is preserved.
	* build_waypoint_from_path defines whether a corner-adjustment is needed; this
	  routine only orchestrates and applies a safe fallback on failure.
================================================================================
*/
* = $1C81
apply_waypoint_update_if_needed:
		// 
		// If actor_path_update_flag[x] == 0 → nothing to do
		// 
		ldx     actor
		lda     actor_path_update_flag,x
		beq     return_done_ok

  
		// 
		// Clear update flag; build path through walkboxes
		// 
		lda     #$00
		sta     actor_path_update_flag,x
		jsr     build_waypoint_from_path

		// 
		// A=$FF → no computed path (overlap) → fall back to destination
		// 
		cmp     #$ff
		bne     return_done_ok

		ldx     actor
		lda     actor_x_dest,x
		sta     actor_tgt_waypoint_x,x
		lda     actor_y_dest,x
		sta     actor_tgt_waypoint_y,x
  
return_done_ok:
		rts
/*
================================================================================
  build_waypoint_from_path
================================================================================
Summary
	Build the next waypoint toward the actor’s snapped destination using the
	walkbox path. If the current and target boxes overlap, stop early. Otherwise
	either keep the straight-line destination or route via the nearest corner.

Arguments
	X  actor index

Global Inputs
	actor                          current actor index (mirrored in X)
	actor_search_depth[]           final BFS depth for actor (or $FF if none)
	actor_discovered_boxes[]       per-actor discovered boxes (stride = 16)
	actor_current_box[]            actor’s current walkbox index
	actor_x/y_dest[]			   desired destination in pixels
	actor_x/y_pos[]				   actor’s current position in pixels
	current_box_ptr                base pointer to walkbox table {L,R,T,B}

Global Outputs
	actor_tgt_waypoint_x/y[] 		waypoint X/Y emitted for this step
	actor_last_box[]               snapshot of last box used
	actor_motion_state[]           set to 1 on overlap early-exit
	actor_search_depth[]           reset to $FF on overlap early-exit

Returns
	A = 	$FF  overlap early-exit, no waypoint change beyond bookkeeping
			$00  waypoint set (either direct destination or corner-adjusted)

Description
	* Ensure walkboxes are loaded; if not resident, return.
	* Read BFS depth; if $FF, fail. Else use (depth-1) to pick the final box.
	* Compute discovered-list offset for this actor and fetch the box id.
	* Point target_box_ptr to that box; point current_box_ptr to actor’s box.
	* Compare boxes:
	  • If REL_OVERLAP_TOUCH: stop motion, reset depth, return $FF.
	  • Else cache relation and classify actor vs target on the orthogonal axis.
	* If no adjustment needed: copy destination to waypoint.
	* If adjustment needed:
	  • For vertical relation: choose X from actor or target {left/right};
	  set Y from relation {top/bottom}.
	  • For horizontal relation: choose Y from actor or target {top/bottom};
	  set X from relation {left/right}.
	* Return $00.

Notes
	* Box edge order is {left, right, top, bottom}. Each walkbox record is 5 bytes.
	* Bit1 of RELPOS_* encodes axis (0=vertical, 1=horizontal).
	* Equality with edges counts as “inside range” during axis checks.
	* Clobbers A, X, Y; flags reflect last LDA immediate on return.
================================================================================
*/
* = $1CA3
build_waypoint_from_path:
		// 
		// X := actor; clear debug scratch
		// 
		ldx     actor
		lda     #$ff
		sta     prev_box_dbg

  
		// 
		// Acquire walkboxes for this costume; RTS if unavailable (A=$FF)
		// 
		jsr     get_walkboxes_for_costume
		cmp     #WALKBOX_NOT_RESIDENT
		bne     load_bfs_depth_or_fail
		rts
  

load_bfs_depth_or_fail:
		// 
		// Read final BFS depth; if $FF → fail and return $FF
		// 
		lda     actor_search_depth,x
		cmp     #$ff
		bne     index_from_depth
		lda     #$ff
		rts

index_from_depth:
		// 
		// depth := depth-1; save; compute per-actor offset base ((idx+1)*16)
		// 
		dec     actor_search_depth,x
		sta     tgt_box_idx                    // note: A still holds prior value in original code
		txa
		asl
		asl
		asl
		asl
		clc
		adc     #DISCOVERED_LIST_STRIDE
		clc
		adc     tgt_box_idx
		tay
		sty     saved_ofs_y


		// 
		// Fetch box id at that depth; snapshot last box and cache index
		// 
		lda     actor_discovered_boxes,y
		sta     actor_last_box,x
		sta     tgt_box_idx

		// 
		// If depth is 0 or $FF skip penultimate-box debug copy
		// 
		ldy     saved_ofs_y
		lda     actor_search_depth,x
		beq     point_to_target_box
		cmp     #$ff
		beq     point_to_target_box

		// 
		// Debug: copy penultimate box into prev_box_dbg
		// 
		dey
		lda     actor_discovered_boxes,y
		sta     prev_box_dbg
		iny


point_to_target_box:
		// 
		// Derive target_box_ptr from current_box_ptr + (target_idx * 5)
		// 
		lda     <current_box_ptr
		sta     $1b                                   // unused temp in original
		lda     >current_box_ptr
		sta     $1c                                   // unused temp in original
		lda     >current_box_ptr
		sta     >target_box_ptr
		lda     tgt_box_idx
		asl
		asl
		clc
		adc     tgt_box_idx
		adc     <current_box_ptr
		sta     <target_box_ptr
		bcc     point_to_current_box
		inc     >target_box_ptr

point_to_current_box:
		// 
		// current_box_ptr := base + (current_idx * 5)
		// 
		lda     actor_current_box,x
		asl
		asl
		clc
		adc     actor_current_box,x
		adc     <current_box_ptr
		sta     <current_box_ptr
		bcc     classify_box_relation_2
		inc     >current_box_ptr

classify_box_relation_2:
		// 
		// Compare current vs target; Y must be 0
		// 
		ldy     #$00
		jsr     classify_box_relation
		cmp     #REL_OVERLAP_TOUCH
		bne     relation_non_overlap

		// 
		// Overlap: stop motion, reset depth, return $FF
		// 
		ldx     actor
		lda     #$01
		sta     actor_motion_state,x
		lda     #$ff
		sta     actor_search_depth,x
		lda     #$ff
		rts

relation_non_overlap:
		// 
		// Cache relation; classify actor coordinate vs target axis
		// 
		sta     box_relpos_code
		jsr     classify_actor_axis_vs_box
		ldx     actor
		sta     actor_axis_class


		// 
		// Decide if adjustment needed: JSR sets Z=1 for “no adjust”
		//   Returns: A=$FF adjust needed, A=$00 no adjust
		// 
		jsr     evaluate_path_adjustment_need
		bne     route_via_corner


		// 
		// No adjustment: copy destination as waypoint; return $00
		// 
		lda     actor_x_dest,x
		sta     actor_tgt_waypoint_x,x
		lda     actor_y_dest,x
		sta     actor_tgt_waypoint_y,x
		lda     #$00
		rts

route_via_corner:
		// 
		// If bit1 set → horizontal relation → branch to vertical axis fix
		// Else vertical relation → branch to horizontal axis fix
		// 
		lda     box_relpos_code
		and     #$02
		bne     corner_fix_for_horizontal_relation

		// 
		// Vertical relation: fix X first, then Y from relation+2 (top/bottom)
		// actor_axis_class: $02 inside → keep actor X
		//   else ($00/$01) pick target left/right via offset 0/1
		// 
		lda     actor_axis_class
		cmp     #$02
		bne     pick_tgt_left_right
		
		lda     actor_x_pos,x
		jmp     write_waypoint_x_then_y

pick_tgt_left_right:
		tay                                         // Y := 0 or 1 (left/right)
		lda     (target_box_ptr),y
		
write_waypoint_x_then_y:
		sta     actor_tgt_waypoint_x,x
		ldy     box_relpos_code
		iny
		iny                                         // offset += 2 → top/bottom
		lda     (target_box_ptr),y
		sta     actor_tgt_waypoint_y,x
		jmp     return_ok

corner_fix_for_horizontal_relation:
		// 
		// Horizontal relation: fix Y first, then X from relation-2 (left/right)
		// actor_axis_class: $02 inside → keep actor Y
		//   else ($00/$01) pick target top/bottom via offset 2/3
		// 
		lda     actor_axis_class
		cmp     #$02
		bne     pick_tgt_top_bottom
		
		lda     actor_y_pos,x
		jmp     write_waypoint_y_then_x

pick_tgt_top_bottom:
		tay
		iny
		iny                                         // +2 → top/bottom
		lda     (target_box_ptr),y
		
write_waypoint_y_then_x:
		sta     actor_tgt_waypoint_y,x
		ldy     box_relpos_code
		dey
		dey                                         // -2 → left/right
		lda     (target_box_ptr),y
		sta     actor_tgt_waypoint_x,x

return_ok:
		lda     #$00
		rts
/*
================================================================================
  classify_box_relation
================================================================================
Summary
	Determine the relative position between two axis-aligned boxes (AABBs).

Arguments
	current_box_ptr  zp pointer → {left, right, top, bottom}
	target_box_ptr   zp pointer → {left, right, top, bottom}

Returns
	A = 	REL_BELOW      		current below target
			REL_ABOVE      		current above target
			REL_RIGHT      		current right of target
			REL_LEFT       		current left of target
			REL_OVERLAP_TOUCH	boxes overlap or only touch

Description
	* Equality counts as overlap: edge or corner contact → OVERLAP.
	* Coordinate convention: larger Y is lower on screen.
	* Offsets: 0=left, 1=right, 2=top, 3=bottom.
	* Axis encoding: bit1 of result set → horizontal (02/03), clear → vertical (00/01).
	* Clobbers A, Y. Preserves X.

Notes
	* Precondition: Y must be #$00 on entry. Initialize if uncertain.
	* O(1) test with early exits on clear separation.
================================================================================
*/
* = $1D9E
classify_box_relation:
		// Vertical test: current.top ? target.bottom
		iny
		iny
		lda     (current_box_ptr),y           // A := current.top
		iny
		cmp     (target_box_ptr),y            // A - target.bottom
		beq     test_vertical_above
		bcs     box_below_target                     // current.top > target.bottom → below


test_vertical_above:
		// Vertical test: current.bottom ? target.top
		lda     (current_box_ptr),y           // A := current.bottom
		dey
		cmp     (target_box_ptr),y            // A - target.top
		bcc     box_above_target                     // current.bottom < target.top → above


		// Horizontal test: current.left ? target.right
		dey
		dey
		lda     (current_box_ptr),y           // A := current.left
		iny
		cmp     (target_box_ptr),y            // A - target.right
		beq     test_horizontal_left
		bcs     box_right_of_target                     // current.left > target.right → right


test_horizontal_left:
		// Horizontal test: current.right ? target.left
		lda     (current_box_ptr),y           // A := current.right
		dey
		cmp     (target_box_ptr),y            // A - target.left
		bcc     box_left_of_target                     // current.right < target.left → left

		lda     #REL_OVERLAP_TOUCH                          // overlap or touch
		jmp     return_relative_position

box_left_of_target:
		lda     #REL_LEFT                          // left
		jmp     return_relative_position

box_right_of_target:
		lda     #REL_RIGHT                          // right
		jmp     return_relative_position

box_above_target:
		lda     #REL_ABOVE                          // above
		jmp     return_relative_position

box_below_target:
		lda     #REL_BELOW                          // below
		
return_relative_position:
		rts
/*
================================================================================
  classify_actor_axis_vs_box
================================================================================
Summary
	Classify an actor’s coordinate against the target box on the appropriate
	axis to detect if a “corner turn” will be required.

Arguments
	X  actor index

Vars/State
	rel_axis_coord        		scratch: selected actor axis coordinate

Global Inputs
	box_relpos_code       		relation code (bit1: 0=vertical,1=horizontal)
	target_box_ptr        		zp → {left, right, top, bottom}
	actor_x/y_pos[]         	per-actor X/Y position table

Returns
	A = 	AXIS_OUTSIDE_LOW   	actor < lower edge (left/top)
			AXIS_OUTSIDE_HIGH  	actor > upper edge (right/bottom)
			AXIS_INSIDE  		actor within [lower..upper]

Description

	* If relation is horizontal (bit1=1), test actor’s Y vs target top/bottom.
	* If relation is vertical   (bit1=0), test actor’s X vs target left/right.
	* Uses Y as edge offset base: 0 for X-axis (left/right), 2 for Y-axis (top/bottom).
	* Equality with either edge counts as inside.

Notes

	* Clobbers A, Y. Preserves X.
	* Assumes target_box_ptr layout: {left, right, top, bottom}.
	* O(1) early-exit comparisons; no carry/overflow guarantees on return.
================================================================================
*/
* = $1DD9
classify_actor_axis_vs_box:
		lda     box_relpos_code
		and     #REL_AXIS_HORIZONTAL_BIT
		tay                                     // Y := $00 (vertical) or $02 (horizontal)  
		bne     select_actor_y                           // A≠0 → horizontal relation → use Y

		// Relative position is vertical → use actor X
		lda     actor_x_pos,x
		jmp     store_axis_coord

select_actor_y:
		// Relative position is horizontal → use actor Y
		lda     actor_y_pos,x

store_axis_coord:
		sta     rel_axis_coord


		// 
		// Range test vs target box on the selected axis.
		// Compare against lower edge (left/top). If actor < lower → return $00.
		// Then compare against upper edge (right/bottom). If ≤ upper → return $02.
		// Otherwise actor > upper → return $01.
		// 
		lda     rel_axis_coord
		cmp     (target_box_ptr),y               // vs lower (left/top)
		bcs     check_against_upper

		lda     #AXIS_OUTSIDE_LOW                            // actor < lower → corner down/right needed
		rts

check_against_upper:
		iny                                     // move to upper (right/bottom)
		cmp     (target_box_ptr),y
		bcc     inside_range
		beq     inside_range

		lda     #AXIS_OUTSIDE_HIGH                            // actor > upper → corner up/left needed
		rts

inside_range:
		lda     #AXIS_INSIDE                            // actor inside target axis range
		rts	
/*
================================================================================
  evaluate_path_adjustment_need
================================================================================
Summary
	Decide if the actor’s straight-line destination requires a corner adjustment.

Arguments
	X  actor index

Returns
	A = 	PATH_ADJUST_REQUIRED 	destination cannot be taken straight
			PATH_NO_ADJUST     		straight path is valid

Global Inputs
	relpos_code                   		RELPOS_* from classify_box_relation
	actor_axis_class              		$00/$01 outside, $02 inside (orthogonal axis)
	cur_box_ptr_zp, tgt_box_ptr_zp 		{left,right,top,bottom}
	actor_dest_x/y_tbl[]
	
Description
	* First gate: orthogonal-axis containment
	  • If actor_axis_class != AXIS_INSIDE ($02) → PATH_ADJUST_REQUIRED.
	* Build “narrowest hall” on the orthogonal axis
	  • Use relpos_code.bit1 to choose axis (0: horizontal, 1: vertical).
	  • hall_lower_bound := max(current.first, target.first).
	  • hall_upper_bound := min(current.second, target.second).
	  • Require strict interior: hall_lower_bound < dest < hall_upper_bound,
	  else PATH_ADJUST_REQUIRED.
	* Second gate: along-relation coordinate vs current box
	  • Use relpos_code.bit0 to pick which current edge is nearer the origin.
	  • If current is farther (bit0=0): require other < current.first.
	  • If current is closer (bit0=1): require other ≥ current.first.
	  • Pass → PATH_NO_ADJUST, fail → PATH_ADJUST_REQUIRED.


Notes
	* Edge equality on the “hall” test counts as adjustment (strict inequality).
	* Clobbers A, Y; preserves X.
	* Known issue in provided code: the vertical path under switch_to_other_axis is
	  unreachable due to a DEY/BNE sequence; branch should test Y without changing it.
================================================================================
*/
* = $1E07
evaluate_path_adjustment_need:
		// 
		// Orthogonal-axis containment: require inside target range
		// A := actor_axis_class ($02 = inside, $00/$01 = outside)
		// 
		lda     actor_axis_class
		cmp     #AXIS_INSIDE
		beq     build_narrowest_hall

	  
		// Outside orthogonal range → adjustment required
		lda     #PATH_ADJUST_REQUIRED
		rts
  

build_narrowest_hall:
		// 
		// Build the “narrowest hall” on the orthogonal axis.
		// Axis select from relpos_code.bit1:
		//   0 → horizontal edges (left/right), Y := $00
		//   1 → vertical   edges (top/bottom), Y := $02
		// hall_lower_bound  := max(current.first , target.first)   (farther from origin)
		// hall_upper_bound := min(current.second, target.second)  (closer to origin)
		// 
		lda     box_relpos_code
		and     #REL_AXIS_HORIZONTAL_BIT
		tay


		// Pick hall_lower_bound = max(firsts)
		lda     (current_box_ptr),y
		cmp     (target_box_ptr),y
		beq     pick_target_as_first_edge
		bcs     set_hall_lower_bound


pick_target_as_first_edge:
		lda     (target_box_ptr),y
		
set_hall_lower_bound:
		sta     hall_lower_bound


		// Pick hall_upper_bound = min(seconds)
		iny
		lda     (current_box_ptr),y
		cmp     (target_box_ptr),y
		bcc     set_hall_upper_bound
		lda     (target_box_ptr),y


set_hall_upper_bound:
		sta     hall_upper_bound


		// 
		// Choose destination coordinate on this orthogonal axis:
		//   bit1=0 → use actor_x_dest   (horizontal edges)
		//   bit1=1 → use actor_y_dest   (vertical edges)
		// 
		lda     box_relpos_code
		and     #REL_AXIS_HORIZONTAL_BIT
		bne     load_dest_vertical


		lda     actor_x_dest,x
		jmp     test_hall_lower_bound
		
load_dest_vertical:
		lda     actor_y_dest,x

test_hall_lower_bound:
		// Require strict inside: hall_lower_bound < dest < hall_upper_bound
		cmp     hall_lower_bound
		beq     return_PATH_ADJUST_REQUIRED                      // touching edge → adjust
		bcs     test_hall_upper_bound

return_PATH_ADJUST_REQUIRED:
		lda     #PATH_ADJUST_REQUIRED                           // dest ≤ hall_lower_bound → adjust
		rts

test_hall_upper_bound:
		cmp     hall_upper_bound
		bcc     switch_to_other_axis
		lda     #PATH_ADJUST_REQUIRED                           // dest ≥ hall_upper_bound → adjust
		rts

switch_to_other_axis:
		// 
		// Now test the OTHER destination coordinate against current box.
		// BUG NOTE: The next two lines make the vertical path unreachable
		// (DEY; BNE always taken for Y=$00 or $02). Kept as-is per no-semantics rule.
		// Correct form would branch based on Y without modifying it.
		// 
		dey
		bne     load_other_horizontal_coord

		lda     actor_y_dest,x
		ldy     box_relpos_code
		iny
		iny
		jmp     store_other_axis_coord

load_other_horizontal_coord:
		lda     actor_x_dest,x
		ldy     box_relpos_code
		dey
		dey

store_other_axis_coord:
		sta     dest_other_axis


		// 
		// Choose which current-box edge is nearer the origin using bit0:
		//   bit0=1 → current is closer (up/left)
		//   bit0=0 → current is farther (down/right)
		//
		// If farther: require other < current.first
		// If closer : require other ≥ current.first
		// 
		lda     box_relpos_code
		and     #REL_NEAR_ORIGIN_BIT
		bne     check_closest_box_edge


		lda     dest_other_axis
		cmp     (current_box_ptr),y
		bcc     return_PATH_NO_ADJUST_alt                    // OK: other < current.first
		lda     #PATH_ADJUST_REQUIRED                           // past edge → adjust
		rts

return_PATH_NO_ADJUST_alt:
		jmp     return_PATH_NO_ADJUST

check_closest_box_edge:
		lda     dest_other_axis
		cmp     (current_box_ptr),y
		beq     return_PATH_NO_ADJUST                      // OK: on edge allowed in this case
		bcs     return_PATH_NO_ADJUST                      // OK: other ≥ current.first
		lda     #PATH_ADJUST_REQUIRED                           // below edge → adjust
		rts

return_PATH_NO_ADJUST:
		lda     #PATH_NO_ADJUST                           // no adjustment needed
		rts			
/*
================================================================================
  Clamp candidate X against diagonal walkbox edge 
================================================================================

Summary
	Adjust candidate_x to respect a diagonal walkbox boundary using a per-row
	ΔX lookup indexed by vertical distance from the box top.

Arguments
	None

Returns
	.A  	RET_ADJUSTED  on paths through finalize_diag_exit
			(early returns leave .A unchanged)

Global Inputs
	closest_box_index             walkbox selector
	candidate_x                   X under test
	candidate_y                   Y under test
	current_box_ptr               ZP pointer to walkbox record base

Global Outputs
	diag_slope_mode               01=up-left, 02=down-right
	diag_clamp_applied            00 cleared on entry, set to 01 on exit path
	candidate_x                   clamped when boundary test passes

Description
	- Resolve walkbox record via closest_box_index and get attribute at +$04.
	- Require attribute bit7 to enable diagonal handling; otherwise return.
	- Mask bits6..2, decode slope code:
		* $08 → up-left (use right − dx)
		* $0C → down-right (use left + dx)
	- Compute dy = candidate_y − top(+2), then dx = diag_dx_lut[dy].
	- Compare adjusted boundary with candidate_x:
		* up-left: if (right − dx) ≥ candidate_x, clamp candidate_x
		and guard vs ROOM_X_MAX.
		* down-right: if (left + dx) ≤ candidate_x, clamp candidate_x.
	- Set diag_clamp_applied and return through finalize_diag_exit.
================================================================================
*/

* = $1B91
adjust_diagonal_boundary:
        lda     #$00
        sta     diag_clamp_applied             // clear adjustment flag

        // ------------------------------------------------------------
        // Resolve walkbox record and read attribute at +$04
        // ------------------------------------------------------------
        lda     closest_box_index
        jsr     get_walkbox_offset            // Y := base of walkbox record
        tya
        clc
        adc     #WALKBOX_ATTR_OFS
        tay
        lda     (current_box_ptr),y
        bmi     diag_attr_enabled                      // require bit7 set for diagonal
        rts

        // ------------------------------------------------------------
        // Decode slope bits (mask 6..2), branch on $0C or $08
        // ------------------------------------------------------------
diag_attr_enabled:
        and     #DIAG_MASK
        cmp     #DIAG_CODE_DOWN_RIGHT
        bne     check_slope_up_left

        // down-right slope (right edge)
        lda     #DIAG_MODE_DOWN_RIGHT
        jmp     store_slope_mode

check_slope_up_left:
        cmp     #DIAG_CODE_UP_LEFT
        bne     no_diagonal_exit

        // up-left slope (left edge)
        lda     #DIAG_MODE_UP_LEFT
        jmp     store_slope_mode

no_diagonal_exit:
        rts

        // ------------------------------------------------------------
        // Compute dy from top(+2), fetch dx from table, clamp X
        // ------------------------------------------------------------
store_slope_mode:
        sta     diag_slope_mode                 // persist slope selector

        lda     candidate_y                   // A := candidate Y
        dey                                   // Y := base + 3 (bottom)
        dey                                   // Y := base + 2 (top)
        sec
        sbc     (current_box_ptr),y             // A := Y - top
        tax                                   // X := dy (table index)

        dey                                   // Y := base + 1 (right)
        lda     diag_slope_mode
        cmp     #DIAG_MODE_UP_LEFT
        bne     handle_down_right_slope

        // up-left slope: boundary = right(+1) - dx
        lda     (current_box_ptr),y             // right
        sec
        sbc     diag_dx_lut,x        // right - dx
        cmp     candidate_x                   // adjusted ≥ candidate X ?
        bcs     room_width_guard
        lda     #RET_NO_ADJUST                         // no adjust (note: exit path forces #$01)
        jmp     finalize_diag_exit

room_width_guard:
        cmp     #ROOM_X_MAX                          // guard vs $A0
        bcc     apply_x_clamp_up
        beq     apply_x_clamp_up
        lda     #$00
apply_x_clamp_up:
        sta     candidate_x
        jmp     finalize_diag_exit

        // down-right slope: boundary = left(+0) + dx
handle_down_right_slope:
        dey                                   // Y := base + 0 (left)
        lda     (current_box_ptr),y             // left
        clc
        adc     diag_dx_lut,x        // left + dx
        cmp     candidate_x                   // adjusted ≤ candidate X ?
        bcc     apply_x_clamp_down
        beq     apply_x_clamp_down
        lda     #RET_NO_ADJUST                          // no adjust (note: exit path forces #$01)
        jmp     finalize_diag_exit

apply_x_clamp_down:
        sta     candidate_x

finalize_diag_exit:
        lda     #RET_ADJUSTED                          // original code forces adjust flag
        sta     diag_clamp_applied
        rts


		