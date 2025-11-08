#importonce

#import "globals.inc"
#import "constants.inc"
#import "sentence_action.asm"

/*
================================================================================
  handle_entity_approach_and_trigger
================================================================================
Summary
	Move the active actor toward the target entity and, upon proximity, set a
	contact sentinel so the queued verb handler executes on the next tick.

Global Inputs
	destination_entity         target entity id (#$00 none, #$FE contact)
	destination_obj_lo         low byte of target object id for routing
	destination_obj_hi         high byte of target object id for routing
	current_kid_idx            active kid index
	actor_for_costume[]        map: costume index → actor index
	actor_motion_state[]       actor motion state array
	dest_x, dest_y             routed destination coordinates
	actor_x_pos[], actor_y_pos[] 	actor position arrays

Global Outputs
	destination_entity          set to ENTITY_CONTACT_SENTINEL (#$FE) on contact,
								cleared to ENTITY_NONE (#$00) when out of range

Description
	- Guard: exit if no destination entity is set.
	- Resolve actor for the current kid. Exit if the actor is not stopped
	(actor_motion_state ≠ ACTOR_STATE_STOPPED).
	- If destination_entity == ENTITY_CONTACT_SENTINEL, clear it and jump to
	execute_verb_handler_for_object (consumes the queued action).
	- Otherwise:
		• Route destination by entity type → dest_x/dest_y.
		• Compute |ΔX| = |dest_x − actor_x|:
			- If |ΔX| > CONTACT_X_THRESHOLD, clear flag and return.
			- If |ΔX| ≤ CONTACT_X_THRESHOLD, continue.
		• Compute |ΔY| = |dest_y − actor_y|:
			- If |ΔY| > CONTACT_Y_THRESHOLD, clear flag and return.
			- If |ΔY| ≤ CONTACT_Y_THRESHOLD:
				· apply_facing_toward_destination
				· destination_entity := ENTITY_CONTACT_SENTINEL (contact latched)
================================================================================
*/

.const ENTITY_CONTACT_SENTINEL    = $FE    // Contact reached; trigger on next tick
.const CONTACT_X_THRESHOLD        = $04    // |ΔX| clamp for contact check
.const CONTACT_Y_THRESHOLD        = $08    // |ΔY| clamp for contact check

* = $0CD0
handle_entity_approach_and_trigger:
        // ----------------------------------------------------
        // Guard: is there a destination entity?
        // If none, return_from_entity_contact immediately.
        // ----------------------------------------------------
        lda     destination_entity
        bne     resolve_actor_and_validate_state
        jmp     return_from_entity_contact

resolve_actor_and_validate_state:
        // ----------------------------------------------------
        // Get actor index for the current kid
        // ----------------------------------------------------
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax

        // ----------------------------------------------------
        // return_from_entity_contact if actor is still moving
        // ----------------------------------------------------
        lda     actor_motion_state,x
        cmp     #ACTOR_STATE_STOPPED
        bne     return_from_entity_contact

        // ----------------------------------------------------
        // If entity already marked as contacted (#$FE),
        // clear flag and run queued action.
        // ----------------------------------------------------
        lda     destination_entity
        cmp     #ENTITY_CONTACT_SENTINEL
        bne     route_and_check_proximity

        lda     #ENTITY_NONE
        sta     destination_entity
        jmp     execute_verb_handler_for_object

route_and_check_proximity:
        // ----------------------------------------------------
        // Route destination coordinates based on entity type
        // ----------------------------------------------------
        ldx     destination_obj_lo
        lda     destination_obj_hi
        ldy     destination_entity
        jsr     route_destination_by_entity_type

        // ----------------------------------------------------
        // Re-resolve actor for current kid
        // ----------------------------------------------------
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax

        // ----------------------------------------------------
        // Compute |ΔX| between actor and destination
        // ----------------------------------------------------
        lda     dest_x
        sec
        sbc     actor_x_pos,x
        bcs     evaluate_x_threshold
        eor     #$FF
        clc
        adc     #$01

evaluate_x_threshold:
        // If |ΔX| == 4 → continue to Y
        // If |ΔX| > 4  → out of range → clear flag & return_from_entity_contact
        // If |ΔX| < 4  → close enough → check Y
        cmp     #CONTACT_X_THRESHOLD
        beq     compute_y_distance
        bcs     clear_contact_flag_far_x
        // else < threshold → check Y

compute_y_distance:
        // ----------------------------------------------------
        // Compute |ΔY| between actor and destination
        // ----------------------------------------------------
        lda     dest_y
        sec
        sbc     actor_y_pos,x
        bcs     evaluate_y_threshold
        eor     #$FF
        clc
        adc     #$01

evaluate_y_threshold:
        // If |ΔY| == 8 → set facing & mark contact
        // If |ΔY| > 8  → out of range → clear flag & return_from_entity_contact
        // If |ΔY| < 8  → set facing & mark contact
        cmp     #CONTACT_Y_THRESHOLD
        beq     align_facing_and_mark_contact
        bcs     clear_contact_flag_far_y

align_facing_and_mark_contact:
        // ----------------------------------------------------
        // Set facing toward entity and mark contact (#$FE)
        // ----------------------------------------------------
        jsr     apply_facing_toward_destination
        lda     #ENTITY_CONTACT_SENTINEL
        sta     destination_entity
        jmp     return_from_entity_contact

clear_contact_flag_far_y:
        lda     #ENTITY_NONE
        sta     destination_entity
        jmp     return_from_entity_contact

clear_contact_flag_far_x:
        lda     #ENTITY_NONE
        sta     destination_entity

return_from_entity_contact:
        rts

/*
================================================================================
  copy_vic_color_ram
================================================================================

Summary
	Copy 680 bytes from the scene color buffer ($6D89–$7030) to Color RAM
	($D828–$DACF) as 17 rows × 40 columns using one outer Y loop and 17
	unrolled LDA/STA pairs per column.

Global Inputs
	Source row bases                 $6D89,$6DB1,$6DD9,$6E01,$6E29,$6E51,$6E79,
									 $6EA1,$6EC9,$6EF1,$6F19,$6F41,$6F69,$6F91,
									 $6FB9,$6FE1,$7009  (each +$28 from prior)
									 
	Destination row bases            $D828,$D850,$D878,$D8A0,$D8C8,$D8F0,$D918,
									 $D940,$D968,$D990,$D9B8,$D9E0,$DA08,$DA30,
									 $DA58,$DA80,$DAA8  (each +$28 from prior)
Global Outputs
	Color RAM range                	 $D828–$DACF written (680 bytes)
	vic_color_ram_copy_done        	 set to TRUE on completion
	
Description
	- Initializes Y to 39 and decrements to 0, copying one column per pass.
	- Uses abs,Y addressing so each of the 17 row bases contributes a byte for
	the current column, minimizing loop overhead.
	- Layout matches VIC Color RAM’s 40-column stride ($28), avoiding 16-bit
	pointer math and page-cross penalties typical of linear copies.

Notes
	- Color RAM uses only the low nibble; high nibble in source bytes is ignored.
	- Intended for vblank/top-of-frame band where the bulk copy will not cause
	visible artifacts.
================================================================================
  
Color RAM sits at $D800–$DBE7 and is accessed as I/O space. Writes are slower
than normal RAM and per-byte overhead dominates. This routine copies the scene’s
17 visible rows by fixing each row’s absolute base and using a single outer Y
loop as the column index.

Advantages of copying in this way:

	• Absolute,Y beats pointer math
		Using abs,Y (LDA/STA $addr,Y) avoids 16-bit pointer adds inside the loop. The
		17 source and 17 destination bases are constants spaced by $28 (40 columns).
		Only Y changes. This removes ADC/INC and page-cross bookkeeping.

	• One branch per column
		The loop body is fully unrolled across rows, so each column costs 17 loads and
		17 stores plus one DEY/BPL. Fewer branches means more predictable timing.

	• Page-crossing is contained
		With abs,Y the CPU handles page crosses automatically. No manual carry fixups
		and no extra instructions per row. Timing variance stays bounded and small.

	• Best fit for I/O write latency
		Color RAM stores only the low nibble, and writes incur I/O wait states. The
		unrolled pattern keeps a steady stream of STA $D8xx,Y without extra ALU work,
		maximizing useful bus cycles during vblank/top-band time.

	• Cache-free determinism
		There is no self-modifying code or ZP pointer churn. Cycle cost per column is
		consistent, which is important when scheduled near raster boundaries.

Trade-offs vs a 16-bit indexed copy:
	* More code bytes, fewer cycles. On a 6502 the cycle savings are worth the
	  bytes for hot paths like frame setup.
	* Hard-coded bases. This is acceptable because the scene area layout is fixed:
	  each successive row is previous +$28 on both source and destination.
	  
================================================================================
*/
* = $1860
copy_vic_color_ram:
        ldy     #VIEW_FULL_SPAN_MINUS1          // Y := 39 (start at last column)

copy_color_column:
        lda     $6D89,y                         // Row 0 source → $D828
        sta     $D828,y
        lda     $6DB1,y                         // Row 1
        sta     $D850,y
        lda     $6DD9,y                         // Row 2
        sta     $D878,y
        lda     $6E01,y                         // Row 3
        sta     $D8A0,y
        lda     $6E29,y                         // Row 4
        sta     $D8C8,y
        lda     $6E51,y                         // Row 5
        sta     $D8F0,y
        lda     $6E79,y                         // Row 6
        sta     $D918,y
        lda     $6EA1,y                         // Row 7
        sta     $D940,y
        lda     $6EC9,y                         // Row 8
        sta     $D968,y
        lda     $6EF1,y                         // Row 9
        sta     $D990,y
        lda     $6F19,y                         // Row 10
        sta     $D9B8,y
        lda     $6F41,y                         // Row 11
        sta     $D9E0,y
        lda     $6F69,y                         // Row 12
        sta     $DA08,y
        lda     $6F91,y                         // Row 13
        sta     $DA30,y
        lda     $6FB9,y                         // Row 14
        sta     $DA58,y
        lda     $6FE1,y                         // Row 15
        sta     $DA80,y
        lda     $7009,y                         // Row 16
        sta     $DAA8,y

        dey                                     // next column (Y := Y-1)
        bpl     copy_color_column               // repeat until Y < 0

        // ------------------------------------------------------------
        // Mark color RAM copy completed
        // ------------------------------------------------------------
        lda     #TRUE
        sta     vic_color_ram_copy_done
        rts

/*
================================================================================
mem_fill_x — write X to memory fill_byte_cnt times starting at dest

  Arguments:
    X      Fill byte to write to each destination address.

  Global Inputs:
    fill_dest_ptr      16-bit base address to start filling (advanced across pages).
    fill_byte_cnt      16-bit number of bytes to write; must be nonzero on entry.

  Global Outputs:
    fill_dest_ptr      Advanced to the first address past the last written byte.
    fill_byte_cnt      Decremented to zero when the routine returns.

  Returns:
    A,Y,flags
      Clobbered; X preserved. Z=1 on return_from_entity_contact (counter reached zero).

  Description:
    - Writes the value from X to *(fill_dest_ptr) and increments fill_dest_ptr
      after each store (handles page crossings via INC low/INC high).
    - Decrements a 16-bit counter and loops until both counter bytes are zero.
    - Y remains 0 the entire time; the base pointer moves instead of Y.

  Notes:
    - Caller must ensure fill_byte_cnt > 0; a zero-length request is invalid.
================================================================================
*/
* = $5D32
mem_fill_x:
        // ------------------------------------------------------------
        // Initialize Y to 0 (use base-pointer incrementing, not Y stepping)
        // ------------------------------------------------------------
        ldy     #$00               // Y stays 0 for (fill_dest_ptr),Y addressing

fill_loop:
        // ------------------------------------------------------------
        // Store fill byte and advance destination pointer (handles page cross)
        // ------------------------------------------------------------
        txa                        // A := fill byte (preserve X)
        sta     (fill_dest_ptr),y  // write A to *fill_dest_ptr
        inc     <fill_dest_ptr     // dest.lo++
        bne     dec_count          // no wrap → skip high-byte increment
        inc     >fill_dest_ptr     // wrapped → dest.hi++

dec_count:
        // ------------------------------------------------------------
        // Decrement 16-bit remaining count (borrow when low byte is 0)
        // ------------------------------------------------------------
        lda     <fill_byte_cnt     // low byte
        bne     dec_count_lo       // if low != 0, no borrow
        dec     >fill_byte_cnt     // borrow: dec high byte
dec_count_lo:
        dec     <fill_byte_cnt     // dec low byte

        // ------------------------------------------------------------
        // Loop until both counter bytes are zero
        // ------------------------------------------------------------
        lda     <fill_byte_cnt     // low
        ora     >fill_byte_cnt     // combine with high; Z=1 iff both zero
        bne     fill_loop          // not done → continue
        rts                        // done (Z=1, Y=0, X preserved)



* = $3C3E

.label candidate_object = $3EA0
.label ancestor_overlay_req = $15
.label check_cursor_contained_within_object = $0
.const  OBJ_SCAN_START_IDX              = $01    // First room object index to test
.const  NO_OBJECT_IDX                   = $00    // X return value when no hit

/*
================================================================================
Summary
    Identify the room object under the current cursor and return its global
    object index. Iterates room objects, filters removed mutable objects,
    resolves parent containment, then dispatches to a bounds hit test.

Arguments
    None (uses globals; cursor position consumed by the hit-test routine)

Returns
    On hit:
        A = obj_idx_hi
        X = obj_idx_lo
    On miss:
        X = #$00  

Description
    • Early-exit if there are no room objects.
    • Scan room indices from 1..room_obj_count inclusive.
    • Immutable objects (hi≠0) are always candidates.
    • Mutable objects are skipped if "removed from room" (attributes bit5).
    • If a parent link exists, compare parent "inside" flag (bit7) against the
      required overlay state; on match, promote parent and re-check to support
      multi-level ancestry.
    • On a candidate, tail-jump to the bounds routine which sets A/X on hit.

Vars/State
    candidate_object         Temp room-object index latched for parent/hit checks.
    ancestor_overlay_req     Temp required overlay/inside state for parent compare.

Global Inputs
    room_obj_count           Total room objects; determines loop bounds.
    room_obj_idx_hi/lo       Map room index → global object index; hi≠0 marks immutable.
    object_attributes        Per-object flags; bit5=removed, bit7=inside-parent.
    parent_idx_tbl           Room-indexed parent link; 0 means no parent.
    ancestor_overlay_req_tbl Required inside/overlay state for containment check.
    (cursor position)        Consumed by check_cursor_contained_within_object.

Global Outputs
    candidate_object         Updated while scanning to hold current candidate.
    ancestor_overlay_req     Written with requirement byte for parent comparison.
================================================================================
*/
find_object_at_cursor:
        // ------------------------------------------------------------
        // Check for presence of room objects
        //
        // - Loads the total room object count.
        // - If count = 0, return immediately (no scan required).
        // - Otherwise, branch to initialization of object scan loop.
        // ------------------------------------------------------------
        ldx     room_obj_count                 // Load room object count; if zero, no scan needed
        bne     init_object_scan               // If X != 0, branch to init; else return
        rts                                    // No objects in room → exit

        // ------------------------------------------------------------
        // Initialize room object scan
        //
        // - Begin iteration at the first valid room object index.
        // - Each loop checks whether the object is immutable (always
        //   present) or mutable (presence depends on attributes).
        // ------------------------------------------------------------
init_object_scan:
        ldx     #OBJ_SCAN_START_IDX            // Initialize scan at first testable room object index

scan_room_objects:
        lda     room_obj_idx_hi,x              // Check immutability: hi≠0 means object is always present
        beq     path_mutable_check             // If hi==0, object is mutable
        jmp     latch_candidate_room_idx       // Immutable → treat as candidate

        // ------------------------------------------------------------
        // Validate mutable object presence
        // - Map room index to global index
        // - Skip if removed-from-room flag (bit5) is set
        // ------------------------------------------------------------
path_mutable_check:
        ldy     room_obj_idx_lo,x              // Map room index X → global object index in Y
        lda     object_attributes,y            // Load global object attributes
        and     #ATTR_REMOVED_FROM_ROOM_MASK   // Mask “removed from room” bit5
        bne     move_to_next_object            // Skip if removed

        // ------------------------------------------------------------
        // Latch candidate room index
        // - Preserve current room object index for parent checks and
        //   bounds testing
        // ------------------------------------------------------------
latch_candidate_room_idx:
        stx     candidate_object               // Cache current room object index as candidate

        // ------------------------------------------------------------
        // Resolve parent containment
        // - Load required overlay/inside state for this object
        // - If a parent exists, confirm its “inside” flag matches the
        //   required state; on match, promote parent and re-check
        // - If no parent, proceed to bounds test
        // ------------------------------------------------------------
resolve_parent_containment:
        lda     ancestor_overlay_req_tbl,x     // Load required parent/overlay state
        sta     ancestor_overlay_req           // Save requirement for comparison
        lda     parent_idx_tbl,x               // Read parent link; zero means no parent
        bne     ancestor_link_present          // If parent exists, validate containment
        jmp     hit_test_candidate_bounds      // No parent → check cursor bounds

        // ------------------------------------------------------------
        // Validate ancestor link and promote if required
        // - Y := parent room index; map to parent’s global index in X
        // - Compare parent “inside” flag (bit7) with required overlay
        // - On mismatch, skip candidate; on match, promote parent and
        //   re-check to support multi-level ancestry
        // ------------------------------------------------------------
ancestor_link_present:
        tay                                    // Y := parent room index
        lda     room_obj_idx_lo,y              // Load parent’s global object index (lo)
        tax                                    // X := parent’s global object index

        lda     object_attributes,x            // Load parent’s attributes
        and     #ATTR_INSIDE_PARENT_MASK       // Isolate parent “inside” flag bit7
        cmp     ancestor_overlay_req           // Compare flag to required overlay state
        bne     skip_due_to_ancestor_req_mismatch // Mismatch → skip this candidate

        tya                                    // Match → promote parent as new subject
        tax                                    // X := parent index
        jmp     resolve_parent_containment     // Recurse to check multi-level ancestry

        // ------------------------------------------------------------
        // Skip candidate on ancestor requirement mismatch
        // - Restore the original candidate room index
        // - Continue scanning with the next room object
        // ------------------------------------------------------------
skip_due_to_ancestor_req_mismatch:
        ldx     candidate_object               // Restore original candidate room index
        jmp     move_to_next_object            // Skip to next object in outer scan

        // ------------------------------------------------------------
        // Hit-test candidate bounds against cursor
        // - Restore candidate room index
        // - Tail-jump to cursor containment test routine
        // ------------------------------------------------------------
hit_test_candidate_bounds:
        ldx     candidate_object               // Restore candidate room index
        jmp     check_cursor_contained_within_object // Jump to cursor-in-bounds test

        // ------------------------------------------------------------
        // Advance to next room object
        // - Increment X and compare with room_obj_count
        // - Inclusive loop: if X == room_obj_count, iterate once more
        // ------------------------------------------------------------
move_to_next_object:
        inx                                    // Increment X (next room object index)
        cpx     room_obj_count                 // Compare with total object count
        bcc     scan_room_objects              // Continue if below limit
        beq     scan_room_objects              // Include final index (equal case)
		
        ldx     #NO_OBJECT_IDX                 // Exhausted all objects → X := #$00
        rts                                    // Return (no hit)
