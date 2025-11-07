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


