#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "actor_animation.asm"
#import "pathing.asm"
#import "actor.asm"

.label destination_obj_hi              = $FE50   // destination object type byte (hi); nonzero means object context
.label destination_obj_lo              = $FE4F   // destination object index (lo); 0..255
.label saved_facing_clip_id            = $0FC8   // mover’s last selected facing clip id
.label destination_entity              = $FE51   // destination entity type: $02=actor, else object
.label saved_param_y                   = $0FC9   // dispatcher-saved Y param (restored on return)
.label snap_probe_x                    = $FE72   // snap input X (pixels) for walkbox clamp
.label snap_probe_y                    = $FE73   // snap input Y (pixels) for walkbox clamp
.label actor_approach_x_offset_pos_byte = $0F65  // writable LDA #imm operand for +X approach offset
.label actor_approach_x_offset_neg_byte = $0F6A  // writable LDA #imm operand for −X approach offset
.label room_destination               = $FDEA   // target room id for the teleport or walk command; 0 means none
.label unused_var_1                   = $26F7   // debug storage: last written destination X coordinate (pixels)
.label unused_var_2                   = $26F8   // debug storage: last written destination Y coordinate (pixels)

.const CLIP_LR_BIT                     = $01     // clip id bit0 toggles left/right
.const LOOP_ONE_SHOT                   = $00     // play clip once (no loop)
.const ACTOR_APPROACH_X_OFFSET_POS     = $04     // +4 px lateral offset when approaching actor
.const ACTOR_APPROACH_X_OFFSET_NEG     = $FC     // −4 px lateral offset when approaching actor

/*
================================================================================
apply_facing_toward_destination
================================================================================

Summary:
    Set up the “walking to destination” facing animation. If the destination is
    another actor, mirror the facing so the standing actor faces the walking one.

Global Inputs:
    destination_obj_hi           Destination object type (hi byte)
    destination_obj_lo           Destination object index (lo byte)
    current_kid_idx                  Costume/actor initiating the facing

Global Outputs:
    saved_facing_clip_id  Saved base facing clip id
    target_clip_id               Clip id to apply
    clip_loop_cnt                Loop count for clip (0 = one-shot)
    active_costume               Active costume to apply clip on

Description:
    • Query the facing clip toward the destination and apply it to current_kid_idx.
    • If the destination is an actor (type $02) and not the plant (id $13),
      mirror the clip’s left/right bit and apply it to that destination actor.
================================================================================
*/
* = $0D49
apply_facing_toward_destination:
        // ------------------------------------------------------------
        // Get facing clip toward destination and apply to current_kid_idx
        // ------------------------------------------------------------
        jsr     select_facing_clip_for_destination   // A := facing clip id
        sta     saved_facing_clip_id                // save base clip
        sta     target_clip_id                             // set target clip

        lda     #LOOP_ONE_SHOT                             // one-shot playback
        sta     clip_loop_cnt

        lda     current_kid_idx                                // select standing kid
        sta     active_costume
        jsr     apply_clip_set                        	   // apply facing clip

        // ------------------------------------------------------------
        // If destination is not a costume, done
        // ------------------------------------------------------------
        lda     destination_obj_hi
        cmp     #OBJ_TYPE_ACTOR                            // object type: costume
        bne     done_1

        // ------------------------------------------------------------
        // Destination is a costume: select it and handle special case
        // ------------------------------------------------------------
        lda     destination_obj_lo                         // destination costume id
        sta     active_costume
        cmp     #COSTUME_ID_PLANT                          // plant id → skip mirroring
        beq     done_1

        // ------------------------------------------------------------
        // Mirror left/right: flip bit0 of the base clip id
        // ------------------------------------------------------------
        lda     saved_facing_clip_id
        eor     #CLIP_LR_BIT                               // toggle L/R variant
        sta     target_clip_id

        lda     #LOOP_ONE_SHOT                             // one-shot playback
        sta     clip_loop_cnt
        jsr     apply_clip_set	                           // apply to destination actor

done_1:
        rts
/*
================================================================================
select_facing_clip_for_destination
================================================================================

Summary:
    Returns the facing clip id the current kid should use before walking toward
    the destination. If the destination is an actor, face left/right based on
    relative X. If it is an object, return its explicit clip if present, else
    default to “stand down”.

Returns:
    .A     Clip id to use (e.g., STAND_LEFT, STAND_RIGHT, STAND_DOWN)

Global Inputs:
     destination_obj_hi               destination type (hi byte)
     destination_obj_lo               destination index (lo byte)
     current_kid_idx                      active kid costume index
     actor_for_costume                table: costume → actor index
     actor_x_pos                      table: actor → X position
     destination_entity               object index or $FF
     object_destination_active        table: object → explicit clip id or 0

Global Outputs:
     dest_x                           temp: destination X (pixels)

Description:
    • If destination is an actor:
        - Compare current kid’s X with destination actor’s X.
        - If kid_x ≤ dest_x → return STAND_RIGHT; else STAND_LEFT.
    • If destination is an object:
        - If destination_entity == $FF → return STAND_DOWN.
        - If object has explicit clip → return it; else STAND_DOWN.

================================================================================
*/
* = $0D82
select_facing_clip_for_destination:
        // ------------------------------------------------------------
        // Branch on destination type: costume vs object
        // ------------------------------------------------------------
        lda     destination_obj_hi
        cmp     #OBJ_TYPE_ACTOR
        bne     target_is_object

		// --------------------------------------------------------------------
		// Destination is a costume: face left/right based on relative X
		// --------------------------------------------------------------------
        // Load destination costume’s X into dest_x
        ldx     destination_obj_lo
        lda     actor_for_costume,x
        tax
        lda     actor_x_pos,x
        sta     dest_x

        // Load current kid’s X into A
        ldx     current_kid_idx
        lda     actor_for_costume,x
        tax
        lda     actor_x_pos,x

        // If kid_x ≤ dest_x → face right; else left
        cmp     dest_x
        bcc     return_stand_right
        beq     return_stand_right

        lda     #CLIP_STAND_LEFT
        jmp     sfcfd_exit                                  

return_stand_right:
        lda     #CLIP_STAND_RIGHT
        jmp     sfcfd_exit                                  

		// --------------------------------------------------------------------
		// Destination is an object: explicit clip or default to “down”
		// --------------------------------------------------------------------
target_is_object:
        ldy     destination_entity
        cpy     #$FF
        beq     return_stand_down                 // unknown object → down

        lda     object_destination_active,y       // explicit clip?
		bne		sfcfd_exit_2
        lda     #CLIP_STAND_DOWN
sfcfd_exit_2:		
        jmp     sfcfd_exit                            // nonzero → use it

return_stand_down:
        lda     #CLIP_STAND_DOWN
sfcfd_exit:
        rts
/*
================================================================================
 Set destination toward an entity (actor or object)
================================================================================

Summary
	Routes destination setup based on entity type in A. Initializes approach
	offsets for actor targets and preserves the caller's Y.

Arguments
	.A  Destination entity type (#$02 = actor; any other = object)
	.X  If actor: destination costume
		If object: moving costume
	.Y  If actor: moving costume
		If object: destination object index

Global Inputs
	current_kid_idx                    player-controlled actor index

Global Outputs
	actor_approach_x_offset_pos_byte  +X approach offset for actor targets
	actor_approach_x_offset_neg_byte  −X approach offset for actor targets

Description
	- Save incoming Y to a temp slot.
	- If A != #$02, branch to object-handling routine.
	- For actor targets, set ±4 approach offsets and load Y := current_kid_idx,
	then fall through to the actor-specific handler.

Notes
	Any fall-through code that needs the caller's original Y must read saved_param_y.
==========================================================================
*/
* = $0F32
route_destination_by_entity_type:
		sty     saved_param_y

		// Decide route: object vs actor based on A
		cmp     #OBJ_TYPE_ACTOR
		bne     set_actor_destination_to_object

		// Actor target: set approach offsets
		lda     #ACTOR_APPROACH_X_OFFSET_POS
		sta     actor_approach_x_offset_pos_byte
		lda     #ACTOR_APPROACH_X_OFFSET_NEG
		sta     actor_approach_x_offset_neg_byte

		// Use current_kid_idx as working Y, then fall through to actor handler
		ldy     current_kid_idx
		// fall through to set_actor_destination_adjacent_to_actor
/*
==========================================================================
  Set destination for actor
==========================================================================

Summary
	Compute a nearby destination next to a target actor. Choose left or right
	side based on relative X, then offset the target’s X by a small amount and
	clamp later.

Arguments
	.X  Destination costume id
	.Y  Moving costume id

Global Inputs
	actor_for_costume[]             map costume → actor index
	actor_x_pos[]                   actor X positions
	actor_y_pos[]                   actor Y positions

Global Outputs
	dest_x                          destination X (pixels)
	dest_y                          destination Y (pixels)

Description
	- Convert both costume ids to actor indices.
	- Copy target actor’s position to dest_{x,y}.
	- If mover_x ≤ target_x, bias to the left; else to the right.
	- Add a signed horizontal offset to dest_x.
	- Tail-jump to boundary adjustment.

Notes
	Offsets are self-modifiable LDA immediates. Other code writes +4 and −4 to
	the operand bytes at the labeled addresses before this runs.
==========================================================================
*/
* = $0F46
set_actor_destination_adjacent_to_actor:
		// ------------------------------------------------------------
		// Map costumes → actor indices in Y (mover) and X (target)
		// ------------------------------------------------------------
		lda     actor_for_costume,y
		tay
		lda     actor_for_costume,x
		tax

		// ------------------------------------------------------------
		// Copy target actor position into dest_y and dest_x
		// ------------------------------------------------------------
		lda     actor_y_pos,x
		sta     dest_y
		lda     actor_x_pos,x
		sta     dest_x

		// ------------------------------------------------------------
		// Decide approach side using mover_x vs dest_x
		//   mover_x ≤ dest_x → stand to the left  (negative offset)
		//   otherwise        → stand to the right (positive offset)
		// ------------------------------------------------------------
		lda     actor_x_pos,y
		cmp     dest_x
		bcc     approach_from_left_or_equal
		beq     approach_from_left_or_equal

		// ------------------------------------------------------------
		// Apply positive offset (+ACTOR_APPROACH_X_OFFSET_POS)
		// ------------------------------------------------------------
		lda     #$04
		jmp     add_offset_to_destination

approach_from_left_or_equal:
		// ------------------------------------------------------------
		// Apply negative offset (−ACTOR_APPROACH_X_OFFSET_NEG)
		// ------------------------------------------------------------
		lda     #$FC

add_offset_to_destination:
		// ------------------------------------------------------------
		// dest_x := dest_x + signed(offset)
		// Boundary correction happens in snap_destination_to_walkbox
		// ------------------------------------------------------------
		clc
		adc     dest_x
		sta     dest_x

		// ------------------------------------------------------------
		// Clamp or adjust by walkbox/bounds, no return here
		// ------------------------------------------------------------
		jmp     snap_destination_to_walkbox
/*
==========================================================================
  Set destination for object
==========================================================================

Summary
	Choose a destination point for an actor to approach an object. If the object
	has explicit destination coordinates, use them. Otherwise target the center
	of the object’s box. Convert object Y units to pixels.

Arguments
	.X  Actor index to move
	.Y  Destination object index

Global Inputs
	object_destination_active[]      nonzero → explicit destination present
	obj_left_col_tbl[]               object left in pixels
	obj_top_row_tbl[]                object top in 4-px rows
	obj_width_tbl[]                  object width in pixels
	obj_height_tbl[]                 object height in 4-px rows
	object_x_destination[]           explicit X in pixels
	object_y_destination[]           explicit Y in 4-px rows

Global Outputs
	dest_x                           destination X (pixels)
	dest_y                           destination Y (pixels)

Description
	- If explicit destination is active, copy X and Y (shifting Y by 2 to get
	pixels) and fall through to boundary adjustment.
	- Else compute center of the object box:
	dest_x := x_start + (width >> 1)
	dest_y := (y_start + (height >> 1)) << 2
	then jump to boundary adjustment.

Notes
	Object Y values are stored in 4-pixel rows; two ASLs convert to pixels.
==========================================================================
*/
* = $0F75
set_actor_destination_to_object:
		// ------------------------------------------------------------
		// Check for explicit destination coordinates on the object
		// ------------------------------------------------------------
		lda     object_destination_active,y
		bne     explicit_destination

		// ------------------------------------------------------------
		// No explicit destination: compute center of the object box
		//   dest_x := x_start + (width >> 1)
		//   dest_y := (y_start + (height >> 1)) << 2
		// ------------------------------------------------------------
no_explicit_destination:
		lda     obj_width_tbl,y
		lsr     
		clc
		adc     obj_left_col_tbl,y
		sta     dest_x

		lda     obj_height_tbl,y
		lsr     
		clc
		adc     obj_top_row_tbl,y
		asl     
		asl     
		sta     dest_y
		jmp     snap_destination_to_walkbox

		// ------------------------------------------------------------
		// Explicit destination present: use it
		//   X in pixels, Y in 4-px rows → convert Y to pixels
		// ------------------------------------------------------------

explicit_destination:
		lda     object_x_destination,y
		sta     dest_x
		lda     object_y_destination,y
		asl     
		asl     
		sta     dest_y
		// fall through to snap_destination_to_walkbox
/*
==========================================================================
  Snap destination to walkbox boundaries
==========================================================================

Summary
	Snap dest_x/dest_y to a valid walkable point in the current room’s walkbox
	set, then restore the caller’s Y and return.

Returns
	.Y  Restored from saved parameter (saved_param_y)

Global Inputs
	dest_x                          tentative destination X (pixels)
	dest_y                          tentative destination Y (pixels)
	current_room                    active room id for walkbox selection

Global Outputs
	dest_x                          snapped destination X (pixels)
	dest_y                          snapped destination Y (pixels)
	walkbox_room                    room id provided to the snap routine

Description
	- Copy dest_{x,y} into scratch probe coords.
	- Provide current_room to walkbox logic via walkbox_room.
	- Call snap_coords_to_walkbox to clamp/snap inside valid walkboxes.
	- Copy snapped coords back to dest_{x,y}.
	- Restore .Y from saved_param_y and return.

Notes
	Flags on return reflect LDY saved_param_y. .X is preserved.
==========================================================================
*/
* = $0FA3
snap_destination_to_walkbox:
		// Copy destination coords into snap probe
		lda     dest_x
		sta     snap_probe_x
		lda     dest_y
		sta     snap_probe_y

		// Provide current room to walkbox snapping
		lda     current_room
		sta     walkbox_room

		// Snap probe coords to nearest valid walkbox point
		jsr     snap_coords_to_walkbox

		// Write back snapped coords to destination
		lda     snap_probe_x
		sta     dest_x
		lda     snap_probe_y
		sta     dest_y

		// Restore caller’s Y and return
		ldy     saved_param_y
		rts
/*
==========================================================================
  Teleport costume to destination
==========================================================================

Summary
	Move the active costume instantly to (dest_x, dest_y) in room_destination.
	If an actor is currently assigned, update its coords, set default facing,
	then unassign. If the destination is the current nonzero room, assign an
	available actor to the costume.

Global Inputs
	active_costume                  index of costume being teleported
	room_destination                target room id (0 = none)
	dest_x, dest_y                  target coords in pixels
	actor_for_costume[]             map costume → actor index
	actor_x_pos[], actor_y_pos[]    actor coordinates
	current_room                    id of the current room

Global Outputs
	costume_room_idx[]              updated to room_destination
	costume_dest_x[], costume_dest_y[]   updated to dest_{x,y}
	actor_x_pos[], actor_y_pos[]    updated if an actor was assigned
	path_direction_for_actor[]      set to default “down” on teleport
	(actor assignment)              unassigned then conditionally re-assigned

Description
	- Copy room_destination and dest_{x,y} into the active costume slots.
	- If an actor is assigned to this costume and is not already at dest, copy
	coords into the actor, set default facing, and unassign it.
	- If room_destination != 0 and equals current_room, assign any available
	actor to the costume.

Notes
	Assumes dest_{x,y} were already validated/snapped by the caller. Two
	write-only bytes persist the last dest_{x,y} for debugging.
==========================================================================
*/
* = $269C
teleport_costume_to_destination:
		// Copy target room into costume state for the active costume
		ldx     active_costume
		lda     room_destination
		sta     costume_room_idx,x

		// Copy target coordinates into costume destination slots
		lda     dest_x
		sta     costume_dest_x,x
		lda     dest_y
		sta     costume_dest_y,x

		// Debug capture of last destination (write-only)
		lda     dest_x
		sta     unused_var_1
		lda     dest_y
		sta     unused_var_2

		// If an actor is assigned to this costume, Y := actor index; else branch
		ldy     actor_for_costume,x
		bmi     reassign_if_in_current_room

		// Early exit if actor already at destination
		lda     costume_dest_x,x
		cmp     actor_x_pos,y
		bne     teleport_actor
		lda     costume_dest_y,x
		cmp     actor_y_pos,y
		beq     done_3

teleport_actor:
		// Teleport actor to costume’s destination and set default facing “down”
		lda     costume_dest_x,x
		sta     actor_x_pos,y
		lda     costume_dest_y,x
		sta     actor_y_pos,y
		lda     #DIRECTION_DOWN
		sta     path_direction_for_actor,y

		// Unassign the actor from this costume (will reassign if needed below)
		jsr     detach_actor_from_costume

reassign_if_in_current_room:
		// If destination room is zero or not the current room, done
		lda     room_destination
		beq     done_3
		cmp     current_room
		bne     done_3

		// Destination is the current room: assign any available actor
		ldx     active_costume
		jsr     assign_costume_to_free_actor

done_3:
		rts
