#importonce
#import "globals.inc"
#import "constants.inc"
#import "render_actor.asm"
/*
Terminology

		- A *limb* is an independently animated body part.
		- A *cel* is one static drawing for a limb (a single frame).
		- A *cel sequence* is an ordered list of cels that form one animation
		  cycle for that limb.
		- A *clip set* (or simply *clip*) combines one cel sequence per limb
		  to create a complete full-body motion or stance, such as "walk" or "stand".
					
*/

// Variables
.label clip_tbl              = $17     	// zp pointer to clip table base (8 bytes per clip, one per limb)
.label actor_limb_temp          = $FC3B   	// alias of prev_limb_frame; temp to preserve X across ops
.label prev_limb_frame 			= $FC3B   	// prior frame index snapshot for change detection
.label target_clip          	= $FDE9   	// target clip id to apply for the active actor/costume
.label limb_index               = $2503   	// limb loop counter 0..7
.label actor_limb_temp_2        = $2502   	// temp storage for computed actor_limb index

// Constants
.const INACTIVE_CEL  			= $FF     	// sentinel: no cel / end-of-sequence
.const INACTIVE_CLIP_SET  		= $FF     	// sentinel: no clip
.const FLIP_SET        			= $80     	// bit7 = 1 → horizontal flip enabled
.const FLIP_CLEAR     			= $00     	// bit7 = 0 → no flip
.const MAX_LIMB_IDX    			= $07     	// highest limb index (8 limbs total)
.const CLIP_SET_LEFT    		= $04     	// standing-left clip id
.const CLIP_SET_RIGHT   		= $05     	// standing-right clip id
.const CLIP_SET_DOWN    		= $06     	// standing-down clip id
.const CLIP_SET_UP      		= $07     	// standing-up clip id
.const DIRECTION_LEFT  			= $01     	// left movement direction code
.const DIRECTION_RIGHT 			= $00     	// right movement direction code
.const DIRECTION_DOWN  			= $80     	// down movement direction code
.const DIRECTION_UP    			= $81     	// up movement direction code
//.const SPEAKING_DISABLED_BIT     = $80   // costume_anim_attrs: bit7=1 → no speaking anim
//.const MOUTH_OPEN_BIT            = $80   // actor_mouth_state: bit7=1 → mouth open
.const CLIP_OFS_MOUTH_CLOSED  	= $10   	// add for closed-mouth variant
.const CLIP_OFS_MOUTH_OPEN    	= $0C   	// add for open-mouth variant
.const CLIP_OFS_DIR_LEFT 		= $00   
.const CLIP_OFS_DIR_RIGHT 		= $01   
.const CLIP_OFS_DIR_UP 			= $03   
.const CLIP_OFS_DIR_DOWN 		= $02    
.const ANIM_LOOP_FOREVER         = $ff   	// clip_loop_cnt value meaning "loop forever"

/*
================================================================================
animate_actor_limbs
================================================================================
Summary:
    Animate all limbs for the active actor.

Arguments:
    active_costume

Returns:
    None

Description:
    - Ensure costume resources are ready.
    - Update speaking animation if needed.
    - Prepare limb cel list and limb offset bookkeeping.
    - Iterate limbs 0..7: update base cel and animate each.
================================================================================
*/
* = $2504
animate_actor_limbs:
        // ------------------------------------------------------------
        // Prepare actor resources and per-limb bookkeeping
        // ------------------------------------------------------------
        jsr     setup_costume_for_actor
        jsr     animate_character_speaking
        jsr     fetch_cel_seq_tbl
        jsr     compute_actor_limbs_ofs
        lda     #$00
        sta     current_limb

animate_actor_limb:
        // ------------------------------------------------------------
        // Animate current limb and advance to next
        // ------------------------------------------------------------
        jsr     update_limb_base_cel
        jsr     animate_limb
        inc     current_limb

        // ------------------------------------------------------------
        // Loop while current_limb < 8 (eight limbs total)
        // ------------------------------------------------------------
        lda     #MAX_LIMB_IDX + 1       // limb count
        cmp     current_limb            // Z=1 when done
        bne     animate_actor_limb
        rts
/*
================================================================================
animate_limb
================================================================================
Summary:
    Advance one limb to its next animation frame, with loop/stop semantics.

Description:
    - Compute limb index = actor_limbs_ofs + current_limb.
    - Bump frame index; probe next cel id from cel_seq_tbl.
    - If next cel == $FF:
        - If arg == $FF → loop from first frame.
        - If arg == $00 → stop; deactivate clips and clear base cels.
        - Else (arg > 0 and != $FF) → decrement arg and loop from first.
    - Write final frame index and, if changed, mark animation dirty (bit0).
================================================================================
*/
* = $2526
animate_limb:
        // ------------------------------------------------------------
        // Compute limb index in X: actor limbs offset + current_limb
        // ------------------------------------------------------------
        lda     actor_limbs_ofs
        clc
        adc     current_limb
        tax

        // ------------------------------------------------------------
        // Snapshot current frame, then increment to candidate next frame
        // ------------------------------------------------------------
        lda     limb_frame_idx,x
        sta     prev_limb_frame
        inc     limb_frame_idx,x

        // ------------------------------------------------------------
        // Lookup cel id for candidate frame; Y = limb_current_cel + frame index
        // ------------------------------------------------------------
        lda     limb_frame_idx,x
        clc
        adc     limb_current_cel,x
        tay
        lda     (cel_seq_tbl),y

        // ------------------------------------------------------------
        // End-of-sequence sentinel? ($FF)
        // ------------------------------------------------------------
        cmp     #$ff
        bne     move_animation_state_if_needed

        // ------------------------------------------------------------
        // Handle loop/stop semantics based on limb_cur_loop_cnt
        // ------------------------------------------------------------
        lda     limb_cur_loop_cnt,x
        beq     stop_animation               // arg == $00 → stop
        cmp     #$ff
        beq     loop_from_first              // arg == $ff → loop
        dec     limb_cur_loop_cnt,x      // countdown looping
loop_from_first:
        lda     #$00
        jmp     set_new_frame

stop_animation:
        // Go back to last valid frame and deactivate clips/base cels
        dec     limb_frame_idx,x
        stx     actor_limb_temp_2

        // Deactivate desired and current animation clips for the actor
        ldx     actor
        lda     #INACTIVE_CLIP_SET
        sta     actor_target_clip,x
        lda     #INACTIVE_CLIP_SET
        sta     actor_current_clip,x

        // Deactivate desired and current base cel for this limb
        ldx     actor_limb_temp_2
        lda     #INACTIVE_CEL
        sta     limb_target_cel_seq,x
        lda     #INACTIVE_CEL
        sta     limb_current_cel_seq,x

        // Load frame after adjustments
        lda     limb_frame_idx,x

set_new_frame:
        sta     limb_frame_idx,x

move_animation_state_if_needed:
        // If frame changed, flag animation refresh (set bit0)
        lda     prev_limb_frame
        cmp     limb_frame_idx,x
        beq     exit_3
        ldx     actor
        lda     actor_animation_state,x
        ora     #$01
        sta     actor_animation_state,x

exit_3:
        rts

/*
================================================================================
update_limb_base_cel
================================================================================
Summary:
    If a limb’s desired base cel changed, commit it and reset the limb state.

Description:
    - Compute limb index = actor_limbs_ofs + current_limb.
    - If desired base cel is $FF, do nothing.
    - If desired equals current, do nothing.
    - Otherwise: copy desired base cel and argument to “current”, recompute
      current cel, reset the limb’s animation frame to 0, and mark animation
      as needing refresh (set bit0 in actor_animation_state).
================================================================================
*/
* = $258E

update_limb_base_cel:
        // ------------------------------------------------------------
        // Compute limb index in X: actor limbs offset + current_limb
        // ------------------------------------------------------------
        lda     actor_limbs_ofs
        clc
        adc     current_limb
        tax

        // ------------------------------------------------------------
        // If desired base cel is inactive → no update
        // ------------------------------------------------------------
        lda     limb_target_cel_seq,x
        cmp     #INACTIVE_CEL
        bne     check_for_new_value
        rts

check_for_new_value:
        // ------------------------------------------------------------
        // Skip if target equals current
        // ------------------------------------------------------------
        cmp     limb_current_cel_seq,x
        bne     assign_new_base_cel
        rts

assign_new_base_cel:
        // ------------------------------------------------------------
        // Commit desired base cel and loop count; recompute current cel
        // ------------------------------------------------------------
        lda     limb_target_cel_seq,x
        sta     limb_current_cel_seq,x
        lda     limb_tgt_loop_cnt,x
        sta     limb_cur_loop_cnt,x
        jsr     compute_limb_current_cel

        // ------------------------------------------------------------
        // Reset limb animation frame and flag actor animation refresh
        // ------------------------------------------------------------
        lda     #$00
        sta     limb_frame_idx,x
        ldx     actor
        lda     actor_animation_state,x
        ora     #$01
        sta     actor_animation_state,x
        rts

/*
================================================================================
compute_limb_current_cel
================================================================================
Summary:
    Compute and cache the current cel index for the selected limb.

Arguments:
    .X = limb index
	
Description:
    - Y := current_limb
    - Read limb base address from cel_seq_tbl[Y]
    - Add limb_current_cel_seq[X] to form cel-list offset
    - Fetch cel index at that offset and store to limb_current_cel[X]
================================================================================
*/
* = $25C3
compute_limb_current_cel:
        // ------------------------------------------------------------
        // limb_current_cel[X] := cel_seq_tbl[ base+offset ]
        // ------------------------------------------------------------
        ldy     current_limb
        lda     (cel_seq_tbl),y
        clc
        adc     limb_current_cel_seq,x
        tay
        lda     (cel_seq_tbl),y
        sta     limb_current_cel,x
        rts

/*
================================================================================
compute_actor_limbs_ofs
================================================================================

Summary
        Compute stride-8 limbs offset for the current actor:
        actor_limbs_ofs = actor_idx * 8

Inputs
        actor                 current actor index (0..N)

Outputs
        actor_limbs_ofs      byte offset used to index per-actor blocks of 8

Description
        Multiplies the actor index by 8 using three ASL operations. The result
        serves as a block stride for tables organized in 8-byte records.
================================================================================
*/
* = $25D3
compute_actor_limbs_ofs:
        lda     actor                  
        clc                            
        asl                            
        asl                            
        asl                            
        sta     actor_limbs_ofs       
        rts

/*
================================================================================
fetch_cel_seq_tbl
================================================================================
Summary:
    Caches the current actor’s cel sequence table into cel_seq_tbl.

Arguments:
    actor

Global Inputs:
    actor_cel_seq_tbl_lo[actor]
    actor_cel_seq_tbl_hi[actor]

Global Outputs:
    cel_seq_tbl

Description:
    - Use actor index to fetch the per-actor cel sequence table pointer.
================================================================================
*/
* = $25DD
fetch_cel_seq_tbl:
        // ------------------------------------------------------------
        // cel_seq_tbl := actor_cel_seq_tbl[actor]
        // ------------------------------------------------------------
        ldx     actor
        lda     actor_cel_seq_tbl_lo,x
        sta     <cel_seq_tbl
        lda     actor_cel_seq_tbl_hi,x
        sta     >cel_seq_tbl
        rts

/*
================================================================================
 setup_clip_set
================================================================================

Summary
	Initialize per-limb desired base cels, flip flags, and desired
	arguments from the actor’s target clip, only when the target clip
	differs from the current clip.

Arguments
	actor                           	current actor index

Vars/State
	actor_target_clip[]          	per-actor target clip id
	actor_current_clip[]         	per-actor active clip id
	actor_clip_loop_cnt[]               per-actor clip loop count
	limb_target_cel_seq[]   		per-(actor,limb) desired base cel
	limb_current_cel_seq[]   		per-(actor,limb) current base cel
	limb_tgt_loop_cnt[]   				per-(actor,limb) target loop count
	limb_flip_tbl[]                 	per-(actor,limb) flip flag ($80 set, $00 clear)
	actor_limbs_ofs                 	actor index * 8

Global Inputs
	actor_clips_{lo,hi}[]   		pointer to clip tables (8 bytes/clip)
	(clip limb entry format: bit7=flip, low7=base cel; $FF means “skip limb”)

Description
	- Ensure costume pointers are initialized.
	- If target clip = INACTIVE, return.
	- Cache clip_tbl pointer for this actor.
	- If target clip equals current clip, return.
	- Set current clip = target clip.
	- Y := clip_set * 8 (first limb entry). Precompute actor_limbs_ofs.
	- For limbs 0..7:
		• Read entry byte at (clip_tbl),Y.
	· If entry == $FF (inactive) → skip limb.
	· Else set desired base cel = entry & $7F; set/clear flip.
	Invalidate current base cel ($FF) when flip state changes.
		• Copy actor_clip_loop_cnt into per-limb loop count.
		• Y++, next limb.
	- Optionally stores (clip_set & 3) at $FD0E,X when clip_set < 8 (kept intact).
============================================================================
*/
* = $25FF
// ----------------------------------------
// Setup and clip selection
// ----------------------------------------
setup_clip_set:
		// Ensure costume resources/pointers are ready
		jsr     setup_costume_for_actor

		// Load target clip for this actor; exit if inactive
		ldx     actor
		lda     actor_target_clip,x
		cmp     #INACTIVE_CLIP_SET
		bne     check_for_new_clip
		rts

		// ----------------------------------------
		// Cache clip table
		// ----------------------------------------
check_for_new_clip:
		lda     actor_clip_tbl_lo,x
		sta     <clip_tbl
		lda     actor_clip_tbl_hi,x
		sta     >clip_tbl

		// ----------------------------------------
		// Exit if clip unchanged
		// ----------------------------------------
		lda     actor_target_clip,x
		cmp     actor_current_clip,x
		bne     commit_new_clip
		rts

		// ----------------------------------------
		// Commit new current clip and prepare indices
		// ----------------------------------------
commit_new_clip:
		sta     actor_current_clip,x        // current ← target

		// Y := clip_set * 8 (8 limb entries per clip)
		asl
		asl
		asl
		tay

		// Compute the offset for _this actor's_ limbs in the global limbs table
		// actor_limbs_ofs := actor * 8
		jsr     compute_actor_limbs_ofs

		// Init limb loop
		ldx     #$00
		stx     limb_index

		// ----------------------------------------
		// Per-limb seeding from clip table
		// ----------------------------------------
prepare_limb_frame:
		// X := actor*8 + limb_index  (per-limb context index)
		clc
		lda     limb_index
		adc     actor_limbs_ofs
		tax

		lda     limb_index // Redundant code, result discarded immediately
		// Read clip limb entry: bit7=flip, low7=base cel; $FF means skip
		lda     (clip_tbl),y
		bpl     bit7_clear


		// bit7_set: flip requested
bit7_set:
		//Inactive limb cel?
		cmp     #INACTIVE_CEL
		beq     advance_limb                   // Inactive → skip this limb


		//Decompose cel sequence index -> target cel sequence
		and     #$7F                        
		sta     limb_target_cel_seq,x

		// If flip was not already set, invalidate the current cel sequence
		lda     limb_flip_tbl,x
		cmp     #FLIP_SET
		beq     set_horizontal_flip
		lda     #INACTIVE_CEL
		sta     limb_current_cel_seq,x


set_horizontal_flip:
		lda     #FLIP_SET
		sta     limb_flip_tbl,x
		jmp     copy_loop_count

		// bit7_clear: no flip requested
bit7_clear:
		//Store cel sequence index -> target cel sequence
		sta     limb_target_cel_seq,x

		// If flip is currently set, invalidate the current cel sequence
		lda     limb_flip_tbl,x
		cmp     #FLIP_CLEAR
		beq     clear_horizontal_flip
		lda     #INACTIVE_CEL
		sta     limb_current_cel_seq,x


clear_horizontal_flip:
		lda     #FLIP_CLEAR
		sta     limb_flip_tbl,x

		// ----------------------------------------
		// Per-limb desired loop count from actor-level loop count
		// ----------------------------------------
copy_loop_count:
		stx     actor_limb_temp              // save limb context index
		ldx     actor
		lda     actor_clip_loop_cnt,x
		ldx     actor_limb_temp              // restore limb context index
		sta     limb_tgt_loop_cnt,x

		// ----------------------------------------
		// Advance to next limb (8 total)
		// ----------------------------------------
advance_limb:
		iny                                   // next clip table entry
		inc     limb_index
		lda     limb_index
		cmp     #MAX_LIMB_IDX + 1
		bne     prepare_limb_frame


		// Optional tail: write (clip_set & 3) when clip_set < 8 (kept as-is)
		ldx     actor
		lda     actor_current_clip,x
		cmp     #$08
		bcs     exit_2
		and     #$03
		sta     $FD0E,x                       // direction code (apparently unused)

exit_2:
		rts
/*
================================================================================
 apply_clip_set
================================================================================

Summary
	Resolve the actor bound to the active costume and set a target clip
	(and argument). If an actor is assigned, update limb base cels and
	derive/store movement direction from the clip. If no actor is assigned,
	accept only standing clips and cache them per costume.

Description
	- Read actor_for_costume[active_costume].
		• If bit7 set (no actor): only accept standing clips in the inclusive
		range [CLIP_SET_LEFT .. CLIP_SET_UP] and write costume_clip_set[].
		• Else (actor assigned):
			· Cache actor index in X and actor.
			· Write actor_target_clip[actor] and actor_clip_loop_cnt[actor].
			· JSR setup_clip_set.
			· Map target_clip to direction:
				LEFT  → DIRECTION_LEFT
				RIGHT → DIRECTION_RIGHT
				DOWN  → DIRECTION_DOWN
				UP    → DIRECTION_UP
				Store in path_direction_for_actor[actor].
================================================================================
*/
* = $2720
apply_clip_set:
		// ------------------------------------------------------------
		// Resolve assigned actor for the active costume; branch if none
		// ------------------------------------------------------------
		ldx     active_costume           	// Load costume index
		lda     actor_for_costume,x      	// Read assigned actor ID for this costume
		bmi     actor_unassigned         	// Bit7 set → no actor assigned for this costume

		// ------------------------------------------------------------
		// Actor assigned: cache actor index and per-actor set/argument
		// ------------------------------------------------------------
		sta     actor                         // Cache actor index for this routine
		tax                                   // X := actor index for per-actor tables
		lda     target_clip               	  // Load requested clip ID
		sta     actor_target_clip,x       	  // Store per-actor target clip
		lda     clip_loop_cnt                 // Load clip argument/variant
		sta     actor_clip_loop_cnt,x         // Store per-actor clip argument

		// ------------------------------------------------------------
		// Init clip set (sets initial cels for each limb)
		// ------------------------------------------------------------
		jsr     setup_clip_set

		// ------------------------------------------------------------
		// Map target clip to path direction
		// $04:left($01), $05:right($00), $06:down($80), $07:up($81)
		// ------------------------------------------------------------
		lda     target_clip              	// Select direction based on clip ID
		cmp     #CLIP_SET_LEFT
		bne     is_clip_right
		lda     #DIRECTION_LEFT              // Move left
		jmp     commit_direction_and_exit

is_clip_right:
		cmp     #CLIP_SET_RIGHT
		bne     is_clip_down
		lda     #DIRECTION_RIGHT             // Move right
		jmp     commit_direction_and_exit

is_clip_down:
		cmp     #CLIP_SET_DOWN
		bne     is_clip_up
		lda     #DIRECTION_DOWN              // Move down
		jmp     commit_direction_and_exit

is_clip_up:
		cmp     #CLIP_SET_UP
		bne     default
		lda     #DIRECTION_UP                // Move up
		jmp     commit_direction_and_exit

default:
		rts                                  // No direction change for other clips

		// ------------------------------------------------------------
		// Store computed direction for this actor and exit
		// ------------------------------------------------------------
commit_direction_and_exit:
		ldx     actor                        // X := actor index for per-actor tables
		sta     path_direction_for_actor,x   // Commit movement direction code for this actor
		jmp     exit_4                         // Join common epilogue

		// ------------------------------------------------------------
		// Actor unassigned: accept only standing sets $04..$07
		// ------------------------------------------------------------
actor_unassigned:
		lda     target_clip            	// Range-check for standing clips [$04..$07]
		cmp     #CLIP_SET_LEFT
		bcc     exit_4                      	// < $04 → not standing → ignore
		cmp     #CLIP_SET_UP
		beq     set_standing_clip_for_costume    // == $07 → accept standing
		bcs     exit_4                      	// > $07 → not standing → ignore
		
		// ------------------------------------------------------------
		// Cache standing set per costume and exit
		// ------------------------------------------------------------
set_standing_clip_for_costume:
		ldx     active_costume           	// X := costume index
		sta     costume_clip_set,x 			// Store standing clip ID for this costume
		
exit_4:
		rts

/*
================================================================================
path_to_clip_direction
================================================================================
Summary:
    Map an actor’s movement direction to a direction clip modifier.

Arguments:
    .X = actor

Returns:
    .A = clip modifier (0..3)

Global Inputs:
    path_direction_for_actor[actor]

Description:
    Direction → Modifier:
        $01 (left)  → $00
        $00 (right) → $01
        $80 (down)  → $02
        $81 (up)    → $03
================================================================================
*/
* = $277E
path_to_clip_direction:
        lda     path_direction_for_actor,x
        cmp     #DIRECTION_LEFT
        bne     check_right
        ldy     #CLIP_OFS_DIR_LEFT

check_right:
        cmp     #DIRECTION_RIGHT
        bne     check_down
        ldy     #CLIP_OFS_DIR_RIGHT

check_down:
        cmp     #DIRECTION_DOWN
        bne     check_up
        ldy     #CLIP_OFS_DIR_DOWN

check_up:
        cmp     #DIRECTION_UP
        bne     return
        ldy     #CLIP_OFS_DIR_UP

return:
        tya
        rts

/*
================================================================================
animate_character_speaking
================================================================================
Summary:
    Select and apply the speaking animation variant for the active actor.

Description:
    - If the actor’s costume lacks a speaking animation, exit.
    - Compute the base animation set from the actor’s facing direction.
    - Choose mouth variant: add +$10 for closed, +$0C for open.
    - Store as desired clip, set no-loop argument, and apply the set.
================================================================================
*/
* = $26F9
animate_character_speaking:
        // ------------------------------------------------------------
        // If costume supports speaking (bit7 clear), continue; else exit
        // N from LDA reflects bit7; BPL → bit7=0 → anim available
        // ------------------------------------------------------------
        ldx     active_costume
        lda     costume_anim_attrs,x
        bpl     speaking_anim_enabled
        rts

speaking_anim_enabled:
        // ------------------------------------------------------------
        // Get clip modifier for current facing direction; A := 0..3
        // ------------------------------------------------------------
        ldx     actor
        jsr     path_to_clip_direction

        // ------------------------------------------------------------
        // Mouth variant: closed (+$10) if bit7 clear, open (+$0C) if set
        // Y used only for the sign test; value not otherwise consumed
        // ------------------------------------------------------------
        clc                                 // prep for mouth offset add
        ldy     actor_mouth_state,x     	// bit7 set → open
        bmi     mouth_open

mouth_closed:
        adc     #CLIP_OFS_MOUTH_CLOSED                        
        jmp     apply_speaking_clip

mouth_open:
        adc     #CLIP_OFS_MOUTH_OPEN                        

apply_speaking_clip:
        sta     target_clip              // select direction+mouth clip id
        lda     #ANIM_LOOP_FOREVER            // loop forever
        sta     clip_loop_cnt
        jsr     apply_clip_set               // commit clip change
        rts
