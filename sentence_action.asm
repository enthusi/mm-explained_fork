#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sentence_text.asm"
#import "destination.asm"

.const SENT_QUEUE_MAX_TOKENS      = $06    // Max number of tokens allowed in the sentence queue
.const SENT_QUEUE_EMPTY_IDX       = $FF    // Sentinel value: queue has no active entries
.const DEFAULT_VERB_WALK_TO       = $0D    // Default “Walk to” verb ID
.const REFRESH_UI_FLAG_ON         = $01    // Nonzero → force sentence bar redraw
.const ENTITY_NONE                = $00    // No destination entity assigned

.const SECOND_KID_NAME_COLUMN = $0B
.const THIRD_KID_NAME_COLUMN = $18


.label find_actor_enclosing_cursor = $0
.label get_object_enclosing_cursor = $0
.label set_script_resource_base_address = $0
.label set_current_script_read_address = $0
.label execute_next_operation = $0

//Queued sentence parts (verbs, direct objects, indirect objects, prepositions)
//Each queue has a maximum size of 6 elements
.label queued_verb_ids = $fe25
.label queued_do_id_lo = $fe2b
.label queued_do_id_hi = $fe31
.label queued_prep_ids = $fe37
.label queued_io_id_lo = $fe3d
.label queued_io_id_hi = $fe43

.label active_verb_id = $fe19
.label active_do_id_lo = $fe1a
.label active_do_id_hi = $fe1b
.label active_prep_id = $fe1c
.label active_io_id_lo = $fe1d
.label active_io_id_hi = $fe1e
.label sentence_parts = $fe1f

.label temp_x = $0e70         // Temp storage for X across call
.label temp_y = $0e71         // Temp storage for Y across call

.const ARG_IS_DO          = $01    // Selector: A==#$01 → direct object, else indirect
.const ARG_IS_IO          = $02    
.const CLICK_TRIGGER_SET        = $01    // Force-run a sentence (forced_sentence_trigger)
.const CLICK_TRIGGER_CLEARED    = $FF    // Cleared state after forced trigger handled
.const UI_REFRESH_REQUEST       = $01    // Request redraw (sentence_bar_needs_refresh)
.const REBUILD_SENTENCE_ON      = $01    // Rebuild action sentence now (needs_sentence_rebuild)
.const OBJ_IDX_NONE             = $00    // No object selected (lo byte sentinel)
.const PREPOSITION_NONE         = $00    // No preposition selected
.const SCRIPT_SLOT_NONE              = $FF    // No script running sentinel
.const GLOBAL_DEFAULTS_SCRIPT_ID     = $03    // Global “verb defaults” script number

* = $077C
process_sentence:
        // ------------------------------------------------------------
        // Process and execute a sentence as needed.
        //
        // - Resets sentence parts when flagged.
        // - Restricts verbs when kid is dead or in a radiation suit.
        // - Handles different control modes (cutscene, keypad, normal).
        // - Triggers sentence completion or updates DO/IO/prepositions
        //   based on cursor hits.
        // ------------------------------------------------------------

        lda     init_sentence_ui_flag
        beq     enforce_verb_limits_if_incapacitated

        // ------------------------------------------------------------
        // Reset sentence parts if required
        // ------------------------------------------------------------
        lda     preposition
        beq     apply_pending_sentence_reset            // no preposition → partial reset only

        lda     #$00
        sta     preposition
        sta     indirect_object_idx_lo
        sta     indirect_object_idx_hi
        sta     direct_object_idx_lo
        sta     direct_object_idx_hi

apply_pending_sentence_reset:
        sta     init_sentence_ui_flag // clear reset flag

        // ------------------------------------------------------------
        // Limit verbs if the current kid is dead or in a radiation suit
        // ------------------------------------------------------------
enforce_verb_limits_if_incapacitated:
        ldx     current_kid_idx
        lda     actor_vars,x
        bpl     dispatch_by_control_mode              // positive → alive/normal
        lda     current_verb_id
        cmp     #NEW_KID_VERB
        beq     dispatch_by_control_mode
        lda     #$00
        sta     current_verb_id                    // restrict to “new kid” only

        // ------------------------------------------------------------
        // Control mode dispatch
        // ------------------------------------------------------------
dispatch_by_control_mode:
        lda     control_mode
        cmp     #CUTSCENE_CONTROL_MODE
        bne     handle_keypad_mode_or_fallthrough
        jmp     run_sentence_if_complete         // cutscene mode

handle_keypad_mode_or_fallthrough:
        cmp     #KEYPAD_CONTROL_MODE
        bne     handle_normal_mode

        // keypad mode → force “Push” verb
        lda     #PUSH_VERB
        sta     current_verb_id

        // ------------------------------------------------------------
        // Normal control mode
        // ------------------------------------------------------------
handle_normal_mode:
        lda     current_verb_id
        cmp     #WHAT_IS_VERB
        bne     handle_click_trigger_or_defer
        lda     #CLICK_TRIGGER_SET
        sta     forced_sentence_trigger           // “What is” → force sentence trigger

handle_click_trigger_or_defer:
        lda     forced_sentence_trigger
        cmp     #CLICK_TRIGGER_SET
        beq     on_forced_trigger
        jmp     run_sentence_if_complete          // no forced trigger → check normally

        // ------------------------------------------------------------
        // Forced-trigger path
        // ------------------------------------------------------------
on_forced_trigger:
        lda     #CLICK_TRIGGER_CLEARED
        sta     forced_sentence_trigger            // clear trigger flag

        lda     current_verb_id
        cmp     #GIVE_VERB
        bne     pick_object_under_cursor

        // “Give” requires an actor as IO
        lda     preposition
        beq     pick_object_under_cursor          // no preposition yet → get DO
        jsr     find_actor_enclosing_cursor       // preposition set → find actor (IO)
        jmp     branch_on_cursor_hit

pick_object_under_cursor:
        jsr     get_object_enclosing_cursor       // returns object in X/A

        // ------------------------------------------------------------
        // No object found under cursor
        // ------------------------------------------------------------
branch_on_cursor_hit:
        cpx     #OBJ_IDX_NONE
        bne     cursor_hit_object_found

        lda     current_verb_id
        cmp     #WALK_TO_VERB
        bne     finalize_after_walkto_nohit

        // “Walk to” without object → clear parts
        lda     #OBJ_IDX_NONE
        sta     direct_object_idx_lo
        sta     preposition
        sta     indirect_object_idx_lo

finalize_after_walkto_nohit:
        jmp     finalize_and_maybe_execute

        // ------------------------------------------------------------
        // Object found under cursor
        // ------------------------------------------------------------
cursor_hit_object_found:
        ldy     preposition
        beq     update_or_confirm_direct_object       // no preposition → DO selection

        // ------------------------------------------------------------
        // Handle indirect object selection
        // ------------------------------------------------------------
        cpx     indirect_object_idx_lo
        bne     guard_reject_do_eq_io
        cmp     indirect_object_idx_hi
        bne     guard_reject_do_eq_io
        ldy     #REBUILD_SENTENCE_ON
        sty     needs_sentence_rebuild           // same IO → trigger rebuild

guard_reject_do_eq_io:
        cpx     direct_object_idx_lo
        bne     commit_new_indirect_object
        cmp     direct_object_idx_hi
        beq     finalize_after_io_update             // reject DO == IO

commit_new_indirect_object:
        stx     indirect_object_idx_lo
        sta     indirect_object_idx_hi

        lda     control_mode
        cmp     #KEYPAD_CONTROL_MODE
        bne     finalize_after_io_update
        lda     #REBUILD_SENTENCE_ON
        sta     needs_sentence_rebuild           // keypad mode → rebuild manually

finalize_after_io_update:
        jmp     finalize_and_maybe_execute

        // ------------------------------------------------------------
        // Handle direct object selection
        // ------------------------------------------------------------
update_or_confirm_direct_object:
        cpx     direct_object_idx_lo
        bne     commit_new_direct_object
        cmp     direct_object_idx_hi
        bne     commit_new_direct_object
        ldy     #REBUILD_SENTENCE_ON
        sty     needs_sentence_rebuild           // same DO → rebuild

commit_new_direct_object:
        stx     direct_object_idx_lo
        sta     direct_object_idx_hi
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh         // mark UI refresh

        lda     control_mode
        cmp     #KEYPAD_CONTROL_MODE
        bne     finalize_and_maybe_execute
        lda     #REBUILD_SENTENCE_ON
        sta     needs_sentence_rebuild

        // ------------------------------------------------------------
        // Finalize sentence processing and run if complete
        // ------------------------------------------------------------
finalize_and_maybe_execute:
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh         // refresh UI

        lda     current_verb_id
        cmp     #WALK_TO_VERB
        bne     run_sentence_if_complete

        lda     #ENTITY_NONE
        sta     destination_entity
        lda     #REBUILD_SENTENCE_ON
        sta     needs_sentence_rebuild
		//Fall through to run_sentence_if_complete


* = $0AF0
refresh_sentence_bar_trampoline:
		jmp refresh_sentence_bar

* = $0874
run_sentence_if_complete:
        // ------------------------------------------------------------
        // Run a sentence if all required parts are available.
        //
        // - If the verb has all necessary components → execute via dispatch_or_enqueue_action.
        // - If parts are missing → refresh the sentence bar and exit.
        // - Special cases handled:
        //      NEW_KID_VERB  → delegate to handle_new_kid_verb
        //      WALK_TO_VERB  → executes even without direct object
        // ------------------------------------------------------------
        jsr     refresh_sentence_bar             // update UI before checks

        lda     needs_sentence_rebuild
        beq     check_verb_validity              // skip if no rebuild needed
        lda     #FALSE
        sta     needs_sentence_rebuild          // clear rebuild flag

check_verb_validity:
        lda     current_verb_id
        bne     dispatch_new_kid_or_next
        jmp     refresh_sentence_bar_trampoline         // no verb → refresh and exit

        // ------------------------------------------------------------
        // Handle "New Kid" verb
        // ------------------------------------------------------------
dispatch_new_kid_or_next:
        cmp     #NEW_KID_VERB
        bne     dispatch_walkto_or_need_do
        jmp     handle_new_kid_verb              // delegate to special handler

        // ------------------------------------------------------------
        // Handle "Walk to" verb (requires no object)
        // ------------------------------------------------------------
dispatch_walkto_or_need_do:
        cmp     #WALK_TO_VERB
        bne     require_direct_object_or_refresh
        jmp     dispatch_or_enqueue_action                   // can execute immediately

        // ------------------------------------------------------------
        // Handle verbs requiring a direct object
        // ------------------------------------------------------------
require_direct_object_or_refresh:
        lda     direct_object_idx_lo
        bne     resolve_preposition_and_io
        jmp     refresh_sentence_bar_trampoline         // no DO → refresh and exit

        // ------------------------------------------------------------
        // Preposition and indirect object logic
        // ------------------------------------------------------------
resolve_preposition_and_io:
        lda     preposition
        beq     select_preposition_for_current_verb_id    // determine preposition if missing
        lda     indirect_object_idx_lo
        bne     commit_execute_sentence
        jmp     refresh_sentence_bar_trampoline         // no IO → refresh and exit

        // ------------------------------------------------------------
        // All parts ready → execute
        // ------------------------------------------------------------
commit_execute_sentence:
        jmp     dispatch_or_enqueue_action

        // ------------------------------------------------------------
        // Preposition determination path
        // ------------------------------------------------------------
select_preposition_for_current_verb_id:
        jsr     select_preposition_for_verb
        bne     save_preposition_and_request_ui_refresh
        jmp     dispatch_or_enqueue_action                   // no prep required → execute

save_preposition_and_request_ui_refresh:
        sta     preposition                      // save selected preposition
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh        // request UI refresh
        // fall through to refresh_sentence_bar

* = $099B
handle_new_kid_verb:
        // ------------------------------------------------------------
        // Handle "New kid" verb selection.
        //
        // - Only allowed in control_mode == NORMAL_CONTROL_MODE.
        // - Maps cursor X position on sentence bar to one of three kids.
        // - Refreshes the UI and invokes validation for kid change.
        // ------------------------------------------------------------
        lda     control_mode
        cmp     #NORMAL_CONTROL_MODE
        bne     set_walk_and_exit                 // proceed if kid change allowed

        // ------------------------------------------------------------
        // Determine which kid was clicked
        // ------------------------------------------------------------
select_by_cursor:
        lda     cursor_x_pos_quarter_relative
        cmp     #SECOND_KID_NAME_COLUMN
        bcs     third_kid_check                  // ≥ second column → maybe kid1 or kid2

        // first kid
        lda     #$00
        jmp     select_kid

third_kid_check:
        cmp     #THIRD_KID_NAME_COLUMN
        bcc     second_kid_selected              // < third → kid1
        beq     second_kid_selected              // = third → kid1
        lda     #$02                             // > third → kid2
        jmp     select_kid

second_kid_selected:
        lda     #$01                             // kid1

        // ------------------------------------------------------------
        // Commit kid change
        // ------------------------------------------------------------
select_kid:
        pha                                      // save kid index
        lda     #WALK_TO_VERB
        sta     current_verb_id                     // reset verb to “Walk to”
        lda     #REFRESH_UI_FLAG_ON
        sta     sentence_bar_needs_refresh
        jsr     refresh_sentence_bar             // update UI
        pla                                      // restore kid index
        jsr     switch_active_kid_if_different          // perform kid switch

        // normalize verb/UI again and exit
set_walk_and_exit:		
        lda     #WALK_TO_VERB
        sta     current_verb_id
        lda     #REFRESH_UI_FLAG_ON
        sta     sentence_bar_needs_refresh
        rts


* = $0B9C
process_sentence_queue_entry:
        // ------------------------------------------------------------
        // Handle one queued sentence.
        //
        // This processes a queued verb sentence into either:
        // - Walking toward a needed object, or
        // - Immediate verb execution if all parts are ready.
        //
        // It skips invalid or redundant sentences and advances
        // the queue pointer.
        // ------------------------------------------------------------
        lda     destination_entity
        beq     check_queue_nonempty     // skip if no current destination
        rts                                    // destination active → exit

check_queue_nonempty:
        ldx     sentq_index
        bpl     check_same_complements
        rts                                    // queue empty → exit

        // ------------------------------------------------------------
        // Skip identical complement sentences (DO == IO)
        // ------------------------------------------------------------
check_same_complements:
        lda     queued_prep_ids,x
        beq     set_active_sentence_tokens       // no preposition → skip test

        lda     queued_do_id_lo,x
        beq     verify_inventory_status  // no DO → skip identical check
        cmp     queued_io_id_lo,x
        bne     verify_inventory_status
        lda     queued_do_id_hi,x
        cmp     queued_io_id_hi,x
        bne     verify_inventory_status

        dec     sentq_index            // identical DO/IO → drop sentence
        rts

        // ------------------------------------------------------------
        // Check if complements are in inventory, else try scripted pickup
        // ------------------------------------------------------------
verify_inventory_status:
        lda     #$01
        jsr     is_sentence_object_owned_by_current_kid // check direct object
        cmp     #$00
        bne     set_active_sentence_tokens

        lda     #$02
        jsr     is_sentence_object_owned_by_current_kid // check indirect object
        cmp     #$00
        bne     set_active_sentence_tokens

        // try pickup for direct object
        lda     #$01
        jsr     has_pickup_script_for_sentence_part
        cmp     #$01
        bne     try_indirect_object_pickup
        lda     #$01
        jsr     enqueue_pickup_for_sentence_part
        rts

try_indirect_object_pickup:
        lda     #$02
        jsr     has_pickup_script_for_sentence_part
        cmp     #$01
        bne     drop_sentence_no_pickup_available
        lda     #$02
        jsr     enqueue_pickup_for_sentence_part
        rts

drop_sentence_no_pickup_available:
        dec     sentq_index            // no pickup scripts → drop sentence
        rts

        // ------------------------------------------------------------
        // Activate current sentence: copy into active_* vars
        // ------------------------------------------------------------
set_active_sentence_tokens:
        ldy     sentq_index
        lda     queued_verb_ids,y
        sta     active_verb_id
        lda     queued_do_id_lo,y
        sta     active_do_id_lo
        lda     queued_do_id_hi,y
        sta     active_do_id_hi
        lda     queued_prep_ids,y
        sta     active_prep_id
        lda     queued_io_id_lo,y
        sta     active_io_id_lo
        lda     queued_io_id_hi,y
        sta     active_io_id_hi

        dec     sentq_index
        dec     sentq_free_slots
        bpl     queue_capacity_ok

        // queue underflow → reset state
        lda     #SENT_QUEUE_EMPTY_IDX
        sta     sentq_index
        lda     #SENT_QUEUE_MAX_TOKENS
        sta     sentq_free_slots
        rts

        // ------------------------------------------------------------
        // Determine if we must walk or execute
        // ------------------------------------------------------------
queue_capacity_ok:
        ldx     active_do_id_lo
        lda     active_do_id_hi
        jsr     resolve_object_if_not_costume   // 0 = in inv, ≠0 = world
        bne     check_walk_to_direct_object

        lda     active_prep_id
        beq     execute_active_verb                 // no prep → execute directly

        // preposition present → ensure indirect object ready
        ldx     active_io_id_lo
        lda     active_io_id_hi
        jsr     resolve_object_if_not_costume
        bne     init_walk_to_indirect_object
        jmp     execute_verb_handler_for_object

        // ------------------------------------------------------------
        // Walk toward indirect object
        // ------------------------------------------------------------
init_walk_to_indirect_object:
        ldx     active_io_id_lo
        lda     active_io_id_hi
        stx     destination_obj_lo
        sta     destination_obj_hi
        jsr     route_destination_by_entity_type
        sty     destination_entity

        lda     dest_x
        sta     var_destination_x
        lda     dest_y
        sta     var_destination_y
        lda     current_kid_idx
        sta     active_costume
        tax
        lda     actor_vars,x
        and     ACTOR_IS_FROZEN
        bne     exit_hsq
        jsr     set_actor_destination
exit_hsq:
        jmp     exit_process_sentence_queue_entry

        // ------------------------------------------------------------
        // Execute verb directly
        // ------------------------------------------------------------
execute_active_verb:
        jmp     execute_verb_handler_for_object

        // ------------------------------------------------------------
        // Walk toward direct object if not in inventory
        // ------------------------------------------------------------
check_walk_to_direct_object:
        lda     control_mode
        cmp     KEYPAD_CONTROL_MODE
        bne     init_walk_to_direct_object

        jsr     execute_verb_handler_for_object  // keypad → no walking
        lda     #$00
        sta     destination_entity
        rts

init_walk_to_direct_object:
        ldx     active_do_id_lo
        lda     active_do_id_hi
        stx     destination_obj_lo
        sta     destination_obj_hi
        jsr     route_destination_by_entity_type
        sty     destination_entity

        lda     dest_x
        sta     var_destination_x
        lda     dest_y
        sta     var_destination_y

        lda     current_kid_idx
        sta     active_costume
        tax
        lda     actor_vars,x
        and     ACTOR_IS_FROZEN
        bne     exit_process_sentence_queue_entry
        jsr     set_actor_destination

exit_process_sentence_queue_entry:
        rts

* = $0AF3
dispatch_or_enqueue_action:
        // ------------------------------------------------------------
        // Reset sentence-queue bookkeeping
		//
        // Seeds free-capacity to max and resets head index to empty
        // sentinel. Prepares queue for a new sentence; actual
        // capacity decrementing occurs elsewhere.
        // ------------------------------------------------------------		
        lda     #SENT_QUEUE_MAX_TOKENS          
        sta     sentq_free_slots     // Reset free-slot counter; decremented elsewhere

        lda     #SENT_QUEUE_EMPTY_IDX            
        sta     sentq_index             // Reset head to “no entries” for pre-increment use

        // ------------------------------------------------------------
        // Early exit for “What is?” verb
		//
        // If current_verb_id equals WHAT_IS_VERB, no action is performed
        // and the routine returns immediately. Otherwise, control
        // falls through to walk-to handling.
        // ------------------------------------------------------------
        lda     current_verb_id                    // Load active verb for dispatch
        cmp     #WHAT_IS_VERB                   
        bne     handle_walk_to                  // Not "What is?" → continue handling
        rts                                     // "What is?" has no action → return

        // ------------------------------------------------------------
        // Walk-to verb dispatch
		//
        // Distinguishes between a bare “Walk to” (no object selected)
        // and a full “Walk to <object>” action. Non–walk-to verbs or
        // walk-to with object are queued for later execution; only a
        // bare walk-to proceeds to immediate movement.
        // ------------------------------------------------------------
handle_walk_to:
        cmp     #WALK_TO_VERB                   // Z=1 if current verb is "Walk to"
        bne     enqueue_sentence                // Different verb → enqueue full sentence

        lda     direct_object_idx_lo            // Test DO presence using low byte
        bne     enqueue_sentence                // DO present → treat as full action and enqueue

        // ------------------------------------------------------------
        // “Walk to” verb with no object → walk to cursor location
		//
        // Resolve acting entity from current kid
        // Sets active_costume := current_kid_idx, maps costume → actor,
        // and stores actor index for subsequent movement/path logic.
        // ------------------------------------------------------------
        lda     current_kid_idx                    // Select current kid as the acting entity
        sta     active_costume                 // Latch kid’s costume as active

        ldx     active_costume                 // X := active costume index for table lookup
        lda     actor_for_costume,x            // A := actor index mapped from costume
        tax                                    // X := actor index 
        stx     actor                          // Persist actor index for subsequent movement/path ops

        // ------------------------------------------------------------
        // Capture and normalize destination coordinates
		//
        // Loads cursor position (quarter X, half Y) into dest_x/dest_y
        // and calls snap_coords_to_walkbox to constrain them to valid
        // walkable terrain.
        // ------------------------------------------------------------
        lda     cursor_x_pos_quarter_absolute   // Get raw cursor X (quarter-pixel units)
        sta     dest_x                          // Seed destination X before clamping

        lda     cursor_y_pos_half_off_by_8      // Get raw cursor Y (half-pixel, offset-by-8)
        sta     dest_y                          // Seed destination Y before clamping

        jsr     snap_coords_to_walkbox          // Clamp dest_x/dest_y to nearest walkable box

        // ------------------------------------------------------------
        // Publish normalized destination for consumers
		//
        // Mirrors dest_x/dest_y into var_destination_x/y so debugging scripts
        // and path/debug routines can read the target without touching
        // actor-local dest registers. No motion is performed here.
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index for movement context
        lda     dest_x                         // Load clamped X coordinate
        sta     var_destination_x              // Store global X destination (for debugging)

        lda     dest_y                         // Load clamped Y coordinate
        sta     var_destination_y              // Store global Y destination (for debugging)

        // ------------------------------------------------------------
        // Frozen-state gate
		//
        // If the current kid is frozen, suppress any path setup or
        // movement. Destination mirrors remain updated but unused.
        // ------------------------------------------------------------
        ldx     current_kid_idx                    
        lda     actor_vars,x                   // Load actor’s state flags
        and     ACTOR_IS_FROZEN                // Mask bit(s) indicating frozen state
        bne     exit_dispatch_or_enqueue_action            // If frozen → skip path setup and exit routine

        // ------------------------------------------------------------
        // Stage path to destination
		//
        // Writes actor_x_dest/y_dest from clamped dest_x/dest_y, then
        // calls snap_and_stage_path_update to build the walking path
        // for the resolved actor.
        // ------------------------------------------------------------
        ldx     actor                          // X := actor index for destination update
        lda     dest_x                         // Load finalized X target
        sta     actor_x_dest,x                 // Commit actor’s horizontal destination

        lda     dest_y                         // Load finalized Y target
        sta     actor_y_dest,x                 // Commit actor’s vertical destination

        jsr     snap_and_stage_path_update     // Build and stage walking path toward dest_x/dest_y
exit_dispatch_or_enqueue_action:
        rts

        // ------------------------------------------------------------
        // Queue a complete sentence for later execution
        // ------------------------------------------------------------
enqueue_sentence:
        // ------------------------------------------------------------
        // Advance queue index
		//
        // Increments sentq_index to point to the next free
        // slot and loads it into X for writing the queued sentence
        // tokens.
        // ------------------------------------------------------------
        inc     sentq_index           // Advance queue pointer to next free slot
        ldx     sentq_index           // X := current queue entry index for storing tokens

        // ------------------------------------------------------------
        // Write current sentence tokens into queue entry X
		//
        // Stores: verb, DO lo/hi, preposition, IO lo/hi into the
        // parallel queued_sentence_* arrays at index X.
        // ------------------------------------------------------------
        lda     current_verb_id                    
        sta     queued_verb_ids,x         

        lda     direct_object_idx_lo            	 
        sta     queued_do_id_lo,x  
        lda     direct_object_idx_hi            	 
        sta     queued_do_id_hi,x  
		
        lda     preposition                    		 
        sta     queued_prep_ids,x  	 

        lda     indirect_object_idx_lo          		
        sta     queued_io_id_lo,x  	
        lda     indirect_object_idx_hi          		
        sta     queued_io_id_hi,x   

        // ------------------------------------------------------------
        // Post-queue verb reset gate
		//
        // If current_verb_id == WALK_TO_VERB, skip UI reset and exit;
        // otherwise fall through to reset defaults.
        // ------------------------------------------------------------
        lda     current_verb_id                   // Reload current verb for post-queue reset test
        cmp     #WALK_TO_VERB                  // Z=1 if it was already "Walk to"
        beq     finalize_and_exit              // If so, skip UI verb reset and exit

        // ------------------------------------------------------------
        // Reset UI verb defaults after queuing
		//
        // Revert current_verb_id to WALK_TO_VERB and clear DO/preposition
        // so the input bar idles on a neutral “Walk to” state.
        // ------------------------------------------------------------
        lda     #WALK_TO_VERB                   // Default UI verb after queuing
        sta     current_verb_id                    // Reset current verb to "Walk to"

        lda     #$00                            // Prepare clear value
        sta     direct_object_idx_lo            // Clear direct object reference
        sta     preposition                     // Clear preposition token

        // ------------------------------------------------------------
        // Finalize and exit
		//
        // Clears destination_entity to indicate no pending target and
        // returns. Queue state and tokens remain committed.
        // ------------------------------------------------------------
finalize_and_exit:
        lda     #$00                            // Prepare clear value
        sta     destination_entity              // Reset destination entity marker (none targeted)
        rts                                     // Return → sentence queued or action completed


* = $0DC5
execute_verb_handler_for_object:
        // ------------------------------------------------------------
        // Execute the correct verb handler for an object
        //
        // If there's a custom handler defined for the combination of
        // verb/object, execute it. Otherwise, execute the default verb
        // handlers accordingly.
        // ------------------------------------------------------------
        // ------------------------------------------------------------
        // Mark sentence bar for refreshing
        // ------------------------------------------------------------
        lda     #TRUE
        sta     sentence_bar_needs_refresh

        // ------------------------------------------------------------
        // Get the active object's resource
        // ------------------------------------------------------------
        ldx     active_do_id_lo
        lda     active_do_id_hi
        jsr     resolve_object_resource
        sty     resource_index_for_script_slot
        sta     script_type_for_script_slot

        // ------------------------------------------------------------
        // Get the object's script handler for the verb, if any
        // ------------------------------------------------------------
        lda     active_verb_id
        jsr     find_object_verb_handler_offset

        // ------------------------------------------------------------
        // Did we find a handler?
        // ------------------------------------------------------------
        bne     guard_read_requires_light

        // ------------------------------------------------------------
        // No handler found → run default handlers
        // ------------------------------------------------------------
        lda     active_verb_id
        cmp     #GIVE_VERB
        bne     guard_walk_to_early_exit

        // ------------------------------------------------------------
        // "Give" verb processing
        // ------------------------------------------------------------
        lda     active_io_id_lo
        cmp     #FIRST_NON_KID_INDEX                    // recipient index ≥ 8? (not a kid)
        bcs     return_after_give_path                    // if not a kid, exit

        ldx     active_do_id_lo
        lda     object_attributes,x
        and     #MSK_HIGH_NIBBLE                    // clear low nibble (owner)
        ora     active_io_id_lo            // set new owner
        sta     object_attributes,x
        jsr     refresh_items_displayed // refresh inventory
return_after_give_path:
        rts

guard_walk_to_early_exit:
        // ------------------------------------------------------------
        // If verb is "walk to", exit
        // ------------------------------------------------------------
        cmp     #WALK_TO_VERB
        beq     return_after_walk_to

launch_global_defaults_script:
        // ------------------------------------------------------------
        // Run global "verb defaults" script (#3)
        // ------------------------------------------------------------
        lda     active_verb_id
        sta     var_active_verb_id
        lda     #SCRIPT_SLOT_NONE
        sta     current_script_slot
        lda     #GLOBAL_DEFAULTS_SCRIPT_ID
        jsr     start_global_script
return_after_walk_to:
        rts

guard_read_requires_light:
        // ------------------------------------------------------------
        // Handle "Read" verb guard (requires light)
        // ------------------------------------------------------------
        pha
        lda     active_verb_id
        cmp     #READ_VERB
        bne     launch_custom_handler
        lda     global_lights_state
        bne     launch_custom_handler
        pla                             // darkness → fallback to defaults
        jmp     launch_global_defaults_script

launch_custom_handler:
        // ------------------------------------------------------------
        // Execute custom handler
        // ------------------------------------------------------------
        pla
        clc
        adc.zp  <room_obj_ofs
        sta     script_offsets_lo
        lda     #$00
        adc.zp  >room_obj_ofs
        sta     script_offsets_hi
        lda     #$00
        sta     current_script_slot
        lda     active_io_id_lo
        sta     var_active_io_id_lo
        jsr     set_script_resource_base_address
        jsr     set_current_script_read_address
        jsr     execute_next_operation
        rts
		
/*
================================================================================
Get an object's script offset for a given verb.

Summary:
Scans the verb-handler list inside an object resource and returns the offset of
the matching handler. Each handler occupies two bytes: {verb_id, script_offset}.
A value of #$00 marks the end of the list, and #$0F indicates a default handler.

Arguments:
        .A = verb index
        (object_rsrc_ptr) = base pointer to object resource

Returns:
        .A = script offset within the object resource, or:
             #$00 if no handler and verb ≠ WALK_TO_VERB
             #$0D if no handler and verb == WALK_TO_VERB

Description:
- The verb-handler list starts at offset #$0E from the resource base.
- Each entry: {verb_id, offset}
- The default handler (#$0F) matches any verb.
================================================================================
*/
.label verb_index       = $0e6f           // Saved verb index for comparisons
.label object_rsrc_ptr  = $15             // ZP pointer to object resource base

.const VERB_TABLE_START_OFS   = $0E    // Start of verb-handler list in object resource
.const VERB_SCAN_SEED_Y       = VERB_TABLE_START_OFS - 2   // Y seed so first INY,INY reads at +$0E
.const DEFAULT_VERB           = $0F    // Default handler id; matches any verb
.const NO_HANDLER_RET         = $00    // Return when no handler and verb ≠ WALK_TO_VERB

* = $0E4A
find_object_verb_handler_offset:
        // ------------------------------------------------------------
        // Store requested verb index
        // ------------------------------------------------------------
        sta     verb_index

        // ------------------------------------------------------------
        // Initialize scan offset (first entry at +VERB_TABLE_START_OFS)
        // ------------------------------------------------------------
        ldy     #VERB_SCAN_SEED_Y

scan_next_verb_entry:
        // ------------------------------------------------------------
        // Advance two bytes to next handler entry
        // ------------------------------------------------------------
        iny
        iny

        // ------------------------------------------------------------
        // Read verb id from handler pair
        // ------------------------------------------------------------
        lda     (object_rsrc_ptr),y

        // ------------------------------------------------------------
        // End of table marker?
        // ------------------------------------------------------------
        bne     evaluate_default_handler

        // ------------------------------------------------------------
        // Reached end of handlers list
        // Return #$0D if WALK_TO_VERB, else #$00
        // ------------------------------------------------------------
        lda     verb_index
        cmp     #WALK_TO_VERB
        beq     return_no_handler
        lda     #NO_HANDLER_RET
return_no_handler:
        rts

evaluate_default_handler:
        // ------------------------------------------------------------
        // Default handler (#$0F) matches any verb
        // ------------------------------------------------------------
        cmp     #DEFAULT_VERB
        bne     compare_verb_id
        jmp     return_handler_offset

compare_verb_id:
        // ------------------------------------------------------------
        // Compare handler’s verb id with requested verb
        // ------------------------------------------------------------
        cmp     verb_index
        bne     scan_next_verb_entry

return_handler_offset:
        // ------------------------------------------------------------
        // Return next byte as handler script offset
        // ------------------------------------------------------------
        iny
        lda     (object_rsrc_ptr),y
        rts
		
/*
================================================================================
Returns whether an object has a custom "pick up" script.

Summary:
Checks the queued sentence’s direct or indirect object for a custom Pick Up
handler. Returns A = #$01 if a script exists, else #$00. Preserves X and Y.

Arguments:
        .A      #$01 → direct object
                any other value → indirect object

Returns:
        .A      #$01 if custom Pick Up script exists
                #$00 otherwise

Description:
- Selects DO or IO from the sentence queue slot at sentq_index.
- Rejects objects with hi == #$02 as non-recipients.
- Resolves object resource and queries verb handler for PICK_UP_VERB.
================================================================================
*/

* = $0E73
has_pickup_script_for_sentence_part:
        // ------------------------------------------------------------
        // Save X and Y
        // ------------------------------------------------------------
        stx     temp_x
        sty     temp_y

        // ------------------------------------------------------------
        // Load current queue index into Y
        // ------------------------------------------------------------
        ldy     sentq_index

        // ------------------------------------------------------------
        // DO vs IO selector: A == #ARG_IS_DO → direct object
        // ------------------------------------------------------------
        cmp     #ARG_IS_DO
        bne     load_indirect_object_index

        // ------------------------------------------------------------
        // Load direct object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     queued_do_id_lo,y
        lda     queued_do_id_hi,y
        jmp     check_actor_class

load_indirect_object_index:
        // ------------------------------------------------------------
        // Load indirect object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     queued_io_id_lo,y
        lda     queued_io_id_hi,y

check_actor_class:
        // ------------------------------------------------------------
        // Recipient class gate: if it's an actor, it's not pickable
        // ------------------------------------------------------------
        cmp     #OBJ_TYPE_ACTOR
        bne     resolve_and_check_pickup_handler

        // ------------------------------------------------------------
        // Not pickable → return False
        // ------------------------------------------------------------
        lda     #FALSE
        rts

resolve_and_check_pickup_handler:
        // ------------------------------------------------------------
        // Resolve object and query Pick Up handler
        // ------------------------------------------------------------
        jsr     resolve_object_resource
        lda     PICK_UP_VERB
        jsr     find_object_verb_handler_offset

        // ------------------------------------------------------------
        // Nonzero → script exists
        // ------------------------------------------------------------
        bne     pickup_handler_present

        // ------------------------------------------------------------
        // No script → return False
        // ------------------------------------------------------------
        lda     #FALSE
        jmp     has_object_pickup_exit

pickup_handler_present:
        // ------------------------------------------------------------
        // Script present → return True
        // ------------------------------------------------------------
        lda     #TRUE

has_object_pickup_exit:
        // ------------------------------------------------------------
        // Restore X and Y, then return
        // ------------------------------------------------------------
        ldx     temp_x
        ldy     temp_y
        rts
		
// ------------------------------------------------------------
// Check if the sentence complement (direct or indirect object)
// is in the current kid's inventory
//
// Arguments:
//     .A = #$01 → check direct object
//           else → check indirect object
//
// Returns:
//     .A = #$01 if object is in current kid's inventory
//           #$00 otherwise
// ------------------------------------------------------------
.const INVCHK_RET_FOUND            = $01    // Function return: in current kid’s inventory
.const INVCHK_RET_NOT_FOUND        = $00    // Function return: not in current kid’s inventory

* = $0EAE
is_sentence_object_owned_by_current_kid:
        // ------------------------------------------------------------
        // Save X register to temporary
        // ------------------------------------------------------------
        stx     temp_x

        // ------------------------------------------------------------
        // Fetch index of current sentence part being analyzed
        // ------------------------------------------------------------
        ldx     sentq_index

        // ------------------------------------------------------------
        // Are we checking for a direct object?
        // ------------------------------------------------------------
        cmp     #ARG_IS_DO
        bne     load_indirect_object_id

        // ------------------------------------------------------------
        // Direct object path
        // ------------------------------------------------------------
        ldy     queued_do_id_lo,x
        lda     queued_do_id_hi,x
        jmp     guard_in_some_inventory

load_indirect_object_id:
        // ------------------------------------------------------------
        // Indirect object path
        // ------------------------------------------------------------
        ldy     queued_io_id_lo,x
        lda     queued_io_id_hi,x

guard_in_some_inventory:
        // ------------------------------------------------------------
        // The object's hi byte represents if it's in *somebody's*
        // inventory (#00 = in inventory, otherwise not)
        // ------------------------------------------------------------
        beq     compare_owner_with_current_kid

        // ------------------------------------------------------------
        // Not in any inventory → return #00
        // ------------------------------------------------------------
        lda     #INVCHK_RET_NOT_FOUND
        rts

compare_owner_with_current_kid:
        // ------------------------------------------------------------
        // Check if the item belongs to the current kid
        // ------------------------------------------------------------
        lda     object_attributes,y
        and     #MSK_LOW_NIBBLE                    // isolate owner nibble
        cmp     current_kid_idx
        bne     return_not_owned_by_current_kid

        // ------------------------------------------------------------
        // In current kid's inventory → return #01
        // ------------------------------------------------------------
        lda     #INVCHK_RET_FOUND
        jmp     exit_inv_check

return_not_owned_by_current_kid:
        // ------------------------------------------------------------
        // Not in current kid's inventory → return #00
        // ------------------------------------------------------------
        lda     #INVCHK_RET_NOT_FOUND

exit_inv_check:
        // ------------------------------------------------------------
        // Restore X register and return
        // ------------------------------------------------------------
        ldx     temp_x
        rts
		

/*
================================================================================
Queues a “Pick Up” sentence for the current sentence complement.

Summary:
Takes either the direct or indirect object from the current queued sentence
and appends a new “Pick Up <object>” command to the sentence queue. Ensures
the queue index remains valid and halts with a debug indicator on overflow.

Arguments:
        .A = #$01 → direct object
             #$02 → indirect object

Returns:
        None (A/X/Y clobbered)
================================================================================
*/
.label object_ptr = $15                     // ZP pointer for target object index
* = $0EE1
enqueue_pickup_for_sentence_part:
        // ------------------------------------------------------------
        // Save X
        // ------------------------------------------------------------
        stx     temp_x

        // ------------------------------------------------------------
        // Fetch current queue index
        // ------------------------------------------------------------
        ldx     sentq_index

        // ------------------------------------------------------------
        // Is it the indirect object? 
        // ------------------------------------------------------------
        cmp     #ARG_IS_IO
        bne     fetch_direct_object

        // ------------------------------------------------------------
        // Indirect object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     queued_io_id_lo,x
        sta     <object_ptr
        lda     queued_io_id_hi,x
        sta     >object_ptr
        jmp     next_sentence_index

fetch_direct_object:
        // ------------------------------------------------------------
        // Direct object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     queued_do_id_lo,x
        sta     <object_ptr
        lda     queued_do_id_hi,x
        sta     >object_ptr

next_sentence_index:
        // ------------------------------------------------------------
        // Advance queue index and validate range
        // ------------------------------------------------------------
        inc     sentq_index
        ldx     sentq_index
        cpx     #SENT_QUEUE_MAX_TOKENS
        bne     queue_pickup

        // ------------------------------------------------------------
        // Invalid sentence part index → debug hang
        // ------------------------------------------------------------
        lda     #$2D
        sta     debug_error_code
        ldy     #MAP_IO_ON
        sty     cpu_port
hangup_loop:
        sta     vic_border_color_reg
        jmp     hangup_loop

queue_pickup:
        // ------------------------------------------------------------
        // Queue a new “Pick Up” sentence for the selected object
        // ------------------------------------------------------------
        lda     PICK_UP_VERB
        sta     queued_verb_ids,x
        lda     #$00
        sta     queued_prep_ids,x
        lda     <object_ptr
        sta     queued_do_id_lo,x
        lda     >object_ptr
        sta     queued_do_id_hi,x

        // ------------------------------------------------------------
        // Restore X and return
        // ------------------------------------------------------------
        ldx     temp_x
        rts
		
/*
================================================================================
  switch_active_kid_if_different
================================================================================
Summary
	Switch control to the selected kid if it differs from the current one,
	stop any running script, recenter the camera, refresh inventory, then
	fall through to sentence/UI re-initialization.

Arguments
    X            UI slot index of the selected kid (indexes kid_ids[]).

Global Inputs
	kid_ids[]                  lookup table: UI slot → kid id
	current_kid_idx                active kid id
	current_script_slot        active script slot id

Global Outputs
	current_kid_idx                ← kid_ids[X]        (if different)
	current_script_slot        ← $FF               (script stopped)

Description
	• Load kid id from kid_ids[X].
	• If it matches current_kid_idx, return with no changes.
	• Otherwise:
		– Write new kid id to current_kid_idx.
		– Stop any running script by setting current_script_slot to $FF.
		– Recenter camera on the new kid’s actor.
		– Refresh the items/inventory display.
	• Execution then falls through to init_sentence_ui_and_queue to reset
	the verb/sentence UI.
================================================================================
*/
* = $29AE
switch_active_kid_if_different:
        // X := desired index, A := kid id for that index
        tax
        lda     kid_ids,x

        // If already current, exit
        cmp     current_kid_idx
        bne     commit_kid_change
        rts
		
commit_kid_change:
        // Commit new kid and stop scripts
        sta     current_kid_idx
        sta     current_kid_idx		//Redundant instruction present in original code
        lda     #$FF
        sta     current_script_slot

        // Recenter camera and refresh inventory
        lda     current_kid_idx
        jsr     fix_camera_on_actor
        jsr     refresh_items_displayed
		//Fall through to init_sentence_ui_and_queue
/*
================================================================================
  init_sentence_ui_and_queue
================================================================================
Summary
	Reset the verb/sentence UI to a known baseline and clear any pending
	command. Set default verb to WALK_TO_VERB and mark the UI for redraw.

Global Outputs
	destination_entity             ← ENTITY_NONE
	sentence_bar_needs_refresh      ← TRUE
	sentq_free_slots   ← SENT_QUEUE_MAX_TOKENS
	sentq_index           ← SENT_QUEUE_EMPTY_IDX
	current_verb_id                   ← WALK_TO_VERB
	direct_object_idx_lo           ← $00
	direct_object_idx_hi           ← $00
	preposition                    ← $00
	indirect_object_idx_lo         ← $00
	indirect_object_idx_hi         ← $00

Description
	• Clear any active destination so pathing is idle.
	• Request a sentence bar refresh on next UI pass.
	• Restore sentence queue to empty with full capacity.
	• Set default verb to “Walk to”.
	• Clear all sentence complements: DO, preposition, IO.
================================================================================
*/
* = $29CC
init_sentence_ui_and_queue:
        // ------------------------------------------------------------
        // Reset all components of the sentence queue system
        //
        // This restores the verb UI state to a default “Walk to” state,
        // empties the token queue, and signals the UI to redraw.
        // ------------------------------------------------------------
        lda     #ENTITY_NONE
        sta     destination_entity            // clear current destination target

        lda     #TRUE
        sta     sentence_bar_needs_refresh     // force UI refresh of sentence bar

        lda     #SENT_QUEUE_MAX_TOKENS
        sta     sentq_free_slots  // reset available slots (max = 6)

        lda     #SENT_QUEUE_EMPTY_IDX
        sta     sentq_index          // mark queue as empty (no active tokens)

        lda     #WALK_TO_VERB
        sta     current_verb_id                  // set default verb “Walk to”

        lda     #$00
        sta     direct_object_idx_lo          // clear direct object index (lo)
        sta     direct_object_idx_hi          // clear direct object index (hi)
        sta     preposition                   // clear preposition field
        sta     indirect_object_idx_lo        // clear indirect object index (lo)
        sta     indirect_object_idx_hi        // clear indirect object index (hi)

        rts
