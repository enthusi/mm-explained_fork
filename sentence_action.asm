/*
================================================================================
Sentence/action system
================================================================================

Summary
	This module powers the point-and-click “sentence” UX: pick a verb, then
	zero or more complements (direct object, optional preposition, optional
	indirect object). It keeps a small stack of pending sentences, decides when
	to walk first vs act immediately, and dispatches either per-object verb
	handlers or global defaults.
	
Terminology
		DO		Direct object
		IO		Indirect object

Core data

	* Current picks (UI state): current_verb_id, direct_object_idx_{lo,hi},
	  preposition, indirect_object_idx_{lo,hi}.
	* Queue (up to 6): parallel arrays of verb/prep/DO/IO plus sentstk_top_idx
	  (top) and sentstk_free_slots (capacity).
	* “Active” copy: once a stacked sentence is selected to run, its tokens are
	  mirrored into active_* so execution code has a stable snapshot.
	* Destination: destination_entity plus dest_{x,y} are used to route walking
	  targets and stage paths. var_destination_{x,y} mirror the clamped target
	  for scripts/debug.

Main loop
	process_sentence:

		1. Optional reset of DO/IO/preposition if init_sentence_ui_flag is set.
		2. If the current kid is incapacitated, restrict verbs to “New kid”.
		3. Control-mode branch:

		   * Cutscene: only try to run completed sentences.
		   * Keypad: force PUSH verb and bypass walking later.
		   * Normal: if verb == “What is?”, set a “forced trigger”.
		4. Forced trigger path:

		   * If verb = GIVE and a preposition is set, pick an actor under cursor
			 as IO; else pick an object under cursor.
		   * Update DO or IO, guarding DO == IO and setting UI-rebuild flags when
			 selections repeat. Always request a sentence-bar refresh.
		5. If verb is Walk to, clear destination_entity and mark a rebuild (so UI
		   reflects the bare walk). Then fall into run_sentence_if_complete.

Completion gate
	run_sentence_if_complete:

		* Always refresh the bar first. If a “rebuild needed” flag was set, clear
		  it and continue.
		* If no verb, just refresh again and exit.
		* NEW KID → handle_new_kid_verb.
		* WALK TO → can execute without objects → dispatch_or_push_action.
		* Other verbs:

		  * Require a DO. If missing, refresh and exit.
		  * If a preposition is required and missing, call select_preposition_for_verb.
			If it returns none, we can run. Otherwise store it, mark refresh, and
			exit until the user picks an IO.
		  * If we have a preposition but no IO yet, refresh and exit.
		  * With all parts present → dispatch_or_push_action.

“New kid”
	handle_new_kid_verb:

		* Only in NORMAL_CONTROL_MODE. Cursor X chooses kid0/1/2 by name columns.
		* Refresh UI, call switch_active_kid_if_different, then normalize back to
		  WALK TO and request refresh.

Dispatch vs stack
	dispatch_or_push_action:

		* Reset stack bookkeeping (free slots to max, head to “empty” sentinel).
		* If verb == WHAT IS → do nothing and return.
		* If verb != WALK TO or DO is present → push_sentence:

		  * Advance sentstk_top_idx and store current tokens to stacked_* arrays.
		  * If verb wasn’t WALK TO, reset the UI to WALK TO and clear DO+prep.
		  * Clear destination_entity and return.
		* Bare WALK TO (no DO):

		  * Resolve acting entity from current kid and copy cursor coords to dest.
		  * Clamp to walkable space, publish var_destination_{x,y}.
		  * If kid is frozen, stop here.
		  * Otherwise write actor_x_dest/y_dest and call snap_and_stage_path_update.

Queued execution
	process_sentence_queue_entry:

		* If a destination is already active, exit. If stack empty, exit.
		* Drop sentences whose DO == IO.
		* Inventory check in priority order:

		  * If either DO or IO is already in current kid’s inventory, proceed.
		  * Else try to push a “Pick up <obj>” sentence for DO, else IO, but
			only if the object has a custom Pick Up handler. If neither can be
			picked up, drop the sentence.
		* Activate the current sentence by copying stacked_* → active_* and pop the
		  stack (with underflow reset protection).
		* Decide walk vs execute:

		  * If DO is in inventory and there is no preposition → execute now.
		  * If a preposition exists, ensure IO is also in inventory; if not, walk
			to IO first.
		  * If DO is not in inventory:

			* In KEYPAD mode: execute now, no walking.
			* Otherwise walk to DO.

Verb execution
	execute_verb_handler_for_object:

		* Refresh bar, resolve the active DO’s resource, and look up a custom
		  handler for active_verb_id via find_object_verb_handler_offset.
		* If none:

		  * GIVE: if recipient is a kid, transfer ownership (owner nibble) and
			refresh inventory; then return.
		  * WALK TO: return.
		  * Else run the global defaults script (#3).
		* Guard: READ requires lights; if dark, fall back to global defaults.
		* If a custom handler exists: compute its script offset from the object’s
		  base, set script read address, and execute_next_operation.

Helper scans and guards
	find_object_verb_handler_offset:

		* Scans the object’s handler table starting at +$0E: {verb_id, offset}
		  pairs, terminated by 0. A special “default” id (#$0F) matches any verb.
		  Returns offset on match. Returns #$00 if absent and verb ≠ WALK TO,
		  or #$0D if absent and verb == WALK TO.

	has_pickup_script_for_sentence_part:

		* Select DO or IO from the stacked entry at sentstk_top_idx. Reject actors.
		  Resolve the object and check for a PICK_UP_VERB handler. Returns 1 if
		  present.

	is_sentence_object_owned_by_current_kid:

		* Given DO vs IO selector, read the stacked object. If its hi byte says
		  “in some inventory”, compare the owner nibble against current_kid_idx
		  and return 1 if it matches, else 0.

	push_pickup_for_sentence_part:

		* Append a “Pick Up <obj>” sentence for the selected complement. Validates
		  stack bounds and enters a visible debug hang if overflow occurs.

Kid switching and UI reset
	switch_active_kid_if_different:

		* Change current_kid_idx if needed, stop any running script, recenter the
		  camera, refresh inventory, then fall through to init_sentence_ui_and_queue.

	init_sentence_ui_and_queue:

		* Clear destination, force a bar refresh, reset stack capacity and head,
		  set default verb to WALK TO, and clear DO/prep/IO.

Typical traces

	* “Walk to” click on floor: bare WALK TO path clamps cursor position into a
	  walkable box and stages a path unless frozen.
	* “Use key on door”: ensure DO “key” is in inventory or push a “Pick up
	  key” first. If a preposition selects “with/door” and IO is not in
	  inventory, walk to door first. When ready, execute custom handler or
	  default.
	* “Give coin to kid”: if IO is a kid, ownership is updated and inventory
	  refreshes without running a script.
================================================================================
*/
#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "sentence_text.asm"
#import "destination.asm"

.label find_actor_enclosing_cursor = $0
.label get_object_enclosing_cursor = $0
.label set_script_resource_base_address = $0
.label set_current_script_read_address = $0
.label execute_next_operation = $0

// Queued sentence parts (parallel LIFO stacks, max depth = 6)
.label stacked_verb_ids        = $fe25    // Stack of verb IDs (index 0..5; top tracked separately)
.label stacked_do_id_lo        = $fe2b    // Stack of direct-object IDs: low byte per entry
.label stacked_do_id_hi        = $fe31    // Stack of direct-object IDs: high byte per entry
.label stacked_prep_ids        = $fe37    // Stack of preposition IDs (one byte per entry)
.label stacked_io_id_lo        = $fe3d    // Stack of indirect-object IDs: low byte per entry
.label stacked_io_id_hi        = $fe43    // Stack of indirect-object IDs: high byte per entry

// Active (dequeued) sentence snapshot
.label active_verb_id          = $fe19    // Active verb ID being processed
.label active_do_id_lo         = $fe1a    // Active direct-object ID: low byte
.label active_do_id_hi         = $fe1b    // Active direct-object ID: high byte
.label active_prep_id          = $fe1c    // Active preposition ID (0 = none)
.label active_io_id_lo         = $fe1d    // Active indirect-object ID: low byte
.label active_io_id_hi         = $fe1e    // Active indirect-object ID: high byte

// Scratch / helpers
.label verb_index              = $0e6f    // Latched verb ID for comparisons/table scans
.label object_rsrc_ptr         = $15      // ZP pointer to object resource base (lo at $15, hi at $16)
.label temp_x                  = $0e70    // Saves X across calls where X must be preserved
.label temp_y                  = $0e71    // Saves Y across calls where Y must be preserved
.label object_ptr              = $15      // ZP pointer used for target object index lookups (alias of object_rsrc_ptr)


.const VERB_TABLE_START_OFS       = $0E    // Byte offset in object resource where {verb,ofs} pairs begin
.const VERB_SCAN_SEED_Y           = VERB_TABLE_START_OFS - 2   // Y init so first INY,INY lands at +$0E entry
.const DEFAULT_VERB               = $0F    // Wildcard verb id; matches any verb in handler scan
.const NO_HANDLER_RET             = $00    // find_object_verb_handler_offset: no handler and verb ≠ WALK_TO

.const INVCHK_RET_FOUND           = $01    // Inventory check result: object owned by current kid
.const INVCHK_RET_NOT_FOUND       = $00    // Inventory check result: object not owned by current kid

.const ARG_IS_DO                  = $01    // Selector A value: operate on direct object
.const ARG_IS_IO                  = $02    // Selector A value: operate on indirect object

.const FORCED_TRIGGER_SET          = $01    // forced_sentence_trigger: set → run sentence immediately
.const FORCED_TRIGGER_CLEARED      = $FF    // forced_sentence_trigger: cleared after handling

.const UI_REFRESH_REQUEST         = $01    // sentence_bar_needs_refresh flag value: request redraw
.const REBUILD_SENTENCE_ON        = $01    // needs_sentence_rebuild flag value: rebuild now

.const OBJ_IDX_NONE               = $00    // Sentinel lo byte for “no object selected”
.const PREPOSITION_NONE           = $00    // No preposition selected

.const SCRIPT_SLOT_NONE           = $FF    // No script running (idle slot sentinel)
.const GLOBAL_DEFAULTS_SCRIPT_ID  = $03    // Global “verb defaults” script identifier

.const SENT_STACK_MAX_TOKENS      = $06    // Max entries allowed on the sentence stack
.const SENT_STACK_EMPTY_IDX       = $FF    // Stack top sentinel for “no entries”

.const DEFAULT_VERB_WALK_TO       = WALK_TO_VERB    // Verb id for “Walk to” default case
.const ENTITY_NONE                = $00    // No destination entity selected/active

.const SECOND_KID_NAME_COLUMN     = $0B    // Column threshold for kid #2 selection in UI
.const THIRD_KID_NAME_COLUMN      = $18    // Column threshold for kid #3 selection in UI

/*
================================================================================
  process_sentence
================================================================================

Summary
	Process the current verb sentence for this frame. Optionally reset parts,
	apply incapacity limits, branch by control mode, handle forced-trigger
	selection via cursor hits, then run the completeness gate.

Global Inputs
	init_sentence_ui_flag             One-shot reset request
	current_kid_idx                   Active kid index
	actor_vars[]                      Kid state flags (sign bit used for incapacitated)
	control_mode                      Current control mode
	current_verb_id                   Active verb token
	current_preposition               Current preposition token
	direct_object_idx_lo/hi           Current direct object id
	indirect_object_idx_lo/hi         Current indirect object id
	cursor_x_pos_quarter_relative     Cursor X on sentence bar (used in subroutines)

Global Outputs
	current_verb_id                   May be clamped to NEW_KID or reset to WALK_TO
	current_preposition, direct_object_idx_lo/hi,
	indirect_object_idx_lo/hi         Cleared on resets or updated from cursor hits
	forced_sentence_trigger           Set/cleared when “What is?” is active
	sentence_bar_needs_refresh        Set when UI must redraw the bar
	needs_sentence_rebuild            Set for one-shot rebuild after certain updates
	destination_entity                Cleared when normalizing WALK_TO without object

Description
	* Reset path:
		  • If init_sentence_ui_flag is set:
			  – If no preposition: clear the flag only.
			  – If a preposition exists: clear prep, IO, and DO; then clear the flag.
	* Incapacity limits:
		  • If the current kid is “incapacitated” (sign bit in actor_vars), force
		  NEW_KID behavior by zeroing current_verb_id unless it is NEW_KID.
	* Control-mode dispatch:
		  • CUTSCENE → jump to run_sentence_if_complete.
		  • KEYPAD   → force current_verb_id = PUSH_VERB, then fall through.
		  • NORMAL   → if WHAT_IS_VERB, set forced_sentence_trigger.
	* Forced trigger:
		  • If forced_sentence_trigger == SET:
			  – If verb == GIVE and a preposition exists, select an actor under the
			  cursor as IO; else select an object under the cursor (DO or IO).
			  – Handle “no hit” (including WALK_TO special case) or commit DO/IO.
			  – Normalize UI flags and fall to the completeness gate.
		  • Otherwise, fall straight to run_sentence_if_complete.
	* Cursor-hit handlers:
		  • DO path: confirm or set DO; request UI refresh; in KEYPAD set rebuild.
		  • IO path: reject DO==IO, confirm or set IO; in KEYPAD set rebuild.
	* Finalization:
		  • Always request a UI refresh before the completeness gate.
		  • If verb is WALK_TO, also clear destination_entity and set rebuild.
		  • Tail-fall to run_sentence_if_complete for execution/stacking.

Notes
	* This routine never executes scripts or stages walking directly; it only
	  mutates sentence parts, UI flags, and routes to the completeness logic.
	* DO/IO equality is explicitly rejected to prevent redundant sentences.
================================================================================
*/
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
        lda     current_preposition
        beq     apply_pending_sentence_reset            // no preposition → partial reset only

        lda     #$00
        sta     current_preposition
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
        lda     #FORCED_TRIGGER_SET
        sta     forced_sentence_trigger           // “What is” → force sentence trigger

handle_click_trigger_or_defer:
        lda     forced_sentence_trigger
        cmp     #FORCED_TRIGGER_SET
        beq     on_forced_trigger
        jmp     run_sentence_if_complete          // no forced trigger → check normally

        // ------------------------------------------------------------
        // Forced-trigger path
        // ------------------------------------------------------------
on_forced_trigger:
        lda     #FORCED_TRIGGER_CLEARED
        sta     forced_sentence_trigger            // clear trigger flag

        lda     current_verb_id
        cmp     #GIVE_VERB
        bne     pick_object_under_cursor

        // “Give” requires an actor as IO
        lda     current_preposition
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
        sta     current_preposition
        sta     indirect_object_idx_lo

finalize_after_walkto_nohit:
        jmp     finalize_and_maybe_execute

        // ------------------------------------------------------------
        // Object found under cursor
        // ------------------------------------------------------------
cursor_hit_object_found:
        ldy     current_preposition
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

/*
================================================================================
  run_sentence_if_complete
================================================================================

Summary
	Run the current verb sentence if all required parts are present. Handles
	special verbs, requests UI refreshes when parts are missing, and dispatches
	either immediate execution or stacking via dispatch_or_push_action.

Vars/State
	needs_sentence_rebuild          Cleared here if set; triggers a one-shot UI rebuild

Global Inputs
	current_verb_id                 Active verb token (0 if none)
	direct_object_idx_lo            Presence check for direct object (lo byte)
	current_preposition                     Current preposition token (0 if none)
	indirect_object_idx_lo          Presence check for indirect object (lo byte)

Global Outputs
	needs_sentence_rebuild          Cleared when observed
	sentence_bar_needs_refresh      Set when preposition was just chosen
	(UI) refresh via refresh_sentence_bar_trampoline
	
	(control flow) tail-calls:
		- handle_new_kid_verb
		- dispatch_or_push_action

Description
	* Always refresh the sentence bar first so the UI stays in sync.
	* If a one-shot sentence rebuild was requested, clear the flag and continue.
	* If no verb is selected, refresh again and exit.
	* If verb = NEW_KID_VERB, delegate to handle_new_kid_verb.
	* If verb = WALK_TO_VERB, execute immediately (no DO required) via
	  dispatch_or_push_action.
	* For all other verbs:
		  • Require a direct object; if missing, refresh and exit.
		  • If preposition is needed but missing, call select_preposition_for_verb:
			  – If it returns 0 → no preposition required; execute now.
			  – If it returns nonzero → save it, request a UI refresh, and exit so
			  the user can choose an indirect object.
		  • If preposition is present but no indirect object yet, refresh and exit.
		  • When all parts are present, execute via dispatch_or_push_action.

Notes
	* This routine performs no movement or script execution itself; it only
	  validates sentence completeness and routes control.
	* UI is refreshed at entry and whenever a missing part blocks execution.
================================================================================
*/
* = $0874
run_sentence_if_complete:
        // ------------------------------------------------------------
        // Run a sentence if all required parts are available.
        //
        // - If the verb has all necessary components → execute via dispatch_or_push_action.
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
        jmp     dispatch_or_push_action                   // can execute immediately

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
        lda     current_preposition
        beq     select_preposition_for_current_verb_id    // determine preposition if missing
        lda     indirect_object_idx_lo
        bne     commit_execute_sentence
        jmp     refresh_sentence_bar_trampoline         // no IO → refresh and exit

        // ------------------------------------------------------------
        // All parts ready → execute
        // ------------------------------------------------------------
commit_execute_sentence:
        jmp     dispatch_or_push_action

        // ------------------------------------------------------------
        // Preposition determination path
        // ------------------------------------------------------------
select_preposition_for_current_verb_id:
        jsr     select_preposition_for_verb
        bne     save_preposition_and_request_ui_refresh
        jmp     dispatch_or_push_action                   // no prep required → execute

save_preposition_and_request_ui_refresh:
        sta     current_preposition                      // save selected preposition
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh        // request UI refresh
        // fall through to refresh_sentence_bar

/*
================================================================================
  handle_new_kid_verb
================================================================================
Summary
	Handle the “New kid” verb. In normal control mode, map the cursor’s X position
	to kid 0/1/2, refresh the UI, and attempt a kid switch. Outside normal control,
	just normalize the verb/UI and exit.

Global Inputs
	control_mode                     Current control mode (must be NORMAL_CONTROL_MODE)
	cursor_x_pos_quarter_relative    Cursor X on the sentence bar (quarter-pixel units)

Global Outputs
	current_verb_id                  Set to WALK_TO_VERB
	sentence_bar_needs_refresh       Set to UI_REFRESH_REQUEST
	(current_kid_idx)                May change via switch_active_kid_if_different

Description
	* If control_mode ≠ NORMAL_CONTROL_MODE:
		  • Normalize UI: set current_verb_id = WALK_TO_VERB and request a bar refresh.
		  • Return.
	* Otherwise:
		  • Determine kid by cursor X:
			  – X < SECOND_KID_NAME_COLUMN  → kid 0
			  – SECOND_KID_NAME_COLUMN ≤ X ≤ THIRD_KID_NAME_COLUMN → kid 1
			  – X > THIRD_KID_NAME_COLUMN   → kid 2
		  • Push kid index on stack (PHA), reset UI to WALK_TO and refresh the bar,
		  then POP and call switch_active_kid_if_different.
		  • Normalize UI again to WALK_TO and request refresh before returning.

Notes
	* The routine always leaves the UI in the neutral “Walk to” state.
	* If the selected kid is already active, switch_active_kid_if_different exits
	  early and no camera/inventory updates occur.
================================================================================
*/
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
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh
        jsr     refresh_sentence_bar             // update UI
        pla                                      // restore kid index
        jsr     switch_active_kid_if_different          // perform kid switch

        // normalize verb/UI again and exit
set_walk_and_exit:		
        lda     #WALK_TO_VERB
        sta     current_verb_id
        lda     #UI_REFRESH_REQUEST
        sta     sentence_bar_needs_refresh
        rts

/*
================================================================================
  process_sentence_queue_entry
================================================================================

Summary
	Process one stacked sentence at the top of the sentence stack. If all parts
	are ready, execute immediately. Otherwise stage walking toward the needed
	object. Skips invalid or redundant entries and advances the stack pointer.

Global Inputs
	sentstk_top_idx                   Current top index (–1 when empty)
	stacked_verb_ids[]                Stacked verb ids
	stacked_do_id_lo/hi[]             Stacked direct-object ids
	stacked_prep_ids[]                Stacked preposition ids
	stacked_io_id_lo/hi[]             Stacked indirect-object ids
	destination_entity                Nonzero if a walk is already in progress
	control_mode
	current_kid_idx, actor_vars[]     Actor state; includes ACTOR_IS_FROZEN
	object_attributes[]               For inventory/owner checks

Global Outputs
	sentstk_top_idx                   Decremented on consume or drop
	sentstk_free_slots                Decremented on consume; reset on underflow
	active_verb_id                    Latched verb for execution
	active_do_id_lo/hi                Latched DO id
	active_prep_id                    Latched preposition
	active_io_id_lo/hi                Latched IO id
	destination_entity                Set when staging a walk
	var_destination_x/var_destination_y
	active_costume                    Acting costume for pathing

Description
	* Early exits:
		  • If a destination is already active, do nothing.
		  • If the stack is empty, do nothing.
	* Redundancy filter:
		• If a preposition is present and DO == IO, drop the entry.
	* Inventory gate:
		  • Check DO and IO ownership for the current kid. If neither is owned,
		  attempt to push a scripted “Pick Up <obj>” for DO, else IO. If neither
		  supports Pick Up, drop the entry.
	* Activation:
		  • Copy the top entry into active_* and pop the stack. Protect against
		  underflow by restoring empty-state invariants and capacity.
	* Dispatch:
		  • If DO is in inventory and no preposition → execute now.
		  • If a preposition exists, ensure IO readiness; if IO not ready → walk to IO.
		  • If DO not in inventory:
			  – In keypad mode → execute directly (no walking).
			  – Otherwise → walk to DO.

Notes
	* Ownership checks use the object’s “in inventory” flag plus owner nibble.
	* Walking publishes var_destination_{x,y}, resolves the destination entity,
	  and respects the ACTOR_IS_FROZEN gate.
================================================================================
*/
* = $0B9C
process_sentence_queue_entry:
        // ------------------------------------------------------------
        // Handle one stacked sentence.
        //
        // This processes a stacked verb sentence into either:
        // - Walking toward a needed object, or
        // - Immediate verb execution if all parts are ready.
        //
        // It skips invalid or redundant sentences and advances
        // the stack pointer.
        // ------------------------------------------------------------
        lda     destination_entity
        beq     check_queue_nonempty     // skip if no current destination
        rts                                    // destination active → exit

check_queue_nonempty:
        ldx     sentstk_top_idx
        bpl     check_same_complements
        rts                                    // stack empty → exit

        // ------------------------------------------------------------
        // Skip identical complement sentences (DO == IO)
        // ------------------------------------------------------------
check_same_complements:
        lda     stacked_prep_ids,x
        beq     set_active_sentence_tokens       // no preposition → skip test

        lda     stacked_do_id_lo,x
        beq     verify_inventory_status  // no DO → skip identical check
        cmp     stacked_io_id_lo,x
        bne     verify_inventory_status
        lda     stacked_do_id_hi,x
        cmp     stacked_io_id_hi,x
        bne     verify_inventory_status

        dec     sentstk_top_idx            // identical DO/IO → drop sentence
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
        jsr     push_pickup_for_sentence_part
        rts

try_indirect_object_pickup:
        lda     #$02
        jsr     has_pickup_script_for_sentence_part
        cmp     #$01
        bne     drop_sentence_no_pickup_available
        lda     #$02
        jsr     push_pickup_for_sentence_part
        rts

drop_sentence_no_pickup_available:
        dec     sentstk_top_idx            // no pickup scripts → drop sentence
        rts

        // ------------------------------------------------------------
        // Activate current sentence: copy into active_* vars
        // ------------------------------------------------------------
set_active_sentence_tokens:
        ldy     sentstk_top_idx
        lda     stacked_verb_ids,y
        sta     active_verb_id
        lda     stacked_do_id_lo,y
        sta     active_do_id_lo
        lda     stacked_do_id_hi,y
        sta     active_do_id_hi
        lda     stacked_prep_ids,y
        sta     active_prep_id
        lda     stacked_io_id_lo,y
        sta     active_io_id_lo
        lda     stacked_io_id_hi,y
        sta     active_io_id_hi

        dec     sentstk_top_idx
        dec     sentstk_free_slots
        bpl     queue_capacity_ok

        // stack underflow → reset state
        lda     #SENT_STACK_EMPTY_IDX
        sta     sentstk_top_idx
        lda     #SENT_STACK_MAX_TOKENS
        sta     sentstk_free_slots
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

/*
================================================================================
  dispatch_or_push_action
================================================================================
Summary
	Prepare the sentence stack for a new command, then either:
		* Walk immediately to the cursor location for a bare “Walk to”, or
		* Push a complete sentence (verb + DO [+ prep + IO]) onto the stacks and
		  normalize the UI back to “Walk to”.

Global Inputs
	current_verb_id
	direct_object_idx_lo, direct_object_idx_hi
	preposition
	indirect_object_idx_lo, indirect_object_idx_hi
	cursor_x_pos_quarter_absolute, cursor_y_pos_half_off_by_8
	current_kid_idx
	actor_for_costume[]               costume → actor index map
	actor_vars[]                      includes ACTOR_IS_FROZEN flag
	WALK_TO_VERB, WHAT_IS_VERB
	SENT_STACK_MAX_TOKENS, SENT_STACK_EMPTY_IDX

Global Outputs
	sentstk_free_slots                reset to SENT_STACK_MAX_TOKENS
	sentstk_top_idx                   reset to SENT_STACK_EMPTY_IDX; incremented on push
	stacked_verb_ids[x]               written on push
	stacked_do_id_lo/hi[x]            written on push
	stacked_prep_ids[x]               written on push
	stacked_io_id_lo/hi[x]            written on push
	current_verb_id                   reset to WALK_TO_VERB after pushing non-walk verbs
	direct_object_idx_lo              cleared after pushing non-walk verbs
	preposition                       cleared after pushing non-walk verbs
	destination_entity                cleared before exit
	active_costume                    set for bare “Walk to”
	actor                             latched actor for movement path
	var_destination_x, var_destination_y
	actor_x_dest[], actor_y_dest[]    written for immediate walk
	(via call) snap_and_stage_path_update()

Description
	* Reset sentence-stack bookkeeping: set sentstk_free_slots to the maximum
	  and sentstk_top_idx to “empty”. If the verb is WHAT_IS_VERB, return.

	* If the verb is WALK_TO and no direct object is selected, treat it as a
	  bare walk:
		  • Resolve the acting entity from current_kid_idx.
		  • Copy cursor coordinates to dest_x/dest_y and clamp to walkable space.
		  • Publish var_destination_x/y for debugging.
		  • If the actor is not frozen, write actor_x_dest/y_dest and call
		  snap_and_stage_path_update. Then return.

	* Otherwise push a full sentence:
		  • Increment sentstk_top_idx and write verb, DO, prep, IO into the
		  stacked_* arrays at index X.
		  • If the verb was not WALK_TO, reset UI defaults by setting
		  current_verb_id = WALK_TO_VERB and clearing DO and preposition.
		  • Clear destination_entity and return.

Notes
	* This routine does not execute verbs. It only stages immediate walking
	  for the bare “Walk to” case or records a sentence for later handling.
	* Indices and capacity are managed as a LIFO stack; range checks occur in
	  the push helpers.
================================================================================
*/
* = $0AF3
dispatch_or_push_action:
        // ------------------------------------------------------------
        // Reset sentence-stack bookkeeping
		//
        // Seeds free-capacity to max and resets head index to empty
        // sentinel. Prepares stack for a new sentence; actual
        // capacity decrementing occurs elsewhere.
        // ------------------------------------------------------------		
        lda     #SENT_STACK_MAX_TOKENS          
        sta     sentstk_free_slots     // Reset free-slot counter; decremented elsewhere

        lda     #SENT_STACK_EMPTY_IDX            
        sta     sentstk_top_idx             // Reset head to “no entries” for pre-increment use

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
        // walk-to with object are stacked for later execution; only a
        // bare walk-to proceeds to immediate movement.
        // ------------------------------------------------------------
handle_walk_to:
        cmp     #WALK_TO_VERB                   // Z=1 if current verb is "Walk to"
        bne     push_sentence                // Different verb → push full sentence

        lda     direct_object_idx_lo            // Test DO presence using low byte
        bne     push_sentence                // DO present → treat as full action and push

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
        bne     exit_dispatch_or_push_action            // If frozen → skip path setup and exit routine

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
exit_dispatch_or_push_action:
        rts

        // ------------------------------------------------------------
        // Queue a complete sentence for later execution
        // ------------------------------------------------------------
push_sentence:
        // ------------------------------------------------------------
        // Advance stack index
		//
        // Increments sentstk_top_idx to point to the next free
        // slot and loads it into X for writing the stacked sentence
        // tokens.
        // ------------------------------------------------------------
        inc     sentstk_top_idx           // Advance stack pointer to next free slot
        ldx     sentstk_top_idx           // X := current stack entry index for storing tokens

        // ------------------------------------------------------------
        // Write current sentence tokens into stack entry X
		//
        // Stores: verb, DO lo/hi, preposition, IO lo/hi into the
        // parallel stacked_sentence_* arrays at index X.
        // ------------------------------------------------------------
        lda     current_verb_id                    
        sta     stacked_verb_ids,x         

        lda     direct_object_idx_lo            	 
        sta     stacked_do_id_lo,x  
        lda     direct_object_idx_hi            	 
        sta     stacked_do_id_hi,x  
		
        lda     current_preposition                    		 
        sta     stacked_prep_ids,x  	 

        lda     indirect_object_idx_lo          		
        sta     stacked_io_id_lo,x  	
        lda     indirect_object_idx_hi          		
        sta     stacked_io_id_hi,x   

        // ------------------------------------------------------------
        // Post-stack verb reset gate
		//
        // If current_verb_id == WALK_TO_VERB, skip UI reset and exit;
        // otherwise fall through to reset defaults.
        // ------------------------------------------------------------
        lda     current_verb_id                   // Reload current verb for post-stack reset test
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
        sta     current_preposition                     // Clear preposition token

        // ------------------------------------------------------------
        // Finalize and exit
		//
        // Clears destination_entity to indicate no pending target and
        // returns. Queue state and tokens remain committed.
        // ------------------------------------------------------------
finalize_and_exit:
        lda     #$00                            // Prepare clear value
        sta     destination_entity              // Reset destination entity marker (none targeted)
        rts                                     // Return → sentence stacked or action completed


/*
================================================================================
  execute_verb_handler_for_object
================================================================================
Summary
	Run the correct verb logic for the active direct object. Prefer a custom
	per-object handler; otherwise fall back to defaults:

		* GIVE with a kid recipient transfers ownership and refreshes inventory.
		* WALK TO does nothing and returns.
		* All other verbs run the global “defaults” script (#3).
		  A guard enforces that READ requires lights; if dark, use defaults.

Global Inputs
	active_do_id_lo, active_do_id_hi      Object to act on
	active_verb_id                         Verb to execute
	active_io_id_lo                        Recipient for GIVE
	object_attributes[]                    Per-object attribute byte; hi nibble kept, low nibble = owner id
	global_lights_state                    Nonzero if lights are on
	room_obj_ofs                           Per-room base offset for object scripts

Global Outputs
	sentence_bar_needs_refresh             Set TRUE to redraw the sentence bar
	resource_index_for_script_slot         Bound to object’s resource for script VM
	script_type_for_script_slot            Script type associated with resource
	var_active_verb_id                     Latched active verb for defaults
	current_script_slot                    Cleared before launching scripts
	script_offsets_lo, script_offsets_hi   Computed handler offset (custom path)
	var_active_io_id_lo                    IO passed to custom handler
	object_attributes[x]                   Updated owner on GIVE→kid
	(refresh) refresh_items_displayed       Invoked after ownership change

Description
	* Mark UI for refresh.
	* Resolve active DO’s resource and bind it to the script VM slot metadata.
	* Look up a custom handler for active_verb_id:
		  • If absent:
			  – If verb = GIVE and IO is a kid: write new owner into object_attributes
			  and refresh inventory; return.
			  – If verb = WALK TO: return.
			  – Else launch the global defaults script (#3) with var_active_verb_id.
		  • If present:
			  – If verb = READ and lights are off: fall back to defaults script.
			  – Else compute absolute script offset (room_obj_ofs + handler_ofs),
			  seed VM state (var_active_io_id_lo, script_offsets_*), set base and
			  read address, then execute_next_operation.
================================================================================
*/
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
  find_object_verb_handler_offset
================================================================================
Summary
	Scan an object’s verb-handler table and return the handler script offset for a
	requested verb. Supports a wildcard “default” handler and a table terminator.

Arguments
	.A      Requested verb id to look up

Vars/State
	verb_index              Latched copy of requested verb id for comparisons

Global Inputs
	object_rsrc_ptr         ZP pointer to start of the object resource

Returns
	.A      Handler script offset (nonzero) if a matching entry is found
			DEFAULT_VERB_WALK_TO (#$0D) if no entry and verb == Walk to
			NO_HANDLER_RET (#$00) if no entry and verb ≠ Walk to

Description
	* Initialize Y so the first INY, INY lands on the first handler entry at
	  VERB_TABLE_START_OFS: each entry is {verb_id, handler_ofs}.
	* Loop over entries:
	  • If verb_id == #$00 → end of table:
		  – If requested verb == WALK_TO_VERB, return #$0D.
		  – Else return #$00.
	  • If verb_id == DEFAULT_VERB (#$0F) → match any verb: return its handler_ofs.
	  • If verb_id == requested verb → return its handler_ofs.
	  • Otherwise advance to the next pair and continue scanning.

Notes
	* Table encoding: pairs of one-byte verb_id followed by one-byte offset,
	  terminated by verb_id == #$00.
	* Clobbers Y during the scan. X is preserved by caller.
================================================================================
*/
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
  has_pickup_script_for_sentence_part
================================================================================
Summary
	Checks whether the selected sentence object (direct or indirect) defines a
	custom “Pick Up” verb handler. Actor-class targets are rejected up front.

Arguments
	.A      Selector for which object to test:
			ARG_IS_DO (#$01) → check direct object
			ARG_IS_IO (#$02) → check indirect object

Global Inputs
	sentstk_top_idx         Top index into the sentence stacks
	stacked_do_id_lo/hi     Direct-object ID pair at each stack entry
	stacked_io_id_lo/hi     Indirect-object ID pair at each stack entry

Returns
	.A      TRUE  (#$01) if a Pick Up handler exists for the chosen object
			FALSE (#$00) if absent or the target is an actor

Description
	* Read the current stack entry (sentstk_top_idx) and select DO or IO per .A.
	* If the object type equals OBJ_TYPE_ACTOR, return FALSE (actors aren’t pickable).
	* Otherwise resolve the object resource, then scan its handler table for
	  PICK_UP_VERB using find_object_verb_handler_offset.
	* Return TRUE if a nonzero handler offset is found; else return FALSE.
	* X and Y are preserved via temp_x and temp_y.

Notes
	* This routine performs no UI changes and does not mutate the sentence stacks.
	* The object “type” check occurs before any script lookup for a fast reject.
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
        // Load current stack index into Y
        // ------------------------------------------------------------
        ldy     sentstk_top_idx

        // ------------------------------------------------------------
        // DO vs IO selector: A == #ARG_IS_DO → direct object
        // ------------------------------------------------------------
        cmp     #ARG_IS_DO
        bne     load_indirect_object_index

        // ------------------------------------------------------------
        // Load direct object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     stacked_do_id_lo,y
        lda     stacked_do_id_hi,y
        jmp     check_actor_class

load_indirect_object_index:
        // ------------------------------------------------------------
        // Load indirect object index (lo→X, hi→A)
        // ------------------------------------------------------------
        ldx     stacked_io_id_lo,y
        lda     stacked_io_id_hi,y

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
/*
================================================================================
 is_sentence_object_owned_by_current_kid
================================================================================
Summary
	Determine if the selected sentence object (direct or indirect) is owned by the
	current kid. Uses the sentence stack top index to select DO or IO, then checks
	inventory flags and the owner nibble.

Arguments
	.A      Selector for which object to check:
				ARG_IS_DO (#$01) → check direct object
				ARG_IS_IO (#$02) → check indirect object

Global Inputs
	sentstk_top_idx         Top index into the sentence stacks
	stacked_do_id_lo/hi     Direct object ID pair for each stack entry
	stacked_io_id_lo/hi     Indirect object ID pair for each stack entry
	object_attributes[]     Per-object attribute byte; low nibble = owner id
	current_kid_idx         Index of the current kid


Returns
	.A      INVCHK_RET_FOUND  (#$01) if the object is in current kid’s inventory
			INVCHK_RET_NOT_FOUND (#$00) otherwise

Description
	* Select DO or IO from the sentence stacks using sentstk_top_idx.
	* The object’s high-byte ID acts as an “in some inventory” flag:
	  #$00 → the object is in an inventory; nonzero → not in any inventory.
	* If in an inventory, read object_attributes[y], mask the owner nibble, and
	  compare to current_kid_idx. Match → FOUND, else NOT_FOUND.
	* X is preserved via temp_x; Y exits holding the object index used for lookup.

Notes
	* High-byte semantics: #$00 means “in inventory somewhere,” not necessarily
	  owned by the current kid. Ownership is decided by the low-nibble owner id.
================================================================================
*/
* = $0EAE
is_sentence_object_owned_by_current_kid:
        // ------------------------------------------------------------
        // Save X register to temporary
        // ------------------------------------------------------------
        stx     temp_x

        // ------------------------------------------------------------
        // Fetch index of current sentence part being analyzed
        // ------------------------------------------------------------
        ldx     sentstk_top_idx

        // ------------------------------------------------------------
        // Are we checking for a direct object?
        // ------------------------------------------------------------
        cmp     #ARG_IS_DO
        bne     load_indirect_object_id

        // ------------------------------------------------------------
        // Direct object path
        // ------------------------------------------------------------
        ldy     stacked_do_id_lo,x
        lda     stacked_do_id_hi,x
        jmp     guard_in_some_inventory

load_indirect_object_id:
        // ------------------------------------------------------------
        // Indirect object path
        // ------------------------------------------------------------
        ldy     stacked_io_id_lo,x
        lda     stacked_io_id_hi,x

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
  push_pickup_for_sentence_part
================================================================================
Summary
	Push a new “Pick Up <object>” sentence onto the sentence stack for either the
	direct object (DO) or the indirect object (IO) selected in the current stack
	entry. Copies the chosen object ID, clears the preposition, and writes the
	PICK_UP_VERB into the stacked verb list.

Arguments
	.A      Complement selector:
			ARG_IS_IO (#$02) → use indirect object from current entry
			ARG_IS_DO (#$01) → use direct object from current entry

Vars/State
	temp_x                  Saves X across the routine
	object_ptr              ZP scratch: receives selected object id (lo/hi)

Global Inputs
	sentstk_top_idx         Current sentence stack top index
	stacked_do_id_lo/hi     DO id pairs at each stack entry
	stacked_io_id_lo/hi     IO id pairs at each stack entry

Global Outputs
	sentstk_top_idx         Incremented on successful push
	stacked_verb_ids[x]     Written with PICK_UP_VERB
	stacked_prep_ids[x]     Cleared to #$00 (no preposition)
	stacked_do_id_lo/hi[x]  Written with selected object id

Description
	* Preserve X in temp_x.
	* Read current stack top (sentstk_top_idx). Select DO or IO based on .A.
	* Copy the chosen object id into object_ptr (lo/hi).
	* Increment sentstk_top_idx and range-check against SENT_STACK_MAX_TOKENS.
		  • On overflow: write debug_error_code (#$2D), map I/O (cpu_port ← MAP_IO_ON),
		  and loop toggling vic_border_color_reg forever.
		  • Otherwise: write PICK_UP_VERB, clear stacked_prep_ids, and store the
		  object id into stacked_do_id_lo/hi at the new top.
	* Restore X and return.

Notes
	* This is a push-only helper. It does not validate whether the object
	  actually supports a Pick Up handler; call has_pickup_script_for_sentence_part
	  beforehand to gate usage.
	* The push writes the object into the DO fields by design; PICK_UP operates
	  on a single direct object without a preposition or IO.
================================================================================
*/
* = $0EE1
push_pickup_for_sentence_part:
        // ------------------------------------------------------------
        // Save X
        // ------------------------------------------------------------
        stx     temp_x

        // ------------------------------------------------------------
        // Fetch current stack index
        // ------------------------------------------------------------
        ldx     sentstk_top_idx

        // ------------------------------------------------------------
        // Is the complement the indirect object? 
        // ------------------------------------------------------------
        cmp     #ARG_IS_IO
        bne     fetch_direct_object

        // ------------------------------------------------------------
        // Indirect object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     stacked_io_id_lo,x
        sta     <object_ptr
        lda     stacked_io_id_hi,x
        sta     >object_ptr
        jmp     next_sentence_index

fetch_direct_object:
        // ------------------------------------------------------------
        // Direct object → copy indices into object_ptr
        // ------------------------------------------------------------
        lda     stacked_do_id_lo,x
        sta     <object_ptr
        lda     stacked_do_id_hi,x
        sta     >object_ptr

next_sentence_index:
        // ------------------------------------------------------------
        // Advance stack index and validate range
        // ------------------------------------------------------------
        inc     sentstk_top_idx
        ldx     sentstk_top_idx
        cpx     #SENT_STACK_MAX_TOKENS
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
        sta     stacked_verb_ids,x
        lda     #$00
        sta     stacked_prep_ids,x
        lda     <object_ptr
        sta     stacked_do_id_lo,x
        lda     >object_ptr
        sta     stacked_do_id_hi,x

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
	current_kid_idx            active kid id
	current_script_slot        active script slot id

Global Outputs
	current_kid_idx            ← kid_ids[X]        (if different)
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
	sentence_bar_needs_refresh     ← TRUE
	sentstk_free_slots   		   ← SENT_STACK_MAX_TOKENS
	sentstk_top_idx           	   ← SENT_STACK_EMPTY_IDX
	current_verb_id                ← WALK_TO_VERB
	direct_object_idx_lo           ← $00
	direct_object_idx_hi           ← $00
	preposition                    ← $00
	indirect_object_idx_lo         ← $00
	indirect_object_idx_hi         ← $00

Description
	• Clear any active destination so pathing is idle.
	• Request a sentence bar refresh on next UI pass.
	• Restore sentence stack to empty with full capacity.
	• Set default verb to “Walk to”.
	• Clear all sentence complements: DO, preposition, IO.
================================================================================
*/
* = $29CC
init_sentence_ui_and_queue:
        // ------------------------------------------------------------
        // Reset all components of the sentence stack system
        //
        // This restores the verb UI state to a default “Walk to” state,
        // empties the token stack, and signals the UI to redraw.
        // ------------------------------------------------------------
        lda     #ENTITY_NONE
        sta     destination_entity            // clear current destination target

        lda     #TRUE
        sta     sentence_bar_needs_refresh     // force UI refresh of sentence bar

        lda     #SENT_STACK_MAX_TOKENS
        sta     sentstk_free_slots  // reset available slots (max = 6)

        lda     #SENT_STACK_EMPTY_IDX
        sta     sentstk_top_idx          // mark stack as empty (no active tokens)

        lda     #WALK_TO_VERB
        sta     current_verb_id                  // set default verb “Walk to”

        lda     #$00
        sta     direct_object_idx_lo          // clear direct object index (lo)
        sta     direct_object_idx_hi          // clear direct object index (hi)
        sta     current_preposition                   // clear preposition field
        sta     indirect_object_idx_lo        // clear indirect object index (lo)
        sta     indirect_object_idx_hi        // clear indirect object index (hi)

        rts
