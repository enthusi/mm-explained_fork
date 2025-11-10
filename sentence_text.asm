#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "text_data.inc"
#import "sentence_action.asm"

.const SENTENCE_BAR_LAST_IDX    = $27    // Last valid cell index (0..$27 ⇒ 40 cells wide)
.const SENTENCE_BAR_END_IDX     = SENTENCE_BAR_LAST_IDX + 1  // One past last cell (#$28 sentinel)

.const OBJ_PREP_BYTE_OFS        = $0B    // Object byte +$0B: USE preposition encoded in bits 7..5
.const OBJ_FOUND_IN_INV         = $00    // Resolver status: found in inventory; obj_ptr_* valid
.const OBJ_FOUND_IN_ROOM        = $01    // Resolver status: found in room; obj_ptr_* valid
.const OBJ_NOT_FOUND            = $FF    // Resolver status: not found; obj_ptr_* unspecified

.const OWNER_IS_ROOM            = $0F    // Owner nibble value meaning “room/no owner”
.const MAX_INVENTORY_INDEX      = $2C    // Highest inventory slot index (scan starts here)

.const OBJ_NAME_FOUND           = $00    // Return code: name pointer resolved successfully

.const KID2_NAME_OFS            = $CEDD  // Sentence bar absolute address of kid #2 name slot
.const KID3_NAME_OFS            = $CEEA  // Sentence bar absolute address of kid #3 name slot

.label sentence_bar_len_used     = $0E72  // Tracks number of characters currently used in the sentence bar
.label sentence_bar_ptr          = $19    // ZP pointer to destination buffer for sentence bar text (lo=$19, hi=$1A)
.label sentence_word_ptr         = $15    // ZP pointer to source word string being appended (lo=$15, hi=$16)

.label obj_idx                   = $15    // ZP: temporary index/pointer used for object id or resource lookup
.label obj_ptr                   = $15    // ZP pointer to resolved object resource or name (lo/hi shared alias)
.label room_obj_ofs              = $17    // ZP: temporary 16-bit offset to object data within room resource

/*
================================================================================
  refresh_sentence_bar
================================================================================

Summary
	Conditionally rebuild the sentence bar. Proceeds only if the refresh flag
	is set and the control mode is neither CUTSCENE nor KEYPAD. Clears the
	refresh flag, then either exits or tail-calls the builder.

Vars/State
	sentence_bar_needs_refresh   nonzero → refresh requested
	control_mode                0=CUTSCENE, 1=KEYPAD, ≥2=normal gameplay

Global Outputs
	sentence_bar_needs_refresh   cleared on entry
	(sentence bar buffer updated indirectly via build_sentence_bar_text)

Description
	• Test sentence_bar_needs_refresh. If zero, return immediately.
	• Clear sentence_bar_needs_refresh.
	• Gate by control_mode:
		– control_mode == KEYPAD_CONTROL_MODE → return.
		– control_mode <= CUTSCENE → return.
		– control_mode ≥ first normal mode → jump build_sentence_bar_text.
================================================================================
*/
* = $08C6
refresh_sentence_bar:
		// ------------------------------------------------------------
		// Refresh gate - proceed only if refresh flag is set
		//
		// If zero → jump return_no_refresh; else → control-mode gate
		// ------------------------------------------------------------	
        lda     sentence_bar_needs_refresh      // A := refresh flag
        bne     gate_by_control_mode           // nonzero → proceed
        jmp     return_no_refresh              // zero → exit

		// ------------------------------------------------------------
		// Control-mode gate — refresh allowed only in normal modes
		//
		// if control_mode == CUTSCENE or KEYPAD → exit;
		// else jump to build_sentence_bar_text
		// ------------------------------------------------------------
gate_by_control_mode:
        // Clear flag, then gate by control mode
        lda     #$00                           // A := 0
        sta     sentence_bar_needs_refresh      // clear refresh flag
        lda     control_mode                   // A := control mode
        cmp     #KEYPAD_CONTROL_MODE           // compare with keypad mode
        beq     return_no_refresh              // keypad → exit
        bcs     build_sentence_bar_text        // > keypad (i.e., >$01) → build

return_no_refresh:
        rts                                    // done

build_sentence_bar_text:
        // ------------------------------------------------------------
        // Build full sentence bar string from active verb and object data
        // ------------------------------------------------------------
        jsr     sentence_bar_init_and_clear       // initialize and clear sentence bar text buffer
        lda     #$00                              
        sta     sentence_bar_len_used             // clear accumulated text length

        // ------------------------------------------------------------
        // Check if current verb is "NEW KID"
        // If so, directly print kid names and exit routine
        // ------------------------------------------------------------
        lda     current_verb_id                      // load currently selected verb id
        cmp     #NEW_KID_VERB                     // compare against "NEW KID" verb id
        bne     ensure_default_verb               // not equal → continue with regular sentence build
        jmp     write_kid_names_to_sentence_bar   // equal → delegate to kid-name writer and return

ensure_default_verb:
        // ------------------------------------------------------------
        // Ensure a valid verb is set before sentence construction
        // ------------------------------------------------------------
        cmp     #$00                              // check if no verb currently selected
        bne     append_verb                       // if not zero → a verb already exists, skip default
        lda     #WALK_TO_VERB                     // load default verb id ("WALK TO")
        sta     current_verb_id                      // store it as the active verb

append_verb:
        // ------------------------------------------------------------
        // Append the current verb token to the sentence bar
        // ------------------------------------------------------------
        tax                                     // X := verb id (index into verb pointer tables)
        lda     verb_pointers_hi,x              
        sta     sentence_word_ptr+1             
        lda     verb_pointers_lo,x              
        sta     sentence_word_ptr               
        jsr     append_word_to_sentence_bar     // append verb string to sentence bar

        // ------------------------------------------------------------
        // Append direct object if present
        // ------------------------------------------------------------
        lda     direct_object_idx_lo            // A := direct object lo-id; zero → no object
        beq     return_refreshed_sentence       // none → finish sentence build
        tax                                     // X := lo-id (resolver requires X)
        lda     direct_object_idx_hi            // A := hi-id (resolver requires A)
        jsr     resolve_object_name             // obj_ptr → object name string
        jsr     append_word_to_sentence_bar     // append name at obj_ptr to sentence bar

        // ------------------------------------------------------------
        // Determine and append preposition
        // ------------------------------------------------------------
        lda     current_preposition                     // A := current preposition index (0 = unset)
        bne     append_preposition_if_any       // already set → skip inference

        jsr     select_preposition_for_verb     // infer preposition from verb/object context → A
        cmp     #PREPOSITION_NONE_NEEDED        // is inference "none needed"?
        beq     append_preposition_if_any       // yes → do not set, just skip append

        sta     current_preposition                     // cache inferred preposition for later reuse

        lda     #$00                            // clear any prior indirect object because
        sta     indirect_object_idx_lo          // grammar changed after inference
        sta     indirect_object_idx_hi

append_preposition_if_any:
        lda     current_preposition                     // A := preposition index; 0 → none selected
        beq     return_refreshed_sentence       // no preposition → finish sentence build

        tax                                     // X := preposition index (table selector)
        lda     preposition_pointers_hi,x       
        sta     sentence_word_ptr+1             
        lda     preposition_pointers_lo,x       
        sta     sentence_word_ptr               
        jsr     append_word_to_sentence_bar     // append preposition string to sentence bar

        // ------------------------------------------------------------
        // "Give" verb special case: validate indirect object type
        // ------------------------------------------------------------
        lda     current_verb_id                  // A := current verb id
        cmp     #GIVE_VERB                    // GIVE verb?
        bne     append_indirect_if_present    // no → skip validation

        lda     indirect_object_idx_hi        // A := indirect object's hi-id
        cmp     #OBJ_HI_COSTUME               // valid only if hi-id is a costume
        beq     append_indirect_if_present    // valid recipient → keep as-is

        lda     #$00                          // invalid recipient → clear indirect object
        sta     indirect_object_idx_lo        
        sta     indirect_object_idx_hi        

append_indirect_if_present:
        // ------------------------------------------------------------
        // Append indirect object (costume or object)
        // ------------------------------------------------------------
        lda     indirect_object_idx_lo        // A := indirect lo-id; 0 → none present
        beq     return_refreshed_sentence     // no indirect → finish sentence build

        tax                                   // X := indirect lo-id (resolver input)
        lda     indirect_object_idx_hi        // A := indirect hi-id (type selector)
        cmp     #OBJ_HI_COSTUME               // costume type? (#$02 = costume)
        bne     resolve_and_append_object_name// no → handle as object

        jsr     resolve_costume_name          // set source ptr to costume name
        jmp     commit_append                 // append and exit this branch

resolve_and_append_object_name:
        jsr     resolve_object_name            // obj_ptr → object name string for current (X/A) id
commit_append:
        jsr     append_word_to_sentence_bar    // append string at sentence_word_ptr/obj_ptr to bar
return_refreshed_sentence:
        rts                                     
/*
================================================================================
  write_kid_names_to_sentence_bar
================================================================================
Summary
	Write the three selected kids’ names into fixed slots of the sentence bar
	text buffer. Slot 1 uses the current destination pointer. Slots 2 and 3
	are written after explicitly setting the destination pointer to their slot
	addresses.

Global Inputs
	 kid1_costume_id                 costume id for kid #1
	 kid2_costume_id                 costume id for kid #2
	 kid3_costume_id                 costume id for kid #3
	 sentence_bar_ptr                ZP dest pointer for sentence text

Global Outputs
	 sentence_bar_ptr                advanced by append routine as it writes

Description
	• Kid #1: X := kid1_costume_id → resolve_costume_name → append_word_to_sentence_bar.
	• Kid #2: set sentence_bar_ptr := slot #2 address → X := kid2_costume_id →
	resolve_costume_name → append_word_to_sentence_bar.
	• Kid #3: set sentence_bar_ptr := slot #3 address → X := kid3_costume_id →
	resolve_costume_name → append_word_to_sentence_bar.

Notes
	• First slot relies on the caller’s initial sentence_bar_ptr configuration.
	• Name length handling and truncation are owned by append_word_to_sentence_bar.
================================================================================
*/
* = $096F
write_kid_names_to_sentence_bar:
		// ------------------------------------------------------------
		// First kid: use current sentence_bar_ptr for slot #1
		// ------------------------------------------------------------
		ldx     kid1_costume_id
		jsr     resolve_costume_name
		jsr     append_word_to_sentence_bar

		// ------------------------------------------------------------
		// Second kid: reposition buffer pointer to slot #2 and print
		// ------------------------------------------------------------
		lda     #<KID2_NAME_OFS
		sta     <sentence_bar_ptr
		lda     #>KID2_NAME_OFS
		sta     sentence_bar_ptr + 1
		ldx     kid2_costume_id
		jsr     resolve_costume_name
		jsr     append_word_to_sentence_bar

		// ------------------------------------------------------------
		// Third kid: reposition buffer pointer to slot #3 and print
		// ------------------------------------------------------------
		lda     #<KID3_NAME_OFS
		sta     <sentence_bar_ptr
		lda     #>KID3_NAME_OFS
		sta     sentence_bar_ptr + 1
		ldx     kid3_costume_id
		jsr     resolve_costume_name
		jsr     append_word_to_sentence_bar

		rts
/*
================================================================================
  resolve_costume_name
================================================================================
Summary
	Load the address of a costume’s name string into sentence_word_ptr so the
	name can be appended to the sentence bar.

Arguments
	.X  		Character index (0..N)
================================================================================
*/
* = $09D8
resolve_costume_name:
        // ------------------------------------------------------------
        // Resolve costume name pointer from index
        //
        // - Use .X as costume index into pointer tables.
        // - Load high and low bytes from name_ptr_hi_tbl/lo.
        // - Combine into sentence_word_ptr for later append.
        // ------------------------------------------------------------
        lda     name_ptr_hi_tbl,x      
        sta     >sentence_word_ptr        
        lda     name_ptr_lo_tbl,x      
        sta     <sentence_word_ptr        
        rts                          
/*
================================================================================
  resolve_object_name
================================================================================

Summary
	Resolves an object’s name pointer by reading its name offset from the object
	resource. Returns A = OBJ_NAME_FOUND if successful. If the object cannot be
	resolved, resets the sentence system and exits.

Arguments
	.X  object_lo_id        low byte of object id
	.A  object_hi_id        high byte of object id

Vars/State
	 obj_ptr				zero-page pointer to object resource

Returns
	.A = OBJ_NAME_FOUND              object found, obj_ptr now points to name
									(sentence queue reset if not found)

Global Outputs
	 obj_ptr				now point to name string

Description
	• Calls resolve_object_resource to find the object resource base.
	• If not found, calls init_sentence_ui_and_stack and returns.
	• If found, reads byte at offset OBJ_NAME_OFS, adds it to obj_ptr_lo/hi to
	relocate pointer to the object’s name string.
	• Returns with A = OBJ_NAME_FOUND and obj_ptr_* pointing directly to the
	name data.

Notes
	• The offset is one byte; a carry increment handles page crossing.
	• No bounds check is performed on the offset.
================================================================================
*/
* = $09E3
resolve_object_name:
		// ------------------------------------------------------------
		// Resolve object resource pointer from given object index/hi-id
		// ------------------------------------------------------------
		jsr     resolve_object_resource          // sets object_pointer to base
		cmp     #OBJ_NOT_FOUND                   // found?
		bne     object_found

		// ------------------------------------------------------------
		// Object not found → invalidate sentence and return
		// ------------------------------------------------------------
		jsr     init_sentence_ui_and_stack
		rts

object_found:
		// ------------------------------------------------------------
		// Add name offset (+OBJ_NAME_OFS) to object base pointer → points to name
		// ------------------------------------------------------------
		ldy     #OBJ_NAME_OFS                    // offset of name pointer byte
		lda     (obj_ptr),y               		 // A := name offset
		clc
		adc     <obj_ptr                   		 // add to base pointer (lo)
		sta     <obj_ptr
		bcc     done                             // no carry → done
		inc     >obj_ptr                 		 // carry → increment hi byte

done:
		lda     #OBJ_NAME_FOUND
		rts
/*
================================================================================
resolve_object_if_not_costume
================================================================================
Summary
	If the object’s hi-id indicates a costume (OBJ_HI_COSTUME), do not resolve
	and return OBJ_NOT_FOUND. Otherwise delegate to resolve_object_resource with
	the same inputs.

Arguments
	.X  	obj_idx_lo   Lo-id of target object
	.A  	obj_idx_hi   Hi-id of target object

Returns
	.A = OBJ_NOT_FOUND          If hi-id = OBJ_HI_COSTUME
	
	Otherwise, pass-through from resolve_object_resource:
		OBJ_FOUND_IN_INV        Found in inventory
		OBJ_FOUND_IN_ROOM       Found in room
		OBJ_NOT_FOUND           Not found

Notes
	This wrapper performs a single sentinel check, then tail-calls the main
	resolver. obj_ptr_* is unchanged when the object is a costume.
================================================================================
*/
* = $09FE
resolve_object_if_not_costume:
        // ------------------------------------------------------------
        // Guard against costume objects
        //
        // - Compares hi-id in A against OBJ_HI_COSTUME sentinel.
        // - If not a costume, delegate to resolve_object_resource.
        // - Costume objects are excluded from resolution (return #$FF).
        // ------------------------------------------------------------
        cmp     #OBJ_HI_COSTUME          // is hi-id the costume sentinel?
        bne     resolve_object_resource   // no → resolve as regular object

        lda     #OBJ_NOT_FOUND           // costume objects not handled here
        rts                               // return “not found / not applicable”
/*
================================================================================
  resolve_object_resource
================================================================================

Summary
	Resolve an object’s absolute data pointer (obj_ptr_lo/hi) from an incoming
	object id pair {lo, hi}. Prefer inventory for movable items owned by the
	player. Otherwise search the current room’s object list. Return a status
	code in .A indicating where the object was found or if it was not found.

Arguments
	.X  		obj_idx_lo   Lo-id of target object
	.A  		obj_idx_hi   Hi-id of target object 

Vars/State
	obj_idx		               scratch: latched id input
	obj_ptr            		   absolute pointer to object data
	room_obj_ofs		       room-local offset to object data

Returns
	.A		OBJ_FOUND_IN_INV        Found in inventory, obj_ptr_* valid
			OBJ_FOUND_IN_ROOM       Found in current room, obj_ptr_* valid
			OBJ_NOT_FOUND           No match, obj_ptr_* unspecified

Global Inputs
	current_room              Current room index
	room_obj_count            Count of objects in current room
	inventory_objects[]       Inventory slot id list
	object_ptr_lo/hi_tbl[]    Inventory object pointer table
	object_attributes[]       Per-object attribute byte low nibble = owner
	room_obj_idx_lo/hi[]      Room object indexes
	room_obj_ofs_tbl[]        Room-local 16-bit offsets
	room_ptr_lo/hi_tbl[]      Room base pointer table

Global Outputs
	obj_ptr					  Absolute object data pointer
	room_obj_ofs			  Room-relative object data offset

Description
	• If hi-id ≠ OBJ_HI_MOVABLE (immovable), skip inventory and search room.
	• If hi-id = OBJ_HI_MOVABLE, read owner nibble:
		– OWNER_IS_ROOM  → search room.
		– otherwise      → scan inventory by lo-id; on miss, fall through to room.
	• Inventory hit: load obj_ptr_* from per-slot tables, clear room_obj_ofs_*,
	return OBJ_FOUND_IN_INV.
	• Room search: scan descending; require lo-id and hi-id match. If movable,
	also require owner nibble = OWNER_IS_ROOM. On match:
		– Fetch 16-bit room-local offset from room_obj_ofs_tbl at index Y*2.
		– Add to room base pointer for current_room to form obj_ptr_*.
		– Return OBJ_FOUND_IN_ROOM.
	• If no match in either source, return OBJ_NOT_FOUND.

Notes
	• Ownership semantics: movable presence in-room is validated via the owner
	nibble. Immovable objects always reside in the room resource.
================================================================================
*/
* = $0A05
resolve_object_resource:
        // Store incoming object indices into ZP vars that double as result pointers
        stx     <obj_idx                 
        sta     >obj_idx                 

        // ------------------------------------------------------------
        // Determine search path based on obj hi-id
		//
        // hi ≠ 0 → immovable → search room only
        // ------------------------------------------------------------
        cmp     #OBJ_HI_MOVABLE
        bne     dispatch_to_room_search     // immovable object → skip inventory lookup

        // ------------------------------------------------------------
        // Movable object ownership dispatch
        //
        // - Read owner nibble to choose source.
        // - OWNER_IS_ROOM → search room; otherwise search inventory.
        // ------------------------------------------------------------
        lda     object_attributes,x            // load full attribute byte
        and     #OWNER_NIBBLE_MASK             // isolate owner nibble (low 4 bits)
        cmp     #OWNER_IS_ROOM                 // is owner = room/no owner?
        beq     dispatch_to_room_search        // yes → object resides in room, skip inventory

        // ------------------------------------------------------------
        // Inventory search
        //
        // - Start from the last slot and scan downward.
        // - Match on lo-id only
        // - On miss, fall through to room search.
        // ------------------------------------------------------------
        ldy     #MAX_INVENTORY_INDEX       // Y := last inventory slot; scan desc
inv_scan_loop:
        lda     inventory_objects,y        // A := lo-id at slot Y
        cmp     <obj_idx                 // matches target lo-id?
        bne     inv_scan_advance           // no → check previous slot

        // ------------------------------------------------------------
        // Inventory hit: commit pointer and return
        //
        // - Load obj_ptr_* from per-slot tables.
        // - Zero room_obj_ofs_* (not applicable to inventory items).
        // - Return with A = OBJ_FOUND_IN_INV.
        // ------------------------------------------------------------
        // Found in inventory → copy object pointer from object tables
        lda     object_ptr_hi_tbl,y    
        sta     >obj_ptr             
        lda     object_ptr_lo_tbl,y    
        sta     <obj_ptr             

        // No room offset for inventory objects
        lda     #$00                   
        sta     <room_obj_ofs
        lda     #$00                   
        sta     >room_obj_ofs

        lda     #OBJ_FOUND_IN_INV      // return code: found in inventory
        rts                            // exit with obj_ptr_* already set

inv_scan_advance:
        dey                         // move to previous inventory slot
        bpl     inv_scan_loop       // continue scan while Y ≥ 0
        // fall through to room search if inventory scan exhausted

        // ------------------------------------------------------------
        // Room search
        //
        // - Descend through room object list (Y from last to 0).
        // - Match lo-id first, then hi-id to confirm object identity.
        // - On mismatch, continue scanning; on match, proceed to presence/commit.
        // ------------------------------------------------------------
* = $0A37		
dispatch_to_room_search:
        ldy     room_obj_count          // Y := last room object index for descending scan
room_scan_loop:
        lda     room_obj_idx_lo,y       // A := candidate object's lo-id at index Y
        cmp     <obj_idx              // match target lo-id?
        bne     room_scan_advance       // no → check previous entry

        lda     room_obj_idx_hi,y       // A := candidate object's hi-id at index Y
        cmp     >obj_idx              // equal to target hi-id?
        bne     room_scan_advance       // no → continue scanning

        // ------------------------------------------------------------
        // Presence check for movable matches
        //
        // - After match, if hi-id == OBJ_HI_MOVABLE, verify the
        //   object is actually present in the room (owner = ROOM).
        // - If immovable (hi-id ≠ 0), accept match and build pointer.
        // ------------------------------------------------------------
        cmp     #OBJ_HI_MOVABLE
        beq     verify_object_present      // movable → validate presence via owner nibble

        jmp     room_match_commit          // immovable → accept match and build pointer

        // ------------------------------------------------------------
        // Verify movable object's in-room presence
        //
        // - Index attributes by lo-id.
        // - Check owner nibble equals OWNER_IS_ROOM.
        // - If not present in room, continue scanning.
        // ------------------------------------------------------------
verify_object_present:
        ldx     <obj_idx             // X := object lo-id to index attributes
        lda     object_attributes,x    // A := attribute byte for this object
        and     #OWNER_NIBBLE_MASK     // isolate owner nibble (low 4 bits)
        cmp     #OWNER_IS_ROOM         // owner must be ROOM/no-owner sentinel
        bne     room_scan_advance      // not present → continue scan

        // ------------------------------------------------------------
        // Commit room match and build absolute object pointer
        //
        // - Convert matched room object index (Y) to byte index (Y*2)
        //   for 16-bit offset table lookup.
        // - Load room-relative offset (lo/hi) from room_obj_ofs_tbl.
        // - Add offset to current room base pointer to produce obj_ptr_*.
        // - Return with A = OBJ_FOUND_IN_ROOM and obj_ptr_* valid.
        // ------------------------------------------------------------
room_match_commit:
        // Compute table index X = Y * 2 to address 16-bit offset entries
        tya                                 
        asl                                 
        tax                                 

        // Load 16-bit offset of object data within the room resource
        lda     room_obj_ofs_tbl,x          
        sta     <room_obj_ofs
        lda     room_obj_ofs_tbl+1,x        
        sta     >room_obj_ofs

        // Build absolute object pointer: room_base[current_room] + room_obj_ofs
        ldx     current_room                
        lda     room_ptr_lo_tbl,x           
        clc                                 
        adc     <room_obj_ofs             
        sta     <obj_ptr                  

        lda     room_ptr_hi_tbl,x           
        adc     >room_obj_ofs             
        sta     >obj_ptr                  

        lda     #OBJ_FOUND_IN_ROOM          // return code: found in room
        rts                                 // obj_ptr_* now points at object data

room_scan_advance:
        dey                         // step to previous room object entry
        bpl     room_scan_loop      // continue scan while Y ≥ 0

        // ------------------------------------------------------------
        // Not found in room
        //
        // - Room scan exhausted without a match.
        // - Return OBJ_NOT_FOUND; obj_ptr_* remains unspecified.
        // ------------------------------------------------------------
        lda     #OBJ_NOT_FOUND      // return code: no matching object
        rts                         // exit with obj_ptr_* undefined
/*
================================================================================
 sentence_bar_init_and_clear
================================================================================
Summary
	Initialize the sentence bar pointer and clear the sentence bar text region
	to zero.

Description
	* Load SENTENCE_BAR_BASE into pointer sentence_bar_ptr (lo, hi).
	* Clear all chars from SENTENCE_BAR_LAST_IDX down to 0 with #$00.
	* Leave sentence_bar_ptr initialized for subsequent appends.
*/
* = $0A82
sentence_bar_init_and_clear:
        // Set pointer to start of sentence bar
        lda     #<SENTENCE_BAR_BASE
        sta     <sentence_bar_ptr
        lda     #>SENTENCE_BAR_BASE
        sta     >sentence_bar_ptr

        // Clear 40 bytes: Y := last index, A := 0, then descend to 0
        ldy     #SENTENCE_BAR_LAST_IDX
        lda     #$00
clear_descend_loop:
        sta     (sentence_bar_ptr),y
        dey
        bpl     clear_descend_loop
        rts
/*
================================================================================
  append_word_to_sentence_bar
================================================================================

Summary
	Appends a NUL-terminated word into the sentence bar at the current write pointer.
	Stops on WORD_TERMINATOR (#$00) or WORD_HARD_STOP (#$40). Writes are trimmed to
	the visible width; the append pointer advances by the total bytes processed.

Global Inputs
	sentence_word_ptr          ptr to source word (lo/hi)
	sentence_bar_ptr           ptr to current append position (lo/hi)
	sentence_bar_len_used      Logical chars processed (including trimmed)

Global Outputs
	(sentence bar memory)      Characters written where width allows
	sentence_bar_ptr           Advanced by processed length (data + terminator)
	sentence_bar_len_used      Incremented per byte processed

Description
	* Initialize Y to $FF so the first INY aligns to byte 0 of the source.
	* Per byte: increment sentence_bar_len_used, read source, check for hard-stop
	  (#$40) or NUL; preserve Z from the NUL compare with PHP/PLP.
	* Only store when sentence_bar_len_used ≤ SENTENCE_BAR_END_IDX; otherwise trim.
	* Loop until terminator condition, then advance sentence_bar_ptr by Y+1 bytes
	  to the next append position.

Notes
	* Pointer advance includes the terminator (+1). This reserves a cell past the
	  last written character. 
================================================================================
*/
* = $0A94
append_word_to_sentence_bar:
        // Initialize source index at -1; first INY aligns Y=0 for byte 0
        ldy     #$ff

		// ------------------------------------------------------------
		// Read next character from word source and process append
		//
		// - Each byte increments logical sentence length
		// - Writes occur only while within visible width
		// - Terminates on WORD_TERMINATOR or WORD_HARD_STOP
		// ------------------------------------------------------------
loop_read_byte:
        inc     sentence_bar_len_used      // count processed characters (even if trimmed)
        iny                                // advance to next source position

        lda     (sentence_word_ptr),y      // fetch character from sentence_word_ptr[Y]

        // Detect explicit hard-stop marker (#$40); skip to termination logic if matched
        cmp     #WORD_HARD_STOP
        beq     check_terminator

        // Compare against NUL terminator (#$00) and save Z flag for later restoration
        cmp     #WORD_TERMINATOR
        php

        // Check if append within visible width before writing
        ldx     sentence_bar_len_used
        cpx     #SENTENCE_BAR_END_IDX       // equal → permit final cell write
        beq     emit_char_if_visible
        bcs     restore_z                   // beyond limit → skip store (trim only)

emit_char_if_visible:
        sta     (sentence_bar_ptr),y        // store A into visible cell Y; width already validated

restore_z:
        plp                                 // restore saved Z from NUL compare to control termination

check_terminator:
        bne     loop_read_byte              // Z=0 → not a terminator, keep reading; Z=1 → stop

		// ------------------------------------------------------------
		// Advance append pointer by processed length (Y+1 bytes total)
		// ------------------------------------------------------------
        iny                                 // include the terminator in count (advance delta by +1)
        tya                                 // A := total bytes processed (data + terminator)
        clc                                 
        adc     <sentence_bar_ptr           // add delta to write pointer
        sta     <sentence_bar_ptr           
        bcc     exit_append                 
        inc     >sentence_bar_ptr           
exit_append:
        rts                                 // done: append ptr now points to next free cell
/*
================================================================================
  select_preposition_for_verb
================================================================================
Summary
	Choose the preposition index for the current verb. For USE, read the
	object’s encoded preposition from its resource. For selected verbs, return
	fixed prepositions. Otherwise return none.

Arguments
	current_verb_id                  verb selector
	direct_object_idx_lo/hi       object id used only when verb = USE

Returns
	.A = 0..7                     For USE: 3-bit index read from object byte
									at OBJ_PREP_BYTE_OFS (bits 7..5)
	.A = PREPOSITION_TO           For GIVE
	.A = PREPOSITION_WITH         For NEW_KID, UNLOCK, FIX
	.A = PREPOSITION_NONE_NEEDED  For all other verbs

Description
	• If verb = USE: call resolve_object_resource with direct object id,
	then read object byte at OBJ_PREP_BYTE_OFS, return bits 7..5 >> 5.
	• If verb = GIVE: return PREPOSITION_TO.
	• If verb ∈ {NEW_KID, UNLOCK, FIX}: return PREPOSITION_WITH.
	• Else: return PREPOSITION_NONE_NEEDED.

Notes
	• Assumes resolve_object_resource leaves a valid obj_ptr_* for the direct
	object. No error handling here.
	• Object layout: preposition for USE is encoded in bits 7..5 of the byte
	at OBJ_PREP_BYTE_OFS.
================================================================================
*/
* = $0ABD
select_preposition_for_verb:
		// ------------------------------------------------------------
		// Dispatch by verb. USE reads object’s preposition bits at +OBJ_PREP_BYTE_OFS.
		// GIVE → "to". NEW_KID/UNLOCK/FIX → "with". Otherwise none.
		// ------------------------------------------------------------
		lda     current_verb_id
		cmp     #USE_VERB
		bne     verb_is_give


        // ------------------------------------------------------------
		// USE verb
        // Resolve object resource pointer for current direct object
        // ------------------------------------------------------------
        ldx     direct_object_idx_lo        // X := object id (lo)
        lda     direct_object_idx_hi        // A := object id (hi)
        jsr     resolve_object_resource     // sets obj_ptr_lo/hi → object data base

        // ------------------------------------------------------------
        // Read and decode preposition bits from object byte +OBJ_PREP_BYTE_OFS
        // ------------------------------------------------------------
        ldy     #OBJ_PREP_BYTE_OFS          // Y := offset to preposition byte in object data
        lda     (obj_ptr),y              // A := raw byte containing preposition (bits 7..5)
        lsr                                 
        lsr                                 
        lsr                                 
        lsr                                 
        lsr                                 
        rts                                 // return index (0..7) in .A

verb_is_give:
		// ------------------------------------------------------------
		// GIVE → "to"
		// ------------------------------------------------------------
		cmp     #GIVE_VERB
		bne     verbs_using_with_prep
		lda     #PREPOSITION_TO
		rts

verbs_using_with_prep:
		// ------------------------------------------------------------
		// NEW_KID/UNLOCK/FIX → "with"; else no preposition
		// ------------------------------------------------------------
		cmp     #NEW_KID_VERB
		beq     return_with_prep
		cmp     #UNLOCK_VERB
		beq     return_with_prep
		cmp     #FIX_VERB
		beq     return_with_prep

		// ------------------------------------------------------------
		// Default: no preposition
		// ------------------------------------------------------------
		lda     #PREPOSITION_NONE_NEEDED
		rts

return_with_prep:
		lda     #PREPOSITION_WITH
		rts
