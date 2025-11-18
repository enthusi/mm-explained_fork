/*
================================================================================
 Sentence bar text builder
================================================================================
Summary
    Builds and maintains the on-screen sentence bar text (VERB [DO] [PREP] [IO])
    for the UI. Gated by refresh flags and control mode, it assembles a readable
    sentence from the current verb, direct object, preposition, and indirect
    object, including special cases such as NEW KID.

Public routines
    refresh_sentence_bar
        Top-level entry point; checks refresh flags and control mode, then
        rebuilds the sentence bar string or leaves it unchanged.

    write_kid_names_to_sentence_bar
        Specialized builder for the NEW KID verb; populates the three kid name
        slots in fixed screen positions.

    clear_sentence_bar
        Clears the sentence bar region in screen RAM and resets the internal
        destination pointer to the bar’s base.

    append_word_to_sentence_bar
        Appends a single word (zero-terminated, with optional hard-stop) into
        the sentence bar, trimming at the visible width and advancing the
        destination pointer past the word terminator.

    resolve_costume_name
        Maps a costume index to its name pointer so it can be appended to the
        sentence bar (used for kid names and costume-based IO for GIVE).

    resolve_object_name
        Locates an object’s resource (inventory or room), computes its name
        pointer from the object header, and prepares it as the next sentence
        word. Resets the sentence system if the object cannot be resolved.

    resolve_object_if_not_costume
        Dispatch helper that bypasses resource lookup when the object hi-byte
        marks a costume; otherwise falls through to resolve_object_resource.

    resolve_object_resource
        Core lookup routine: given an object id, first tries to find it in the
        active inventory (if movable and owned by room), then searches the
        current room’s object list. On success, computes obj_ptr to the object
        header within either inventory or room resource.

    select_preposition_for_verb
        Chooses or infers a preposition for the current verb and DO. Handles
        data-driven USE prepositions from the object’s prep byte and hard-coded
        mappings for verbs like GIVE, NEW KID, UNLOCK, and FIX.

Description
    The file revolves around a single refresh entry point, refresh_sentence_bar.
    It first checks sentence_bar_needs_refresh and returns early if no rebuild
    is requested. It then gates by control_mode so that cutscene/keypad states
    either skip or fully rebuild the textual sentence only when appropriate. In
    normal gameplay, it clears the sentence bar, resets internal length and
    destination pointer, and branches into either a generic sentence builder or
    the special NEW KID layout.

    For standard verbs, the builder ensures there is always a sensible verb
    (promoting VERB_NONE to VERB_WALK_TO), then looks up and appends the verb
    text. If no direct object is selected, the sentence stops there. Otherwise
    it resolves and appends the DO name via resolve_object_name, which in turn
    uses resolve_object_resource to find the object in inventory or in the
    current room and then derives the name pointer from the object header.

    Prepositions are handled in two paths: if the script has already chosen a
    preposition, it is used directly; otherwise select_preposition_for_verb
    infers one from the verb and DO. For USE, the decision is data-driven via a
    packed preposition index in the object’s prep byte. For other verbs like
    GIVE, NEW KID, UNLOCK, and FIX, the mapping is hard-coded to the expected
    preposition or to “none needed”. When a new preposition is inferred, any
    existing indirect object is cleared so the sentence structure stays
    consistent.

    After a preposition is chosen, it may be appended as another word. Finally,
    if an indirect object is present, the builder resolves its name either via
    the costume path (for GIVE recipients) or the same object-resource path
    used for the DO, and appends that name as the last component of the
    sentence.

    Words are appended using append_word_to_sentence_bar, which reads a
    zero-terminated word from sentence_word_ptr and writes it into the sentence
    bar up to a logical limit defined by SENTENCE_BAR_END_IDX. It respects a
    hard-stop marker that terminates the word early without storing the marker,
    maintains a logical “used length” counter for trimming, and advances the
    destination pointer past the terminator so subsequent words automatically
    chain.

    The object-resolution routines encapsulate the engine’s underlying layout:
    movable objects with OWNER_IS_ROOM are searched in inventory first,
    otherwise the current room’s object index table is used. On a match, a
    per-room offset is combined with the room resource base so callers receive
    a canonical obj_ptr into the object header independent of where it resides.
    Failure to resolve an object triggers a reset of the sentence UI, ensuring
    broken references do not leave half-built sentences on screen.
================================================================================
*/

#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "text_data.inc"
#import "sentence_action.asm"

.const SENTENCE_BAR_LAST_IDX    	= $27    // Last valid cell index (0..$27 ⇒ 40 cells wide)
.const SENTENCE_BAR_END_IDX     	= SENTENCE_BAR_LAST_IDX + 1  // One past last cell (#$28 sentinel)

.const OBJ_PREP_BYTE_OFS        	= $0B    // Object byte +$0B: USE preposition encoded in bits 7..5
.const OBJ_FOUND_IN_INV         	= $00    // Resolver status: found in inventory; obj_ptr_* valid
.const OBJ_FOUND_IN_ROOM        	= $01    // Resolver status: found in room; obj_ptr_* valid
.const OBJ_NOT_FOUND            	= $FF    // Resolver status: not found; obj_ptr_* unspecified

.const OWNER_IS_ROOM            	= $0F    // Owner nibble value meaning “room/no owner”
.const MAX_INVENTORY_INDEX      	= $2C    // Highest inventory slot index (scan starts here)

.const OBJ_NAME_FOUND           	= $00    // Return code: name pointer resolved successfully

.const KID2_NAME_OFS            	= $CEDD  // Sentence bar absolute address of kid #2 name slot
.const KID3_NAME_OFS            	= $CEEA  // Sentence bar absolute address of kid #3 name slot

.label sentence_bar_len_used     	= $0E72  // Tracks number of characters currently used in the sentence bar
.label sentence_bar_ptr_lo       	= $19    // ZP pointer to destination buffer for sentence bar text (lo)
.label sentence_bar_ptr_hi       	= $1A    // ZP pointer to destination buffer for sentence bar text (hi)
.label sentence_word_ptr_lo      	= $15    // ZP pointer to source word string being appended (lo)
.label sentence_word_ptr_hi      	= $16    // ZP pointer to source word string being appended (hi)

.label obj_idx_lo                   = $15    // ZP: temporary index/pointer used for object id or resource lookup
.label obj_idx_hi                   = $16    // ZP: temporary index/pointer used for object id or resource lookup
.label obj_ptr_lo                   = $15    // ZP pointer to resolved object resource or name (lo/hi shared alias)
.label obj_ptr_hi                   = $16    // ZP pointer to resolved object resource or name (lo/hi shared alias)
.label room_obj_ofs_lo              = $17    // ZP: temporary 16-bit offset to object data within room resource
.label room_obj_ofs_hi              = $18    // ZP: temporary 16-bit offset to object data within room resource

/*
================================================================================
  refresh_sentence_bar
================================================================================
Summary
	Conditionally rebuild the sentence bar. Proceeds only if the refresh flag
	is set and the control mode is neither CUTSCENE nor KEYPAD. Clears the
	refresh flag, then either exits or tail-calls the builder.

Vars/State
	sentence_bar_needs_refresh  	true → refresh requested
	control_mode                	0=CUTSCENE, 1=KEYPAD, ≥2=normal gameplay

Global Outputs
	sentence_bar_needs_refresh   	cleared on entry

Description
	• Test sentence_bar_needs_refresh. If zero, return immediately.
	• Clear sentence_bar_needs_refresh.
	• Gate by control_mode:
		– control_mode == CONTROL_MODE_KEYPAD → return.
		– control_mode <= CUTSCENE → return.
		– control_mode ≥ first normal mode → jump build_sentence_bar_text.
================================================================================
*/
* = $08C6
refresh_sentence_bar:
		// ------------------------------------------------------------
		// Refresh gate - proceed only if refresh is needed
		// ------------------------------------------------------------	
        lda     sentence_bar_needs_refresh      
        bne     gate_by_control_mode       		// true → proceed
        jmp     rsb_exit              			// false → exit

		// ------------------------------------------------------------
		// Control-mode gate - refresh allowed only in normal modes
		//
		// if control_mode == CUTSCENE or KEYPAD → exit;
		// else jump to build_sentence_bar_text
		// ------------------------------------------------------------
gate_by_control_mode:
        // Clear flag
        lda     #$00                           	
        sta     sentence_bar_needs_refresh      
		
		// Gate by control mode
        lda     control_mode                   	
        cmp     #CONTROL_MODE_KEYPAD           	
        beq     return_no_refresh              	// keypad or cutscene mode → exit
        bcs     build_sentence_bar_text        	// otherwise → continue

return_no_refresh:
        rts                                    	

build_sentence_bar_text:
        // ------------------------------------------------------------
        // Build full sentence bar string from active verb and object data
        // ------------------------------------------------------------
        jsr     clear_sentence_bar     // initialize and clear sentence bar text buffer
        lda     #$00                              
        sta     sentence_bar_len_used           // clear accumulated text length

        // ------------------------------------------------------------
        // Check if current verb is "NEW KID"
        // If so, directly print kid names and exit routine
        // ------------------------------------------------------------
        lda     current_verb_id                	
        cmp     #VERB_NEW_KID                   
        bne     ensure_default_verb             // not equal → continue with regular sentence build
        jmp     write_kid_names_to_sentence_bar // equal → delegate to kid-name writer and return

ensure_default_verb:
        // ------------------------------------------------------------
        // Ensure a valid verb is set before sentence construction
        // ------------------------------------------------------------
		// Is there a verb selected?
        cmp     #VERB_NONE                      
        bne     append_verb                     
		
		// No verb -> force "Walk to"
        lda     #VERB_WALK_TO                   
        sta     current_verb_id                 

append_verb:
        // ------------------------------------------------------------
        // Append the current verb token to the sentence bar
        // ------------------------------------------------------------
		// Resolve verb string pointer
        tax                                     // X := verb id (index into verb pointer tables)
        lda     verb_pointers_hi,x              
        sta     sentence_word_ptr_hi             
        lda     verb_pointers_lo,x              
        sta     sentence_word_ptr_lo            
		
		// Append verb string to sentence bar
        jsr     append_word_to_sentence_bar     

        // ------------------------------------------------------------
        // Append direct object if present
        // ------------------------------------------------------------
        lda     direct_object_idx_lo            // A := direct object lo-id; zero → no object
        beq     rsb_exit       					// none → finish sentence build
		
		// Resolve object string pointer
        tax                                     // X := lo-id (resolver requires X)
        lda     direct_object_idx_hi            // A := hi-id (resolver requires A)		
        jsr     resolve_object_name             // obj_ptr → object name string
		
		// Append object string
        jsr     append_word_to_sentence_bar     

        // ------------------------------------------------------------
        // Resolve and append preposition, if any
        // ------------------------------------------------------------
        lda     current_preposition             // A := current preposition index (0 = unset)
        bne     append_preposition_if_any       // already set → skip inference

		// Resolve preposition based on the verb -> A
        jsr     select_preposition_for_verb     
		
		// If none needed, skip
        cmp     #PREPOSITION_NONE_NEEDED        
        beq     append_preposition_if_any       

		// Cache inferred preposition for later reuse
        sta     current_preposition                     

		// Clear indirect object references as the preposition changed
        lda     #$00                            
        sta     indirect_object_idx_lo          
        sta     indirect_object_idx_hi

append_preposition_if_any:
		// Any preposition set? If not, exit
        lda     current_preposition                     
        beq     rsb_exit       

		// Resolve preposition string
        tax                                     // X := preposition index (table selector)		
        lda     preposition_pointers_hi,x       
        sta     sentence_word_ptr_hi             
        lda     preposition_pointers_lo,x       
        sta     sentence_word_ptr_lo               
		
		// Append preposition string to sentence bar
        jsr     append_word_to_sentence_bar     

        // ------------------------------------------------------------
        // "Give" verb special case: validate indirect object is a costume
        // ------------------------------------------------------------
		// Verb is 'Give'? If not, skip validation
        lda     current_verb_id            		
        cmp     #VERB_GIVE                    	
        bne     append_indirect_if_present    	

		// Indirect object is a costume? If so, continue
        lda     indirect_object_idx_hi        	
        cmp     #OBJ_HI_COSTUME               	
        beq     append_indirect_if_present    	

		// It's not a costume so it's invalid, clear it
        lda     #$00                          	
        sta     indirect_object_idx_lo        
        sta     indirect_object_idx_hi        

append_indirect_if_present:
        // ------------------------------------------------------------
        // Append indirect object
        // ------------------------------------------------------------
		// Indirect object present? If not, exit
        lda     indirect_object_idx_lo        
        beq     rsb_exit     

		// Dispatch indirect object resolution (costume vs. object)
        tax                                   // X := indirect lo-id (resolver input)
        lda     indirect_object_idx_hi        // A := indirect hi-id (type selector)
        cmp     #OBJ_HI_COSTUME               // costume type?
        bne     resolve_and_append_object_name// no → handle as object

		// Resolve costume
        jsr     resolve_costume_name          
        jmp     commit_append                 

resolve_and_append_object_name:
		// Resolve object
        jsr     resolve_object_name            
		
commit_append:
		// Append indirect object token
        jsr     append_word_to_sentence_bar    
		
rsb_exit:
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
		sta     sentence_bar_ptr_lo
		lda     #>KID2_NAME_OFS
		sta     sentence_bar_ptr_hi
		ldx     kid2_costume_id
		jsr     resolve_costume_name
		jsr     append_word_to_sentence_bar

		// ------------------------------------------------------------
		// Third kid: reposition buffer pointer to slot #3 and print
		// ------------------------------------------------------------
		lda     #<KID3_NAME_OFS
		sta     sentence_bar_ptr_lo
		lda     #>KID3_NAME_OFS
		sta     sentence_bar_ptr_hi
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
	.X  		Character index
================================================================================
*/
* = $09D8
resolve_costume_name:
        lda     name_ptr_hi_tbl,x      
        sta     sentence_word_ptr_hi
        lda     name_ptr_lo_tbl,x      
        sta     sentence_word_ptr_lo        
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
		jsr     resolve_object_resource     
		
		// Did we find the object? If so, continue
		cmp     #OBJ_NOT_FOUND
		bne     object_found

		// ------------------------------------------------------------
		// Object not found → invalidate sentence and return
		// ------------------------------------------------------------
		jsr     init_sentence_ui_and_stack
		rts

object_found:
		// ------------------------------------------------------------
		// Add name offset to object base pointer
		// ------------------------------------------------------------
		ldy     #OBJ_NAME_OFS               // offset of name pointer byte
		lda     (obj_ptr_lo),y              // A := name offset
		clc
		adc     obj_ptr_lo                  // add to base pointer (lo)
		sta     obj_ptr_lo
		bcc     resolve_object_name_exit    // no carry → done
		inc     obj_ptr_hi                	// carry → increment hi byte

resolve_object_name_exit:
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
        // ------------------------------------------------------------
		// Is object a costume? (Hi-id == costume)
        cmp     #OBJ_HI_COSTUME          
        bne     resolve_object_resource  // no → resolve as regular object

		// Yes - return not found
        lda     #OBJ_NOT_FOUND           
        rts                              
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
	.X  						Lo-id of target object
	.A  						Hi-id of target object 

Vars/State
	obj_idx		               	scratch: latched id input
	obj_ptr            		   	absolute pointer to object data
	room_obj_ofs		       	room-local offset to object data

Returns
	.A		OBJ_FOUND_IN_INV    Found in inventory, obj_ptr_* valid
			OBJ_FOUND_IN_ROOM   Found in current room, obj_ptr_* valid
			OBJ_NOT_FOUND       No match, obj_ptr_* unspecified

Global Inputs
	current_room              Current room index
	
State	
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
	nibble. Immovable objects always reside in a room resource.
================================================================================
*/
* = $0A05
resolve_object_resource:
        // Store incoming object indices into ZP vars that double as result pointers
        stx     obj_idx_lo                 
        sta     obj_idx_hi

        // ------------------------------------------------------------
        // Determine search path based on whether the object is movable or not
        // ------------------------------------------------------------
        cmp     #OBJ_HI_MOVABLE
        bne     dispatch_to_room_search // immovable object → skip inventory lookup

        // ------------------------------------------------------------
        // Movable object ownership dispatch
        // ------------------------------------------------------------
        // Resolve owner
        lda     object_attributes,x     // load full attribute byte
        and     #OWNER_NIBBLE_MASK      // isolate owner nibble
        cmp     #OWNER_IS_ROOM          // is owner = room/no owner?
		
		// Is it a room?
        beq     dispatch_to_room_search // yes → object resides in room, skip inventory

        // ------------------------------------------------------------
        // Movable object not in room -> Inventory search
        // ------------------------------------------------------------		
        // Start from the last inventory slot and scan downward.
        ldy     #MAX_INVENTORY_INDEX       
inv_scan_loop:
        // Match on lo-id only
        lda     inventory_objects,y     // A := lo-id at slot Y
        cmp     obj_idx_lo              // matches target lo-id?
        bne     inv_scan_advance        // no → check previous slot

        // ------------------------------------------------------------
        // Inventory hit: commit pointer and return
        // ------------------------------------------------------------
        // Found in inventory → copy object pointer from object tables
        lda     object_ptr_hi_tbl,y    
        sta     obj_ptr_hi
        lda     object_ptr_lo_tbl,y    
        sta     obj_ptr_lo             

        // No room offset for inventory objects
        lda     #$00                   
        sta     room_obj_ofs_lo
        lda     #$00                   
        sta     room_obj_ofs_hi

		// Return that we found the object in the inventory
        lda     #OBJ_FOUND_IN_INV      
        rts                            

inv_scan_advance:
		// Continue scan while Y ≥ 0
        dey                         
        bpl     inv_scan_loop       
		
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
		// Resolve total count of objects in this room
        ldy     room_obj_count          
room_scan_loop:
        lda     room_obj_idx_lo,y       // A := candidate object's lo-id at index Y
        cmp     obj_idx_lo              // match target lo-id?
        bne     room_scan_advance       // no → check previous entry

        lda     room_obj_idx_hi,y       // A := candidate object's hi-id at index Y
        cmp     obj_idx_hi       		// equal to target hi-id?
        bne     room_scan_advance       // no → continue scanning

        // ------------------------------------------------------------
        // Presence check for movable objects
        // ------------------------------------------------------------
        cmp     #OBJ_HI_MOVABLE
        beq     verify_object_present   // movable → validate presence via owner nibble

        jmp     room_match_commit       // immovable → accept match and build pointer

        // ------------------------------------------------------------
        // Verify object's in-room presence
        // ------------------------------------------------------------
verify_object_present:
        // Resolve owner
        ldx     obj_idx_lo             			
        lda     object_attributes,x     
        and     #OWNER_NIBBLE_MASK      
        cmp     #OWNER_IS_ROOM          // owner must be ROOM
        bne     room_scan_advance       // not present → continue scan

        // ------------------------------------------------------------
        // Found in room: commit match and build absolute object pointer
        // ------------------------------------------------------------
room_match_commit:
        // Compute table index X = Y * 2 to address 16-bit offset entries
        tya                                 
        asl                                 
        tax                                 

        // Resolve offset of object data within the room resource
        lda     room_obj_ofs_tbl,x          
        sta     room_obj_ofs_lo
        lda     room_obj_ofs_tbl+1,x        
        sta     room_obj_ofs_hi

        // Resolve absolute object pointer: room_base[current_room] + room_obj_ofs
        ldx     current_room                
        lda     room_ptr_lo_tbl,x           
        clc                                 
        adc     room_obj_ofs_lo             
        sta     obj_ptr_lo                  

        lda     room_ptr_hi_tbl,x           
        adc     room_obj_ofs_hi
        sta     obj_ptr_hi

		// Return: object found in room
        lda     #OBJ_FOUND_IN_ROOM      
        rts                             

room_scan_advance:
        dey                         	// step to previous room object entry
        bpl     room_scan_loop      	// continue scan while Y ≥ 0

        // ------------------------------------------------------------
        // Not found in room
        // ------------------------------------------------------------
		// Return code: object not found
        lda     #OBJ_NOT_FOUND      	
        rts                         	
/*
================================================================================
 clear_sentence_bar
================================================================================
Summary
	Initialize the sentence bar pointer and clear the sentence bar text region
	to zero.

Description
	* Load SENTENCE_BAR_BASE into pointer sentence_bar_ptr (lo, hi).
	* Clear all chars from SENTENCE_BAR_LAST_IDX down to 0 with #$00.
	* Leave sentence_bar_ptr initialized for subsequent appends.
================================================================================
*/
* = $0A82
clear_sentence_bar:
        // Set pointer to start of sentence bar
        lda     #<SENTENCE_BAR_BASE
        sta     sentence_bar_ptr_lo
        lda     #>SENTENCE_BAR_BASE
        sta     sentence_bar_ptr_hi

        // Clear 40 bytes: Y := last index, A := 0, then descend to 0
        ldy     #SENTENCE_BAR_LAST_IDX
        lda     #$00
clear_descend_loop:
        sta     (sentence_bar_ptr_lo),y
        dey
        bpl     clear_descend_loop
        rts
/*
================================================================================
  append_word_to_sentence_bar
================================================================================

Summary
	Appends a NUL-terminated word into the sentence bar at the current write pointer.
	Stops on WORD_TERMINATOR or WORD_HARD_STOP. Writes are trimmed to the visible width; 
	the append pointer advances by the total bytes processed.

Global Inputs
	sentence_word_ptr          ptr to source word (lo/hi)
	sentence_bar_ptr           ptr to current append position (lo/hi)
	sentence_bar_len_used      Logical chars processed (including trimmed)

Global Outputs
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
		// ------------------------------------------------------------
loop_read_byte:
        inc     sentence_bar_len_used      // count processed characters (even if trimmed)
        iny                                // advance to next source position

        // Fetch character from sentence word
		lda     (sentence_word_ptr_lo),y      

        // Detect explicit hard-stop marker (#$40); skip to termination logic if matched
        cmp     #WORD_HARD_STOP
        beq     check_terminator

        // Compare against terminator (#$00) and save Z flag for later restoration
        cmp     #WORD_TERMINATOR
        php

        // Check if append within visible width before writing
		// If so, emit char; otherwise, skip it
        ldx     sentence_bar_len_used
        cpx     #SENTENCE_BAR_END_IDX       
        beq     emit_char_if_visible
        bcs     restore_z                   // beyond limit → skip store (trim only)

emit_char_if_visible:
		// Store A into visible cell Y; width already validated
        sta     (sentence_bar_ptr_lo),y        

restore_z:
        // Restore saved Z from NUL compare to control termination
		plp                                 

check_terminator:
        // Z=0 → not a terminator, keep reading; Z=1 → stop
		bne     loop_read_byte              

		// ------------------------------------------------------------
		// String terminated - advance append pointer by processed length (Y+1 bytes total)
		// ------------------------------------------------------------
        iny                                 // include the terminator in count (advance delta by +1)
        tya                                 // A := total bytes processed (data + terminator)
        clc                                 
        adc     sentence_bar_ptr_lo    		// add delta to write pointer
        sta     sentence_bar_ptr_lo           
        bcc     exit_append                 
        inc     sentence_bar_ptr_hi
		
exit_append:
        rts                                 
/*
================================================================================
  select_preposition_for_verb
================================================================================
Summary
	Choose the preposition index for the current verb. For USE, read the
	object’s encoded preposition from its resource. For selected verbs, return
	fixed prepositions. Otherwise return none.

Arguments
	current_verb_id         		verb selector
	direct_object_idx_lo/hi       	object id used only when verb = USE

Returns
	A
		PREPOSITION_TO/WITH        	For USE: 3-bit index read from object byte
									at OBJ_PREP_BYTE_OFS (bits 7..5)
		PREPOSITION_TO           	For GIVE
		PREPOSITION_WITH         	For NEW_KID, UNLOCK, FIX
		PREPOSITION_NONE_NEEDED  	For all other verbs

Description
	• If verb = USE: call resolve_object_resource with direct object id,
	then read object byte at OBJ_PREP_BYTE_OFS, return bits 7..5 >> 5.
	• If verb = GIVE: return PREPOSITION_TO.
	• If verb ∈ {NEW_KID, UNLOCK, FIX}: return PREPOSITION_WITH.
	• Else: return PREPOSITION_NONE_NEEDED.

Notes
	• Object layout: preposition for USE is encoded in bits 7..5 of the byte
	at OBJ_PREP_BYTE_OFS.
================================================================================
*/
* = $0ABD
select_preposition_for_verb:
		// ------------------------------------------------------------
		// Dispatch by verb
		// ------------------------------------------------------------
		lda     current_verb_id
		cmp     #VERB_USE
		bne     verb_is_give

        // ------------------------------------------------------------
		// USE verb - object determines the preposition
        // ------------------------------------------------------------
        // Resolve object 
        ldx     direct_object_idx_lo        // X := object id (lo)
        lda     direct_object_idx_hi        // A := object id (hi)
        jsr     resolve_object_resource     // sets obj_ptr_lo/hi → object data base

        // ------------------------------------------------------------
        // Read and decode preposition bits from object byte +OBJ_PREP_BYTE_OFS
        // ------------------------------------------------------------
        ldy     #OBJ_PREP_BYTE_OFS     		// Y := offset to preposition byte in object data
        lda     (obj_ptr_lo),y              	// A := raw byte containing preposition (bits 7..5)
        lsr                                 
        lsr                                 
        lsr                                 
        lsr                                 
        lsr                                 
        rts                                 // return preposition index (0..7) in .A

verb_is_give:
		// ------------------------------------------------------------
		// GIVE verb → preposition "to"
		// ------------------------------------------------------------
		cmp     #VERB_GIVE
		bne     verbs_using_with_prep
		lda     #PREPOSITION_TO
		rts

verbs_using_with_prep:
		// ------------------------------------------------------------
		// NEW_KID/UNLOCK/FIX → preposition "with"
		// ------------------------------------------------------------
		cmp     #VERB_NEW_KID
		beq     return_with_prep
		cmp     #VERB_UNLOCK
		beq     return_with_prep
		cmp     #VERB_FIX
		beq     return_with_prep

		// ------------------------------------------------------------
		// Default: no preposition
		// ------------------------------------------------------------
		lda     #PREPOSITION_NONE_NEEDED
		rts

return_with_prep:
		lda     #PREPOSITION_WITH
		rts


/*
Pseudo-code

// -----------------------------------------------------------------------------
// refresh_sentence_bar
// -----------------------------------------------------------------------------
function refresh_sentence_bar():
    if sentence_bar_needs_refresh == false:
        return

    sentence_bar_needs_refresh = false

    // Gate by control mode: only rebuild in “normal” mode
    if control_mode == CONTROL_MODE_KEYPAD:
        return

    if control_mode == CONTROL_MODE_CUTSCENE:
        return

    // Normal gameplay: rebuild the bar
    clear_sentence_bar()
    sentence_bar_len_used = 0

    // NEW KID is a special layout: write three kid names into fixed slots
    if current_verb_id == VERB_NEW_KID:
        write_kid_names_to_sentence_bar()
        return

    // Ensure there is always a verb
    if current_verb_id == VERB_NONE:
        current_verb_id = VERB_WALK_TO

    // 1) Append verb
    set_sentence_word_ptr_to_verb(current_verb_id)
    append_word_to_sentence_bar()

    // 2) If there is no Direct Object, we’re done
    if direct_object_idx_lo == 0:
        return

    // 3) Append Direct Object name
    if not resolve_object_name(direct_object_idx_lo, direct_object_idx_hi):
        // Object missing → sentence UI is reset inside resolve_object_name
        return
    append_word_to_sentence_bar()

    // 4) Resolve/Infer preposition
    if current_preposition != PREPOSITION_NONE:
        prep = current_preposition
    else:
        prep = select_preposition_for_verb(current_verb_id, direct_object_idx_lo, direct_object_idx_hi)
        if prep == PREPOSITION_NONE_NEEDED:
            return

        // Store inferred preposition; clear IO because semantics changed
        current_preposition = prep
        indirect_object_idx_lo = 0
        indirect_object_idx_hi = 0
    end if

    // 5) Append preposition (if any)
    if prep != PREPOSITION_NONE and prep != PREPOSITION_NONE_NEEDED:
        set_sentence_word_ptr_to_preposition(prep)
        append_word_to_sentence_bar()
    end if

    // 6) Enforce GIVE IO must be a costume; otherwise clear IO
    if current_verb_id == VERB_GIVE:
        if indirect_object_idx_hi != OBJ_HI_COSTUME:
            indirect_object_idx_lo = 0
            indirect_object_idx_hi = 0
        end if
    end if

    // 7) Append IO name if present
    if indirect_object_idx_lo == 0 and indirect_object_idx_hi == 0:
        return

    if current_verb_id == VERB_GIVE:
        // IO is a costume recipient
        resolve_costume_name(indirect_object_idx_lo)    // kid index
        append_word_to_sentence_bar()
    else:
        // IO is an object
        if not resolve_object_name(indirect_object_idx_lo, indirect_object_idx_hi):
            return
        append_word_to_sentence_bar()
    end if
end function


// -----------------------------------------------------------------------------
// write_kid_names_to_sentence_bar
// -----------------------------------------------------------------------------
function write_kid_names_to_sentence_bar():
    // Kid #1: use whatever the current destination pointer is
    resolve_costume_name(KID1_INDEX)
    append_word_to_sentence_bar()

    // Kid #2: force pointer to second name slot
    sentence_bar_ptr = KID2_NAME_OFS
    resolve_costume_name(KID2_INDEX)
    append_word_to_sentence_bar()

    // Kid #3: force pointer to third name slot
    sentence_bar_ptr = KID3_NAME_OFS
    resolve_costume_name(KID3_INDEX)
    append_word_to_sentence_bar()
end function


// -----------------------------------------------------------------------------
// clear_sentence_bar
// -----------------------------------------------------------------------------
function clear_sentence_bar():
    sentence_bar_ptr = SENTENCE_BAR_BASE

    for col from 0 to SENTENCE_BAR_LAST_IDX:
        sentence_bar_ptr[col] = CLEAR_CHAR
    end for

    sentence_bar_len_used = 0
end function


// -----------------------------------------------------------------------------
// append_word_to_sentence_bar
// -----------------------------------------------------------------------------
function append_word_to_sentence_bar():
    // sentence_word_ptr points to a word encoded as:
    //   chars..., WORD_TERMINATOR (0), optionally WORD_HARD_STOP somewhere before.
    //
    // - WORD_TERMINATOR ends the word.
    // - WORD_HARD_STOP also ends the word but is not stored in the output.
    // - sentence_bar_len_used tracks logical characters attempted, used to trim.
    //
    src_index = 0

    while true:
        sentence_bar_len_used += 1

        ch = sentence_word_ptr[src_index]
        src_index += 1

        if ch == WORD_HARD_STOP:
            // Hard stop: do not store this marker, but end the word
            break
        end if

        // Decide if this character is within visible width
        visible_slot = (sentence_bar_len_used - 1)   // conceptual position
        if visible_slot < SENTENCE_BAR_END_IDX:
            // Store this char into the current bar slot
            sentence_bar_ptr[visible_slot] = ch
        else:
            // Beyond visible width: ignore the write but still advance len_used
        end if

        if ch == WORD_TERMINATOR:
            break
        end if
    end while

    // Advance the bar pointer past the entire word, including the terminator,
    // so the next append starts after this word (no sentinel is written).
    sentence_bar_ptr += src_index
end function


// -----------------------------------------------------------------------------
// resolve_costume_name
// -----------------------------------------------------------------------------
function resolve_costume_name(costume_index):
    // Lookup costume → name pointer and set sentence_word_ptr for append_word
    sentence_word_ptr = costume_name_ptr_table[costume_index]
end function


// -----------------------------------------------------------------------------
// resolve_object_name
// -----------------------------------------------------------------------------
function resolve_object_name(obj_lo, obj_hi) -> bool:
    // Locate the object in inventory or the current room, then move its
    // obj_ptr to the embedded name field and set sentence_word_ptr to it.
    if not resolve_object_resource(obj_lo, obj_hi):
        // Failed to find the object → reset sentence UI and abort
        init_sentence_ui_and_stack()
        return false
    end if

    // obj_ptr now points at the object header
    name_offset = read_byte(obj_ptr + OBJ_NAME_OFS)
    sentence_word_ptr = obj_ptr + name_offset
    return true
end function


// -----------------------------------------------------------------------------
// resolve_object_if_not_costume
// -----------------------------------------------------------------------------
function resolve_object_if_not_costume(obj_lo, obj_hi) -> bool:
    // If this “object” is actually a costume id, treat as not-found here
    if obj_hi == OBJ_HI_COSTUME:
        return false
    end if

    return resolve_object_resource(obj_lo, obj_hi)
end function


// -----------------------------------------------------------------------------
// resolve_object_resource
// -----------------------------------------------------------------------------
function resolve_object_resource(obj_lo, obj_hi) -> bool:
    // Inputs: obj_lo/obj_hi describe an object id.
    // Output: obj_ptr points to object header if found; returns true/false.

    obj_idx_lo = obj_lo
    obj_idx_hi = obj_hi

    // 1) For movable objects, try inventory first (if candidate for inventory)
    if obj_idx_hi == OBJ_HI_MOVABLE:
        owner = object_owner_nibble(obj_idx_lo)    // from object_attributes[]
        if owner != OWNER_IS_ROOM:
            // Scan inventory slots top-down for a matching object id
            for slot from MAX_INVENTORY_INDEX down to 0:
                if inventory_objects[slot] == obj_idx_lo:
                    // Found in inventory: object pointer comes from inventory table
                    obj_ptr = inventory_object_ptr_table[slot]
                    room_obj_ofs = 0
                    return true
                end if
            end for
        end if
        // If movable and owned by room, we fall through to room search
    end if

    // 2) Search the current room’s object list
    // room_obj_count is treated as “last valid object index” for this loop
    max_index = room_obj_count

    for i from max_index down to 0:
        if room_obj_idx_lo[i] != obj_idx_lo:
            continue
        end if

        if room_obj_idx_hi[i] != obj_idx_hi:
            continue
        end if

        // For movable objects, verify it is actually present in the room
        if obj_idx_hi == OBJ_HI_MOVABLE:
            if not verify_object_present_in_room(obj_idx_lo):
                continue
            end if
        end if

        // Match found in room: compute pointer = room base + per-object offset
        offset = room_obj_ofs_table[i]
        room_base = room_ptr_table[current_room]
        obj_ptr = room_base + offset
        room_obj_ofs = offset
        return true
    end for

    // Not found anywhere
    return false
end function


// -----------------------------------------------------------------------------
// verify_object_present_in_room (local helper, conceptual)
// -----------------------------------------------------------------------------
function verify_object_present_in_room(obj_lo) -> bool:
    // Re-check the object’s owner nibble from its global attributes.
    owner = object_owner_nibble(obj_lo)
    return (owner == OWNER_IS_ROOM)
end function


// -----------------------------------------------------------------------------
// select_preposition_for_verb
// -----------------------------------------------------------------------------
function select_preposition_for_verb(verb_id, do_lo, do_hi) -> Preposition:
    // 1) Data-driven preposition for USE, based on the object’s prep byte
    if verb_id == VERB_USE:
        if not resolve_object_if_not_costume(do_lo, do_hi):
            return PREPOSITION_NONE_NEEDED
        end if

        // obj_ptr points to object header; preposition index packed in one byte
        prep_byte = read_byte(obj_ptr + OBJ_PREP_BYTE_OFS)
        prep_index = (prep_byte >> 5) & 0b00000111   // high 3 bits

        return preposition_from_index(prep_index)
    end if

    // 2) Hard-coded mappings for other verbs
    if verb_id == VERB_GIVE:
        return PREPOSITION_TO
    end if

    if verb_id == VERB_NEW_KID or
       verb_id == VERB_UNLOCK  or
       verb_id == VERB_FIX:
        return PREPOSITION_WITH
    end if

    // 3) Default: no preposition needed
    return PREPOSITION_NONE_NEEDED
end function
*/