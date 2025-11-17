#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "text_data.inc"
#import "hotspots_metadata.inc"
#import "cursor.asm"
#import "input_scan.asm"

// ------------------------------------------------------------
// Hotspot lookup and metadata
// ------------------------------------------------------------
.const  HOTSPOT_END          	  = $8A    // Sentinel “end hotspot” index (out of table range)
.const  HOTSPOT_REC_STRIDE        = $06    // Bytes per hotspot record (row/col bounds, type, aux)

// ------------------------------------------------------------
// Hotspot types (matches hotspot_type field at +4)
// ------------------------------------------------------------
.const  HOTSPOT_TYPE_ITEM         = $02    // Inventory item cell (2×2 grid entry)
.const  HOTSPOT_TYPE_SCROLL       = $03    // Inventory scroll arrow (up/down)

// ------------------------------------------------------------
// Screen and color memory
// ------------------------------------------------------------
.const  SCREEN_BASE               = $CC00  // Text screen RAM base
.const  COLOR_BASE                = $D800  // VIC-II color RAM base


// ------------------------------------------------------------
// Scroll eligibility thresholds
// ------------------------------------------------------------
.const  MIN_OFFSET_FOR_UP         = $01    // Can scroll up if display offset ≥ 1
.const  ITEMS_THRESHOLD_FOR_DOWN  = $05    // Can scroll down if ≥5 items remain below view

// ------------------------------------------------------------
// Viewport paging
// ------------------------------------------------------------
.const  VISIBLE_SLOTS             = $04    // Number of visible item cells per page (2×2)
.const  INVENTORY_PAGE_STEP       = $02    // Items advanced per scroll click

// ------------------------------------------------------------
// Cursor render corrections (pixel/grid tuning)
// ------------------------------------------------------------
.const  Y_BUMP1_THRESH            = $90    // If Y ≥ $90, add +1 to computed row
.const  Y_BUMP2_THRESH            = $08    // If (Y & $0F) ≥ $08, add +1 to computed row
.const  Y_CORR_BIAS               = $28    // Final row bias (+40 decimal)
.const  POS16_LO_BIAS             = $80    // Add to low byte of 16-bit row base
.const  X_OFFSET_PIX              = $06    // X pixel offset applied to cursor position


// ------------------------------------------------------------
// Zero-page pointers
// ------------------------------------------------------------
// Source text pointer (lo at $C5, hi at $C6). Used by text copy/render.
.label src_ptr               = $C5       // ZP pointer to text source
// Destination pointers (lo at $C7, hi at $C8). Dual use:
// - Screen character destination
// - Color RAM row pointer (paired with COLOR_BASE paging logic)
.label scr_dest_ptr                  = $C7       // ZP pointer to screen destination

// ------------------------------------------------------------
// Hotspot metadata view (decoded fields for current hotspot)
// Offsets correspond to the per-entry layout:
// +0 row_start, +1 row_end_ex, +2 col_start, +3 col_end_ex, +4 type
// ------------------------------------------------------------
.label hotspot_row_start     = $F243     // Current hotspot row start (inclusive)
.label hotspot_row_end_ex    = $F244     // Current hotspot row end (exclusive)
.label hotspot_col_start     = $F245     // Current hotspot column start (inclusive)
.label hotspot_col_end_ex    = $F246     // Current hotspot column end (exclusive)
.label hotspot_type          = $F247     // Current hotspot type enum

// ------------------------------------------------------------
// Self-modifying call targets (patched JSR $0000 vectors)
// Each label marks the two-byte destination for the inline JSR
// ------------------------------------------------------------
.label inlined_max_column        = $F63D  // Patched target for column limit routine
.label inlined_max_row           = $F645  // Patched target for row limit routine
.label inlined_column_length     = $F605  // Patched target for column length calc
.label inlined_render_handler    = $F3AA  // Patched target for render handler
.label inlined_dehighlight_handler = $F344 // Patched target for dehighlight handler
.label inlined_highlight_handler = $F366  // Patched target for highlight handler
.label inlined_click_handler     = $F386  // Patched target for click handler

// ------------------------------------------------------------
// Inventory view state and shared temporaries
// Note: $CB8C is shared scratch. Only one alias is valid at a time.
// ------------------------------------------------------------
.label inv_display_item_offset = $CB8B    // First visible inventory index (display offset)

.label inv_count_accum         = $CB8C    // TEMP: running count of inventory items
.label owned_ordinal_remaining = $CB8C    // TEMP: remaining owned-item ordinal
.label tmp_row_index           = $CB8C    // TEMP: screen row index (0–25)
.label hotspot_text_color      = $CB8F    // Active text color attribute for hotspot rendering
                                            // Used when highlighting or dehighlighting labels
                                            // Written before drawing text into COLOR_BASE rows

// ------------------------------------------------------------
// Hotspot operand table
// Maps hotspot → operand used by render/handlers (0–3 display slot)
// ------------------------------------------------------------
.label hotspot_operand_slot    = $F248    // Table byte: 0..3 for item grid slot

// ------------------------------------------------------------
// Cursor/grid position math scratch
// ------------------------------------------------------------
.label raw_row_base_lo         = $CB7E    // Row base low byte before bias/corrections
.label pos16_lo                = $CB8C    // 16-bit position low (shared scratch)
.label pos16_hi                = $CB8D    // 16-bit position high

/*
================================================================================
refresh_inventory_regions_with_io_guard
================================================================================

Summary
    Safely refresh inventory hotspots by temporarily switching to cutscene
    control mode and banking I/O in, then restoring both on exit.

Global Inputs
    control_mode      current UI control state; read to decide early exit

Global Outputs
    control_mode      restored to its prior value after the redraw

Description
    - If already in cutscene mode, return immediately.
    - Save current control_mode on the stack and force cutscene mode.
    - Map in I/O and color RAM via cpu_port.
    - Call refresh_inventory_regions to redraw items and arrows.
    - Map I/O back out and restore the original control_mode.
================================================================================
*/
* = $5F2B
refresh_inventory_io_guarded:
        lda     control_mode                 // load current control mode
        cmp     #CUTSCENE_CONTROL_MODE        // are we already in cutscene mode?
        beq     return_early_if_cutscene     // yes → nothing to redraw safely here

        lda     control_mode                 // save previous control mode
        pha
        lda     #CUTSCENE_CONTROL_MODE       // force cutscene mode to suppress input/side effects
        sta     control_mode

        ldy     #MAP_IO_IN                   // map in I/O and color RAM ($D000–$DFFF)
        sty     cpu_port

        jsr     refresh_inventory_regions    // redraw item names and scroll arrows

        ldy     #MAP_IO_OUT                  // restore normal memory mapping
        sty     cpu_port

        pla                                  // restore prior control mode
        sta     control_mode

return_early_if_cutscene:
        rts                                   
/*
================================================================================
step_cursor_and_dispatch_hotspot
================================================================================

Summary
    Update cursor physics, resolve the current hotspot, and handle lifecycle
    transitions and clicks. On hotspot change, clears button edge state,
    runs dehighlight for the old hotspot, updates physics, then runs highlight
    for the new hotspot. Detects a fresh click edge and dispatches the click
    handler.

Arguments
    None

Vars/State
    hotspot_entry_ofs           current hotspot index; updated to new value
    button_presses              latched count/flag for processed clicks
    inlined_dehighlight_handler two-byte JSR target patched per hotspot type
    inlined_highlight_handler   two-byte JSR target patched per hotspot type
    inlined_click_handler       two-byte JSR target patched per hotspot type

Global Inputs
    hotspot_type                hotspot type table indexed by hotspot_entry_ofs

Global Outputs
    hotspot_entry_ofs           set to the newly detected hotspot index
    button_presses              cleared on hotspot change; incremented on click

Returns
    A, X, Y clobbered
    NZC unspecified

Description
    - Step cursor physics and update grid coordinates.
    - Find hotspot at cursor (X := new index).
    - If index differs from hotspot_entry_ofs:
        • Clear button_presses.
        • Run dehighlight for the previous hotspot via patched JSR.
        • Commit hotspot_entry_ofs := X.
        • Update cursor physics using the new hotspot.
        • Run highlight for the new hotspot via patched JSR.
    - Detect a rising-edge click; if present, dispatch the type-specific click
      handler via patched JSR and increment button_presses.

Notes
    - Uses self-modifying JSR targets to avoid indirect call overhead.
    - The “no hotspot” sentinel and handler tables are defined elsewhere.
================================================================================
*/
* = $F316
step_cursor_and_dispatch_hotspot:
        // ------------------------------------------------------------
        // Update cursor physics and determine current hotspot
        //
        // - Steps cursor motion and drag.
        // - Updates grid coordinates.
        // - Determines cursor hotspot index (X = new hotspot).
        // ------------------------------------------------------------
        jsr     cursor_physics_step
        jsr     update_cursor_grid_coords
        jsr     find_hotspot_at_cursor

        // ------------------------------------------------------------
        // If hotspot unchanged, skip to click handling
        // ------------------------------------------------------------
        cpx     hotspot_entry_ofs
        beq     handle_click

        // ------------------------------------------------------------
        // Handle hotspot change → reset button state
        // ------------------------------------------------------------
        txa
        pha
        lda     #$00
        sta     button_presses

        // ------------------------------------------------------------
        // Run exit handler for previous hotspot to dehighlight text
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     no_dehighlight_needed

        lda     hotspot_type,x
        asl
        tay
        lda     hotspot_dehighlight_handlers,y
        sta     inlined_dehighlight_handler
        lda     hotspot_dehighlight_handlers+1,y
        sta     inlined_dehighlight_handler+1

        jsr     $0000                  // invoke dehighlight handler

no_dehighlight_needed:
        pla
        sta     hotspot_entry_ofs

        // ------------------------------------------------------------
        // Update hotspot-specific cursor physics (accel/drag)
        // ------------------------------------------------------------
        jsr     update_cursor_physics_from_hotspot

        // ------------------------------------------------------------
        // Run entry handler for new hotspot to highlight text
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     handle_click

        lda     hotspot_type,x
        asl
        tay
        lda     hotspot_highlight_handlers,y
        sta     inlined_highlight_handler
        lda     hotspot_highlight_handlers+1,y
        sta     inlined_highlight_handler+1

        jsr     $0000                  // invoke highlight handler

handle_click:
        // ------------------------------------------------------------
        // Handle new button press edge and dispatch click handlers
        // ------------------------------------------------------------
        jsr     detect_fire_press_edge
        beq     exit_hotspot_handler

        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     register_press_and_exit

        lda     hotspot_type,x
        asl
        tay
        lda     hotspot_click_handlers,y
        sta     inlined_click_handler
        lda     hotspot_click_handlers+1,y
        sta     inlined_click_handler+1

        jsr     $0000                  // invoke click handler
register_press_and_exit:		
        inc     button_presses
exit_hotspot_handler:
        rts
/*
================================================================================
render_all_hotspots
================================================================================

Summary
    Iterate all UI hotspots and render each by dispatching to its type-specific
    handler via a self-modified JSR target. Stops when HOTSPOT_END is reached,
    then updates cursor physics from the final hotspot context.

Arguments
    None

Vars/State
    hotspot_entry_ofs       current hotspot index; incremented in +6 strides
    inlined_render_handler  two-byte JSR target patched with the handler address

Global Inputs
    hotspot_type            hotspot type table; selects render handler per index
    hotspot_render_handlers table of handler addresses; indexed by type*2

Returns
    A, X, Y  clobbered
    NZC      unspecified
    Tail-jumps to update_cursor_physics_from_hotspot on completion

Description
    - Initialize hotspot_entry_ofs to 0.
    - Loop:
        • If hotspot_entry_ofs == HOTSPOT_END, exit the loop.
        • Read type = hotspot_type[hotspot_entry_ofs].
        • Compute table index = type*2.
        • Patch inlined_render_handler with the two-byte address from
          hotspot_render_handlers[index .. index+1].
        • JSR $0000 to invoke the patched handler.
        • Advance hotspot_entry_ofs by the record stride.
      When finished, jump to update_cursor_physics_from_hotspot.

Notes
    - Uses self-modifying code to avoid indirect JSR overhead per hotspot.
================================================================================
*/
* = $F38C
render_all_hotspots:
        // ------------------------------------------------------------
        // Initialize hotspot iteration at #$00
        // ------------------------------------------------------------
        lda     #$00
        sta     hotspot_entry_ofs

render_hotspot:
        // ------------------------------------------------------------
        // Skip drawing if hotspot_entry_ofs == HOTSPOT_END
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        beq     next_region

        // ------------------------------------------------------------
        // Resolve and call hotspot entry handler
        //
        // - hotspot_type[X] → handler index
        // - index*2 → word offset into hotspot_render_handlers table
        // - Patch JSR $0000 target at inlined_render_handler
        // - Invoke handler via the inlined JSR
        // ------------------------------------------------------------
        lda     hotspot_type,x
        asl
        tay

        lda     hotspot_render_handlers,y
        sta     inlined_render_handler
        lda     hotspot_render_handlers+1,y
        sta     inlined_render_handler+1

        // Call patched handler
        jsr     $0000

next_region:
        // ------------------------------------------------------------
        // Advance to next hotspot. 
        // Exit once HOTSPOT_END is reached.
        // ------------------------------------------------------------
        clc
        lda     hotspot_entry_ofs
        adc     #HOTSPOT_REC_STRIDE
        sta     hotspot_entry_ofs

        cmp     #HOTSPOT_END
        bne     render_hotspot

        // ------------------------------------------------------------
        // Done drawing all regions → update cursor physics from hotspot
        // ------------------------------------------------------------
        jmp     update_cursor_physics_from_hotspot
/*
================================================================================
compute_corrected_cursor_pos
================================================================================

Summary
    Compute adjusted cursor coordinates and a scaled 16-bit position used by
    downstream logic. Applies two Y bump thresholds and a fixed vertical bias,
    then assembles a 16-bit value from row base and X position, left-shifts it,
    and returns the overflow and high byte.

Arguments
    None

Vars/State
    pos16_lo                 low byte of assembled 16-bit position (written)
    pos16_hi                 high byte of assembled 16-bit position (written)

Global Inputs
    cursor_y_pos             current cursor Y in pixels
    raw_row_base_lo          low byte of current row base address
    cursor_x_pos             current cursor X in pixels

Returns
    A  overflow bit from the 16-bit left shift (0 or 1)
    X  high byte of ( {pos16_hi:pos16_lo} << 1 )
    Y  corrected Y = cursor_y_pos after bumps + vertical bias
    NZC unspecified except as noted for A

Description
    - Read cursor_y_pos and, if it meets each threshold, add 1 per threshold.
    - Add fixed vertical bias and place result in Y.
    - Build 16-bit value:
        • pos16_lo := raw_row_base_lo + POS16_LO_BIAS
        • pos16_hi := cursor_x_pos + X_OFFSET_PIX
    - Perform a 16-bit left shift across {pos16_hi:pos16_lo}.
        • X := shifted high byte
        • A := final carry (overflow) from the shift

Notes
    - Thresholds and offsets (Y_BUMP1_THRESH, Y_BUMP2_THRESH, Y_CORR_BIAS,
      POS16_LO_BIAS, X_OFFSET_PIX) are constants defined elsewhere.
    - Caller interprets (X, A) as a coarse column/overflow pair; Y carries the
      corrected row for subsequent table lookups.
================================================================================
*/
* = $F3BC
compute_corrected_cursor_pos:
        // ------------------------------------------------------------
        // Compute adjusted cursor coordinates and scaled position
        //
        // If cursor_y_pos ≥ $90 → add 1
        // If cursor_y_pos ≥ $08 → add 1
        // Then Y := cursor_y_pos + $28 (vertical offset correction)
        //
        // pos16_lo := raw_row_base_lo + $80     (row temp)
        // pos16_hi := cursor_x_pos + 6 (column temp)
        //
        // Performs a 16-bit left shift on {pos16_hi:pos16_lo}.
        //   - X = high byte of shifted value
        //   - A = overflow bit from shift (0 or 1)
        //
        // Returns:
        //   .A = carry/overflow from shift
        //   .X = high byte of (16-bit value << 1)
        //   .Y = corrected cursor_y_pos + $28
        // ------------------------------------------------------------

        lda     cursor_y_pos
        cmp     #Y_BUMP1_THRESH
        adc     #$00
        cmp     #Y_BUMP2_THRESH
        adc     #$00
        adc     #Y_CORR_BIAS
        tay

        clc
        lda     raw_row_base_lo
        adc     #POS16_LO_BIAS
        sta     pos16_lo

        lda     cursor_x_pos
        adc     #X_OFFSET_PIX
        sta     pos16_hi

        lda     pos16_lo
        asl     
        lda     pos16_hi
        rol     
        tax

        lda     #$00
        rol     
        rts

* = $F3E8
dummy_handler:
		rts
/*
================================================================================
apply_highlight_color_and_fill
================================================================================

Summary
    Select the highlight color for the current hotspot type and fill its
    rectangular area in color RAM.

Arguments
    None

Vars/State
    (none)

Global Inputs
    hotspot_entry_ofs                  current hotspot index
    hotspot_type                       hotspot type for the current index
    hotspot_apply_highlight_color_and_fill  lookup table of highlight colors, indexed by type

Global Outputs
    hotspot_text_color                 receives the selected highlight color

Returns
    None; tail-calls fill_hotspot_color_span. A, X, Y clobbered. NZC unspecified.

Description
    - Load hotspot type from hotspot_type[hotspot_entry_ofs].
    - Read highlight color from the color table and store into hotspot_text_color.
    - Jump to fill_hotspot_color_span to paint the hotspot rectangle.
================================================================================
*/
* = $F3E9
apply_highlight_color_and_fill:
        // ------------------------------------------------------------
        // Set highlight color for current hotspot, then fill it
        //
        // X := hotspot_entry_ofs
        // Y := handler type for this hotspot
        // A := highlight color for that handler
        // hotspot_text_color := A
        // Jumps to fill_hotspot_color_span to apply it.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_type,x
        lda     hotspot_apply_highlight_color_and_fill,y
        sta     hotspot_text_color
        jmp     fill_hotspot_color_span
/*
================================================================================
apply_normal_color_and_fill
================================================================================

Summary
    Restore the normal color for the current hotspot type and fill its region
    in color RAM.

Arguments
    None

Vars/State
    (none)

Global Inputs
    hotspot_entry_ofs      current hotspot index
    hotspot_type           per-hotspot type table; indexed by hotspot_entry_ofs
    hotspot_normal_colors  lookup table of normal colors; indexed by hotspot type

Global Outputs
    hotspot_text_color     receives the selected normal color for this hotspot

Returns
    None; tail-calls fill_hotspot_color_span. A, X, Y clobbered by callees.
    NZC unspecified.

Description
    - Load hotspot type from hotspot_type[hotspot_entry_ofs].
    - Fetch its normal color from hotspot_normal_colors[type] into A.
    - Store A into hotspot_text_color.
    - Jump to fill_hotspot_color_span to paint the hotspot rectangle.
================================================================================
*/
* = $F3F8
apply_normal_color_and_fill:
        // ------------------------------------------------------------
        // Restore normal color for current hotspot, then fill it
        //
        // X := hotspot_entry_ofs
        // Y := handler type for this hotspot
        // A := normal color for that handler
        // hotspot_text_color := A
        // Jumps to fill_hotspot_color_span to apply it.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_type,x
        lda     hotspot_normal_colors,y
        sta     hotspot_text_color
        jmp     fill_hotspot_color_span
/*
================================================================================
enter_verb_hotspot_render
================================================================================

Summary
    Render the verb label for this hotspot, then apply the normal dehighlight
    color fill.

Arguments
    None

Vars/State
    (none)

Global Inputs
    (none)

Returns
    None; tail-calls apply_normal_color_and_fill. A, X, Y clobbered by callees.
    NZC unspecified.

Description
    - Call render_verb_label to draw the verb text.
    - Jump to apply_normal_color_and_fill to paint baseline colors.

Notes
    - Operand decoding and color selection are handled by the called helpers.
================================================================================
*/
* = $F407
enter_verb_hotspot_render:
        // ------------------------------------------------------------
        // Entered a verb hotspot: render its label, then dehighlight
        // ------------------------------------------------------------
        jsr     render_verb_label
        jmp     apply_normal_color_and_fill
/*
================================================================================
enter_item_hotspot_render
================================================================================

Summary
    Render the inventory item text for this hotspot, then apply the normal
    dehighlight color fill.

Arguments
    None

Returns
    None; tail-calls apply_normal_color_and_fill. A, X, Y are clobbered by
    callees. NZC unspecified.

Description
    - Call render_item_name_if_owned to draw the item’s name or nothing.
    - Jump to apply_normal_color_and_fill to paint baseline colors.

Notes
    - Ownership, operand decoding, and color selection are handled by the
      called helpers.
================================================================================
*/
* = $F40D
enter_item_hotspot_render:
        // ------------------------------------------------------------
        // Entered an inventory-item hotspot: render item text, then dehighlight
        // ------------------------------------------------------------
        jsr     render_item_name_if_owned
        jmp     apply_normal_color_and_fill
/*
================================================================================
enter_scroll_hotspot_render
================================================================================

Summary
    Render the inventory scroll arrow for this hotspot (if eligible), then
    apply the normal dehighlight color fill.

Arguments
    None

Returns
    None; tail-calls apply_normal_color_and_fill. A, X, Y clobbered by callees.
    NZC unspecified

Description
    - Call render_scroll_arrow_if_eligible to draw the up/down arrow or nothing.
    - Jump to apply_normal_color_and_fill to paint baseline colors.

Notes
    - This routine does not examine operands or state directly; eligibility and
      colors are resolved by the called helpers.
================================================================================
*/
* = $F413
enter_scroll_hotspot_render:
        // ------------------------------------------------------------
        // Entered an inventory-scroll hotspot: render arrow (if any), then dehighlight
        // ------------------------------------------------------------
        jsr     render_scroll_arrow_if_eligible
        jmp     apply_normal_color_and_fill
/*
================================================================================
enter_sentence_bar_render
================================================================================

Summary
    Render an empty string into the sentence-bar hotspot, then tail-call the
    normal dehighlight color fill routine.

Arguments
    None

Vars/State
    src_ptr    source pointer set to the empty string before rendering

Returns
    None; tail-calls apply_normal_color_and_fill. A, X, Y clobbered by callees.
    NZC unspecified.

Description
    - Point src_ptr at the empty string.
    - Call blit_text_to_hotspot_row to clear the hotspot text.
    - Jump to apply_normal_color_and_fill to paint baseline colors.
================================================================================
*/
* = $F419
enter_sentence_bar_render:
        // ------------------------------------------------------------
        // Entered action-bar background: render empty string, then dehighlight
        // Points source to $F609 which contains a single #$00 byte.
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr                           // <src_ptr := $F609 (lo)
        lda     #>empty_string
        sta     src_ptr + 1                           // >src_ptr := $F609 (hi)
        jsr     blit_text_to_hotspot_row
        jmp     apply_normal_color_and_fill
/*
================================================================================
click_room_scene_mark
================================================================================

Summary
    Mark that the room scene was clicked.

Arguments
    None

Global Outputs
    room_scene_clicked_flag    set to TRUE to signal a room-scene click

Returns
    A  := TRUE
    X  := preserved
    Y  := preserved
    NZC := unspecified

Description
    - Store TRUE into room_scene_clicked_flag.
    - Return.

Notes
    - Flag is level-triggered; upstream logic should clear it after handling.
================================================================================
*/
* = $F427
click_room_scene_mark:
        // ------------------------------------------------------------
        // Mark that the room scene was clicked
        // ------------------------------------------------------------
        lda     #TRUE
        sta     room_scene_clicked_flag
        rts
/*
================================================================================
click_set_verb_and_refresh
================================================================================

Summary
    Set the current verb from the hotspot operand. If any button press is
    latched, request sentence rebuild; otherwise request sentence UI init.
    Always mark the sentence bar for refresh.

Arguments
    None

Vars/State
    (none)

Global Inputs
    hotspot_entry_ofs       current hotspot index to read operand
    hotspot_operand_slot    per-hotspot operand table; verb index at [X]
    button_presses          nonzero indicates a button press is latched

Global Outputs
    current_verb_id         updated to selected verb id
    needs_sentence_rebuild  set when a button press is latched
    init_sentence_ui_flag   set when no button press is latched
    sentence_bar_needs_refresh  set to request bar redraw

Returns
    A  clobbered
    X  clobbered
    Y  preserved
    NZC unspecified

Description
    - Load verb index from hotspot_operand_slot[hotspot_entry_ofs] and store it
      in current_verb_id.
    - If button_presses ≠ 0: set needs_sentence_rebuild; else set
      init_sentence_ui_flag.
    - Set sentence_bar_needs_refresh and return.
================================================================================
*/
* = $F42D
click_set_verb_and_refresh:
        // ------------------------------------------------------------
        // Handle verb click in the action bar
        //
        // Load the verb index from the hotspot operand and set it as the
        // current verb. If any button was pressed, request a sentence
        // rebuild; otherwise initialize the sentence UI/stack. Always
        // mark the sentence bar for refresh before exit.
        // ------------------------------------------------------------

        // Fetch verb for the interaction hotspot
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x

        // Set it as the current verb
        sta     current_verb_id

        // If any button presses are latched, rebuild the sentence
        lda     button_presses
        beq     reset_sentence_ui_and_stack

        // Rebuild the action sentence
        lda     #TRUE
        sta     needs_sentence_rebuild
        jmp     mark_sentence_bar_refresh

reset_sentence_ui_and_stack:
        // ------------------------------------------------------------
        // Reset the sentence queue/UI
        // ------------------------------------------------------------
        lda     #TRUE
        sta     init_sentence_ui_flag

mark_sentence_bar_refresh:
        // ------------------------------------------------------------
        // Mark sentence bar for refresh and exit
        // ------------------------------------------------------------
        lda     #TRUE
        sta     sentence_bar_needs_refresh
        rts
/*
================================================================================
click_sentence_bar_rebuild
================================================================================

Summary
    Request a rebuild of the action sentence.

Arguments
    None

Global Outputs
    needs_sentence_rebuild    set to TRUE to signal sentence reconstruction

Returns
    A  := TRUE
    X  := preserved
    Y  := preserved
    NZC := unspecified

Description
    - Store TRUE into needs_sentence_rebuild.
    - Return.
================================================================================
*/
* = $F44D
click_sentence_bar_rebuild:
        // ------------------------------------------------------------
        // Rebuild the action sentence
        // ------------------------------------------------------------
        lda     #TRUE
        sta     needs_sentence_rebuild
        rts
/*
================================================================================
click_inventory_item_set_do_or_io
================================================================================

Summary
    Handle click on an inventory item hotspot. Uses the hotspot operand as the
    visible cell index, checks ownership via find_owned_slot_for_display, and
    sets either the Direct Object (no preposition pending) or the Indirect
    Object (preposition pending and item differs from current DO). Always marks
    the sentence bar for refresh; triggers sentence rebuild if a button press
    is latched.

Arguments
    None

Vars/State
    direct_object_idx_lo    low byte of current Direct Object index; written here
    direct_object_idx_hi    high byte of current Direct Object index; written here
    indirect_object_idx_lo  low byte of current Indirect Object index; written here
    indirect_object_idx_hi  high byte of current Indirect Object index; written here
    needs_sentence_rebuild  flag to request sentence rebuild; may be set here
    sentence_bar_needs_refresh  flag to refresh sentence bar; set here on exit

Global Inputs
    hotspot_entry_ofs       current hotspot index; selects operand
    hotspot_operand_slot    per-hotspot operand; visible cell index 0..3
    current_preposition     nonzero means a preposition is pending
    button_presses          nonzero means a button was pressed since last clear

Returns
    C  preserved from find_owned_slot_for_display on early exit path
    Y  inventory slot index when the item is owned (on the success path)
    A  clobbered
    X  clobbered
    NZ  unspecified

Description
    - Read display cell index from hotspot_operand_slot[hotspot_entry_ofs].
    - Call find_owned_slot_for_display:
        • If carry is set, item is not owned → skip object updates.
        • If carry is clear, Y holds the inventory slot:
            · If no preposition is pending, commit Direct Object = (X=cell, Y=slot).
            · If a preposition is pending, keep DO if it matches (X,Y);
              otherwise commit Indirect Object = (X,Y).
    - If any button press is latched, set needs_sentence_rebuild.
    - Always set sentence_bar_needs_refresh before returning.

Notes
    - This routine does not clear button_presses; upstream code owns that.
    - Object index pairs are stored as (lo, hi) bytes for later consumers.
================================================================================
*/
* = $F452
click_inventory_item_set_do_or_io:
        // ------------------------------------------------------------
        // Handle click on an inventory item hotspot
        //
        // A := display slot index (0..3) via hotspot_operand_slot[X].
        // JSR find_owned_slot_for_display:
        //   - On CLC (owned): .Y = inventory slot index; proceed to set DO/IO.
        //   - On SEC (not owned): skip to UI refresh.
        // Updates:
        //   - If no preposition pending: set Direct Object = (X,Y).
        //   - Else: if (X,Y) already equals Direct Object, keep as-is; otherwise set Indirect Object = (X,Y).
        // Always marks sentence bar for refresh; rebuilds sentence if any button was pressed.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
        jsr     find_owned_slot_for_display
        bcs     post_click_ui_update          // C=1 → not owned

        // ------------------------------------------------------------
        // Item is owned → decide whether it becomes DO or IO
        // ------------------------------------------------------------
        ldy     #$00
        lda     current_preposition
        bne     check_is_current_do                            // preposition set → possible IO path
        stx     direct_object_idx_lo             // commit DO := (X,Y)
        sty     direct_object_idx_hi
        jmp     post_click_ui_update

        // ------------------------------------------------------------
        // Preposition set: if already DO, keep; else commit as IO
        // ------------------------------------------------------------
check_is_current_do:		
        cpx     direct_object_idx_lo             // same DO lo?
        bne     commit_indirect_object
        cpy     direct_object_idx_hi             // same DO hi?
        bne     commit_indirect_object
        jmp     post_click_ui_update

        // ------------------------------------------------------------
        // Commit Indirect Object := (X,Y)
        // ------------------------------------------------------------
commit_indirect_object:		
        stx     indirect_object_idx_lo
        sty     indirect_object_idx_hi

post_click_ui_update:
        // ------------------------------------------------------------
        // Post-click UI updates: maybe rebuild sentence, always refresh bar
        // ------------------------------------------------------------
        lda     button_presses                   // any button latched?
        beq     return_refresh_bar
        lda     #TRUE
        sta     needs_sentence_rebuild

        // ------------------------------------------------------------
        // Mark sentence bar for refresh and exit
        // ------------------------------------------------------------
return_refresh_bar:		
        lda     #TRUE
        sta     sentence_bar_needs_refresh
        rts
/*
================================================================================
click_inventory_scroll_page
================================================================================

Summary
    Handle click on an inventory scroll arrow. Operand 0=up, 1=down. Adjust
    inv_display_item_offset by ±INVENTORY_PAGE_STEP and then refresh inventory
    regions.

Arguments
    None

Global Inputs
    hotspot_entry_ofs       current hotspot index to read the operand
    hotspot_operand_slot    per-hotspot operand; 0=up, 1=down
    inv_display_item_offset current window start index before adjustment

Global Outputs
    inv_display_item_offset updated window start index after adjustment

Returns
    None; tail-calls refresh_inventory_regions on down path.
    On up path, falls through to the next routine. A, X modified. NZC unspecified.

Description
    - Read operand from hotspot_operand_slot[hotspot_entry_ofs].
    - If operand=1 (down): add INVENTORY_PAGE_STEP to inv_display_item_offset,
      commit, and jump to refresh_inventory_regions.
    - If operand=0 (up): subtract INVENTORY_PAGE_STEP. If result is negative,
      clamp to 0. Commit and fall through.

Notes
    - INVENTORY_PAGE_STEP is a constant defined elsewhere.
    - Caller should not expect an RTS here; control either jumps to the refresh
      routine or falls through into the next routine.
================================================================================
*/
* = $F48F
click_inventory_scroll_page:
        // ------------------------------------------------------------
        // Handle click on inventory scroll arrow
        //
        // Operand (0 = up, 1 = down) determines scroll direction.
        // Adjusts inv_display_item_offset by ±2 and triggers
        // refresh of all visible items and arrows.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
        bne     scroll_page_down
		jmp		scroll_page_up

scroll_page_down:
        // ------------------------------------------------------------
        // Scroll down: add 2 to item display offset
        // ------------------------------------------------------------
        clc
        lda     inv_display_item_offset
        adc     #INVENTORY_PAGE_STEP
        sta     inv_display_item_offset
        jmp     refresh_inventory_regions

scroll_page_up:
        // ------------------------------------------------------------
        // Scroll up: subtract 2 from item display offset
        // Clamp to 0 if subtraction underflows
        // ------------------------------------------------------------
        sec
        lda     inv_display_item_offset
        sbc     #INVENTORY_PAGE_STEP
        bpl     commit_display_offset
        lda     #$00
commit_display_offset:
        sta     inv_display_item_offset
        // Fall through to next routine	
/*
================================================================================
refresh_inventory_regions
================================================================================

Summary
    Refresh all inventory-related hotspots (items and arrows). Clamps the
    window offset to the highest valid page, then scans all hotspots and
    re-renders those whose type is inventory item or scroll arrow.

Arguments
    None

Global Inputs
    inv_display_item_offset    index of the top-left visible item; may be clamped
    hotspot_entry_ofs          current hotspot index; preserved across this pass
    hotspot_type               per-hotspot type table read during the scan

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - Compute max valid window start as max(count_owned - 4, 0).
    - Clamp inv_display_item_offset if it exceeds that value.
    - Save current hotspot_entry_ofs, then iterate all hotspots from 0
      in +6 strides until HOTSPOT_END:
        • If type == ITEM, call render_item_name_if_owned.
        • If type == SCROLL, call render_scroll_arrow_if_eligible.
    - Restore hotspot_entry_ofs and return.

Notes
    - ITEM and SCROLL type ids, HOTSPOT_END, and HOTSPOT_REC_STRIDE are
      constants defined elsewhere.
    - The visible grid holds 4 item cells; paging step depends on layout.
================================================================================
*/
* = $F4B3
refresh_inventory_regions:
        // ------------------------------------------------------------
        // Refresh all inventory-related hotspots (items and arrows)
        //
        // Clamps inv_display_item_offset to ensure it does not exceed
        // the highest valid page. Then scans all hotspots and refreshes
        // those of type “inventory item” (#$02) or “scroll arrow” (#$03).
        // ------------------------------------------------------------

        // ------------------------------------------------------------
        // Clamp display offset to (owned_items - 4) minimum 0
        // ------------------------------------------------------------
        jsr     count_active_kid_inventory_items
        sec
        sbc     #VISIBLE_SLOTS
        bpl     clamp_window_start
        lda     #$00
clamp_window_start:
        cmp     inv_display_item_offset
        bpl     push_current_region
        sta     inv_display_item_offset

push_current_region:
        // ------------------------------------------------------------
        // Preserve current hotspot index
        // ------------------------------------------------------------
        lda     hotspot_entry_ofs
        pha

        // ------------------------------------------------------------
        // Begin scanning all hotspots from hotspot #$00
        // ------------------------------------------------------------
        lda     #$00
        sta     hotspot_entry_ofs

scan_all_hotspots:
        ldx     hotspot_entry_ofs
        lda     hotspot_type,x

        // ------------------------------------------------------------
        // hotspot type #$02 → inventory item
        // ------------------------------------------------------------
        cmp     #HOTSPOT_TYPE_ITEM
        bne     dispatch_scroll_arrow
        jsr     render_item_name_if_owned
        jmp     advance_region_or_finish

dispatch_scroll_arrow:
        // ------------------------------------------------------------
        // hotspot type #$03 → inventory scroll arrow
        // ------------------------------------------------------------
        cmp     #HOTSPOT_TYPE_SCROLL
        bne     advance_region_or_finish
        jsr     render_scroll_arrow_if_eligible

advance_region_or_finish:
        // ------------------------------------------------------------
        // Advance to next hotspot (+6 stride) and continue
        // ------------------------------------------------------------
        clc
        lda     hotspot_entry_ofs
        adc     #HOTSPOT_REC_STRIDE
        sta     hotspot_entry_ofs
        cmp     #HOTSPOT_END
        bne     scan_all_hotspots

        // ------------------------------------------------------------
        // Restore hotspot and exit
        // ------------------------------------------------------------
        pla
        sta     hotspot_entry_ofs
        rts
/*
================================================================================
render_scroll_arrow_if_eligible
================================================================================

Summary
    Render the inventory scroll arrow (up or down) into the hotspot row if the
    paging condition is met; otherwise render an empty string.

Arguments
    None

Vars/State
    src_ptr                 source pointer for arrow text (written here)

Global Inputs
    hotspot_entry_ofs       current hotspot index used to read operand
    hotspot_operand_slot    per-hotspot operand; 0=up, 1=down
    inv_display_item_offset index of the top-left visible item
    inv_arrow_ptr_lo        low bytes of arrow text pointers (indexed by operand)
    inv_arrow_ptr_hi        high bytes of arrow text pointers (indexed by operand)

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - If operand=0 (up): show arrow only when inv_display_item_offset ≥ 1.
    - If operand=1 (down): show arrow only when
        count_active_kid_inventory_items() - inv_display_item_offset ≥ 5.
    - On success: load arrow text pointer and blit into the hotspot row.
    - On failure: point src_ptr to the empty string and blit.

Notes
    - Threshold constants and the empty string label are defined elsewhere.
    - Uses blit_text_to_hotspot_row for rendering.
================================================================================
*/
* = $F4F7
render_scroll_arrow_if_eligible:
        // ------------------------------------------------------------
        // Render inventory scroll arrow (up or down) in hotspot 
        //
        // Operand (0 = up, 1 = down) defines which arrow to display.
        // The up arrow is shown only if the inventory offset ≥ 1.
        // The down arrow is shown only if there are ≥ 5 items past
        // the current offset (more pages to scroll).
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x
        bne     check_down_arrow

        // ------------------------------------------------------------
        // Up arrow: show only if inv_display_item_offset ≥ 1
        // ------------------------------------------------------------
        lda     inv_display_item_offset
        cmp     #MIN_OFFSET_FOR_UP
        jmp     decide_arrow_display

check_down_arrow:
        // ------------------------------------------------------------
        // Down arrow: show only if remaining items ≥ 5
        // ------------------------------------------------------------
        jsr     count_active_kid_inventory_items
        sec
        sbc     inv_display_item_offset
        cmp     #ITEMS_THRESHOLD_FOR_DOWN

decide_arrow_display:
        // ------------------------------------------------------------
        // If comparison fails → render empty string (no arrow)
        // ------------------------------------------------------------
        bcc     no_arrow

        // ------------------------------------------------------------
        // Valid arrow → get text pointer and render
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x
        lda     inv_arrow_ptr_lo,y
        sta     src_ptr
        lda     inv_arrow_ptr_hi,y
        sta     src_ptr+1
        jmp     rsaie_trampoline

no_arrow:
        // ------------------------------------------------------------
        // Point to empty string
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr
        lda     #>empty_string
        sta     src_ptr+1
rsaie_trampoline:		
        jmp     blit_text_to_hotspot_row
/*
================================================================================
render_verb_label
================================================================================

Summary
    Copy the verb text referenced by the hotspot into the hotspot’s screen row.

Arguments
    None

Vars/State
    src_ptr                     source pointer for text bytes (written here)

Global Inputs
    hotspot_entry_ofs           current hotspot index used to read operand
    hotspot_operand_slot        per-hotspot operand; verb index for this entry
    verb_pointers_lo            table of verb text pointers, low bytes
    verb_pointers_hi            table of verb text pointers, high bytes

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - Load verb index from hotspot_operand_slot[hotspot_entry_ofs].
    - Build src_ptr from verb_pointers_{lo,hi}[index].
    - Jump to blit_text_to_hotspot_row to render into the hotspot row.

Notes
    - Assumes verb text is zero-terminated and compatible with the blitter’s
      space/terminator handling.
================================================================================
*/
* = $F530
render_verb_label:
        // ------------------------------------------------------------
        // Copy the text of a verb into the hotspot 
        //
        // Loads the hotspot’s operand (verb index) and resolves the
        // pointer to the corresponding verb string, then blits the text
        // into the screen row for that hotspot.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        ldy     hotspot_operand_slot,x

        // ------------------------------------------------------------
        // Resolve pointer to verb text
        // ------------------------------------------------------------
        lda     verb_pointers_lo,y
        sta     src_ptr
        lda     verb_pointers_hi,y
        sta     src_ptr+1

        // ------------------------------------------------------------
        // Render the verb text into the hotspot row
        // ------------------------------------------------------------
        jmp     blit_text_to_hotspot_row
/*
================================================================================
render_item_name_if_owned
================================================================================

Summary
    Copy the current inventory item’s name into its hotspot row. Uses the
    hotspot’s operand to select a visible display cell, checks ownership
    against the current kid, and either blits the resolved name or an empty
    string into the screen region.

Arguments
    None

Vars/State
    src_ptr                     source pointer for text bytes (written here)

Global Inputs
    hotspot_entry_ofs           current hotspot index for this render
    hotspot_operand_slot        table: maps hotspot to display cell index (0..3)
    object_ptr_lo_tbl           table: per-inventory-slot object base pointer lo
    object_ptr_hi_tbl           table: per-inventory-slot object base pointer hi

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - Read the hotspot’s display cell index from hotspot_operand_slot[X].
    - Call find_owned_slot_for_display:
        • If C=1, set src_ptr to the empty string and blit.
        • If C=0, Y holds the matching inventory slot index.
    - On C=0, build src_ptr from object_ptr_{lo,hi}_tbl[Y], then add the
      object’s name offset at byte +$0D and blit via blit_text_to_hotspot_row.

Notes
    - Assumes find_owned_slot_for_display returns Y = inventory slot index
      when C=0 (owned). On C=1, Y is not used.
    - Name offset (+$0D) is part of the object record layout.
================================================================================
*/
* = $F543
render_item_name_if_owned:
        // ------------------------------------------------------------
        // Copy the current item's name text into its hotspot 
        //
        // Determines if the item corresponding to the hotspot belongs
        // to the current kid. If not, sets src_ptr to an empty string.
        // Otherwise, computes the name pointer and blits it to screen.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        lda     hotspot_operand_slot,x
        jsr     find_owned_slot_for_display
        bcc     resolve_name_ptr_and_blit

        // ------------------------------------------------------------
        // Item not owned → set src_ptr to empty string at $F609
        // ------------------------------------------------------------
        lda     #<empty_string
        sta     src_ptr
        lda     #>empty_string
        sta     src_ptr+1
        jmp     blit_text_trampoline

resolve_name_ptr_and_blit:
        // ------------------------------------------------------------
        // Resolve object pointer for inventory slot in .Y
        // ------------------------------------------------------------
        lda     object_ptr_lo_tbl,y
        sta     src_ptr
        lda     object_ptr_hi_tbl,y
        sta     src_ptr+1

        // ------------------------------------------------------------
        // Add name offset from byte +$0D within object record
        // ------------------------------------------------------------
        ldy     #OBJ_NAME_OFS
        clc
        lda     src_ptr
        adc     (src_ptr),y
        sta     src_ptr
        bcc     blit_text_trampoline
        inc     src_ptr+1

blit_text_trampoline:
        jmp     blit_text_to_hotspot_row
/*
================================================================================
find_owned_slot_for_display
================================================================================

Summary
    Resolve whether the Nth visible inventory item (by display index) is owned
    by the current kid. Converts display index to an ordinal within the stream
    of owned items, scans inventory slots, and returns via the carry flag.

Arguments
    A  input display index in 0..3 (0=top-left)

Vars/State
    owned_ordinal_remaining    remaining count to reach the target owned item

Global Inputs
    inv_display_item_offset    index of top-left item in the window
    inventory_objects          inventory slot table; $00 means empty slot
    object_attributes          per-object attributes; low nibble encodes owner id
    current_kid_idx            active kid index used to match ownership

Returns
    C  clear if the targeted visible item is owned by current kid
       set   if not owned or no such item
    A  clobbered
    X  clobbered
    Y  clobbered
    NZ  unspecified

Description
    - Compute target ordinal: display_index + inv_display_item_offset.
    - Iterate inventory slots:
        • Skip empty entries.
        • Compare each object’s owner nibble with current_kid_idx.
        • On each match, decrement owned_ordinal_remaining.
        • When it underflows (<0), the Nth owned item has been reached → C=0.
    - If the scan ends without underflow, no owned item occupies that cell → C=1.

Notes
    - Slot count and owner-nibble mask are constants defined elsewhere.
    - This routine only answers “is owned” for the visible cell; it does not
      return the object id or its absolute slot index.
================================================================================
*/
* = $F573
find_owned_slot_for_display:
        // ------------------------------------------------------------
        // Check if a displayed inventory item belongs to the current kid
        //
        // Input:  .A = display index (0–3)
        //         inv_display_item_offset = index of top-left item
        //
        // Returns: Carry clear = item owned by current kid
        //          Carry set   = item not owned
        // ------------------------------------------------------------
        clc
        adc     inv_display_item_offset
        sta     owned_ordinal_remaining

        // ------------------------------------------------------------
        // Scan through all inventory slots
        // ------------------------------------------------------------
        ldy     #$00

scan_slots_for_owned_match:
        // Load object ID from slot; skip if empty (#$00)
        ldx     inventory_objects,y
        beq     advance_slot_or_finish

        // Check if item belongs to the active kid
        lda     object_attributes,x
        and     #OWNER_NIBBLE_MASK
        cmp     current_kid_idx
        bne     advance_slot_or_finish

        // Match → decrement remaining ordinal
        dec     owned_ordinal_remaining
        bpl     advance_slot_or_finish

        // Found the Nth owned item → carry clear
        clc
        rts

advance_slot_or_finish:
        // Move to next slot and continue until all checked
        iny
        cpy     #INVENTORY_SLOTS
        bne     scan_slots_for_owned_match

        // None matched → carry set
        sec
        rts
/*
================================================================================
count_active_kid_inventory_items
================================================================================

Summary
    Count how many inventory objects are owned by the current kid. Scans the
    inventory slot table, filters empty slots, checks the owner nibble against
    current_kid_idx, and accumulates a total.

Arguments
    None

Vars/State
    inv_count_accum    accumulator for the running item count (written here)

Global Inputs
    inventory_objects     inventory slot table; each entry is an object id or $00
    object_attributes     per-object attribute bytes; low nibble encodes owner id
    current_kid_idx       active kid index to match against object owner nibble

Returns
    A  := total number of owned items
    X  := clobbered
    Y  := clobbered
    NZC := unspecified

Description
    - Initialize inv_count_accum to zero and Y to 0.
    - For each slot:
        • Read object id; skip if $00.
        • Read object_attributes[id], mask owner nibble, compare to current_kid_idx.
        • If equal, increment inv_count_accum.
    - When Y reaches INVENTORY_SLOTS, move inv_count_accum to A and return.

Notes
    - OWNER_NIBBLE_MASK and INVENTORY_SLOTS are constants defined elsewhere.
    - Empty slots are encoded as $00 in inventory_objects.
================================================================================
*/
* = $F599
count_active_kid_inventory_items:
        // ------------------------------------------------------------
        // Count all items belonging to the current kid
        //
        // Loops through the inventory object list and counts how many
        // items are owned by the active kid (current_kid_idx). Returns
        // total count in .A.
        // ------------------------------------------------------------
        lda     #$00
        sta     inv_count_accum
        ldy     #$00

scan_slot_check_owner:
        // ------------------------------------------------------------
        // Load object ID from inventory slot; skip if empty (#$00)
        // ------------------------------------------------------------
        ldx     inventory_objects,y
        beq     advance_slot_or_exit

        // ------------------------------------------------------------
        // Check if object belongs to the active kid
        // ------------------------------------------------------------
        lda     object_attributes,x
        and     #OWNER_NIBBLE_MASK
        cmp     current_kid_idx
        bne     advance_slot_or_exit

        // ------------------------------------------------------------
        // Match found → increment counter
        // ------------------------------------------------------------
        inc     inv_count_accum

advance_slot_or_exit:
        // ------------------------------------------------------------
        // Advance to next slot; stop after all 45 entries
        // ------------------------------------------------------------
        iny
        cpy     #INVENTORY_SLOTS
        bne     scan_slot_check_owner

        // ------------------------------------------------------------
        // Load final count into .A and return
        // ------------------------------------------------------------
        lda     inv_count_accum
        rts
/*
================================================================================
blit_text_to_hotspot_row
================================================================================

Summary
    Copy one text row from src_ptr into the active hotspot’s screen region.
    Converts $40 and $00 to space; a $00 also switches to fill mode to pad
    remaining columns with spaces. Uses start-inclusive, end-exclusive bounds.

Arguments
    None

Vars/State
    scr_dest_ptr               destination pointer into screen RAM (lo/hi)
    inlined_column_length  self-modified byte for CPY #column_count
    src_ptr                source pointer for text bytes (lo/hi)

Global Inputs
    hotspot_entry_ofs      current hotspot index; HOTSPOT_END means none active
    hotspot_row_start      row start (inclusive) per hotspot, indexed by X
    hotspot_col_start      column start (inclusive) per hotspot, indexed by X
    hotspot_col_end_ex     column end (exclusive) per hotspot, indexed by X
    screen_row_offsets_lo  low bytes of per-row screen offsets
    screen_row_offsets_hi  high bytes of per-row screen offsets

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - Guard: if hotspot_entry_ofs == HOTSPOT_END, return.
    - Compute scr_dest_ptr = SCREEN_BASE + screen_row_offset[row_start] + col_start.
    - Compute column_count = col_end_ex - col_start and patch CPY immediate.
    - Copy bytes from src_ptr:
        • Treat $40 and $00 as space.
        • When $00 is seen, enter fill mode and write spaces for the rest.
    - Stop when Y reaches column_count.

Notes
    - Fill mode uses X ≠ 0 as the fast-path signal to skip source reads.
    - SCREEN_BASE is fixed elsewhere; this routine only builds the offset.
================================================================================
*/
* = $F5BB
blit_text_to_hotspot_row:
        // ------------------------------------------------------------
        // Copy one text row to the hotspot’s screen region
        //
        // Writes characters from (src_ptr) into screen RAM ($CC00 page)
        // spanning the column range defined by the hotspot. Terminator
        // bytes (#$40 or #$00) are converted to spaces, and #$00 also
        // triggers filling the remaining columns with spaces.
        // ------------------------------------------------------------
        ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        bne     compute_screen_ptr_for_hotspot
        rts

compute_screen_ptr_for_hotspot:
        // ------------------------------------------------------------
        // Compute destination pointer:
        // scr_dest_ptr = SCREEN_BASE + screen_row_offsets[row_start]
        // scr_dest_ptr += hotspot_col_start
        // ------------------------------------------------------------
        ldy     hotspot_row_start,x
        clc
        lda     screen_row_offsets_lo,y
        adc     #<SCREEN_BASE
        sta     scr_dest_ptr
        lda     screen_row_offsets_hi,y
        adc     #>SCREEN_BASE
        sta     scr_dest_ptr+1

        // Add column offset
        clc
        lda     scr_dest_ptr
        adc     hotspot_col_start,x
        sta     scr_dest_ptr
        bcc     calc_hotspot_col_width
        inc     scr_dest_ptr+1

calc_hotspot_col_width:
        // ------------------------------------------------------------
        // Compute column count = col_end_ex - col_start
        // ------------------------------------------------------------
        sec
        lda     hotspot_col_end_ex,x
        sbc     hotspot_col_start,x
        sta     inlined_column_length          // patch CPY immediate

        // Initialize src_ptr and index
        ldy     #$00
        ldx     #$00

copy_or_fill_loop:
        // ------------------------------------------------------------
        // Copy loop with terminator handling
        // ------------------------------------------------------------
        txa
        bne     emit_char_to_screen                  // X != 0 → fill spaces

        lda     (src_ptr),y                     // read next source byte
        cmp     #WORD_HARD_STOP
        bne     check_space_conversion
        lda     #$00                           // convert hard stop → 0

check_space_conversion:
        cmp     #$00
        bne     emit_char_to_screen
        lda     #SPACE_CHAR                    // convert 0 → space
        tax                                    // enter fill mode

emit_char_to_screen:
        sta     (scr_dest_ptr),y
        iny
        cpy     #$12                           // patched with col_end_ex
        bne     copy_or_fill_loop
        rts
/*
================================================================================
fill_hotspot_color_span
================================================================================

Summary
    Fill the active hotspot’s rectangular area in color RAM using the current
    hotspot color. Uses start-inclusive, end-exclusive bounds. Patches loop
    limits via self-modified bytes for speed.

Arguments
    None

Vars/State
    scr_dest_ptr              destination pointer into color RAM (lo/hi)
    tmp_row_index         working copy of current row index
    inlined_max_column    self-modified byte for CPY #col_end_ex
    inlined_max_row       self-modified byte for CPY #row_end_ex

Global Inputs
    hotspot_entry_ofs       current hotspot index; HOTSPOT_END means none active
    hotspot_row_start       row start (inclusive) per hotspot, indexed by X
    hotspot_row_end_ex      row end (exclusive) per hotspot, indexed by X
    hotspot_col_start       column start (inclusive) per hotspot, indexed by X
    hotspot_col_end_ex      column end (exclusive) per hotspot, indexed by X
    screen_row_offsets_lo   low bytes of per-row screen offsets
    screen_row_offsets_hi   high bytes of per-row screen offsets
    hotspot_text_color      color byte to write into the region

Returns
    None; clobbers A, X, Y. NZC unspecified.

Description
    - If no hotspot is active, return immediately.
    - Patch the column and row end limits into inlined_max_column and
      inlined_max_row to drive CPY immediate bounds in the inner loops.
    - For each row from row_start to row_end_ex:
        • Build scr_dest_ptr = COLOR_BASE + screen_row_offset[row]
        • Fill columns [col_start, col_end_ex) with hotspot_text_color.

Notes
    - Self-modifies two CPY #imm operands to avoid extra compares.
    - COLOR_BASE is assumed fixed by the calling context.
================================================================================
*/
* = $F60A
fill_hotspot_color_span:
        // ------------------------------------------------------------
        // Guard: exit when no hotspot is active
        //
        // Loads the current hotspot index and compares it against the
        // sentinel HOTSPOT_END. If equal, no region needs coloring,
        // so return immediately; otherwise proceed to load bounds.
        // ------------------------------------------------------------
		ldx     hotspot_entry_ofs
        cpx     #HOTSPOT_END
        bne     patch_rowcol_limits
        rts

patch_rowcol_limits:
        // ------------------------------------------------------------
        // Inline hotspot’s max column and max row as self-modified bytes
        // ------------------------------------------------------------
        lda     hotspot_col_end_ex,x
        sta     inlined_max_column           // replaces CPY immediate
        lda     hotspot_row_end_ex,x
        sta     inlined_max_row              // replaces CPY immediate

        // ------------------------------------------------------------
        // Initialize current row index = hotspot_row_start[X]
        // ------------------------------------------------------------
        ldy     hotspot_row_start,x

row_begin_compute_color_ptr:
        sty     tmp_row_index                

        // ------------------------------------------------------------
        // Compute color buffer address for this row:
        // scr_dest_ptr = COLOR_BASE + screen_row_offset[Y]
        // ------------------------------------------------------------
        clc
        lda     screen_row_offsets_lo,y
        adc     #<COLOR_BASE
        sta     scr_dest_ptr
        lda     screen_row_offsets_hi,y
        adc     #>COLOR_BASE
        sta     scr_dest_ptr+1

        // ------------------------------------------------------------
        // Prepare column loop
        // ------------------------------------------------------------
        ldy     hotspot_col_start,x
        lda     hotspot_text_color

fill_row_colors_loop:
        sta     (scr_dest_ptr),y
        iny
        cpy     #$0F                         // patched with col_end_ex
        bne     fill_row_colors_loop

        // ------------------------------------------------------------
        // Advance to next row and compare with max
        // ------------------------------------------------------------
        ldy     tmp_row_index
        iny
        cpy     #$15                         // patched with row_end_ex
        bne     row_begin_compute_color_ptr
        rts
/*
================================================================================
find_hotspot_at_cursor
================================================================================

Summary
    Hit-test the UI hotspot table using the current cursor pixel position.
    Converts cursor_x_pos and cursor_y_pos into cell coordinates (x/4, y/8),
    scans hotspot records in +6-byte strides, and returns the matching hotspot
    index. Uses inclusive start bounds and exclusive end bounds.

Arguments
    None (reads globals only)

Vars/State
    cursor_x_cell    written: x cell coordinate = cursor_x_pos >> 2
    cursor_y_cell    written: y cell coordinate = cursor_y_pos >> 3

Global Inputs
    cursor_x_pos         current cursor X position in pixels
    cursor_y_pos         current cursor Y position in pixels
    hotspot_row_start    row start (inclusive) per hotspot, indexed by X
    hotspot_row_end_ex   row end (exclusive) per hotspot, indexed by X
    hotspot_col_start    column start (inclusive) per hotspot, indexed by X
    hotspot_col_end_ex   column end (exclusive) per hotspot, indexed by X

Returns
    X  := hotspot index on hit, or HOTSPOT_END if none
    A  := clobbered (last compared value)
    Y  := preserved
    NZC := unspecified

Description
    - Derive cell coordinates from pixel positions for coarse grid hit-testing.
    - Iterate hotspot records with stride HOTSPOT_REC_STRIDE.
    - For each record, require:
        • row_start ≤ y_cell < row_end_ex
        • col_start ≤ x_cell < col_end_ex
    - On first match, return immediately with X = index.
    - If no match, return with X = HOTSPOT_END.

Notes
    - Bounds use start-inclusive, end-exclusive semantics (“_end_ex”).
    - Table layout is assumed to be tightly packed with fixed stride.
    - Caller should treat HOTSPOT_END as “no selection”.
================================================================================
*/
* = $F65C
find_hotspot_at_cursor:
        // ------------------------------------------------------------
        // Compute cursor_x_cell = cursor_x_pos / 4
        // ------------------------------------------------------------
        lda     cursor_x_pos
        lsr     
        lsr     
        sta     cursor_x_cell

        // ------------------------------------------------------------
        // Compute cursor_y_cell = cursor_y_pos / 8
        // ------------------------------------------------------------
        lda     cursor_y_pos
        lsr     
        lsr     
        lsr     
        sta     cursor_y_cell

        // ------------------------------------------------------------
        // Initialize hotspot index to #$00
        // ------------------------------------------------------------
        ldx     #$00

test_hotspot_bounds:
        // ------------------------------------------------------------
        // Check if Y cell coordinate within hotspot bounds
        // ------------------------------------------------------------
        lda     cursor_y_cell
        cmp     hotspot_row_start,x
        bcc     advance_to_next_hotspot
		
        cmp     hotspot_row_end_ex,x
        bcs     advance_to_next_hotspot

        // ------------------------------------------------------------
        // Check if X cell coordinate within hotspot bounds
        // ------------------------------------------------------------
        lda     cursor_x_cell
        cmp     hotspot_col_start,x
        bcc     advance_to_next_hotspot
		
        cmp     hotspot_col_end_ex,x
        bcs     advance_to_next_hotspot

        // ------------------------------------------------------------
        // Hotspot found → return with .X = hotspot index
        // ------------------------------------------------------------
        rts

advance_to_next_hotspot:
        // ------------------------------------------------------------
        // Move to next hotspot (add stride of 6 to index)
        // ------------------------------------------------------------
        clc
        txa
        adc     #HOTSPOT_REC_STRIDE
        tax

        // ------------------------------------------------------------
        // Check if all hotspots tested
        // ------------------------------------------------------------
        cpx     #HOTSPOT_END
        bne     test_hotspot_bounds

        // ------------------------------------------------------------
        // No matching hotspot → return with .X = HOTSPOT_END
        // ------------------------------------------------------------
        rts
		