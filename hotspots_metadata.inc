// ------------------------------------------------------------
// Hotspot metadata layout (per-entry, byte offsets)
//
// +0: row_start          Inclusive start row (grid units)
// +1: row_end_excl       Exclusive end row
// +2: col_start          Inclusive start column (grid units)
// +3: col_end_excl       Exclusive end column
// +4: hotspot_type       Enum:
//                         0 = ROOM_SCENE
//                         1 = VERB
//                         2 = INVENTORY_ITEM
//                         3 = INVENTORY_SCROLL
//                         4 = SENTENCE_BAR
// +5: aux_value          Meaning depends on type:
//                         - VERB: verb_id
//                         - INVENTORY_ITEM: position index in 2×2 grid
//                           0 = top-left, 1 = top-right,
//                           2 = bottom-left, 3 = bottom-right
//                         - INVENTORY_SCROLL: scroll direction
//                           0 = up, 1 = down
//                         - ROOM_SCENE/SENTENCE_BAR: unused
//
// Notes:
// - Ranges use half-open intervals [start, end).
// - Coordinates are grid-based and must match cursor grid math.
// ------------------------------------------------------------

//Hotspots metadata
* = $F243
        .byte $01, $12		// Room scene
        .byte $00, $28
        .byte $00, $00        

        .byte $12, $13		// Sentence bar
        .byte $00, $28
        .byte $04, $00        

        .byte $13, $14
        .byte $00, $08
        .byte $01, $09        // "Push" verb

        .byte $13, $14
        .byte $08, $0F
        .byte $01, $01        // "Open" verb

        .byte $13, $14
        .byte $0F, $18
        .byte $01, $0D        // "Walk to" verb

        .byte $13, $14
        .byte $18, $20
        .byte $01, $07        // "New kid" verb

        .byte $13, $14
        .byte $20, $28
        .byte $01, $04        // "Turn on" verb

        .byte $14, $15
        .byte $00, $08
        .byte $01, $0A        // "Pull" verb

        .byte $14, $15
        .byte $08, $0F
        .byte $01, $02        // "Close" verb

        .byte $14, $15
        .byte $0F, $18
        .byte $01, $0E        // "Pick up" verb

        .byte $14, $15
        .byte $18, $20
        .byte $01, $08        // "Unlock" verb

        .byte $14, $15
        .byte $20, $28
        .byte $01, $05        // "Turn off" verb

        .byte $15, $16
        .byte $00, $08
        .byte $01, $03        // "Give" verb

        .byte $15, $16
        .byte $08, $0F
        .byte $01, $0C        // "Read" verb

        .byte $15, $16
        .byte $0F, $18
        .byte $01, $0F        // "What is" verb

        .byte $15, $16
        .byte $18, $20
        .byte $01, $0B        // "Use" verb

        .byte $15, $16
        .byte $20, $28
        .byte $01, $06        // "Fix" verb

        .byte $16, $17
        .byte $00, $12
        .byte $02, $00        // Top-left inventory item

        .byte $16, $17
        .byte $12, $16
        .byte $03, $00        // Inventory scroll up arrow

        .byte $16, $17
        .byte $16, $28
        .byte $02, $01        // Top-right inventory item

        .byte $17, $18
        .byte $00, $12
        .byte $02, $02        // Bottom-left inventory item

        .byte $17, $18
        .byte $12, $16
        .byte $03, $01        // Inventory scroll down arrow

        .byte $17, $18
        .byte $16, $28
        .byte $02, $03        // Bottom-right inventory item

* = $F2CD
hotspot_normal_colors:
        .byte $00, $05, $04, $06, $04

* = $F2D2
hotspot_apply_highlight_color_and_fill:
        .byte $00, $07, $07, $07, $07

* = $F2DC
// ------------------------------------------------------------
// Inventory scroll arrow tile rows (per-row character IDs)
//
// Layout: [bg_char, left_half, right_half, end_marker]
// - bg_char    : background fill character (typically $20 = space)
// - left_half  : tile id for left half of arrow
// - right_half : tile id for right half of arrow
// - end_marker : $00 terminator for row definition
//
// Row indices:
//   0 → row of the arrow up graphic
//   1 → row of the arrow down graphic
// ------------------------------------------------------------
inv_arrow_row_up:
        .byte   $20, $01, $02, $00   // top row: [space, up-left, up-right, end]
inv_arrow_row_down:
        .byte   $20, $03, $04, $00   // bottom row: [space, down-left, down-right, end]

* = $F2E4
// ------------------------------------------------------------
// Pointers to inventory arrow rows
//
// These tables map row index → row data address.
// Use the same row index for both tables:
//   0 → inv_arrow_row_up
//   1 → inv_arrow_row_down
// ------------------------------------------------------------
inv_arrow_ptr_lo:
        .byte   <inv_arrow_row_up, <inv_arrow_row_down
inv_arrow_ptr_hi:
        .byte   >inv_arrow_row_up, >inv_arrow_row_down

* = $F2E8
hotspot_highlight_handlers:
        // ------------------------------------------------------------
        // Word table of hotspot HIGHLIGHT handlers (lo, hi per entry)
        // Indexed by hotspot_type:
        //   [0]=ROOM_SCENE, [1]=VERB, [2]=INVENTORY_ITEM,
        //   [3]=INVENTORY_SCROLL, [4]=SENTENCE_BAR
        // ------------------------------------------------------------
        .byte   $E8, $F3    // [0] ROOM_SCENE          → do_nothing           ($F3E8)
        .byte   $E9, $F3    // [1] VERB                → apply_highlight_color_and_fill     ($F3E9)
        .byte   $E9, $F3    // [2] INVENTORY_ITEM      → apply_highlight_color_and_fill     ($F3E9)
        .byte   $E9, $F3    // [3] INVENTORY_SCROLL    → apply_highlight_color_and_fill     ($F3E9)
        .byte   $E9, $F3    // [4] SENTENCE_BAR        → apply_highlight_color_and_fill     ($F3E9)

* = $F2F2
hotspot_click_handlers:
        // ------------------------------------------------------------
        // Word table of hotspot CLICK handlers (lo, hi per entry)
        // Indexed by hotspot_type:
        //   [0]=ROOM_SCENE, [1]=VERB, [2]=INVENTORY_ITEM,
        //   [3]=INVENTORY_SCROLL, [4]=SENTENCE_BAR
        // ------------------------------------------------------------
        .byte   $27, $F4    // [0] ROOM_SCENE          → click_room_scene_mark       ($F427)
        .byte   $2D, $F4    // [1] VERB                → click_set_verb_and_refresh     ($F42D)
        .byte   $52, $F4    // [2] INVENTORY_ITEM      → click_inventory_item_set_do_or_io   ($F452)
        .byte   $8F, $F4    // [3] INVENTORY_SCROLL    → click_inventory_scroll_page ($F48F)
        .byte   $4D, $F4    // [4] SENTENCE_BAR        → click_sentence_bar_rebuild     ($F44D)

* = $F2FC
hotspot_dehighlight_handlers:
        // ------------------------------------------------------------
        // Word table of hotspot DEHIGHLIGHT handlers (lo, hi per entry)
        // Indexed by hotspot_type:
        //   [0]=ROOM_SCENE, [1]=VERB, [2]=INVENTORY_ITEM,
        //   [3]=INVENTORY_SCROLL, [4]=SENTENCE_BAR
        // ------------------------------------------------------------
        .byte   $E8, $F3    // [0] ROOM_SCENE          → do_nothing              ($F3E8)
        .byte   $F8, $F3    // [1] VERB                → apply_normal_color_and_fill      ($F3F8)
        .byte   $F8, $F3    // [2] INVENTORY_ITEM      → apply_normal_color_and_fill      ($F3F8)
        .byte   $F8, $F3    // [3] INVENTORY_SCROLL    → apply_normal_color_and_fill      ($F3F8)
        .byte   $F8, $F3    // [4] SENTENCE_BAR        → apply_normal_color_and_fill      ($F3F8)

* = $F306
hotspot_render_handlers:
        // ------------------------------------------------------------
        // Word table of hotspot ENTRY/RENDER handlers (lo, hi per entry)
        // Indexed by hotspot_type:
        //   [0]=ROOM_SCENE, [1]=VERB, [2]=INVENTORY_ITEM,
        //   [3]=INVENTORY_SCROLL, [4]=SENTENCE_BAR
        // ------------------------------------------------------------
        .byte   $E8, $F3    // [0] ROOM_SCENE          → do_nothing                        ($F3E8)
        .byte   $07, $F4    // [1] VERB                → enter_verb_hotspot_render             ($F407)
        .byte   $0D, $F4    // [2] INVENTORY_ITEM      → enter_item_hotspot_render             ($F40D)
        .byte   $13, $F4    // [3] INVENTORY_SCROLL    → enter_scroll_hotspot_render     ($F413)
        .byte   $19, $F4    // [4] SENTENCE_BAR        → enter_sentence_bar_render     ($F419)

* = $F609
empty_string:
	.byte $00        // empty string
