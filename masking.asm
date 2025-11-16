#importonce
#import "globals.inc"
#import "constants.inc"
#import "render_actor.asm"

.label sprite_row_ptr              = $80    // ZP: pointer to current 3-byte sprite scanline (row buffer)
.label sprite_base_cached          = $CB68  // Cached low byte of actor’s sprite base address
.label y_coordinate                = $FC28  // Current sprite scanline Y position (bottom of actor, pixels)
.label y_top_limit                 = $FC29  // Topmost Y (exclusive) for masking loop
.label y_top_unclamped             = $FC3B  // Original unclamped top Y (saved if clamped to 0)
.label tile_row_idx                = $FD23  // Y & 7 → intra-tile row index (0–7)
.label mask_col_idx                = $FD24  // Current tile column index for foreground mask lookup
.label sprite_byte_idx             = $CB67  // Sprite byte selector within row (0–2 for 24-pixel span)

.label mask_ptr1                   = $43  // ZP: 16-bit pointer #1 → mask pattern row for column 1
.label mask_ptr2                   = $45  // ZP: 16-bit pointer #2 → mask pattern row for column 2
.label mask_ptr3                   = $47  // ZP: 16-bit pointer #3 → mask pattern row for column 3

.label pixel_mask                  = $CB6A  // Scratch byte: assembled 8-bit mask for current sprite byte

.const MASK_BYTES_PER_TILE         = $08    // 8 bytes per tile mask pattern (one per scanline)
.const MASK_BLOCK_HDR_SIZE         = $04    // Size of mask pattern table header before pattern data
.const MASK_EVEN_BITS              = %01010101  // Bitmask selecting even bits before duplication for multicolor

* = $D640
// Low-byte offsets into mask layer per tile row (Y>>3), relative to mask_base_ptr
mask_row_ofs_lo:
.byte $00,$28,$50,$78,$A0,$C8,$F0,$18
.byte $40,$68,$90,$B8,$E0,$08,$30,$58
.byte $80,$A8,$D0,$F8,$20,$48,$70,$98,$C0

* = $D659
// High-byte offsets into mask layer per tile row (Y>>3), relative to mask_base_ptr
mask_row_ofs_hi:
.byte $00,$00,$00,$00,$00,$00,$00,$01
.byte $01,$01,$01,$01,$01,$02,$02,$02
.byte $02,$02,$02,$02,$03,$03,$03,$03,$03


/*
==================================================================
  mask_actor_with_foreground_layer
==================================================================

Summary
	Mask one actor’s sprite against the room’s foreground layer, processing
	scanlines from the actor’s current Y down to a computed top bound. Uses
	per-tile row masks and builds three pattern pointers per scanline to
	AND-mask the 24-pixel sprite row (3 bytes). Supports multicolor sprites
	by duplicating logical bits to fat-pixel pairs in apply_mask.

Arguments
	actor_tile_x_coordinate		Starting tile column for this actor on the current mask row.

Returns
	None

Global Inputs
	actor                           current actor index
	character_sprite_y[]            per-actor sprite Y (pixels, bottom)
	actor_sprite_index[]            per-actor sprite ID
	actor_sprite_y_extent[]         max upward extent (pixels)
	mask_base_ptr                   base pointer to mask layer
	mask_row_ofs_lo/hi[]            per-row offsets (y>>3) into mask layer
	actor_sprite_base               resolved sprite base (lo/hi)

Global Outputs
	sprite bytes at sprite_row_ptr  AND-masked in place per scanline
	y_top_unclamped                 set only when clamping top→0
	y_coordinate, y_top_limit       working scanline bounds
	mask_row_ptr, mask_col_idx      current mask row pointer and column
	sprite_base_cached              cached sprite base (lo/hi)
	sprite_row_ptr                  pointer to current sprite row
	tile_row_idx, sprite_byte_idx   per-scanline indices (0..7 and 0..2)

Description
	- Compute top bound: top = y − extent − 1, then handle visibility:
		• If fully below screen (both y and top > SCREEN_Y_MAX), return.
		• If y ≤ SCREEN_Y_MAX but top > SCREEN_Y_MAX, save y_top_unclamped and set top=0.
		• Clamp y to ≤ SCREEN_Y_MAX when needed.
	- Resolve and cache sprite base for this actor.
	- Select mask row: row_index = y >> 3, then mask_row_ptr = mask_base_ptr + offset[row].
	- Initialize mask_col_idx from actor_tile_x_coordinate and call
	build_mask_pattern_ptrs to compute mask_ptr1..3.
	- For each scanline from y down to top:
		• Compute sprite_row_ptr = sprite_base_cached + sprite_row_offsets[y].
		• If (y & 7) == 0, step mask_row_ptr back by VIEW_COLS and rebuild mask_ptr1..3.
		• Decrement y, set tile_row_idx = y & 7.
		• sprite_byte_idx := 0, then call apply_mask (updates all 3 bytes).
	- Exit when y == top.

Notes
	- Multicolor (“fat”) sprites are handled in apply_mask by duplicating
	even→odd bits so each logical pixel maps to a 2-bit pair.
==================================================================
*/

* = $30DB
mask_actor_with_foreground_layer:
		// ----------------------------------------------
        // Load the actor’s vertical sprite position into the working Y register
        //   X := actor index
        //   A := character_sprite_y[X]  → current sprite Y coordinate in pixels
        //   y_coordinate := A            → store as working vertical position (bottom)
		// ----------------------------------------------
        ldx     actor
        lda     character_sprite_y,x
        sta     y_coordinate


		// ----------------------------------------------
        // Compute upper bound of sprite coverage
        //   top = current Y − sprite vertical extent − 1
        //   Subtracts the actor’s maximum upward offset to get top row
		// ----------------------------------------------
        lda     y_coordinate
        sec                                 // prepare for subtraction
        sbc     actor_sprite_y_extent,x     // subtract actor’s sprite height offset
        sta     y_top_limit                 // store preliminary top coordinate
        dec     y_top_limit                 // adjust one pixel above extent

		// ----------------------------------------------
        // Early-exit test: skip masking if actor fully below visible screen
        //   Compare top and bottom coordinates against SCREEN_Y_MAX ($91)
        //   If both are greater than the screen limit, actor is offscreen → return
		// ----------------------------------------------
        lda     #SCREEN_Y_MAX
        cmp     y_top_limit                  // check if screen limit >= top
        bcs     clamp_y_to_screen_max        // if yes, actor may be partially visible
        cmp     y_coordinate                 // else check bottom edge (current Y)
        bcs     force_top_to_0_when_y_visible // if screen limit >= Y, actor still visible
        rts                                  // both beyond limit → no visible overlap


force_top_to_0_when_y_visible:
		// ----------------------------------------------
        // Visible-bottom, offscreen-top case:
        //   Save unclamped top, then clamp top to 0 so processing starts at row 0.
		// ----------------------------------------------
        lda     y_top_limit
        sta     y_top_unclamped              // preserve pre-clamp top
        lda     #$00
        sta     y_top_limit                  // clamp top to screen origin

clamp_y_to_screen_max:
		// ----------------------------------------------
        // Clamp bottom edge to the visible limit:
        //   If y > SCREEN_Y_MAX, set y := SCREEN_Y_MAX; else leave as-is.
		// ----------------------------------------------
        lda     #SCREEN_Y_MAX
        cmp     y_coordinate                  // is limit ≥ y?
        bcs     y_clamp_done                  // yes → already within limit
        lda     #SCREEN_Y_MAX
        sta     y_coordinate                   // no  → clamp to limit
		
y_clamp_done:
		// ----------------------------------------------
        // Resolve this actor’s sprite graphics base and cache it (lo/hi)
		// ----------------------------------------------
        ldx     actor                        // X := actor index
        ldy     actor_sprite_index,x         // Y := sprite ID for this actor
        jsr     set_actor_sprite_base        // sets actor_sprite_base (lo/hi)
        lda     actor_sprite_base
        sta     sprite_base_cached           // cache base low byte
        lda     actor_sprite_base + 1
        sta     sprite_base_cached + 1       // cache base high byte

		// ----------------------------------------------
        // Select mask row for this scanline: row_index = y >> 3
		// ----------------------------------------------
        lda     y_coordinate
        lsr                                 // y >> 1
        lsr                                 // y >> 2
        lsr                                 // y >> 3 (8-pixel tile rows)
        tax                                 // X := row_index
		
		// ----------------------------------------------
        // Compute absolute pointer to current mask-layer row: mask_row_ptr = mask_base_ptr + mask_row_ofs[X]
		// ----------------------------------------------
        clc
        lda     mask_row_ofs_lo,x            // A := row offset (lo)
        adc     mask_base_ptr               // add base lo
        sta     mask_row_ptr                // store row ptr lo
        lda     mask_row_ofs_hi,x            // A := row offset (hi)
        adc     mask_base_ptr + 1               // add base hi + carry
        sta     mask_row_ptr + 1                // store row ptr hi

		// ----------------------------------------------
        // Initialize tile-column cursor and build three pattern pointers for this row
		// ----------------------------------------------
        lda     actor_tile_x_coordinate      // A := starting tile column
        sta     mask_col_idx                 // seed column index used by the builder
        jsr     build_mask_pattern_ptrs      // compute mask_ptr1..3 for columns col..col+2

compute_row_ptrs:
		// ----------------------------------------------
        // Termination check for scanline loop: stop when current Y reaches top
		// ----------------------------------------------
        ldx     y_coordinate                 // X := current scanline (pixels)
        cpx     y_top_limit                  // compare against upper bound
        beq     exit_mask_actor                        // equal → no more rows to process


		// ----------------------------------------------
        // Point to this scanline’s 3-byte sprite row:
        //   sprite_row_ptr = sprite_base_cached + sprite_row_offsets[y]
		// ----------------------------------------------
        lda     sprite_row_offsets_lo,x      // A := row offset lo for scanline y
        clc
        adc     sprite_base_cached          // add base lo
        sta     sprite_row_ptr              // store row pointer lo
        lda     sprite_row_offsets_hi,x      // A := row offset hi for scanline y
        adc     sprite_base_cached + 1          // add base hi + carry
        sta     sprite_row_ptr + 1              // store row pointer hi

		// ----------------------------------------------
        // If at a tile-row boundary (y & 7 == 0), step mask row and rebuild pointers
		// ----------------------------------------------
        txa
        and     #$07                        // test low 3 bits of y
        bne     scanline_loop_apply_mask    // not boundary → keep current mask row

		// ----------------------------------------------
        // Move to previous mask row: subtract one screen row (VIEW_COLS bytes = 40)
		// ----------------------------------------------
        lda     mask_row_ptr               // A := row_ptr_lo
        sec                                  // prepare borrow for 16-bit subtract
        sbc     #VIEW_COLS                   // lo := lo - 40
        sta     mask_row_ptr
        bcs     mask_row_stepped_reindex     // no borrow → high byte unchanged
        dec     mask_row_ptr + 1                // borrow occurred → decrement high byte


mask_row_stepped_reindex:
		// ----------------------------------------------
        // Reinitialize mask-column cursor for the new row and rebuild pattern pointers
		// ----------------------------------------------
        ldx     actor_tile_x_coordinate      // X := starting tile column for this actor
        stx     mask_col_idx                 // seed column index on the new mask row
        jsr     build_mask_pattern_ptrs      // recompute mask_ptr1..3 for columns col..col+2

scanline_loop_apply_mask:
		// ----------------------------------------------
        // Loop guard: if current scanline is above the top bound, stop
		// ----------------------------------------------
        ldx     y_coordinate                 // X := current scanline (pixels)
        cpx     y_top_limit                  // compare against upper limit
        bcc     exit_mask_actor                        // y < top → no more rows to process


		// ----------------------------------------------
        // Advance to previous scanline and derive row index within the 8-pixel tile
		// ----------------------------------------------
        dex                                 // y := y - 1 (one scanline up)
        stx     y_coordinate                // store updated scanline
        txa                                 // A := y
        and     #$07                        // A := y & 7 → 0..7 within tile row
        sta     tile_row_idx                // cache tile-relative row index

		// ----------------------------------------------
        // Initialize per-scanline byte cursor, then mask the three sprite bytes
		// ----------------------------------------------
        ldy     #$00                        // sprite_byte_idx := 0
        sty     sprite_byte_idx             // select first of 3 bytes in the row
        jsr     apply_mask                  // apply foreground mask to this scanline

		// ----------------------------------------------
        // Iterate: jump back to compute pointers for the next scanline
		// ----------------------------------------------
        jmp     compute_row_ptrs

exit_mask_actor:
        rts                                 
		
/*
==============================================================================
  build_mask_pattern_ptrs
==============================================================================

Summary
	Build three 16-bit pointers to 8-byte mask pattern rows for the three
	sprite bytes that overlap the current tile columns on this mask row.

Arguments
	mask_col_idx		Tile-column cursor for the current mask row (increments as used).
	mask_row_ptr		Pointer to the current mask-layer row (indexed by Y).

Returns
	mask_ptr1			Pointer to pattern row for column 1.
	mask_ptr2			Pointer to pattern row for column 2.
	mask_ptr3			Pointer to pattern row for column 3.

Global Inputs
	mask_bit_patterns_lo/hi		Base address of the mask pattern table (lo/hi).

Global Outputs
	mask_col_idx				Advanced three times (one per column).

Description
	- Read mask index byte at mask_row_ptr[mask_col_idx], then ++mask_col_idx.
	- Compute byte offset = index * 8 + 4:
		* Use three rounds of 16-bit left shift over X:A to emulate ×8.
		* Add a 4-byte table header skip with carry into X if needed.
	- Form absolute pointer = mask_bit_patterns_base + offset.
	- Store pointer to mask_ptr1/2/3 for the three adjacent columns.

==============================================================================

Emulating 16-bit shift A <<=1 with carry into X (hi:lo)

This register dance simulates an absent "rol x" by routing the ASL carry from A
into X, yielding a 16-bit left shift over the pair X:A (X=high, A=low).

Sequence
	asl a   ; A <<= 1, C := old A7
	tay     ; Y := A (stash post-shift A)
	txa     ; A := X (bring high byte into A)
	rol a   ; A := (A<<1)|C  → X := (X<<1)|old A7
	tax     ; X := A (commit shifted high byte)
	tya     ; A := Y (restore shifted low byte)

Net effect per round
	Before: C=?, X=xxxx_xxxx, A=aaaa_aaaa
	After : C:=X7, X:=xxxx_xxxa7, A:=aaa a_aaa0
	(bit7 of pre-shift A becomes bit0 of X; A low shifts in 0)
	Result is a 16-bit value in X:A. One round = multiply by 2.

Why Y is used
	Y temporarily holds the post-ASL A while X is updated via ROL. TYA then
	restores the shifted low byte to A without extra memory traffic.

Repeat counts
	Run 1 time → ×2
	Run 3 times → ×8 (used to index 8-byte entries)

Flags
	ASL/ROL update N,Z,C as usual (C carries across into X). TAX/TXA/TAY/TYA
	update N,Z. Code relying on flags should read them immediately.

==============================================================================
*/


* = $3199
build_mask_pattern_ptrs:
		// ----------------------------------------------
        // Column 1: fetch mask index from the current mask-layer row
        //   Y = current tile-column index
        //   A = byte value (mask index) from that column on this row
		// ----------------------------------------------
        ldy     mask_col_idx
        inc     mask_col_idx                   // advance to next column for next fetch
        lda     (mask_row_ptr),y               // read mask index byte from row_ptr[Y]


		// ----------------------------------------------
		// 16-bit: (x:a) = a * MASK_BYTES_PER_TILE (×8 via three rounds)
        // Round 1 of 3: propagate A’s carry into X so (X:A) <<= 1
		// ----------------------------------------------
        ldx     #$00                    // X := 0 (high byte starts at zero)
        asl                             // A := A << 1, C := old A7
        tay                             // Y := shifted A (stash low byte)
        txa                             // A := X (bring high byte into A)
        rol                             // A := (A << 1) | C  → new high byte with carry-in
        tax                             // X := A (commit updated high byte)
        tya                             // A := Y (restore shifted low byte)
		
		// round 2 of 3: (X:A) <<= 1 with carry propagation
        asl     
		tay
		txa
		rol     
		tax
		tya
		
		// round 3 of 3: (X:A) <<= 1 with carry propagation
		asl     
		tay
		txa
		rol     
		tax
		tya

		// ----------------------------------------------
        // Add table header skip to 16-bit offset in X:A
        //   A = low byte, X = high byte
        //   offset := offset + MASK_BLOCK_HDR_SIZE
		// ----------------------------------------------
        clc                                 // clear carry for 16-bit add
        adc     #MASK_BLOCK_HDR_SIZE        // A := A + const, C=carry out
        bcc     store_ptr1                  // if no carry, high byte unchanged
        inx                                 // else carry → bump high byte (X := X+1)


store_ptr1:
		// ----------------------------------------------
        // Form pointer #1: base (mask_bit_patterns) + 16-bit offset in X:A
		// ----------------------------------------------
        clc                                 // prepare 16-bit add on A(low), X(high)
        adc     mask_bit_patterns_lo        // A := A + base_lo
        sta     mask_ptr1                   // store low byte
        txa                                 // A := X (bring high byte)
        adc     mask_bit_patterns_hi        // A := X + base_hi + carry
        sta     mask_ptr1 + 1               // store high byte


		// ----------------------------------------------
		// Repeat for column #2
		// ----------------------------------------------
		ldy     mask_col_idx
		inc     mask_col_idx
		lda     (mask_row_ptr),y

		ldx     #$00
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya

		clc
		adc     #MASK_BLOCK_HDR_SIZE
		bcc     store_ptr2
		inx


store_ptr2:
		clc
		adc     mask_bit_patterns_lo
		sta     mask_ptr2
		txa
		adc     mask_bit_patterns_hi
		sta     mask_ptr2 + 1


		// ----------------------------------------------
		// Repeat for column #3
		// ----------------------------------------------
		ldy     mask_col_idx
		inc     mask_col_idx
		lda     (mask_row_ptr),y

		ldx     #$00
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya
		asl     
		tay
		txa
		rol     
		tax
		tya

		clc
		adc     #MASK_BLOCK_HDR_SIZE
		bcc     store_ptr3
		inx


store_ptr3:
		clc
		adc     mask_bit_patterns_lo
		sta     mask_ptr3
		txa
		adc     mask_bit_patterns_hi
		sta     mask_ptr3 + 1
		rts

/*
==============================================================================
  apply_mask
==============================================================================

Summary
	Apply the foreground mask to one sprite row across its three bytes.
	Clears sprite pixels where the mask is solid so the actor renders behind
	foreground tiles.

Arguments
	tile_row_idx                0..7 row within an 8×8 tile
	mask_ptr1..mask_ptr3        pointers to per-column mask-index rows
	sprite_row_ptr              pointer to the actor’s sprite row (3 bytes)

Returns
	None

Global Inputs
	global_mask_patterns_ptr    base pointer to 8-byte patterns per index

Global Outputs
	sprite_byte_idx             advanced from 0→1→2 across the three bytes

Description
	- For each of the three adjacent tile columns:
		* Fetch mask index for this tile row, then fetch the pattern byte.
		* Duplicate even bits into odd neighbors (pair each pixel's bit).
		* AND the result into the corresponding sprite byte.

==============================================================================

Bit duplication for C64 multicolor ("fat") sprites

C64 multicolor sprites display fat pixels: each visible pixel uses 2 adjacent
bits instead of 1. The VIC-II interprets bits in pairs (b2k, b2k+1) as one
color index (00,01,10,11). This halves horizontal resolution (12 pixels wide
instead of 24) but doubles pixel width.

The mask patterns here are generated in single-bit form, one bit per logical
pixel column. To apply them to multicolor sprites, each logical bit must cover
two adjacent hardware bits—duplicating each even-position bit into its odd
neighbor.

Instruction sequence:
	sta pixel_mask        	; save raw 8-bit pattern (P)
	and #MASK_EVEN_BITS     ; keep even bits b0,b2,b4,b6 → isolate logical pixels
	asl                     ; shift those bits into odd positions b1,b3,b5,b7
	ora pixel_mask        	; combine with original → duplicate each even bit

Result:
	Each pair (b2k,b2k+1) now holds identical values.
	If a logical pixel is masked (0), both bits become 0.
	If visible (1), both bits become 1.
	This ensures the mask aligns with the doubled-width multicolor pixels.

Without duplication, only half of the visual pixel pairs would be masked,
producing artifacts. This duplication step maintains consistent coverage for
fat-pixel multicolor sprite data.

==============================================================================
*/

* = $3224
apply_mask:
		// ----------------------------------------------
        // Column 1: fetch tile-row mask index and build bitmask for this row
		// ----------------------------------------------
        ldy     tile_row_idx
        lda     (mask_ptr1),y               	// A = mask index for tile_row_idx
        tay                                   	// Y = index into pattern table
        lda     (global_mask_patterns_ptr),y    // A = 8-bit pattern byte
        sta     pixel_mask                		// save raw pattern
        and     #MASK_EVEN_BITS              	// isolate even pixel bits (0,2,4,6)
        asl                                   	// move them into odd positions (1,3,5,7)
        ora     pixel_mask                		// duplicate each even bit into its paired odd bit
		
		// ----------------------------------------------
        // Apply mask to this sprite byte and advance to the next
		// ----------------------------------------------
        ldy     sprite_byte_idx              	// Y := 0/1/2 selects sprite_row_ptr[Y]
        and     (sprite_row_ptr),y           	// A := A & sprite_row_ptr[Y]
        sta     (sprite_row_ptr),y           	// write masked byte back to sprite row
        inc     sprite_byte_idx              	// move to next sprite byte (0→1→2)


		// ----------------------------------------------
		// Column 2: repeat for sprite byte 1
		// ----------------------------------------------
		ldy     tile_row_idx
		lda     (mask_ptr2),y
		tay
		lda     (global_mask_patterns_ptr),y
		sta     pixel_mask
		and     #MASK_EVEN_BITS
		asl     
		ora     pixel_mask
		ldy     sprite_byte_idx
		and     (sprite_row_ptr),y
		sta     (sprite_row_ptr),y
		inc     sprite_byte_idx

		// ----------------------------------------------
		// Column 3: repeat for sprite byte 2
		// ----------------------------------------------
		ldy     tile_row_idx
		lda     (mask_ptr3),y
		tay
		lda     (global_mask_patterns_ptr),y
		sta     pixel_mask
		and     #MASK_EVEN_BITS
		asl     
		ora     pixel_mask
		ldy     sprite_byte_idx
		and     (sprite_row_ptr),y
		sta     (sprite_row_ptr),y
		rts


/* 
Pseudo-code for the masking routine:

mask_actor_with_foreground_layer(actor_tile_x_coordinate):
    # Load working Y (bottom of sprite in pixels)
    y = character_sprite_y[actor]                                  

    # Compute top bound = y - actor_sprite_y_extent - 1
    top = y - actor_sprite_y_extent[actor] - 1                      

    # Visibility handling vs SCREEN_Y_MAX
    if SCREEN_Y_MAX < top:
        if SCREEN_Y_MAX < y: 
            return  # fully below screen                                
        else:
            y_top_unclamped = top
            top = 0                                                     

    if y > SCREEN_Y_MAX:
        y = SCREEN_Y_MAX                                               

    # Resolve and cache this actor’s sprite base pointer
    set_actor_sprite_base(actor_sprite_index[actor])
    sprite_base_cached = actor_sprite_base                              

    # Select current mask row from y>>3 and mask_row_ofs tables
    row_index = y >> 3
    mask_row_ptr = mask_base_ptr + mask_row_ofs[row_index]              

    # Seed mask column and build three pattern pointers for this scanline
    mask_col_idx = actor_tile_x_coordinate
    build_mask_pattern_ptrs()                                           

    # Scanline loop
    while True:
        # Loop guard: stop when y < top
        if y < top:
            return                                                      

        # Advance to previous scanline and cache row-in-tile
        y = y - 1
        tile_row_idx = y & 7                                            

        # Set sprite byte cursor and apply mask to the three bytes of this row
        sprite_byte_idx = 0
        apply_mask()                                                    

        # Prepare for next scanline:
        # - Recompute sprite_row_ptr from cached base + per-row offset
        # - If crossed an 8-line boundary (tile_row_idx == 7 before decrement),
        #   step to previous mask_row and rebuild pointers
        # (implemented by the code path labeled compute_row_ptrs → build pointers)
        goto compute_row_ptrs                                           


==============================================================================

Text diagrams of bottom) and top vs. screen [0..SCREEN_TOP].
[] = sprite span before clamp. <> = span after clamp. X = culled.

------------------------------
1. Fully below screen → culled
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
                                      [ top ........ bottom ]
Result: X  (both top and bottom > SCREEN_TOP)


------------------------------
2. Bottom clipped to SCREEN_TOP
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
                   [ top ................. bottom ]
                   < top ..SCREEN_TOP>
Action: bottom := SCREEN_TOP


------------------------------
3. Fully visible (no clamp)
------------------------------


0                                    SCREEN_TOP
|------------------------------------|
            [ top ........ bottom ]
            < top ........ bottom >
Action: none


------------------------------
4. Top clipped to 0
------------------------------


	0                                    SCREEN_TOP
	|------------------------------------|
 [  top .................. bottom ]
    < 0 .................. bottom >
Action: top := 0


------------------------------
5. Fully above screen → culled
------------------------------


					0                                    SCREEN_TOP
					|------------------------------------|
[ top .... bottom ]
Result: X  (both top and y < 0)


*/