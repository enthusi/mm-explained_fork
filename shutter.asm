/*
================================================================================
  shutter.asm — Screen shutter transition system
================================================================================
Credits: @enthusi for figuring out the logic of this module.

Summary
        Implements the horizontal and vertical "shutter" effect used for room
        transitions. The system supports both closing and opening sequences,
        alternates between double-buffered frame buffers, and synchronizes
        with raster IRQ timing for smooth animation.

Modules
        - open_shutter: triggers the opening transition and unhides the cursor.
        - configure_shutter_buffers_and_colors: sets up source/destination frame
          buffers and optional color RAM initialization.
        - do_shutter_effect: animates the shutter’s perimeter over multiple passes.
        - plot_one_tile_of_shutter_effect: plots a single tile cell per edge step.
        - copy_vic_color_ram_wrapper: safely updates $D800 color RAM.
        - ensure_raster_irq_ready: guarantees raster IRQ environment readiness.

Globals
        shutter_direction_flag   0=closing, ≠0=opening
        frame_buffer             active frame buffer selector
        dst_buffer_ptr, src_buffer_ptr
        color_ram_ptr, color_ram_page_ptr
        rect_left_x..rect_bottom_y, rect_left_dx..rect_bottom_dy
        shutter_iter_count       iteration counter for perimeter passes

Description
        - Provides visual transitions between scenes using tile-by-tile rectangle
          expansion or contraction (shutter motion).
        - Uses two precomputed delta-table entries that define edge bounds,
          signed per-edge increments, and iteration counts.
        - Opening sequence reveals previous image and optionally restores color RAM.
        - Closing sequence masks current view and prepares the next frame buffer.

================================================================================
*/
#importonce
#import "constants.inc"
#import "irq_handlers.asm"
#import "misc.asm"

.label shutter_direction_flag = $B7      // 0=closing, ≠0=opening

.label src_buffer_ptr         = $9E      // Source frame buffer base (lo/hi)
.label dst_buffer_ptr         = $A0      // Destination frame buffer base (lo/hi)

.label color_ram_ptr          = $A2      // Color RAM base pointer (lo/hi)
.label color_ram_page_ptr     = $A4      // Color RAM page pointer (lo/hi)

.label src_tile_ptr           = $A6      // Per-tile source address (lo/hi)
.label dst_tile_ptr           = $A8      // Per-tile destination address (lo/hi)

.label rect_left_x            = $AE      // Current rectangle left X
.label rect_top_y             = $AF      // Current rectangle top Y
.label rect_right_x           = $B0      // Current rectangle right X
.label rect_bottom_y          = $B1      // Current rectangle bottom Y

.label rect_left_dx           = $B2      // Delta for left edge per pass
.label rect_top_dy            = $B3      // Delta for top edge per pass
.label rect_right_dx          = $B4      // Delta for right edge per pass
.label rect_bottom_dy         = $B5      // Delta for bottom edge per pass

.label shutter_iter_count     = $B6      // Remaining perimeter passes

.const DELTA_TABLE_ENTRY_SIZE = $09     // Size in bytes of one shutter delta-table entry ({L,T,R,B,DL,DT,DR,DB,COUNT})
.const DELTA_TABLE_OPEN_OFS   = $09     // Offset (in bytes) to the second entry for the opening sequence

* = $D68B
shutter_delta_table:
        // ------------------------------------------------------------
        // Shutter delta table
        // Each 9-byte entry defines one shutter animation configuration:
        // {left_x, top_y, right_x, bottom_y, dL, dT, dR, dB, iter_count}
        //
        // Entry #0 → closing: start full frame, shrink inward each pass.
        // Entry #1 → opening: start small center area, expand outward.
        // ------------------------------------------------------------
        .byte $00, $00, $27, $10, $01, $01, $FF, $FF, $09   // closing sequence parameters
        .byte $08, $08, $1F, $08, $FF, $FF, $01, $01, $09   // opening sequence parameters

/*
================================================================================
  open_shutter
================================================================================
Summary
	Run the shutter effect in opening mode using the preselected delta-table
	entry, then unhide the cursor.

Global Outputs
	hide_cursor_flag   set to FALSE to show the cursor

Description
	- Load A with SHUTTER_OPEN to request the opening path.
	- Load X with DELTA_TABLE_OPEN_OFS to pick the table entry.
	- Call do_shutter_effect to animate the opening shutter.
	- Clear hide_cursor_flag so the cursor becomes visible.
================================================================================
*/
* = $D6C6
open_shutter:
        // Opening: fill from the other buffer and run the shutter effect
        lda     #SHUTTER_OPEN        
        ldx     #DELTA_TABLE_OPEN_OFS
        jsr     do_shutter_effect

        // Show cursor
        lda     #FALSE
        sta     hide_cursor_flag
        rts

/*
================================================================================
  configure_shutter_buffers_and_colors
================================================================================

Summary
	Configure frame-buffer pointers for the shutter effect. For closing,
	select only the destination buffer. For opening, select destination
	and source buffers, then initialize VIC color RAM via a wrapper. Seed
	RAM pointers used later by the drawing code.

Vars/State
	No local persistent state. Clobbers A and status flags. X is preserved
	across the color-RAM init path; Y is not preserved.

Global Inputs
	shutter_direction_flag   0=closing, ≠0=opening
	frame_buffer             active rendered buffer selector

Global Outputs
	dst_buffer_ptr           set to target frame buffer base (lo/hi)
	src_buffer_ptr           set on opening path to source buffer base (lo/hi)
	color_ram_ptr            set to COLOR_LAYER_COL_0 (lo/hi)
	color_ram_page_ptr       set to TILE_DEFS_ADDR (lo/hi)
	VIC color RAM ($D800)    initialized by copy_vic_color_ram_wrapper on opening

Description
	- Test direction flag to choose closing vs opening path.
	- Closing: write destination buffer pointer only based on frame_buffer.
	- Opening: write destination and source buffer pointers in opposite order
	of frame_buffer to reveal previous image; then call the color-RAM
	wrapper with X preserved.
	- In both paths, seed two RAM pointers used by later tile/color code.
================================================================================
*/
* = $D6D3		
configure_shutter_buffers_and_colors:
		// ------------------------------------------------------------
		// Select shutter path by direction flag.
		//
		// 0 → closing: later code sets only destination buffer.
		// ≠0 → opening: later code sets dst+src and initializes color RAM.
		// Branch to prepare_open_path for opening; fall through for closing.
		// ------------------------------------------------------------
        lda     shutter_direction_flag
        bne     prepare_open_path 

		// ------------------------------------------------------------
		// Closing path
		//
		// Choose destination buffer for closing path.
		// frame_buffer == #$01 → dst := VIEW_FRAME_BUF_0 ($C828).
		// frame_buffer ≠  #$01 → branch to select_dst_buf1 (dst := VIEW_FRAME_BUF_1).
		// ------------------------------------------------------------
        lda     frame_buffer
        cmp     #$01
        bne     select_dst_buf1                   // frame_buffer ≠ 1 → use buffer1
		
        lda     #<VIEW_FRAME_BUF_0                            
        sta     dst_buffer_ptr
        lda     #>VIEW_FRAME_BUF_0
        sta     dst_buffer_ptr + 1
        jmp     skip_color_ram_init

select_dst_buf1:
        lda     #<VIEW_FRAME_BUF_1                            
        sta     dst_buffer_ptr
        lda     #>VIEW_FRAME_BUF_1
        sta     dst_buffer_ptr + 1
        jmp     skip_color_ram_init

		// ------------------------------------------------------------
		// Opening path: pick dst and src based on active frame buffer.
		//
		// frame_buffer == #$01 → dst := VIEW_FRAME_BUF_1 ($CC28), src := VIEW_FRAME_BUF_0 ($C828).
		// frame_buffer ≠  #$01 → branch to select_open_dst_buf2_src_buf1 (dst := $C828, src := $CC28).
		// ------------------------------------------------------------
prepare_open_path:
        lda     frame_buffer
        cmp     #$01
        bne     select_open_dst_buf2_src_buf1   

        lda     #<VIEW_FRAME_BUF_1              
        sta     dst_buffer_ptr
        lda     #>VIEW_FRAME_BUF_1
        sta     dst_buffer_ptr + 1
        lda     #<VIEW_FRAME_BUF_0              
        sta     src_buffer_ptr
        lda     #>VIEW_FRAME_BUF_0
        sta     src_buffer_ptr + 1
        jmp     init_color_ram_for_open

select_open_dst_buf2_src_buf1:
        lda     #<VIEW_FRAME_BUF_0              
        sta     dst_buffer_ptr
        lda     #>VIEW_FRAME_BUF_0
        sta     dst_buffer_ptr + 1
        lda     #<VIEW_FRAME_BUF_1              
        sta     src_buffer_ptr
        lda     #>VIEW_FRAME_BUF_1
        sta     src_buffer_ptr + 1

		// ------------------------------------------------------------
		// Initialize VIC color RAM for opening path.
		//
		// Preserve X across the wrapper call (TXA/PHA … PLA/TAX) since the
		// wrapper may clobber it. A and Y are not preserved here; caller
		// does not rely on them after return. The wrapper populates $D800
		// from prepared source data to reveal the scene cleanly.
		// ------------------------------------------------------------
init_color_ram_for_open:
        txa                                     
        pha
        jsr     copy_vic_color_ram_wrapper      
        pla
        tax

		// ------------------------------------------------------------
		// Seed RAM pointers used by shutter drawing.
		//
		// color_ram_ptr := COLOR_LAYER_COL_0 (source color-layer data in RAM).
		// color_ram_page_ptr := TILE_DEFS_ADDR (tile/bitmap base in RAM).
		//
		// Note: this does not point to VIC $D800. Both paths share this setup.
		// ------------------------------------------------------------
skip_color_ram_init:
        lda     #<COLOR_LAYER_COL_0             
        sta     color_ram_ptr                   
        lda     #>COLOR_LAYER_COL_0
        sta     color_ram_ptr + 1               
        lda     #<TILE_DEFS_ADDR                
        sta     color_ram_page_ptr              
        lda     #>TILE_DEFS_ADDR
        sta     color_ram_page_ptr + 1          
        rts
/*
================================================================================
  do_shutter_effect
================================================================================

Summary
	Animate a rectangular “shutter” by drawing the perimeter of a shrinking
	or expanding rectangle in tile units. Initializes buffers and, for the
	opening case, color RAM. For each pass, sweeps the four edges in order
	and then applies signed per-edge deltas until the iteration count ends.

Arguments
	A       Direction flag (#$00 = closing, ≠#$00 = opening)
	X       Index into shutter_delta_table for the parameter entry to use

Vars/State
	rect_left_x .. rect_bottom_y    Rectangle bounds in tiles (updated each pass)
	rect_left_dx..rect_bottom_dy    Signed per-edge deltas applied each pass
	shutter_iter_count              Remaining perimeter passes (decremented to zero)

Global Inputs
	shutter_delta_table    Table of 9-byte entries {L,T,R,B,DL,DT,DR,DB,COUNT}
	frame_buffer           Active frame buffer selector (used via setup routine)

Global Outputs
	dst_buffer_ptr         Destination frame buffer base (set by setup routine)
	src_buffer_ptr         Source frame buffer base for opening (set by setup)
	color_ram_ptr          Seeded to color-layer source data (set by setup)
	color_ram_page_ptr     Seeded to tile/bitmap base (set by setup)
	VIC color RAM ($D800)  Initialized on opening via wrapper called by setup
	Screen pixels          Tiles plotted by plot_one_tile_of_shutter_effect

Description
	- Save direction flag and call configure_shutter_buffers_and_colors to set
	frame-buffer pointers and, for opening, initialize VIC color RAM.
	- Load 9-byte rectangle/delta/iteration parameters from shutter_delta_table
	entry X into the rect_* and shutter_iter_count variables.
	- Each iteration:
		• Call ensure_raster_irq_ready for timing.
		• Sweep edges in order: top (L→R), right (T→B), bottom (R→L), left (B→T),
		plotting one tile at each step.
		• Apply signed per-edge deltas to {left, top, right, bottom}.
		• Decrement iteration count and repeat until zero.
================================================================================
*/
* = $D734
do_shutter_effect:
        // Initialize effect mode and buffers
        // Save open/close flag, select dst/src frame buffers, and prep color RAM when opening.
        sta     shutter_direction_flag
        jsr     configure_shutter_buffers_and_colors

		// ------------------------------------------------------------
        // Load rectangle parameters from delta table
        // Reads 9 bytes into (rect_left_x..rect_iter_count):
        // {L, T, R, B, DL, DT, DR, DB, COUNT}.
        // These define the shutter bounds and per-edge deltas for animation.
		// ------------------------------------------------------------
        ldy     #$00
load_rect_params:
        lda     shutter_delta_table,x           // read delta table byte
        sta.abs rect_left_x,y                   // store sequentially into rect param block
        inx                                     // advance table index
        iny                                     // advance destination index
        cpy     #DELTA_TABLE_ENTRY_SIZE         // finished the entry?
        bne     load_rect_params                // if not, continue

		// ------------------------------------------------------------
		// Main iteration step
		// Refresh raster timing, then seed start tile coords (X:=left, Y:=top)
		// ------------------------------------------------------------
effect_main_loop:
        jsr     ensure_raster_irq_ready

        ldx     rect_left_x
        ldy     rect_top_y
		
		// ------------------------------------------------------------
		// Draw first tile at (left,top), then sweep top edge left→right
		// Advance X until it reaches rect_right_x
		// ------------------------------------------------------------
        jsr     plot_one_tile_of_shutter_effect
sweep_top_edge_lr:                              
        cpx     rect_right_x
        beq     sweep_right_edge_tb
        jsr     plot_one_tile_of_shutter_effect
        inx
        jmp     sweep_top_edge_lr

		// ------------------------------------------------------------
		// Sweep right edge top→bottom
		// Advance Y until it reaches rect_bottom_y
		// ------------------------------------------------------------
sweep_right_edge_tb:                            
        cpy     rect_bottom_y
        beq     sweep_bottom_edge_rl
        jsr     plot_one_tile_of_shutter_effect
        iny
        jmp     sweep_right_edge_tb

        // ------------------------------------------------------------
        // Sweep bottom edge right→left
        // Retreat X until it reaches rect_left_x
        // ------------------------------------------------------------
sweep_bottom_edge_rl:
        cpx     rect_left_x
        beq     sweep_left_edge_bt
        jsr     plot_one_tile_of_shutter_effect
        dex
        jmp     sweep_bottom_edge_rl

		// ------------------------------------------------------------
		// Sweep left edge bottom→top
		// Retreat Y until it reaches rect_top_y
		// ------------------------------------------------------------
sweep_left_edge_bt:                             
        cpy     rect_top_y
        beq     update_edges_and_iter
        jsr     plot_one_tile_of_shutter_effect
        dey
        jmp     sweep_left_edge_bt

		// ------------------------------------------------------------
		// Update rectangle edges and advance iteration
		// Apply signed deltas to {left, top, right, bottom} in order X=3..0.
		// ------------------------------------------------------------
update_edges_and_iter:
        ldx     #$03                            // apply deltas to L,T,R,B
apply_edge_deltas_loop:
        clc
        lda     rect_left_x,x
        adc     rect_left_dx,x
        sta     rect_left_x,x
        dex
        bpl     apply_edge_deltas_loop

        // ------------------------------------------------------------
        // Decrement remaining passes; loop if not finished
        // ------------------------------------------------------------
        dec     shutter_iter_count
        bne     effect_main_loop
        rts

/*
================================================================================
  plot_one_tile_of_shutter_effect
================================================================================
Summary
	Compute per-row source and destination pointers for the given tile
	coordinates, then either write black (closing) or copy a byte from
	the source buffer (opening).

Arguments
	X  → tile column index
	Y  → tile row index

Returns
	X, Y restored to their entry values.
	A clobbered. Processor flags modified.

Global Inputs
	shutter_direction_flag   0 = closing; ≠0 = opening
	src_buffer_ptr           base pointer to source framebuffer (lo/hi)
	dst_buffer_ptr           base pointer to destination framebuffer (lo/hi)
	screen_row_offsets_lo    table of row offsets low bytes (Y*40)
	screen_row_offsets_hi    table of row offsets high bytes (Y*40)

Global Outputs
	src_tile_ptr             per-row source pointer (lo/hi) updated
	dst_tile_ptr             per-row destination pointer (lo/hi) updated
	(dst buffer byte)        written with black or copied from source

Description
	- Save Y and X on stack for restoration.
	- Build row-local pointers by adding Y*40 to base src/dst column bases.
	- If shutter_direction_flag = 0, write #$00 to destination (black).
	- Else copy one byte from source to destination.
	- Restore registers and return.
================================================================================
*/
* = $D78E
plot_one_tile_of_shutter_effect:
		// Save X and Y on the stack
        tya                                     
        pha
        txa                                     
        pha

		// ------------------------------------------------------------
		// Build per-row source pointer for (X,Y)
		// Use base src_buffer_ptr (column base) plus Y*40 row offset.
		// Writes lo/hi into src_tile_ptr for the current row; X steps 2→0.
		// ------------------------------------------------------------
        ldx     #$02                            // X := 2 → build hi/lo pairs in order: (2,1) then (0,-1)
build_row_ptrs:
        lda     src_buffer_ptr,x                // A := src base pointer component (lo/hi selected by X)
        clc                                     // clear carry for 8-bit add
        adc     screen_row_offsets_lo,y         // add low byte of (Y*40) row offset
        sta     src_tile_ptr,x                  // store result into src_tile_ptr component (lo/hi)


		// ------------------------------------------------------------
		// Complete per-row source pointer high byte
		// Add row high offset (with carry) to src base high, then store to src_tile_ptr hi.
		// ------------------------------------------------------------
        lda     src_buffer_ptr + 1,x            // A := src base pointer high byte selected by X
        adc     screen_row_offsets_hi,y         // add high byte of row offset plus carry from low-byte add
        sta     src_tile_ptr + 1,x              // write high byte into src_tile_ptr component


		// ------------------------------------------------------------
		// Advance pointer build loop
		// Step X through hi/lo components for two pointer pairs (2→0).
		// Continue until all components written for this row.
		// ------------------------------------------------------------
        dex                                     // X := X-1 → advance to next component (switch hi↔lo)
        dex                                     // X := X-1 → move to previous pointer pair (2→0)
        bpl     build_row_ptrs                  // loop while X ≥ 0 to finish both lo/hi pairs

        // restore X
		pla                                     
        tax
        tay

		// ------------------------------------------------------------
		// Branch on shutter mode
		// If shutter_direction_flag ≠ 0, take open path to copy from source.
		// If 0, fall through to closing path and fill destination with black.
		// ------------------------------------------------------------
        lda     shutter_direction_flag          
        bne     open_copy_from_src              

		// ------------------------------------------------------------
		// Closing path: write black to dest and skip open-copy
		// ------------------------------------------------------------
        sta     (dst_tile_ptr),y                // Closing path: A==0 → write black byte to dest cell
        jmp     exit_plot_tile                  // Skip copy path and exit

		// ------------------------------------------------------------
		// Open path: copy one tile byte from source to destination
		// Uses computed row pointers; preserves mode semantics (reveal).
		// ------------------------------------------------------------
open_copy_from_src:
        lda     (src_tile_ptr),y                // Opening: read byte from source tile at (X,Y)
        sta     (dst_tile_ptr),y                // Write the byte to destination tile at (X,Y)

exit_plot_tile:
		//restore Y
        pla                                     
        tay
        rts

