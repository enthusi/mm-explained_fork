/*================================================================================
Room graphics layers resource — build directory and per-column decode snapshots

Overview:
- Allocates and populates a “room gfx layers” resource that caches, per column,
  the decompressor’s entry state for three compressed layers: tile, color, mask.
- Produces a 24-byte directory (12×16-bit bases) pointing to four per-layer
  lists (src_lo, src_hi, mode+count, run_symbol), each list of length room_width.
- Enables O(1) resume at any column during rendering/scrolling without decoding
  prior columns.

Public routine:
- setup_room_columns_with_decode_snapshots

Private routines:
- snapshot_column_decoder_state
- assert_pointer_lists_match

Data layout (structure-of-arrays):
- column_decode_snapshot_bases: 24 bytes (12 bases × 2), lo/hi interleaved.
- For i=0..23, list i stores one byte per column, laid out consecutively.
- The byte needed for column c is at: list_base[i] + c.

Workflow:
- Free previous layers block (if any), compute size = room_width×12 + MEM_HDR_LEN,
  allocate, and write the 4-byte header.
- Lay out 12 lists back-to-back (each room_width bytes), record their 16-bit bases
  into column_decode_snapshot_bases and mirrored_bases.
- Run assert_pointer_lists_match to verify directory vs mirror before bulk writes.
- For each layer (tile, color, mask):
  • decomp_src_ptr := room_data_ptr + layer_ofs
  • bind list bases (src_lo/src_hi/mode+count/run_symbol)
  • snapshot_column_decoder_state: record per-column state and advance 17 bytes.

Invariants:
- room_width fits in 8 bits; lists spaced exactly room_width bytes apart.
- 12 lists total: 3 layers × 4 lists; directory size fixed at 24 bytes.
- directory bases are absolute.

Failure mode (guard):
- If directory and mirror diverge, assert_pointer_lists_match sets debug flag,
  maps I/O, and halts by writing the border color

Notes:
- Only one room is active; only one resource instance exists at a time.
- The precomputed values constitute the decompressor’s engine state at each
  column start (src pointer, packed mode+count, run symbol).

================================================================================

Overview

Allocates and populates the “room gfx layers” resource (type $04, index $00).
The resource is a cache that holds, per column, the exact source pointer of the
column’s compressed data and the initial decoder state needed to resume
decompression on demand. These precomputed values are the decompressor’s engine
state at the start of each column.

Layers covered (three independent compressed streams):
- Tile matrix (which character/tile to display)
- Color (how the tile pixels are colored)
- Mask

Rationale:
- The room’s layer data is stored compressed. To render quickly—especially during
  scrolling—we need O(1) random access to any column without re-decoding earlier
  columns. This resource precomputes:
  • start-of-column source pointer (lo/hi)
  • packed mode+remaining-count
  • run symbol (for run mode)
  Collectively, these constitute the decompressor’s column-entry state.
- With per-column resume points, the engine can copy previously visible columns
  and decode only the newly revealed column each frame.
- No full-room pre-render is required; only visible columns are produced, saving
  substantial memory for wide rooms.

Notes:
- Three sets of per-column metadata are generated (one per layer).
  
 ================================================================================
 
Memory layout for precomputed per-column pointer bytes

For a single column, the engine conceptually needs 24 bytes (12 pointers × 2 bytes). 
Listed in logical order for one column:
  0: tile.src_lo.ptr_lo         1: tile.src_lo.ptr_hi
  2: tile.src_hi.ptr_lo         3: tile.src_hi.ptr_hi
  4: tile.mode_cnt.ptr_lo       5: tile.mode_cnt.ptr_hi
  6: tile.run_sym.ptr_lo        7: tile.run_sym.ptr_hi
  8: color.src_lo.ptr_lo        9: color.src_lo.ptr_hi
 10: color.src_hi.ptr_lo       11: color.src_hi.ptr_hi
 12: color.mode_cnt.ptr_lo     13: color.mode_cnt.ptr_hi
 14: color.run_sym.ptr_lo      15: color.run_sym.ptr_hi
 16: mask.src_lo.ptr_lo        17: mask.src_lo.ptr_hi
 18: mask.src_hi.ptr_lo        19: mask.src_hi.ptr_hi
 20: mask.mode_cnt.ptr_lo      21: mask.mode_cnt.ptr_hi
 22: mask.run_sym.ptr_lo       23: mask.run_sym.ptr_hi

However, these 24 bytes are NOT stored contiguously per column. Instead, memory
is organized as 24 separate lists (streams), one list per logical byte above.
Each list holds one byte per column, laid out consecutively in column order.

High-level layout (structure-of-arrays across columns):

  list[ 0]  = tile.src_lo.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[ 1]  = tile.src_lo.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[ 2]  = tile.src_hi.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[ 3]  = tile.src_hi.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[ 4]  = tile.mode_cnt.lo:    B[0], B[1], ..., B[room_width-1]
  list[ 5]  = tile.mode_cnt.hi:    B[0], B[1], ..., B[room_width-1]
  list[ 6]  = tile.run_sym.lo:     B[0], B[1], ..., B[room_width-1]
  list[ 7]  = tile.run_sym.hi:     B[0], B[1], ..., B[room_width-1]

  list[ 8]  = color.src_lo.ptr_lo: B[0], B[1], ..., B[room_width-1]
  list[ 9]  = color.src_lo.ptr_hi: B[0], B[1], ..., B[room_width-1]
  list[10]  = color.src_hi.ptr_lo: B[0], B[1], ..., B[room_width-1]
  list[11]  = color.src_hi.ptr_hi: B[0], B[1], ..., B[room_width-1]
  list[12]  = color.mode_cnt.lo:   B[0], B[1], ..., B[room_width-1]
  list[13]  = color.mode_cnt.hi:   B[0], B[1], ..., B[room_width-1]
  list[14]  = color.run_sym.lo:    B[0], B[1], ..., B[room_width-1]
  list[15]  = color.run_sym.hi:    B[0], B[1], ..., B[room_width-1]

  list[16]  = mask.src_lo.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[17]  = mask.src_lo.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[18]  = mask.src_hi.ptr_lo:  B[0], B[1], ..., B[room_width-1]
  list[19]  = mask.src_hi.ptr_hi:  B[0], B[1], ..., B[room_width-1]
  list[20]  = mask.mode_cnt.lo:    B[0], B[1], ..., B[room_width-1]
  list[21]  = mask.mode_cnt.hi:    B[0], B[1], ..., B[room_width-1]
  list[22]  = mask.run_sym.lo:     B[0], B[1], ..., B[room_width-1]
  list[23]  = mask.run_sym.hi:     B[0], B[1], ..., B[room_width-1]

Where B[i] is the byte for column i for that list.

Directory (24 bytes of 12×16-bit bases):
  list_base_dir[0..23] holds the base address for each list above (two bytes per
  entry, lo then hi). The i-th list’s byte for column c is at:
      addr = list_base_dir[i] + c

Visualization (columns across, lists down):

         col0  col1  col2  ...  colN-1
  L0:    B0    B1    B2         B(N-1)
  L1:    B0    B1    B2         B(N-1)
  ...
  L23:   B0    B1    B2         B(N-1)

To reconstruct the 24 bytes for a given column c, read one byte from each list:
  for i in 0..23: out[i] = *(list_base_dir[i] + c)
================================================================================*/


#importonce
#import "globals.inc"
#import "constants.inc"
#import "registers.inc"
#import "decompressor.asm"
#import "memory_mgmt.asm"

.label mirrored_bases         = $44bb   	  // mirror of the 24-byte directory (12×16-bit list bases) for sanity checks
.label column_decode_snapshot_bases = $64     // canonical 24-byte directory of 12×16-bit list bases (lo/hi interleaved)

// setup_room_columns_with_decode_snapshots vars
// Note: several zero-page addresses are aliased under different names across steps.
// Each pair (lo/hi) is a 16-bit little-endian pointer/value; aliases indicate reuse.
.label col_src_lo_list           = $17     // 16-bit base of list #0/#8/#16 (per-column src_lo bytes)     (lo at $17, hi at $19)
.label col_src_hi_list           = $19     // 16-bit base of list #1/#9/#17 (per-column src_hi bytes)
.label col_emit_rem_list         = $1b     // 16-bit base of list #2/#10/#18 (packed mode+count per column)
.label col_run_symbol_list       = $1d     // 16-bit base of list #3/#11/#19 (run symbol per column)

.label word_lo                   = $15     // scratch low byte for general 16-bit math (aliased by width_x*, rsrc_total_bytes_local)
.label word_hi                   = $16     // scratch high byte for general 16-bit math (aliased by width_x*, rsrc_total_bytes_local)

.label alloc_base                = $4f     // 16-bit base address returned by mem_alloc for the resource block (lo/hi)

.label rsrc_total_bytes_local    = $15     // 16-bit total size = (room_width×12) + MEM_HDR_LEN (aliases word_lo/word_hi)
.label width_x12                 = $15     // 16-bit product width×12 (aliases word_lo/word_hi)
.label width_x8                  = $15     // 16-bit product width×8  (aliases word_lo/word_hi)
.label width_x4                  = $17     // 16-bit product width×4  (aliases col_src_lo_list low/high)

.label room_width_16bit          = $15     // room_width as 16-bit (lo=width, hi=0); reused temp (aliases word_lo/word_hi)

.label rsrc_write_ptr            = $17     // 16-bit write cursor inside the allocated block; advances by room_width per list

.label room_data_ptr             = $44b9   // 16-bit base pointer to the active room’s compressed data (lo/hi in RAM)

// snapshot_column_decoder_state
.label room_gfx_x_saved          = $41f9   // scratch byte to preserve X across JSR decomp_stream_next

// assert_pointer_lists_match
.const CMP_LEN                   = $18     // number of bytes to compare (24)
.const BORDER_WHITE              = $01     // border color written on hang (visual halt indicator)



/*================================================================================
  setup_room_columns_with_decode_snapshots — allocate/layout room gfx-layer buffers, bind lists,
  and snapshot per-column decoder state for TILE/COLOR/MASK layers

  Arguments:
    (none)

  Vars/State:
    width_x4                 ─ width × 4 (16-bit temp)
    width_x8                 ─ width × 8 (via doubling ×4; 16-bit temp)
    width_x12                ─ width × 12 (payload size; 16-bit temp)
    rsrc_total_bytes_local   ─ total size = width×12 + MEM_HDR_LEN (16-bit)
    alloc_base               ─ base address returned by mem_alloc (lo/hi)
    room_data_ptr            ─ base pointer to the current room resource (lo/hi)

  Global Inputs:
    room_width               ─ number of columns (bytes per list)
    current_room             ─ room index into room_ptr_{lo,hi}_tbl
    room_ptr_lo_tbl          ─ table of per-room base pointers (low)
    room_ptr_hi_tbl          ─ table of per-room base pointers (high)
    tile_matrix_ofs          ─ offset of TILE layer payload from room base (lo/hi)
    color_layer_ofs          ─ offset of COLOR layer payload from room base (lo/hi)
    mask_layer_ofs           ─ offset of MASK layer payload from room base (lo/hi)

  Global Outputs:
    room_gfx_layers_lo/hi    ─ published handle of the gfx-layers resource
    room_gfx_layers_active_ptr ─ cached handle for “still current?” checks
    rsrc_write_ptr           ─ cursor within the allocated resource block
    column_decode_snapshot_bases ─ 24-byte directory of 12×16-bit list bases
    mirrored_bases        ─ mirrored copy of the same 24 bytes for sanity checks
    decomp_src_ptr           ─ set per layer to the start of that layer’s stream
    col_src_lo_list          ─ list base for per-column source low bytes
    col_src_hi_list          ─ list base for per-column source high bytes
    col_emit_rem_list        ─ list base for packed mode+remaining-count bytes
    col_run_symbol_list      ─ list base for per-column run symbol bytes

  Returns:
    A,X,Y,flags              ─ clobbered
    side effects             ─ allocates/publishes resource; fills directory +
                               mirror; sets list bases; invokes snapshot routine
                               to write per-column initial decode state

  Description:
    - Free previously published gfx-layers block (if any).
    - Compute payload size as width×12 using shifts/adds (×4, double→×8, then sum),
      add MEM_HDR_LEN, and allocate a new resource block.
    - Initialize the standard 4-byte resource header and publish the handle.
    - Lay out 12 lists (3 layers × 4 lists), each of length room_width bytes,
      and record their 16-bit bases into the canonical directory and a mirror.
    - For TILE, COLOR, MASK:
        • decomp_src_ptr := room_data_ptr + layer_offset.
        • Bind the four list bases (src_lo/src_hi/mode+count/run_symbol).
        • Call snapshot_column_decoder_state to record per-column entry state and
          advance the decoder by 17 bytes per column.
		  
  ================================================================================
Why rsrc_write_ptr is incremented by room_width each iteration:

- Each pointer list holds 1 byte per column, so the list size is room_width * 1 bytes.
- After recording the current list’s base, the next list must begin exactly
  room_width bytes later to pack lists contiguously without gaps or overlap.
- The 16-bit add (low then high with carry) correctly handles page crossings.
- Repeating this yields 12 back-to-back regions: per layer, four lists
  (src_lo, src_hi, mode+count, run_symbol), each length room_width.
  
  ================================================================================
  
The 6502 has no multiply instruction; we use shifts and additions to compute width×12

- Express 12 as (8 + 4). Compute width×8 and width×4 separately, then add them.
- Shifts make powers of two: ASL multiplies the low byte by 2; any overflow goes
  into the carry flag. ROL on a high byte rotates that carry in, forming a 16-bit
  product. Two ASL/ROL pairs yield ×4; doubling that 16-bit value yields ×8.
- Algorithm:
  1) Clear the high accumulator, load A := width.
  2) ASL; ROL high   → width×2 (16-bit)
  3) ASL; ROL high   → width×4 (16-bit). Save as the ×4 partial.
  4) ASL low; ROL high on that 16-bit value → width×8 (16-bit).
  5) CLC; ADC low(×8)+low(×4) → low(×12); then ADC high(×8)+high(×4)+carry → high.
- Result: exact 16-bit width×12 using only shifts (ASL/ROL) and additions (ADC),
  with carry propagation handling page crossings automatically.
  
================================================================================*/
* = $4312
setup_room_columns_with_decode_snapshots:
        /*----------------------------------------
           Release previously published gfx-layers block if present
           - Treat room_gfx_layers_hi == 0 as “no active block”
           - Pass block address in X:lo, Y:hi to mem_release
         * --------------------------------------- */
        ldx     room_gfx_layers_lo            // prepare address (lo) of current layers block
        ldy     room_gfx_layers_hi            // prepare address (hi); Y==0 means “no block”
        beq     calculate_rsrc_size           // no active block → skip free and size it
        jsr     mem_release                   // free previous layers block (ABI: X=lo, Y=hi)

        /*------------------------------------------------------------
           Compute resource payload size (width × 12):
			- width_x8  = room_width * 8
			- width_x4  = room_width * 4
			- width_x12 = width_x8 + width_x4
			- rsrc_total_bytes_local = width_x12 + 4
			
           - Build width × 4 as a 16-bit product using shifts and carry propagation:
             • ASL shifts A left by 1 (×2). Any bit7 overflow becomes C.
             • ROL width_x8 + 1 rotates C into the high byte’s bit0, extending precision.
             • Repeating ASL+ROL a second time multiplies by 2 again (overall ×4)
               while accumulating any new overflow into width_x8 + 1.
             • Result after two ASL/ROL pairs:
                 width_x8 = low byte of (width × 4)
                 width_x8 + 1 = high byte of (width × 4)
				 
           - Cache width × 4 into width_x4, then double to get width × 8. 
		   Finally sum (×8) + (×4) → width × 12 in width_x12.
        ------------------------------------------------------------*/
calculate_rsrc_size:
        /*----------------------------------------
           - Derive width × 4 (but store it in width_x8 temporarily)
         * --------------------------------------- */
        lda     #$00                          // clear high byte accumulator for upcoming 16-bit products
        sta     width_x8 + 1

        lda     room_width                    
        asl                                   // form width × 2 (carry feeds next ROL)
        rol     width_x8 + 1                     // propagate carry into high byte
        asl                                   // width × 4
        rol     width_x8 + 1                     // accumulate high byte of (width × 4)
        sta     width_x8                     // low byte of width × 4

		/*----------------------------------------
           - Copy the 16-bit ×4 value from width_x8 into width_x4
         * --------------------------------------- */        
		sta     width_x4                     // cache ×4: low
        lda     width_x8 + 1
        sta     width_x4 + 1                     // cache ×4: high

        /*----------------------------------------
           Derive the width × 8 term in place from the ×4 product
         * --------------------------------------- */
		asl     width_x8                     // double ×4 → ×8 (low)
        rol     width_x8 + 1                     // …and its high via carry

        /*----------------------------------------
           Combine ×8 and ×4 terms to produce width × 12
         * --------------------------------------- */
        clc                                   // clear carry before 16-bit addition
        lda     width_x8                     // low byte of ×8
        adc     width_x4                     // + low byte of ×4 → low byte of ×12 (C may set)
        sta     width_x12                    // store low byte result
        lda     width_x8 + 1                     // high byte of ×8
        adc     width_x4 + 1                     // + high byte of ×4 (+carry) → high byte of ×12
        sta     width_x12 + 1                    // store high byte result
		
        /*----------------------------------------
           Add resource header length to payload size
         * --------------------------------------- */
        clc                                   // prepare for 16-bit add
        lda     width_x12                    // payload size: low byte
        adc     #MEM_HDR_LEN                  // + header length (low)
        sta     rsrc_total_bytes_local       // total size: low byte
        lda     width_x12 + 1                    // payload size: high byte
        adc     #$00                          // include carry from low-byte add
        sta     rsrc_total_bytes_local + 1       // total size: high byte

        /*----------------------------------------
           Allocate heap block for room-layers resource
           - Request size computed in rsrc_total_bytes_local (X:lo, Y:hi)
           - mem_alloc returns base address in X:lo, Y:hi
         * --------------------------------------- */
        ldx     rsrc_total_bytes_local       // allocation size (low)
        ldy     rsrc_total_bytes_local + 1      // allocation size (high)
        jsr     mem_alloc                     // allocate; returns base in X:lo, Y:hi

        /*------------------------------------------------------------
           Stash allocated block base for later writes/publishing
           - Cache mem_alloc’s return (X:lo, Y:hi) into alloc_base
           - Subsequent code uses alloc_base to initialize/publish the resource
        ------------------------------------------------------------*/
        stx     alloc_base
        sty     alloc_base + 1

        /*------------------------------------------------------------
           Initialize and write standard 4-byte resource header
           - Set type/index fields for a “room gfx layers” resource
           - rsrc_hdr_init emits the header at alloc_base
        ------------------------------------------------------------*/
        lda     #RSRC_TYPE_ROOM_LAYERS        // header.type := room layers ($04)
        sta     rsrc_resource_type
        lda     #RSRC_INDEX_GFX_LAYERS        // header.index := gfx layer slot ($00)
        sta     rsrc_resource_index
        jsr     rsrc_hdr_init                  // write header at alloc_base

        /*------------------------------------------------------------
           Publish newly allocated block as the active gfx-layers resource
           - Copy alloc_base (X:lo, Y:hi) into the public room_gfx_layers pointer
        ------------------------------------------------------------*/
        ldx     alloc_base                    // X := block base (low)
        ldy     alloc_base + 1                    // Y := block base (high)
        stx     room_gfx_layers_lo            // publish low byte
        sty     room_gfx_layers_hi            // publish high byte

        /*------------------------------------------------------------
           Cache base address for validity checks and initialize write cursor
           - Mirror alloc_base into room_gfx_layers_active_ptr (for “still current?” tests)
           - Initialize rsrc_write_ptr to the start of the allocated block
        ------------------------------------------------------------*/
        stx     room_gfx_layers_active_ptr    // active_ptr.lo := base.lo
        sty     room_gfx_layers_active_ptr + 1    // active_ptr.hi := base.hi
        stx     rsrc_write_ptr                // write_ptr.lo := base.lo
        sty     rsrc_write_ptr + 1                // write_ptr.hi := base.hi

        // Advance output pointer past the resource header.
        /*------------------------------------------------------------
           Advance write cursor past the 4-byte resource header
           - Add MEM_HDR_LEN to rsrc_write_ptr (16-bit little-endian)
           - Use CLC/ADC to propagate carry from low to high byte
        ------------------------------------------------------------*/
        clc                                   // prepare for 16-bit addition
        lda     rsrc_write_ptr               // load write_ptr low
        adc     #<MEM_HDR_LEN                 // + header length (low)
        sta     rsrc_write_ptr               // commit updated low
        lda     rsrc_write_ptr + 1               // load write_ptr high
        adc     #>MEM_HDR_LEN                 // + header length (high) + carry from low
        sta     rsrc_write_ptr + 1               // commit updated high

        /*------------------------------------------------------------
           Prepare room_width as a 16-bit value for address arithmetic
           - Store low byte, clear high byte (width fits in 8 bits here)
        ------------------------------------------------------------*/
        lda     room_width                    
        sta     room_width_16bit             // width.low := A
        lda     #$00                          // zero high byte
        sta     room_width_16bit + 1             // width.high := 0

        /*------------------------------------------------------------
           Lay out directory of 12 pointer-list bases and mirror them
           - For each list: write rsrc_write_ptr.{lo,hi} to the canonical table
             and to the mirrored_bases array for sanity checking
           - After each write, advance rsrc_write_ptr by room_width bytes
           - Iterate Y over pairs (0..$16, step 2) → 12 base addresses total
        ------------------------------------------------------------*/
		
		
        /*------------------------------------------------------------
           Record 16-bit base for each pointer list (canonical + mirror)
           - Y iterates over byte pairs (0,2,...): list index = Y >> 1
           - Write rsrc_write_ptr.{lo,hi} to column_decode_snapshot_bases and
             to mirrored_bases for a sanity check mirror
        ------------------------------------------------------------*/
        ldy     #$00                          // start at first list (pair index 0)
compute_pointer_list_start:
        lda     rsrc_write_ptr               // current list base: low byte
        sta.a   column_decode_snapshot_bases,y   // canonical table low byte at offset Y
        sta     mirrored_bases,y           // mirror low byte (legacy/debug)
        lda     rsrc_write_ptr + 1               // current list base: high byte
        sta.a   column_decode_snapshot_bases+1,y // canonical table high byte at offset Y+1
        sta     mirrored_bases+1,y         // mirror high byte

        /*------------------------------------------------------------
           Advance list base by room_width (16-bit add)
           - Moves rsrc_write_ptr to the next list’s data region
        ------------------------------------------------------------*/
        clc                                   // prepare for 16-bit addition
        lda     rsrc_write_ptr               // low: base
        adc     room_width_16bit             // + low(width) → next base low
        sta     rsrc_write_ptr               // commit low
        lda     rsrc_write_ptr + 1               // high: base
        adc     room_width_16bit + 1             // + high(width) + carry → next base high
        sta     rsrc_write_ptr + 1               // commit high

        /*------------------------------------------------------------
           Step to next pointer-list pair and loop over all 12 lists
           - Each base consumes two bytes in the directory (lo/hi)
           - Y advances by 2; stop when Y == $18 (12 pairs × 2 bytes)
        ------------------------------------------------------------*/
        iny                                   // Y += 1 → odd offset (hi)
        iny                                   // Y += 1 → next pair (lo)
        cpy     #$18                          // done after 12 pairs (0..$18)
        bne     compute_pointer_list_start    // more lists pending → continue

        /*------------------------------------------------------------
           Resolve base pointer for the current room resource
           - X := current_room; index into room_ptr_{lo,hi}_tbl
           - Store the selected room base into room_data_ptr (lo/hi)
        ------------------------------------------------------------*/
        ldx     current_room                  // X := active room index
        lda     room_ptr_lo_tbl,x             // fetch room base low byte
        sta     room_data_ptr                // room_data_ptr.lo := low
        lda     room_ptr_hi_tbl,x             // fetch room base high byte
        sta     room_data_ptr + 1                // room_data_ptr.hi := high

        /*------------------------------------------------------------
           Prepare TILE layer: bind source, bind list bases, snapshot state
           - decomp_src_ptr := room_data_ptr + tile_matrix_ofs
           - Lists (#0..#7): src_lo, src_hi, mode+count, run_symbol (each lo/hi)
           - Calls snapshot_column_decoder_state to record per-column entry state
        ------------------------------------------------------------*/
		 
        /*------------------------------------------------------------
           Bind TILE layer source pointer (room_data_ptr + tile_matrix_ofs)
           - 16-bit little-endian add; CLC ensures correct carry behavior
           - Result placed in decomp_src_ptr (decoder will read from here)
        ------------------------------------------------------------*/
        clc                                   // prepare for 16-bit addition
        lda     room_data_ptr                // base: room data low
        adc     tile_matrix_ofs              // + tile layer offset low
        sta     decomp_src_ptr               // src.low := sum
        lda     room_data_ptr + 1                // base: room data high
        adc     tile_matrix_ofs + 1              // + tile layer offset high (+carry)
        sta     decomp_src_ptr + 1               // src.high := sum

        /*------------------------------------------------------------
           Bind TILE layer pointer lists from the directory
           - Map precomputed entries #0..#7 to list bases:
             src_lo, src_hi, mode+count, run_symbol (each lo/hi)
        ------------------------------------------------------------*/
        lda     column_decode_snapshot_bases+0    // TILE lists: bind src_lo list base (low)
        sta     col_src_lo_list
        lda     column_decode_snapshot_bases+1    // …src_lo list base (high)
        sta     col_src_lo_list + 1

        lda     column_decode_snapshot_bases+2    // bind src_hi list base (low)
        sta     col_src_hi_list
        lda     column_decode_snapshot_bases+3    // …src_hi list base (high)
        sta     col_src_hi_list + 1

        lda     column_decode_snapshot_bases+4    // bind mode+count list base (low)
        sta     col_emit_rem_list
        lda     column_decode_snapshot_bases+5    // …mode+count list base (high)
        sta     col_emit_rem_list + 1

        lda     column_decode_snapshot_bases+6    // bind run_symbol list base (low)
        sta     col_run_symbol_list
        lda     column_decode_snapshot_bases+7    // …run_symbol list base (high)
        sta     col_run_symbol_list + 1

        /*------------------------------------------------------------
           Snapshot per-column decoder state for the TILE layer
           - Records src_lo/src_hi, packed mode+count, and run_symbol per column
           - Advances the decoder by 17 bytes per column to the next boundary
        ------------------------------------------------------------*/
        jsr     snapshot_column_decoder_state  // capture TILE column entry state; advance 17/col

        /*---------------------------------------
         * Layer: COLOR
         *  - decomp_src_ptr  := room_data_ptr + color_layer_ofs
         *  - pointer lists    := column_decode_snapshot_bases[8..15]
         *    (#8/#9  → src_lo list base,
         *     #10/#11→ src_hi list base,
         *     #12/#13→ mode-counter list base,
         *     #14/#15→ repeat-symbol list base)
         *  - Precompute per-column initial decoder state.
         *--------------------------------------*/
        clc
        lda     room_data_ptr
        adc     color_layer_ofs
        sta     decomp_src_ptr              // src base.lo
        lda     room_data_ptr + 1
        adc     color_layer_ofs + 1
        sta     decomp_src_ptr + 1              // src base.hi

        // Bind list bases for COLOR layer
        lda     column_decode_snapshot_bases+8
        sta     col_src_lo_list       // src_lo list
        lda     column_decode_snapshot_bases+9
        sta     col_src_lo_list + 1
        lda     column_decode_snapshot_bases+10
        sta     col_src_hi_list       // src_hi list
        lda     column_decode_snapshot_bases+11
        sta     col_src_hi_list + 1
        lda     column_decode_snapshot_bases+12
        sta     col_emit_rem_list             // mode-counter list
        lda     column_decode_snapshot_bases+13
        sta     col_emit_rem_list + 1
        lda     column_decode_snapshot_bases+14
        sta     col_run_symbol_list         // repeat-symbol list
        lda     column_decode_snapshot_bases+15
        sta     col_run_symbol_list + 1

        // Generate the per-column initial state for COLOR layer decoding
        jsr     snapshot_column_decoder_state

        /*---------------------------------------
         * Layer: MASK
         *  - decomp_src_ptr  := room_data_ptr + mask_layer_ofs
         *  - pointer lists    := column_decode_snapshot_bases[16..23]
         *    (#16/#17 → src_lo list base,
         *     #18/#19 → src_hi list base,
         *     #20/#21 → mode-counter list base,
         *     #22/#23 → repeat-symbol list base)
         *  - Precompute per-column initial decoder state.
         *--------------------------------------*/
        clc
        lda     room_data_ptr
        adc     mask_layer_ofs
        sta     decomp_src_ptr              // src base.lo
        lda     room_data_ptr + 1
        adc     mask_layer_ofs + 1
        sta     decomp_src_ptr + 1              // src base.hi

        // Bind list bases for MASK layer
        lda     column_decode_snapshot_bases+16
        sta     col_src_lo_list       // src_lo list
        lda     column_decode_snapshot_bases+17
        sta     col_src_lo_list + 1
        lda     column_decode_snapshot_bases+18
        sta     col_src_hi_list       // src_hi list
        lda     column_decode_snapshot_bases+19
        sta     col_src_hi_list + 1
        lda     column_decode_snapshot_bases+20
        sta     col_emit_rem_list             // mode-counter list
        lda     column_decode_snapshot_bases+21
        sta     col_emit_rem_list + 1
        lda     column_decode_snapshot_bases+22
        sta     col_run_symbol_list         // repeat-symbol list
        lda     column_decode_snapshot_bases+23
        sta     col_run_symbol_list + 1

        // Generate the per-column initial state for MASK layer decoding
        jsr     snapshot_column_decoder_state

        rts
/*================================================================================
  snapshot_column_decoder_state — record per-column decoder entry state for
  random access

  Arguments:
    (none)

  Vars/State:
    room_gfx_x_saved        ─ scratch used to preserve X around decomp_stream_next

  Global Inputs:
    decomp_src_ptr          ─ current decoder source pointer (lo/hi)
    decomp_emit_mode        ─ current emission mode flag byte (bit7 mirrors mode)
    decomp_emit_rem         ─ remaining count for the current literal/run
    decomp_run_symbol       ─ byte to repeat when in run mode
    room_width              ─ number of columns to snapshot

  Global Outputs:
    col_src_lo_list         ─ table[y] ← low byte of source pointer at column start
    col_src_hi_list         ─ table[y] ← high byte of source pointer at column start
    col_emit_rem_list       ─ table[y] ← packed (bit7=mode, bits6..0=count)
    col_run_symbol_list     ─ table[y] ← run symbol at column start (ignored if literal)
    decomp_src_ptr          ─ advanced by exactly 17 decoded bytes per column

  Returns:
    A,X,Y,flags             ─ clobbered

  Description:
    - Begin with a fail-fast sanity check (assert_pointer_lists_match) so pointer
      list destinations are known-good before bulk writes.
    - Initialize the 4-symbol decoder (decomp_dict4_init); after this, the source
      pointer targets the payload bytes to be decoded.
    - For each column y in [0, room_width):
        • Snapshot source pointer into (col_src_lo_list[y], col_src_hi_list[y]).
        • Pack the starting emission state into col_emit_rem_list[y]:
            · bit7 = 1 → literal (direct) mode; 0 → run mode.
            · bits6..0 = remaining count for the current mode.
        • Store decomp_run_symbol into col_run_symbol_list[y] (relevant for run).
        • Consume exactly 17 decoded bytes via decomp_stream_next to advance the
          decoder to the next column boundary without storing pixels here.
    - Enables O(1) resume at any column by restoring the saved pointer and packed
      emission state instead of re-decoding prior columns.

 ================================================================================
 Snapshot compressor state for this column - tech details

 When we jump into the middle of a compressed stream (start of an arbitrary
 column), we must *fully* reconstruct the decoder state without consuming any
 bytes from prior columns. There are three pieces of state we need per column
 snapshot:

   1) Stream position:      decomp_src_ptr  (we save the pointer for column y)
   2) Emit mode + remaining count: literal-vs-run and its 7-bit counter
   3) Run symbol:           the byte to repeat if in run mode

 The live decoder indicates the mode via a flag (N=1 → literal, N=0 → run). We
 do not persist CPU flags, so we encode the mode into the top bit of the saved
 counter byte:

   bit7 = 1  → literal (direct) mode
   bit7 = 0  → run mode
   bits6..0  → remaining count for that mode

 This "packed" byte lets the resume code reestablish the exact mode without
 having to re-read the stream. If we didn’t set bit7 when starting in literal
 mode, a remaining count of N would be ambiguous with "run for N" — the next
 decomp_stream_next would then produce the wrong output and drift the stream
 state. By forcing bit7 appropriately, the first resumed decode step behaves
 identically to continuing from the previous column: it emits the correct type
 (literal vs run), uses the correct remaining count, and decrements
 consistently.

 Note: The 4-byte dictionary has already been consumed globally via
 decomp_dict4_init before taking snapshots, so the per-column resume only needs
 (src_ptr, packed_mode+count, run_symbol) to be exact.

 Source of truth (from the active decompressor):
   decomp_emit_mode  : N flag mirrors mode (N=1 literal, N=0 run)
   decomp_emit_rem   : 7-bit remaining count
   decomp_run_symbol : byte to repeat if in run mode
	  
================================================================================*/
* = $4463
snapshot_column_decoder_state:
        jsr     assert_pointer_lists_match    // sanity check: confirm scratch tables/mirrors are consistent
        jsr     decomp_dict4_init         // init 4-symbol decoder: read dict (+0..+3), src now points at payload

        ldy     #$00                      // Y = column index (0 .. room_width-1)

snapshot_this_column:
        // Capture decoder source at the start of this column’s payload
        // (little-endian pointer split across two parallel lists)
        lda     decomp_src_ptr
        sta     (col_src_lo_list),y       // write low byte of src to list[y]
        lda     decomp_src_ptr + 1
        sta     (col_src_hi_list),y       // write high byte of src to list[y]

		 
		 // Encode initial emission state for this column into two tables:
        //   - col_emit_rem_list[y]: 7-bit remaining count, with bit7=1 iff starting in literal (direct) mode
        //   - col_run_symbol_list[y]: byte to repeat if starting in run mode (ignored for literal mode)
        lda     decomp_emit_mode           // N=1 → literal mode, N=0 → run mode
        bpl     handle_run_mode            // branch if run mode (bit7 clear)

        // Literal (direct) mode: set the mode flag (bit7=1) and keep the low 7 bits as the count
        lda     #$80
        ora     decomp_emit_rem            // bit7:=1 | count(6..0)
        jmp     commit_mode_counter        // unconditional (result is never zero)

handle_run_mode:
        // Run mode: store the count as-is (bit7 remains 0 → run)
        lda     decomp_emit_rem

commit_mode_counter:
        sta     (col_emit_rem_list),y      // save packed mode+count for column y
        lda     decomp_run_symbol
        sta     (col_run_symbol_list),y    // save run symbol for column y (meaningful in run mode)

        // Advance the decoder by one full column worth of bytes.
        // Column height = 17 rows ⇒ consume 17 decoded bytes (loop: 16..0).
        ldx     #$10                      // X = 16 … 0 → total 17 iterations

consume_column_bytes:
        stx     room_gfx_x_saved          // preserve X across subroutine (callee may clobber X)
        jsr     decomp_stream_next        // emit 1 byte; advances decomp_src_ptr & internal state
        ldx     room_gfx_x_saved          // restore loop counter
        dex                               // next row in this column
        bpl     consume_column_bytes      // keep going until X becomes -1

        // Move on to the next column; stop after the last column (Y == room_width).
        iny                               // column++
        cpy     room_width                // done when Y == room_width
        bne     snapshot_this_column      // else snapshot the next column’s initial state

        rts
/*================================================================================
  assert_pointer_lists_match — compare two 24-byte pointer-base tables; halt on mismatch

  Arguments:
    (none)

  Global Inputs:
    column_decode_snapshot_bases  ─ canonical 24-byte table (2×12 entries) built earlier
    mirrored_bases          ─ debug mirror of the same 24 bytes for sanity checking

  Global Outputs:
    debug_error_code           ─ set to 1 on the first mismatch (unchanged on success)

  Returns:
    A,Y,flags                  ─ clobbered on success; X preserved
    (no return on failure)     ─ infinite loop writes vic_border_color_reg after flagging error

  Description:
    - Walk both byte tables in lockstep for y ∈ [0, CMP_LEN); require exact equality.
    - On first mismatch:
        • Store 1 to debug_error_code (latched failure indicator).
        • Map I/O via cpu_port := MAP_IO so VIC registers are visible.
        • Enter a tight loop writing vic_border_color_reg to signal a hard halt.
    - On full match:
        • Return normally with Y == CMP_LEN (Z=1 from the final CPY).

  Notes:
    - Serves as a fail-fast guard before bulk writes (e.g., precompute_column_
      decompression_states). Divergence usually indicates stride/base arithmetic
      bugs, aliasing/overlap, or post-setup memory corruption.
	  
 ================================================================================
 Why this sanity check runs before decompression - tech details

 - Verifies the canonical and mirrored 24-byte pointer-base tables still match;
   any drift implies wrong destinations for subsequent writes.
 - The precompute/decompression step emits many bytes per column across multiple
   lists; bad bases would spray data into unrelated memory and corrupt state.
   
================================================================================*/
* = $449B
assert_pointer_lists_match:
        /*----------------------------------------
           Initialize scan index (Y := 0) to begin byte-by-byte compare
         * --------------------------------------- */
        ldy     #$00                          // start at first element (y=0)

compare_next:
        /*----------------------------------------
           Compare corresponding bytes of the two tables at index Y
           - cmp sets Z=1 iff bytes are equal; beq takes the match path
           - on mismatch, fall through to the error handler (no return)
         * --------------------------------------- */
        lda     column_decode_snapshot_bases,y   // A := column_decode_snapshot_bases[Y]
        cmp     mirrored_bases,y           // compare against mirrored_bases[Y] (Z=1 ↔ equal)
        beq     advance_or_return                          // equal → continue with next index; else mismatch path

        /*----------------------------------------
           Mismatch encountered:
           - record an error code (1) for diagnostics
           - map I/O via $01 so VIC registers are visible
           - hard-halt by continuously writing the border color (visual cue)
         * --------------------------------------- */
        lda     #$01                          // prepare error code and border color value (=1)
        sta     debug_error_code              // debug_error_code := 1 (latched failure indicator)
        ldy     #MAP_IO_IN                      // Y := mapping value for $01 (I/O enabled)
        sty     cpu_port       				  // ensure address of vic_border_color_reg targets VIC, not RAM/ROM

mismatch_halt_loop:
        sta     vic_border_color_reg              // border := 1 (visible hang); A remains 1
        jmp     mismatch_halt_loop                   // infinite loop (no return on failure)

advance_or_return:
        /*----------------------------------------
           Advance to next byte; terminate when Y == CMP_LEN
         * --------------------------------------- */
        iny                                   // Y++
        cpy     #CMP_LEN                      // Z=1 when Y reached compare length
        bne     compare_next                // more bytes pending → keep comparing
        rts                                   // all bytes matched → return
