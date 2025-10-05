;===============================================================================
; Module: Hybrid RLE + 4-symbol dictionary decompressor (streaming, 1 byte/call)
;
; Summary:
;   Implements a compact decompression format that combines Run-Length Encoding
;   (RLE) with a 4-entry symbol dictionary. 
;
;	The decoder exposes a streaming API: you first load the dictionary from the input, 
;	then repeatedly call a routine that returns the next decompressed byte. 
;	Two helper routines can “skip” decompressed bytes without storing them (useful to fast-forward).
;
; Arguments / State (zero-page & memory):
;   compressed_data_ptr ($27/$28)        16-bit pointer to the compressed input stream.
;   symbol_dictionary ($0100..$0103)     4 symbols (bytes) copied from the stream and
;                                        referenced by index during dictionary runs.
;   decompress_mode ($29)                Current mode flag (#$00 = DIRECT, #$FF = RUN).
;                                        Only bit 7 is tested to select the emission path.
;   mode_counter ($2A)                   Remaining outputs in the current operation;
;                                        stores L meaning “emit L+1” total bytes.
;   symbol_to_repeat ($2B)               Latched byte to output while in RUN mode
;                                        (both ad-hoc and dictionary runs).
;   y_temp ($2D)                         Scratch to preserve Y inside the main routine.
;   data_count ($2E/$2F)                 Requested count for “skip” helpers
;                                        (8/16-bit, depending on the routine).
;
; Public entry points:
;   setup_symbol_dictionary              Loads 4 bytes from compressed_data_ptr into
;                                        symbol_dictionary, advances compressed_data_ptr
;                                        by 4, and clears the current mode/counter.
;   get_next_decompressed_byte           Returns 1 decompressed byte per call in A;
;                                        maintains state across calls.
;   read_next_compressed_data            Fetches one byte from (compressed_data_ptr),
;                                        advances the pointer.
;   skip_compressed_data_16bit           Discards decompressed bytes until data_count
;                                        ($2E/$2F) reaches zero.
;   skip_compressed_data_8bit            Discards decompressed bytes using an 8-bit
;                                        count loaded from $2E.
;
; Returns:
;   get_next_decompressed_byte           A = next decompressed byte (Y preserved).
;   Other routines                       None (state/pointers updated as side effects).
;
; What is RLE (Run-Length Encoding)?
;
;   RLE is a simple compression technique for repeating values: instead of storing
;   “AAAAAA” as six separate ‘A’ bytes, we store a pair (run-length, symbol) and
;   expand it at decode time. 
;
;	This format uses RLE in two flavors:
;       AD-HOC RUN — repeats a literal byte read from the stream.
;       DICTIONARY RUN — repeats a byte looked up in a small dictionary.
;
; What is the dictionary and why use it?
;
;   Many data sets have a few very common symbols (e.g., space, zero, newline).
;   A dictionary lets us assign a short index (here 2 bits → 4 entries) to those
;   frequent symbols. By referencing them via an index instead of storing the
;   full byte each time, we save space. The compressor first writes the 4 symbol
;   values; the decoder loads them with setup_symbol_dictionary and can then use
;   their indices in control bytes to trigger dictionary runs.
;
; The dictionary's purpose is to hold the symbols having the highest frequencies.
; This allows using just 2 bits (for the 4 elements) to reference each symbol,
; while leaving the remaining bits of the byte to express a run length.
; This obviously saves space.
;
; We can partition the set of symbols into two groups:
;	-symbols in the dictionary
;	-symbols not in the dictionary
;
; Thus, runs can reference a dictionary symbol or an ad-hoc (non-dictionary) symbol.
;
; When using RLE doesn't make sense (because symbols are not being repeated), we need to
; store the symbols directly as in their original form. Otherwise, we would be 
; actually increasing the space needed.
;
; "Direct" mode is thus also needed to properly handle these cases.
; 
; There are then 3 main operations defined:
;
;	-repeat an ad-hoc value				expressed as a run-length and the ad-hoc repeated symbol
;	-repeat a dictionary value			expressed as a run-length and the dictionary symbol index
;	-direct mode						expressed as a counter, followed by one or more symbols
;
; Format overview:
;   -Each new “operation” begins with a control byte. 
;	-We denote its lower bits as an encoded length L. 
;	-Important: L means the decoder will emit L+1 bytes.
;   -That convention allows lengths up to 64 for direct/ad-hoc runs and 32 for
;   dictionary runs, while keeping the control byte compact.
;
;   1) DIRECT mode (bits 7..6 = 00, i.e., control < $40)
;      L = control (0..63) → output the next (L+1) bytes literally.
;      Layout: [control] [b0] [b1] ... [bL]
;
;   2) AD-HOC RUN (bits 7..6 = 01, i.e., $40..$7F)
;      L = control & $3F (0..63), next byte is the literal to repeat.
;      Output (L+1) copies of that literal.
;      Layout: [control] [literal]
;
;   3) DICTIONARY RUN (bit 7 = 1, i.e., ≥ $80)
;      index = (control >> 5) & 3 selects one of 4 dictionary symbols.
;      L = control & $1F (0..31) → output (L+1) copies of that symbol.
;      Layout: [control]   (no extra byte)
;
; Streaming state machine:
;
;   mode_counter holds L (“count-1”): 
;		when starting an operation we emit the first byte immediately; 
;		on subsequent calls we decrement first, then emit.
;
;   decompress_mode is #$00 for DIRECT and #$FF for RUN; 
;		only bit 7 matters, enabling a quick branch (BIT + BMI) to the proper emission path.
;
;   symbol_to_repeat is captured at operation start for both AD-HOC and DICT runs.
;
; Typical usage:
;
;   1) Point compressed_data_ptr at the compressed buffer.
;   2) JSR setup_symbol_dictionary   ; reads 4 symbols into $0100..$0103
;   3) Loop calling get_next_decompressed_byte until you’ve produced the desired
;      number of decompressed bytes; store or process A each time.
;   4) Use the skip helpers to fast-forward without materializing bytes.
;
; Limits & notes:
;
;   DIRECT / AD-HOC: L ≤ 63 → emit ≤ 64 bytes; DICT: L ≤ 31 → emit ≤ 32 bytes.
;   symbol_dictionary resides in page 1 ($0100..$0103), which is the 6502 stack
;   page; we're safe as long as the stack never grows down to these addresses.
;
;   All routines update the shared decoder state and compressed_data_ptr so that
;   decoding can be paused/resumed seamlessly (including after skips).
;===============================================================================


;===========================================
; Zero-page variables and constants
;===========================================
compressed_data_ptr = $27    ; 16-bit pointer to compressed input stream (low at $27, high at $28)
decompress_mode     = $29    ; current mode flag ($00 = direct, $FF = run). Only bit 7 is tested
mode_counter        = $2a    ; remaining outputs for the current operation (stores L, meaning L+1 total outputs)
symbol_to_repeat    = $2b    ; byte value to output while in run mode
y_temp              = $2d    ; temporary storage for Y register within get_next_decompressed_byte
data_count          = $2e    ; decompressed-byte skip counter (low at $2e, high at $2f) for skip helpers
symbol_dictionary   = $0100	 ; dictionary of symbols (4 symbols: $0100-0103)

DIRECT_MODE         = #$00   ; constant: direct mode selector
RUN_MODE            = #$FF   ; constant: run mode selector

* = $0104
;===========================================
; Initializes the symbol dictionary with 4 entries.
;
; Arguments:
;	compressed_data_ptr ($27/$28)	16-bit pointer to the start of compressed input data.
;
; Returns:
;	None.
;
; Description:
;	Reads four bytes from the current compressed data stream and copies them into
;	the symbol dictionary. These four symbols are used as the dictionary entries
;	for dictionary-based runs during decompression.
;
;	After copying, the compressed data pointer is advanced by four bytes so the
;	next read will begin immediately after the dictionary. The routine also resets
;	the decompression state (mode and counter) so that the next decompression call
;	starts cleanly.
;===========================================
setup_symbol_dictionary:
       ; Copy 4 dictionary bytes from input: Y = 3..0
       LDY #$03
       ;------------------------------------------------
copy_loop:
       LDA (compressed_data_ptr),Y   ; read source byte at ptr+Y
       STA symbol_dictionary,Y       ; store to dictionary [$0100..$0103]
       DEY                           ; next lower index
       BPL copy_loop                 ; loop until Y = $FF
       ;------------------------------------------------
       ; Advance input pointer by 4 (past the dictionary)
       CLC
       LDA <compressed_data_ptr
       ADC #$04
       STA <compressed_data_ptr
       LDA compressed_data_ptr + 1
       ADC #$00
       STA compressed_data_ptr + 1
       ;------------------------------------------------
       ; Reset state: no active operation, counter = 0
       LDA #$00
       STA mode_counter
       STA decompress_mode
       RTS

;===========================================
; Retrieves the next decompressed byte from the stream
;
; Arguments:
;	compressed_data_ptr ($27/$28)	Pointer to current read position in compressed data.
;	symbol_dictionary ($0100–$0103)	4-entry symbol lookup table initialized earlier.
;	mode_counter ($2A)				Remaining repetitions or direct bytes to output.
;	decompress_mode ($29)			Indicates current mode (#$00 = direct, #$FF = run).
;	symbol_to_repeat ($2B)			Holds symbol currently being repeated in run mode.
;
; Returns:
;	A — The next decompressed byte.
;
;
; Description:
;	This is the core decompression routine implementing a hybrid RLE and dictionary
;	encoding scheme. Each control byte from the input determines one of three modes:
;
;	  • Direct Mode (bits 7–6 = 00)
;			The byte value specifies how many literal bytes follow directly in the stream (L+1 bytes total)
;
;	  • Ad-hoc Run (bits 7–6 = 01)
;			Bits 5-0 give a run length (L+1) and the following byte is the literal symbol to repeat
;
;	  • Dictionary Run (bit 7 = 1)
;			Bits 6–5 select one of four dictionary symbols and bits 4–0 give a run length (L+1)
;
; Note that the symbol dictionary has to be set up first, before calling this routine.
; This is done via setup_symbol_dictionary.
;===========================================
get_next_decompressed_byte:
       ; Preserve Y (routine uses Y)
       STY y_temp
       ; If an operation is already active (counter > 0), continue it
       LDA mode_counter
       BNE repeat_operation
       ;--------------------------------------------------
       ; No active op: fetch a control byte and configure the next op
       JSR read_next_compressed_data
       ; Classify by top bits
       CMP #$40
       BCS byte_ge_40
       ;--------------------------------------------------
       ; DIRECT: ctrl < $40 → L in A. Set counter=L and output first raw byte.
       ; (Total bytes in this direct block = L+1)
       ;--------------------------------------------------
       STA mode_counter
       ; Set direct mode (bit7 clear)
       LDA DIRECT_MODE
       JMP set_mode
       ;--------------------------------------------------
byte_ge_40:
       CMP #$80
       BCS byte_ge_80
       ;--------------------------------------------------
       ; AD-HOC RUN: $40 ≤ ctrl < $80
       ; Low 6 bits = L (repeat count-1), next byte = literal to repeat
       ;--------------------------------------------------
       AND #$3F
       STA mode_counter
       ; Get the literal to repeat
       JSR read_next_compressed_data
       JMP set_symbol_to_repeat
       ;--------------------------------------------------
       ; DICTIONARY RUN: ctrl ≥ $80
       ; Bits 4..0 = L, bits 6..5 = dictionary index
       ;--------------------------------------------------
byte_ge_80:
       ; Extract L (run length-1) to counter
       TAX
       AND #$1F		
       STA mode_counter
       ; Recover full ctrl in A
       TXA
       ; Compute index = (ctrl >> 5) & 3
       LSR A
       LSR A
       LSR A
       LSR A
       LSR A
       AND #$03	
       TAX
       ; Fetch symbol from dictionary
       LDA symbol_dictionary,X
       ;--------------------------------------------------
       ; Initialize run with chosen symbol, then mark mode as RUN
set_symbol_to_repeat:
       STA symbol_to_repeat
       ; Set run mode (bit7 set)
       LDA RUN_MODE
set_mode:
       STA decompress_mode
       ; New op just set up: skip the pre-decrement path and emit first byte
       JMP check_mode
       ;--------------------------------------------------
repeat_operation:
       ; Active op: decrement remaining count before emitting this byte
       DEC mode_counter
       ;--------------------------------------------------
check_mode:
       ; Decide emission path by bit7 of mode: RUN (negative) vs DIRECT (non-negative)
       BIT decompress_mode
       BMI in_run_mode
       ;--------------------------------------------------
       ; DIRECT: output next raw byte from input
       ;--------------------------------------------------
       JSR read_next_compressed_data
       JMP exit
       ;--------------------------------------------------
       ; RUN: output previously latched symbol_to_repeat
       ;--------------------------------------------------
in_run_mode:
       LDA symbol_to_repeat
exit:
       ; Restore Y and return byte in A
       LDY y_temp
       RTS

;===========================================
; Reads one byte from the compressed data stream.
;
; Arguments:
;	compressed_data_ptr ($27/$28) — Pointer to current position in compressed data.
;
; Returns:
;	A — Byte read from compressed data.
;
; Description:
;	Fetches a single byte from the memory location pointed to by compressed_data_ptr,
;	then automatically increments the pointer. This routine is used by higher-level
;	decompression functions whenever a new byte is required from the input stream.
;===========================================
read_next_compressed_data:
       LDY #$00                      ; Y=0 so (ptr),Y reads at ptr
       LDA (compressed_data_ptr),Y   ; fetch byte
       INC <compressed_data_ptr      ; bump low byte
       BNE exit_2                    ; if not wrapped, done
       INC compressed_data_ptr + 1   ; else bump high byte
exit_2:
       RTS

;===========================================
; Skips a specified amount of decompressed data (16-bit count).
;
; Arguments:
;	data_count ($2E/$2F) — 16-bit counter specifying how many decompressed bytes to skip.
;
; Returns:
;	None.
;
; Description:
;	Runs the decompression routine repeatedly to simulate producing data without
;	storing it. Each decompressed byte decrements data_count until it reaches zero.
;	This version supports skipping up to 65,535 bytes.
;
;	This function is useful for fast-forwarding through portions of compressed
;	data without needing to actually copy or process the output.
;===========================================
skip_compressed_data_16bit:
       ; If data_count == 0, nothing to skip
       LDA <data_count
       ORA data_count + 1
       BNE next_byte
       RTS
       ;---------------------------------
next_byte:
       ; Consume one decompressed byte (discard result)
       JSR get_next_decompressed_byte
       ; Decrement 16-bit data_count (low then high if needed)
       LDA <data_count
       BNE dec_lo_counter
       ; data_count low is zero → borrow from high
       DEC data_count + 1
dec_lo_counter:
       DEC <data_count
       JMP skip_compressed_data_16bit

;===========================================
; Skips a specified amount of decompressed data (8-bit count).
;
; Arguments:
;	data_count ($2E) — 8-bit counter specifying how many decompressed bytes to skip.
;
; Returns:
;	None.
;
; Description:
;	A compact version of skip_compressed_data_16bit for skipping up to 255 bytes.
;	It repeatedly calls the decompression routine, discarding each byte produced,
;	until the counter reaches zero. Commonly used for small, localized skips in
;	the compressed data stream.
;===========================================
skip_compressed_data_8bit:
       ; Load 8-bit count once; if zero, done
       LDY data_count
       BNE next_byte_8
       RTS
next_byte_8:
       ; Consume one decompressed byte (discard result)
       JSR get_next_decompressed_byte
       DEY
       BNE next_byte_8
       RTS


; HYBRID RLE + 4-SYMBOL DICTIONARY DECOMPRESSOR — TEXT FLOW DIAGRAM
; =================================================================
;
; [setup_symbol_dictionary]
; ------------------------
; ENTRY
;   │
;   ├─ Y ← #$03                                ; copy 4 bytes (indices 3..0)
;   ├─ LOOP:  A ← (compressed_data_ptr),Y
;   │         symbol_dictionary[Y] ← A
;   │         Y ← Y-1
;   │         IF Y ≥ 0 THEN LOOP
;   │
;   ├─ Advance compressed_data_ptr by +4       ; skip past dictionary
;   │
;   ├─ mode_counter ← 0
;   ├─ decompress_mode ← 0                     ; clear state
;   └─ RTS
;
;
; [get_next_decompressed_byte]
; ----------------------------
; ENTRY
;   │
;   ├─ Save Y into y_temp
;   │
;   ├─ IF mode_counter ≠ 0 THEN
;   │      ├─ mode_counter ← mode_counter - 1  ; continue current op
;   │      └─ GOTO EMIT
;   │
;   ├─ ELSE  (no active op: parse a control byte)
;   │      ├─ ctrl ← read_next_compressed_data()
;   │      │
;   │      ├─ IF ctrl < $40  (DIRECT) THEN
;   │      │     ├─ mode_counter ← ctrl        ; L (will emit L+1 total)
;   │      │     ├─ decompress_mode ← $00      ; DIRECT_MODE (bit7=0)
;   │      │     └─ GOTO EMIT
;   │      │
;   │      ├─ IF $40 ≤ ctrl < $80  (AD-HOC RUN) THEN
;   │      │     ├─ mode_counter ← (ctrl & $3F)       ; L
;   │      │     ├─ symbol_to_repeat ← read_next_compressed_data()
;   │      │     ├─ decompress_mode ← $FF             ; RUN_MODE (bit7=1)
;   │      │     └─ GOTO EMIT
;   │      │
;   │      └─ ELSE  (ctrl ≥ $80 → DICTIONARY RUN)
;   │            ├─ L        ←  ctrl & $1F
;   │            ├─ index    ← (ctrl >> 5) & 3
;   │            ├─ mode_counter ← L
;   │            ├─ symbol_to_repeat ← symbol_dictionary[index]
;   │            ├─ decompress_mode ← $FF             ; RUN_MODE
;   │            └─ GOTO EMIT
;   │
; EMIT:
;   │  Decide by decompress_mode bit7:
;   │
;   ├─ IF RUN_MODE (bit7=1) THEN
;   │      A ← symbol_to_repeat
;   │
;   ├─ ELSE (DIRECT_MODE, bit7=0)
;   │      A ← read_next_compressed_data()
;   │
;   ├─ Restore Y from y_temp
;   └─ RTS   ; A = next decompressed byte
;
;
; [read_next_compressed_data]
; ---------------------------
; ENTRY
;   │
;   ├─ A ← (compressed_data_ptr)               ; with Y=0
;   ├─ compressed_data_ptr.low  ← +1
;   ├─ IF low wrapped to 0 THEN
;   │      compressed_data_ptr.high ← +1
;   └─ RTS   ; A = fetched byte
;
;
; [skip_compressed_data_16bit]
; ----------------------------
; ENTRY
;   │
;   ├─ IF data_count == 0 THEN RTS
;   │
;   ├─ LOOP:
;   │     JSR get_next_decompressed_byte       ; discard A
;   │     IF data_count.low == 0 THEN
;   │         data_count.high ← data_count.high - 1
;   │     data_count.low  ← data_count.low - 1
;   │     IF data_count != 0 THEN LOOP
;   │
;   └─ RTS
;
;
; [skip_compressed_data_8bit]
; ---------------------------
; ENTRY
;   │
;   ├─ Y ← data_count.low
;   ├─ IF Y == 0 THEN RTS
;   │
;   ├─ LOOP:
;   │     JSR get_next_decompressed_byte       ; discard A
;   │     Y ← Y - 1
;   │     IF Y ≠ 0 THEN LOOP
;   │
;   └─ RTS
;
;
; FORMAT & STATE (for reference while reading the flow)
; -----------------------------------------------------
; • Control byte selects operation and encodes length L (which means “emit L+1 bytes”):
;     DIRECT           : 00LLLLLL       (ctrl < $40) → then read (L+1) literal bytes
;     AD-HOC RUN       : 01LLLLLL val   ($40..$7F)   → repeat ‘val’ (L+1) times
;     DICTIONARY RUN   : 1IILLLLL       (≥ $80)      → index=II (0..3), repeat dict[index] (L+1) times
; • Internal state across calls:
;     mode_counter     : remaining outputs for the current op (stores L; the routine emits on entry,
;                        and pre-decrements on subsequent calls).
;     decompress_mode  : $00 (DIRECT) / $FF (RUN); only bit7 is tested at EMIT time.
;     symbol_to_repeat : latched at op start for both AD-HOC and DICT runs.
;     y_temp           : saves caller’s Y.
; • Contract: call setup_symbol_dictionary once (copies 4 bytes, advances input by 4, clears state),
;   then call get_next_decompressed_byte repeatedly to stream the output. The skip_* helpers advance
;   the same state while discarding data, allowing fast-forwarding within the compressed stream.
