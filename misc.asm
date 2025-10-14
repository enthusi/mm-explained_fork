#importonce

#import "globals.inc"

/*================================================================================
mem_fill_x — write X to memory fill_byte_cnt times starting at dest

  Arguments:
    X      Fill byte to write to each destination address.

  Global Inputs:
    fill_dest_ptr      16-bit base address to start filling (advanced across pages).
    fill_byte_cnt      16-bit number of bytes to write; must be nonzero on entry.

  Global Outputs:
    fill_dest_ptr      Advanced to the first address past the last written byte.
    fill_byte_cnt      Decremented to zero when the routine returns.

  Returns:
    A,Y,flags
      Clobbered; X preserved. Z=1 on exit (counter reached zero).

  Description:
    - Writes the value from X to *(fill_dest_ptr) and increments fill_dest_ptr
      after each store (handles page crossings via INC low/INC high).
    - Decrements a 16-bit counter and loops until both counter bytes are zero.
    - Y remains 0 the entire time; the base pointer moves instead of Y.

  Notes:
    - Caller must ensure fill_byte_cnt > 0; a zero-length request is invalid.
================================================================================*/
* = $5D32
mem_fill_x:
        /*------------------------------------------------------------
           Initialize Y to 0 (use base-pointer incrementing, not Y stepping)
        ------------------------------------------------------------*/
        ldy     #$00                          // Y stays 0 for (fill_dest_ptr),Y addressing

fill_loop:
        /*------------------------------------------------------------
           Store fill byte and advance destination pointer (handles page cross)
        ------------------------------------------------------------*/
        txa                                   // A := fill byte (preserve X)
        sta     (fill_dest_ptr),y              // write A to *fill_dest_ptr
        inc     <fill_dest_ptr                  // dest.lo++
        bne     dec_count             // no wrap → skip high-byte increment
        inc     >fill_dest_ptr                // wrapped → dest.hi++

dec_count:
        /*------------------------------------------------------------
           Decrement 16-bit remaining count (borrow when low byte is 0)
        ------------------------------------------------------------*/
        lda     <fill_byte_cnt                  // low byte
        bne     dec_count_lo        // if low != 0, no borrow
        dec     >fill_byte_cnt                // borrow: dec high byte
dec_count_lo:
        dec     <fill_byte_cnt                  // dec low byte

        /*------------------------------------------------------------
           Loop until both counter bytes are zero
        ------------------------------------------------------------*/
        lda     <fill_byte_cnt                  // low
        ora     >fill_byte_cnt                // combine with high; Z=1 iff both zero
        bne     fill_loop // not done → continue
        rts                                   // done (Z=1, Y=0, X preserved)

