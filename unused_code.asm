#importonce
#import "globals.inc"
#import "constants.inc"
#import "actor_animation.asm"

* = $25EA
unused_routine_1:
    jsr     compute_actor_limbs_ofs
    clc
    adc     current_limb
    tax
    lda     $FD12
    sta     limb_target_cel_seq ,x
    lda     $FD13
    sta     limb_tgt_loop_cnt,x
    rts

* = $21BD
.byte $00

* = $2501
.byte $00
* = $2502
.byte $00
* = $2503
.byte $00

* = $2E78
		sta		$FD12
		stx		$FD13
		ldy		#$07
		sty		current_limb
unused_dummy_1:		
		jsr		unused_routine_1
		dec		current_limb
		bpl		unused_dummy_1
		rts


* = $50AF
    // Save A, X, Y while optionally performing full sound cleanup
    pha
    txa
    pha
    tya
    pha

    // Request full cleanup and stop the tracked lowest-priority sound if any
    lda     #$ff
    sta     stop_sound_cleanup_mode
    lda     tracked_lowest_priority_sound
    beq     $50c1                           // zero → no sound tracked, skip cleanup
    jsr     stop_sound_full_cleanup

    // Restore Y, X, A and return
    pla
    tay
    pla
    tax
    pla
    rts
