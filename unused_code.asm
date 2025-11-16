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
