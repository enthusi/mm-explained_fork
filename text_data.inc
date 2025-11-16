#importonce

/*
Word pointers to character names
Note: Pointers to "Dr Fred" (f14e) and "Meteor" (f199) are repeated twice
		"Plant" is repeated 3 times (f1a0)
		"Sandy" is repeated 4 times (f1a6)
(24 in total - from " " to "Sandy")
*/

* = $F0F2
name_ptr_hi_tbl:
        .byte $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1
        .byte $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1

* = $F10A
name_ptr_lo_tbl:
        .byte $22,$24,$28,$2E,$33,$3B,$43,$49,$4E,$4E,$56,$61
        .byte $6A,$7A,$8A,$99,$99,$A0,$A0,$A0,$A6,$A6,$A6,$A6
/*
;f122	0	" "
;f124	1	"Syd"
;f128	2	"Razor"
;f12e	3	"Dave"
;f133	4	"Michael"
;f13b	5	"Bernard"
;f143	6	"Wendy"
;f149	7	"Jeff"
;f14e	8	"Dr Fred"
;f14e	9	"Dr Fred"
;f156	A	"Nurse Edna"
;f161	B	"Weird Ed"
;f16a	C	"Dead Cousin Ted"
;f17a	D	"Purple Tentacle"
;f18a	E	"Green Tentacle"
;f199	F	"Meteor"
;f199	10	"Meteor"
;f1a0	11	"Plant"
;f1a0	12	"Plant"
;f1a0	13	"Plant"		- This seems to be the only plant actor used in scripts
;f1a6	14	"Sandy"
;f1a6	15	"Sandy"
;f1a6	16	"Sandy"		- Sandy in talk show and in the lab
;f1a6	17	"Sandy"		- Sandy
*/

* = $F122
.byte $20,$00                                      // " "
.byte $53,$79,$64,$00                              // "Syd"
.byte $52,$61,$7A,$6F,$72,$00                      // "Razor"
.byte $44,$61,$76,$65,$00                          // "Dave"
.byte $4D,$69,$63,$68,$61,$65,$6C,$00              // "Michael"
.byte $42,$65,$72,$6E,$61,$72,$64,$00              // "Bernard"
.byte $57,$65,$6E,$64,$79,$00                      // "Wendy"
.byte $4A,$65,$66,$66,$00                          // "Jeff"
.byte $44,$72,$20,$46,$72,$65,$64,$00              // "Dr Fred"
.byte $4E,$75,$72,$73,$65,$20,$45,$64,$6E,$61,$00  // "Nurse Edna"
.byte $57,$65,$69,$72,$64,$20,$45,$64,$00          // "Weird Ed"
.byte $44,$65,$61,$64,$20,$43,$6F,$75,$73,$69,$6E,$20,$54,$65,$64,$00  // "Dead Cousin Ted"
.byte $50,$75,$72,$70,$6C,$65,$20,$54,$65,$6E,$74,$61,$63,$6C,$65,$00  // "Purple Tentacle"
.byte $47,$72,$65,$65,$6E,$20,$54,$65,$6E,$74,$61,$63,$6C,$65,$00      // "Green Tentacle"
.byte $4D,$65,$74,$65,$6F,$72,$00                  // "Meteor"
.byte $50,$6C,$61,$6E,$74,$00                      // "Plant"
.byte $53,$61,$6E,$64,$79,$00                      // "Sandy"

* = $F20B
verb_pointers_hi:
.byte $F1,$F1,$F1,$F1
.byte $F1,$F1,$F1,$F1
.byte $F1,$F1,$F1,$F1
.byte $F1,$F1,$F2,$00

* = $F21B  
verb_pointers_lo:
.byte $AC,$B1,$B7,$BC
.byte $C4,$CD,$D1,$D9
.byte $E0,$E5,$EA,$EE
.byte $F3,$FB,$03

* = $F1AC
  .byte $4F,$70,$65,$6E,$00                              // "Open"
  .byte $43,$6C,$6F,$73,$65,$00                          // "Close"
  .byte $47,$69,$76,$65,$00                              // "Give"
  .byte $54,$75,$72,$6E,$20,$6F,$6E,$00                  // "Turn on"
  .byte $54,$75,$72,$6E,$20,$6F,$66,$66,$00              // "Turn off"
  .byte $46,$69,$78,$00                                  // "Fix"
  .byte $4E,$65,$77,$20,$4B,$69,$64,$00                  // "New Kid"
  .byte $55,$6E,$6C,$6F,$63,$6B,$00                      // "Unlock"
  .byte $50,$75,$73,$68,$00                              // "Push"
  .byte $50,$75,$6C,$6C,$00                              // "Pull"
  .byte $55,$73,$65,$00                                  // "Use"
  .byte $52,$65,$61,$64,$00                              // "Read"
  .byte $57,$61,$6C,$6B,$20,$74,$6F,$00                  // "Walk to"
  .byte $50,$69,$63,$6B,$20,$75,$70,$00                  // "Pick up"
  .byte $57,$68,$61,$74,$20,$69,$73,$00                  // "What is"

* = $F22A
preposition_pointers_hi:
.byte $F2,$F2,$F2,$F2

* = $F22E
preposition_pointers_lo:
.byte $33,$36,$3B,$3E

* = $F233
.byte $69,$6E,$00 // "in"
.byte $77,$69,$74,$68,$00 // "with"
.byte $6F,$6E,$00 // "on"
.byte $74,$6F,$00 // "to"

* = $F241
empty_msg:
.byte $20, $00
