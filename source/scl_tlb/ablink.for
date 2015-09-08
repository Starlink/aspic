!
!+	procedure ABLINK
!
!	display two images, one in bottom 8 bits of ARGS memory, the
!	other in top 8 bits, then run BLINKER program which allows
!	use of trackerball buttons to flip between the two, both
!	manually and automatically.
!
!
ACLEAR
HEXFILE FLNAME=MASK8
ADISP XC= YC=
HEXFILE FLNAME=ZPAFLIP
HEXFILE FLNAME=MASK8
ADISP XC= YC=
HEXFILE FLNAME=ZPABACK
BLINKER
LOOK BLINKER
HEXFILE FLNAME=MASK8
HEXFILE FLNAME=ZPABACK
HEXFILE FLNAME=MASK8
