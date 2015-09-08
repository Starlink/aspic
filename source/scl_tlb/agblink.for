!
!+	procedure AGBLINK
!
!	display two images, one in bottom 8 bits of ARGS memory, the
!	other in top 8 bits, then run GBLINKER program which allows
!	use of trackerball buttons to flip between the two, both
!	manually and automatically.
!
!	the GBLINKER program allows the use of a different LUT
!	for each of the two images.
!
!
ACLEAR
HEXFILE FLNAME=MASK8
ADISP XC= YC=
HEXFILE FLNAME=ZPAFLIP
HEXFILE FLNAME=MASK8
ADISP XC= YC=
HEXFILE FLNAME=ZPABACK
GBLINKER
LOOK GBLINKER
HEXFILE FLNAME=MASK8
HEXFILE FLNAME=ZPABACK
HEXFILE FLNAME=MASK8
