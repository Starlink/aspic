      SUBROUTINE CHKLIM(XPOS,YPOS,CSIZ)                                         
C+
C     LOCAL VARIABLES
C
C       GIVEN:		(ARGUMENTS)
C	 CSIZ		XPOS
C	 YPOS
C
C	RETURNED	(ARGUMENTS)
C	   XPOS		YPOS
C
C	J.A COOKE/UOE/1981
C
C     CHECKS LIMITS ON SQUARE CURSOR POSITION
C-
      INTEGER XPOS,YPOS,CSIZ(2),LOLIMX,HILIMX,LOLIMY,HILIMY,
     -  OFSX,OFSY
      LOLIMX=1
      HILIMX=510-CSIZ(1)
      LOLIMY=1
      HILIMY=510-CSIZ(2)
      IF(XPOS.LT.LOLIMX)XPOS=LOLIMX
      IF(XPOS.GT.HILIMX)XPOS=HILIMX
      IF(YPOS.LT.LOLIMY)YPOS=LOLIMY
      IF(YPOS.GT.HILIMY)YPOS=HILIMY
      RETURN
      END
C
C
C
C***************************************************************
C***************************************************************
