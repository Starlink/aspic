      SUBROUTINE QUAD(IXR,IYR,ISUB)                                             
C+
C   QUAD
C
C     REQUESTS USER FOR QUADRANT
C	
C     RETURNED:	(ARGUMENTS)
C   IXR         X-coordinate of left edge of quadrant
C   IYR         Y-coordinate of bottom edge of quadrant
C   ISUB        quadrant number
C
C     JAC/UOE/1981
C-
 
      CHARACTER*1 RPLY
 
      CALL READC('REPLY','SPECIFY QUADRANT','A','A','D',RPLY,IST)
      IF(RPLY.EQ.'A')THEN
        IXR=256
        IYR=256
        ISUB=1
      ELSE IF(RPLY.EQ.'B')THEN
        IXR=0
        IYR=256
        ISUB=2
      ELSE IF(RPLY.EQ.'C')THEN
        IXR=0
        IYR=0
        ISUB=3
      ELSE IF(RPLY.EQ.'D')THEN
        IXR=256
        IYR=0
        ISUB=4
      ENDIF
 
      END
C
C
C
C
C
C***************************************************************
C***************************************************************
