      SUBROUTINE REBIN(INPUT,N,OUTPUT,M,BINEND,MM)                              
C+
C   REBIN
C
C     1-D GENERAL-PURPOSE REBIN
C
C     GIVEN:	(ARGUMENTS)
C     INPUT - INPUT DATA ARRAY
C     BINEND - ARRAY OF BINENDS OF OUTPUT ARRAY, REFERRED TO INPUT
C              FIRST INPUT BIN STARTS AT BINEND ZERO.
C     LENGTH OF INPUT MUST CONTAIN OUTPUT
C     N   - NO. OF INPUT BINS
C     M   - NO. OF OUTPUT BINS
C     MM  - M+1
C
C   Returned   (arguments)
C   OUTPUT      output data array
C
C   J.A.Cooke/UOE/1981
C-
      REAL INPUT(N),OUTPUT(M),BINEND(MM),NEXTIN
C
C     LOOP FOR EACH OUTPUT BIN
C
      DO 90 I=1,M
      START=BINEND(I)
      FINISH=BINEND(I+1)
      OUT=0
C
C     FOR EACH BIN:
C
      NEXTIN=INT(START)+1.
   20 IF(NEXTIN.GT.FINISH) GO TO 70
      FRACT=(NEXTIN-START)*INPUT(INT(START)+1)
      OUT=OUT+FRACT
      START=NEXTIN
      NEXTIN=NEXTIN+1
      GO TO 20
   70 FRACT=(FINISH-START)*INPUT(INT(START)+1)
      OUT=OUT+FRACT
C
C     FINISHED THE BIN
C
   90 OUTPUT(I)=OUT
      RETURN
      END
C
C
C
C
C
C****************************************************************
C****************************************************************
