      SUBROUTINE ARGROW2(PIC,ICOL,IROW,I1,I2,J,IBUFF,ISIZE,
     1          TRIM,LLOG,VLO,VHI,IXOR,IYOR)
      INTEGER*2 IBUFF(ISIZE)
	REAL PIC(ICOL,IROW)
	LOGICAL TRIM,LLOG
      INTEGER*2 I2DUMMY
C
      K=0
      VL=VLO
      D=VHI-VLO
      IF (ABS(D).LT.1E-10) D=1E10
      IF (LLOG) THEN
         IF (VL.LT.1E-10) THEN
            SCALE=255.0/LOG(1E10*D+1)
         ELSE
            SCALE=255.0/LOG(VHI/VLO)
         ENDIF
      ELSE IF (TRIM) THEN
         SCALE=255.0/D
      ENDIF
	K=0
      DO I=I1,I2
	K=K+1
         IF (LLOG) THEN
            IF (VL.LT.1E-10) THEN
               V=SCALE*LOG(1E10*(PIC(I,J)-VL)+1)
            ELSE
		IF (PIC(I,J).EQ.0) THEN
			V=0.0
		ELSE
	               V=SCALE*LOG(PIC(I,J)/VL)
		ENDIF
            ENDIF
         ELSE IF (TRIM) THEN
            V=SCALE*(PIC(I,J)-VL)
         ELSE
            V=MOD(PIC(I,J),32768.0)
         ENDIF
         IBUFF(K)=NINT(MIN(MAX(V,0.0),255.0))
      END DO
      NX1=IXOR+I1-1
      NY1=IYOR+J-1
      NP=I2-I1+1
      CALL SRPXI2(IBUFF,ISIZE,NP,1,NX1,NY1,16,.FALSE.,I2DUMMY,1)
      END
