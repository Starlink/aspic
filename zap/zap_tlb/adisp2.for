      SUBROUTINE ADISP2(PIC,NX,NY,TRIM,LLOG,IXC,IYC,VLO,VHI,IPIC)
      REAL PIC(NX*NY),VLO,VHI,VL,D,SCALE,V
      INTEGER NX,NY,IXC,IYC
      INTEGER*2 IPIC(NX*NY),IDUMMY
      LOGICAL TRIM,LLOG
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
      DO I=1,NX*NY
         IF (LLOG) THEN
            IF (VL.LT.1E-10) THEN
               V=SCALE*LOG(1E10*(PIC(I)-VL)+1)
            ELSE
		IF (PIC(I).EQ.0) THEN
			V=0.0
		ELSE
              		 V=SCALE*LOG(PIC(I)/VL)
		ENDIF
            ENDIF
         ELSE IF (TRIM) THEN
            V=SCALE*(PIC(I)-VL)
         ELSE
            V=MOD(PIC(I),32768.0)
         ENDIF
         IPIC(I)=NINT(MIN(MAX(V,0.0),255.0))
      END DO
      CALL SRPXI2(IPIC,NX,NX,NY,IXC-NX/2,IYC-NY/2,
     &               16,.FALSE.,IDUMMY,1)
      END
