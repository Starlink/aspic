      SUBROUTINE CONTOUR(IDEV,SIZE,HT,NH,STSAM,ENSAM,
     1                  STREC,ENREC,IDENS,IX,IY,ITITLE,
     1                  IBLK,PENS)
      INTEGER STSAM,ENSAM,STREC,ENREC
      REAL*4 IDENS(IX,IY)
      INTEGER*4 ITITLE(10)
      REAL*4 XD(2),YD(2),AX,AY,HT(30)
      CHARACTER CURPOS*15,CHEIGHT*40
      LOGICAL PENS
      IF (IX.GE.IY) THEN
         AX=SIZE
         AY = AX * REAL(IY)/REAL(IX)
      ELSE
         AY=SIZE
         AX = AY * REAL(IX)/REAL(IY)
      ENDIF
      XD(1)=FLOAT(STSAM)
      XD(2)=FLOAT(ENSAM)
      YD(1)=FLOAT(STREC)
      YD(2)=FLOAT(ENREC)
      LC = 1 + ENSAM -STSAM
      LR = 1 + ENREC - STREC
      CALL JBAXES(XD,2,AX,'X-AXIS',6,YD,2,AY,'Y-AXIS',6)
      CALL TITLE(1,2,ITITLE,40)
      IF(IBLK.NE.1) THEN
      LC = LC/IBLK
      LR = LR/IBLK
      ENDIF
      DO 116 K=1,NH
        IF (PENS) THEN
		IF(IDEV.EQ.1.OR.IDEV.EQ.15) THEN
		MP = MOD(K,10)+1
		ELSE
		MP = MOD(K,3)+1
		ENDIF
	      CALL PEN(MP)
        ENDIF
      WRITE(CHEIGHT,9999) HT(K)
9999  FORMAT('DRAWING CONTOUR AT HEIGHT', F8.1)
      CALL WRUSER(CHEIGHT,ISTATUS)
116   CALL LEVEL(HT(K),IDENS,XD(1),REAL(IBLK),LC,
     1                        YD(1),REAL(IBLK),LR)
      CALL END PLT
      RETURN
      END
