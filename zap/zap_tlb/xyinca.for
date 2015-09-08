      SUBROUTINE XYINCA(ID,X,Y,MAXLEN,LEN,IDENT,ILEVEL,IERR,KOVCL,
     +                  KFXOUT,NUMINP,COMFAC,DX,DY,ISX,ISY,KOPT,TCOL)
C
C
C
      CHARACTER IDBUF*20,INBUF*80,PRBUF*80,TCOL*1
      LOGICAL EXIT,IDENT,FINISH
      REAL X(MAXLEN),Y(MAXLEN)
      BYTE ID(20,MAXLEN)
C
C CHECK ARGUMENTS
C
      IF(MAXLEN.LT.LEN) THEN
	IERR=1
      ELSE
	IERR=0
C
C CHECK LENGTH OF LIST IS NOT -VE, INITIALLISE BLANK IDENTIFIER
C COUNT TO AFTER INPUT LIST
C
      LEN=MAX(0,LEN)
      NBLANK= LEN + 1
C
C  Clear ARGS overlay planes if wanted
C
      IF (KOVCL.EQ.1) THEN
         DO L = 8,15
            CALL ARGS_CLS(L)
         ENDDO
      ENDIF
C
C WRITE CROSSES FROM INPUT LIST POSNS IF REQUIRED
C
      IF (KFXOUT.EQ.1.AND.LEN.GT.0) THEN
         IF (TCOL.EQ.'B') NPLANE = 8
         IF (TCOL.EQ.'W') NPLANE = 9
         IF (TCOL.EQ.'R') NPLANE = 10
         IF (TCOL.EQ.'Y') NPLANE = 11
         IF (TCOL.EQ.'C') NPLANE = 12
         IF (TCOL.EQ.'M') NPLANE = 13
         IF (TCOL.EQ.'G') NPLANE = 14
         CALL ARGS_OVOPN(NPLANE,TCOL)
         CALL ARGS_NUMIM(IDARGS)
         DO K = 1,LEN
            KX = INT(X(K))
            KY = INT(Y(K))
            XA = (REAL(KX)-DX+1.0-1.0)/COMFAC
            YA = (REAL(KY)-DY+1.0-1.0)/COMFAC
            IF (XA.GE.1.0.AND.YA.GE.1.0.AND.
     +          XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY)) THEN
               NUM = K
               IF (NUMINP.EQ.1) NUM = -1
               KN = -1
               IF (MOD(K,2).EQ.0) KN = 1
               IF (KOPT.EQ.1) CALL CROSS(IDARGS,XA,YA,NUM,TCOL)
               IF (KOPT.EQ.2) CALL ATICK(IDARGS,XA,YA,KN,NUM,TCOL)
               IF (KOPT.EQ.3) CALL SPOT(IDARGS,XA,YA,NUM,TCOL)
            ENDIF
         ENDDO
      CALL ARGS_OVCL(NPLANE,.FALSE.)
      ENDIF
C
C
C  Get positions from ARGS using cursor and marking positions
C LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
C ------------------------------------------------------------
C
      CALL ARGS_OVOPN(10,'R')
      CALL ARGS_NUMIM(IDARGS)
	EXIT=.FALSE.
   67 CONTINUE
      IF((.NOT.EXIT).AND.(LEN.LE.MAXLEN)) THEN
C
C CALL ASP_PAN TO GET COORDINATES FROM ARGS SCREEN
C AND DRAW A CROSS AT THAT POSITION
C
          CALL ASP_PAN(IX,IY,XA,YA)
C
          IF (KOPT.EQ.1.OR.KOPT.EQ.2) CALL CROSS(IDARGS,XA,YA,-1,'R')
          IF (KOPT.EQ.3) CALL SPOT(IDARGS,XA,YA,-1,'R')
          X(LEN+1) = COMFAC*XA + 1.0 + DX - 1.0
          Y(LEN+1) = COMFAC*YA + 1.0 + DY - 1.0
C
C TEST IF USER WANTS TO STOP
C
          FINISH=((X(LEN+1).LE.DX).AND.(Y(LEN+1).LE.DY))
C
C IF NOT, PRINT POSITION IF REQUIRED
C
          IF(.NOT.FINISH) THEN
            IF(ILEVEL.GE.2) THEN
              KN = LEN + 1
              KXN = INT(X(KN)+0.01)
              KYN = INT(Y(KN)+0.01)
              WRITE(PRBUF,64) KN,KXN,KYN
   64         FORMAT(' ',I5,5X,2I7)
              CALL WRUSER(PRBUF,ISTAT)
            ENDIF
C
C IF IDENTIFIER REQUIRED, OBTAIN FROM ENVIRONMENT
C
          INBUF=' '
            IF(IDENT) THEN
	      CALL RDKEYC('IDENTITY',.FALSE.,1,INBUF,NVAL,ISTAT)
              CALL CNPAR('IDENTITY',ISTAT)
              CALL LBGONE(INBUF)
            ENDIF
            IDBUF=INBUF
          ENDIF
C
C IF STOPPING, SET EXIT
C
          IF(FINISH) THEN
            EXIT=.TRUE.
          ELSE
C
C TEST IF LIST OF INPUT HAS OVERFLOWED
C
              IF(LEN.GE.MAXLEN) THEN
		EXIT=.TRUE.
	      ELSE
C
C INCREMENT LIST LENGTH IF IT WILL NOT OVERFLOW
C
		LEN=LEN+1
		EXIT=.FALSE.
C
C TREAT THE SPECIAL CASES OF BLANK IDENTIFIER OR '#N'
C ----------------------------------------------------
C
C REMOVE LEADING BLANKS FROM IDENTIFIER AND TEST IF ALL BLANK
C
		CALL LBGONE(IDBUF)
		IF(IDBUF.EQ.' ') THEN
C
C IF BLANK, GENERATE AN IDENTIFIER FROM THE BLANK COUNT IN THE FORM
C '#N' AND INCREMENT THE BLANK COUNT
C
		  WRITE(IDBUF,'(I20)')NBLANK
		  IDBUF(1:1)='#'
	          CALL LBGONE(IDBUF(2:))
		  NBLANK=NBLANK+1
C
C IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
C IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
C RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
C SEQUENTIALLY NUMBERED '#N' FORM
C
		ELSE IF(IDBUF(1:1).EQ.'#') THEN
		  CALL CTOI(IDBUF(2:),NB,ISTATB)
		  IF(ISTATB.EQ.0) THEN
		    NBLANK=NB+1
		    WRITE(IDBUF,'(I20)')NB
		    IDBUF(1:1)='#'
		    CALL LBGONE(IDBUF(2:))
		  ENDIF
		ENDIF
C
C PUT ID INTO IDENTIFIER LIST
C
		DO 16 I=1,20
		  ID(I,LEN)=ICHAR(IDBUF(I:I))
   16		CONTINUE
	      ENDIF
	    ENDIF
C
C IF LIST IS FULL, RETURN
C
	    IF(LEN.GE.MAXLEN) THEN
	      EXIT=.TRUE.
	    ENDIF
	    GO TO 67
	  ENDIF
      CALL ARGS_OVCL(10,.FALSE.)
	ENDIF
C
	END
C
C
C
 
 
 
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C