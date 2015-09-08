      SUBROUTINE XYINCB(XY,LSTLEN,ILEVEL,IERR,KOVC,NUMINP,
     +                  COMFAC,DX,DY,ISX,ISY,LENST,LENEND,
     +                  KLEN,KALLIN,KINSID,KSAVE)
C
C
C
      CHARACTER PRBUF*80,KOVC*1
      LOGICAL EXIT,FINISH
      REAL XY(3,LSTLEN)
C
C  Set up which stars are wanted
C
      DO K = 1,LSTLEN
         XY(3,K) = 1.0
      ENDDO
      IF (KALLIN.EQ.2) THEN
         IF (LENST.NE.1) THEN
            DO K = 1,LENST-1
               XY(3,K) = 0.0
            ENDDO
         ENDIF
         IF (LENEND.NE.LSTLEN) THEN
            DO K = LENEND,LSTLEN
               XY(3,K) = 0.0
            ENDDO
         ENDIF
      ENDIF
C
C OPEN OVERLAY PLANE FOR WRITING CROSSES AND CLEAR IT IF REQUIRED
C GREEN CROSSES FOR INPUT LIST
C
      IF (KOVC.EQ.'N') THEN
         CALL ARGS_OVOPN(8,'B')
      ELSE
         CALL ARGS_OVOP(8,'B')
      ENDIF
C
C WRITE CROSSES FROM INPUT LIST POSNS
C
         CALL ARGS_NUMIM(IDARGS)
         DO K = LENST,LENEND
            XA = (XY(1,K)-DX+1.0-1.0)/COMFAC
            YA = (XY(2,K)-DY+1.0-1.0)/COMFAC
            IF ((XA.GE.1.0.AND.YA.GE.1.0
     +          .AND.XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY))
     +          .OR.(KINSID.EQ.2)) THEN
               IF (NUMINP.EQ.1) THEN
                  CALL CROSS(IDARGS,XA,YA)
               ELSE
                  CALL CROSSA(IDARGS,XA,YA,K)
               ENDIF
            ENDIF
         ENDDO
C
C  CLOSE INPUT LIST OVERLAY
C
      CALL ARGS_OVCL(8,.FALSE.)
      IF (KOVC.EQ.'N') THEN
         CALL ARGS_OVOPN(9,'R')
      ELSE
         CALL ARGS_OVOP(9,'R')
      ENDIF
C
C  Write header for typing output
C
      IF (ILEVEL.EQ.2) THEN
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('    No     X shift    Y shift',ISTAT)
      ENDIF
C
C LOOP WHILE EXIT HAS NOT BEEN SET AND LIST HAS NOT OVERFLOWED
C ------------------------------------------------------------
C
      CALL ARGS_NUMIM(IDARGS)
      LENA = LENST - 1
	EXIT=.FALSE.
      DO WHILE (.NOT.EXIT)
         LENA = LENA + 1
C
C  PUT CURSOR AT NEXT POSN
C
         XA = (XY(1,LENA)-DX)/COMFAC
         YA = (XY(2,LENA)-DY)/COMFAC
         IF ((XA.GE.1.0.AND.YA.GE.1.0
     +       .AND.XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY))
     +       .OR.(KINSID.EQ.2)) THEN
            CALL ARGS_UTOA(IDARGS,XA,YA,KPX,KPY,ISTAT)
            CALL ARCEMA(KPX,KPY,KFX,KFY,0)
C
C CALL ASP_PAN TO GET COORDINATES FROM ARGS SCREEN
C AND DRAW A CROSS AT THAT POSITION
C
             CALL ASP_PAN(IX,IY,XA,YA)
C
             CALL CROSS(IDARGS,XA,YA)
C
C  Test if this means finish
C
             FINISH = (XA.LE.1.0.AND.YA.LE.1.0)
             IF (.NOT.FINISH) THEN
C
C  Test if remove this point
C
                IF (XA.LE.1.0.AND.YA.GE.REAL(ISY)) THEN
                   XY(3,LENA) = 0.0
                ELSE
C
C  If real position, store, and type out if wanted
C
                   XN = XA*COMFAC + 1.0 + DX - 1.0
                   YN = YA*COMFAC + 1.0 + DY - 1.0
                   KXN = XN - XY(1,LENA)
                   KYN = YN - XY(2,LENA)
                   IF (ILEVEL.GE.2) THEN
                      WRITE(PRBUF,64) LENA,KXN,KYN
   64                 FORMAT(' ',I5,5X,I7,4X,I7)
                      CALL WRUSER(PRBUF,ISTAT)
                   ENDIF
                   XY(1,LENA) = XN
                   XY(2,LENA) = YN
                ENDIF
             ENDIF
          ENDIF
C
C  IF REACHED END OF LIST, OR USER FINISHED, EXIT
C
         IF((LENA.EQ.LENEND).OR.FINISH)EXIT=.TRUE.
      ENDDO
C
C
C
      CALL ARGS_OVCL(9,.FALSE.)
C
C  Reject those outside displayed area if so wanted
C
      IF (KSAVE.EQ.2) THEN
         DO K = 1,LSTLEN
            XA = (XY(1,K)-DX)/COMFAC
            YA = (XY(2,K)-DY)/COMFAC
            IF ((XA.LT.1.0.OR.YA.LT.1.0
     +           .OR.XA.GT.REAL(ISX).OR.YA.GT.REAL(ISY))
     +           .OR.(KINSID.EQ.2)) THEN
               XY(3,K) = 0.0
            ENDIF
         ENDDO
      ENDIF
C
C  Add up how many positions to be stored
C
      KLEN = 0
      DO K = 1,LSTLEN
         IF (XY(3,K).GT.0.5) KLEN = KLEN + 1
      ENDDO
C
C
C
	END
C
C
C
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
