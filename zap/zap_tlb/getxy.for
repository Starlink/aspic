      SUBROUTINE GETXY(KXPOS,KYPOS,KXS,KYS)
C
C
C
      INTEGER KXPOS(2),KYPOS(2)
      REAL UX(2),UY(2)
      INTEGER KX(2),KY(2)
      CHARACTER*72 TEXT,TEXTAR
C
C
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C  Get if ARGS wanted and display factors
C
      KCUR = 1
      CALL GETCMD('CURSOR','YES,NO.',1,KCUR,TEXT,KTEXT,ISTAT)
      IF (KCUR.EQ.1) THEN
         CALL ARGS_NUMIM(ID)
         CALL SRINIT(0,.FALSE.,KASTAT)
         CALL ARGS_RDPAR('COMPRE',1,TEXTAR,NVALS,ISTAT)
         IF (ISTAT.EQ.0) THEN
            READ(TEXTAR,987)KXB,KXE,KYB,KYE,KCOMP
  987       FORMAT(5I10)
            COMFAC = REAL(KCOMP)
            DX = REAL(KXB)
            DY = REAL(KYB)
         ELSE
            COMFAC = 1.0
            DX = 1.0
            DY = 1.0
         ENDIF
      ELSE
         KASTAT = 1
      ENDIF
      CALL CNPAR('CURSOR',ISTAT)
C
C If ARGS is wanted, use cursor to get corners of area
C
      IF (KASTAT.EQ.0) THEN
C
         CALL ARGS_CUROP('14','G')
	CALL BLABELS(9,'Y','RESET','SMALLER','BIGGER','CORNER')
         CALL ASP_PAN(IX,IY,UX(1),UY(1))
         KX(1) = COMFAC*UX(1) + 1.0 + DX - 1.0
         KY(1) = COMFAC*UY(1) + 1.0 + DY - 1.0
         CALL ARGS_OVOP(8,'G')
         CALL CROSS(ID,UX(1),UY(1))
C
         CALL ASP_PAN(IX,IY,UX(2),UY(2))
         KX(2) = COMFAC*UX(2) + 1.0 + DX - 1.0
         KY(2) = COMFAC*UY(2) + 1.0 + DY - 1.0
         CALL ARGS_CURCL
C
         CALL ARGS_OVCL(8,.FALSE.)
         CALL ARGS_OVOP(8,'G')
         CALL RECTAN(ID,UX,UY)
         CALL ARGS_OVCL(8,.FALSE.)
	CALL ARGS_OVCLR(9)
C
      ELSE
C
C If keyboard wanted, ask it
C
         KX(1) = 1
         KX(2) = KXS
         CALL RDKEYI('XAREA',.TRUE.,2,KX,NVAL,IERR)
         CALL CNPAR('XAREA',ISTAT)
         KY(1) = 1
         KY(2) = KYS
         CALL RDKEYI('YAREA',.TRUE.,2,KY,NVAL,IERR)
         CALL CNPAR('YAREA',ISTAT)
      END IF
C
C  Check for out of range
C
      IF (KX(1).LT.1) KX(1) = 1
      IF (KX(2).LT.1) KX(2) = 1
      IF (KY(1).LT.1) KY(1) = 1
      IF (KY(2).LT.1) KY(2) = 1
      IF (KX(1).GT.KXS) KX(1) = KXS
      IF (KX(2).GT.KXS) KX(2) = KXS
      IF (KY(1).GT.KYS) KY(1) = KYS
      IF (KY(2).GT.KYS) KY(2) = KYS
C
C   Now unscramble the bottom left hand corner and
C   the top right hand one.
C
      KXPOS(1)=MIN(KX(1),KX(2))
      KXPOS(2)=MAX(KX(1),KX(2))
      KYPOS(1)=MIN(KY(1),KY(2))
      KYPOS(2)=MAX(KY(1),KY(2))
C
C  If cursor used type out posn
C
      IF (KASTAT.EQ.0) THEN
         WRITE(TEXT,900)KXPOS
  900    FORMAT(' ','X RANGE = ',2I6)
         CALL WRUSER(TEXT,ISTAT)
         WRITE(TEXT,901)KYPOS
  901    FORMAT(' ','Y RANGE = ',2I6)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RECTAN *
C      *            *
C      **************
C
C S/R TO DRAW A RECTANGLE ON THE ARGS
C
C
C ----------------------------------------------------
C
C
C
