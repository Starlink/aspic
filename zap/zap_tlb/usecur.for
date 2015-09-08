      SUBROUTINE USECUR(IDEV,AMIN)
C
C
C
      CHARACTER*1 COL(4)
      CHARACTER*72 TEXT
      LOGICAL LOOP
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C  See if want to use cursor
C
      KW = 2
      CALL GETCMD('CURSOR','YES,NO.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('CURSOR',ISTAT)
      IF (KW.EQ.1) THEN
C
C         Put out how to use
C
         IF (IDEV.EQ.3.OR.IDEV.EQ.4) THEN
            CALL WRUSER('TYPE SPACE TO GET',JSTAT)
            CALL WRUSER('POSN',JSTAT)
            CALL WRUSER('SET TO LEFT OF BOX',JSTAT)
            CALL WRUSER('TO EXIT',JSTAT)
            CALL WRUSER(' ',JSTAT)
         ENDIF
         IF (IDEV.EQ.2) THEN
            CALL WRUSER('PRESS WHITE BUTTON FOR POSN ',JSTAT)
            CALL WRUSER('SET TO LEFT OF BOX TO EXIT',JSTAT)
            CALL WRUSER(' ',JSTAT)
         ENDIF
C
C        Get posns and put out as values, until posn to left of diag pic
C
         LOOP = .TRUE.
         DO WHILE (LOOP)
            CALL CURSOR(XA,YA)
            IF (XA.GE.AMIN) THEN
               WRITE(TEXT,902)YA,XA
  902          FORMAT(1H ,G14.4,2X,G14.4)
               CALL WRUSER(TEXT,JSTAT)
            ELSE
               LOOP = .FALSE.
            ENDIF
         ENDDO
C
C
C
      ENDIF
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOLINE *
C      *            *
C      **************
C
C
C      This s/r puts line on the display
C
C --------------------------------------------------------------
C
C
C
