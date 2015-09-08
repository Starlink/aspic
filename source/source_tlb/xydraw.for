C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   XYDRAW  *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               XYDRAW
C
C
C          FUNCTION:-
C              Draws out a graph of a set of pairs of numbers taken
C              from two XYlists as values of X vs Y.
C              The numbers are taken from file A, parameter no A1 and file
C              B, parameter no B1, where A,A1,B,B1 are input by the user.
C              The  output  can be put out on the Args, the Tek, or the
C              Versatec, the Calcomp, the GOC, or the CC81.
C              Lines can also be drawn on the diagram from keyboard,
C              cursor or file posns.
C
C
C          USE:-
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              The 1st input XY list
C
C         PARNOA                              The paramter number in the
C                                             first file to take the
C                                             X values from.
C
C         INPUTB                              The 2nd input file.
C
C         PARNOB                              The parameter number in the
C                                             second file to take the Y
C                                             values from.
C
C         DEVICE         ARGS                 The display device for the
C                                             graph. Choices are ARGS,GOC,
C                                             TEKTRONIX,VERSATEC,CC81,
C                                             CALCOMP.
C
C         DEVSIZE        Various              For graphs,the
C                                             size of the picture.
C
C         DEVLIMX        Min,Max              For graphs, the ranges of Xs
C         DEVLIMY                             and Ys to be plotted out.
C
C         DOTNUM         No                   Flag for points plotted out
C                                             to have the number of the star
C                                             in the output list to be
C                                             plotted by the point. Choices
C                                             are YES/NO.
C
C         TEXTX                               Text to write on X axis.
C
C         TEXTY          As for TEXTX         Text to write on Y axis.
C
C         LINE1          None                 Flag for method of putting
C         LINE2                               line on diagram. Choices are
C                                             CURSOR,FILE,KEYBOARD,NONE.
C                                             XY points are joined
C                                             together with straight lines
C                                             starting with the 1st and
C                                             finishing with the last before
C                                             a null entry. In FILE and KEY
C                                             a break in the line is made
C                                             by a repeat entry. In CURSOR
C                                             an exit is made by placing it
C                                             to the left of the diagram.
C                                             LINE1 is asked for first, and
C                                             if used, asked again as LINE2
C                                             until not used.
C
C         FILE                                If FILE was chosen, this is
C                                             the XY list file containing
C                                             the posns. (A posn exactly the
C                                             same as the previous one means
C                                             a no line to be drawn between
C                                             the posns before and after it.)
C
C         XY                                  If KEYBOARD chosen, this is
C                                             the X,Y posns to input. If
C                                             FILE chosen, this is the XY
C                                             offset to apply to all the
C                                             posns.
C
C         TEXT                                For hard-copy output,this
C                                             is some text to be put below
C                                             the diagram.
C
C         CURSOR         No                   Flag for using cursor on
C                                             devices with a cursor, to
C                                             get magnitudes of posns on
C                                             the diagram. If YES, then
C                                             cursor is enabled, if NO not.
C
C         AGAIN           No                  Flag to output the display
C                                             again. Choices are YES/NO.
C
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Request for magnitudes at that position to  be  typed
C                     out
C
C         WHITE 2     as 1
C
C         WHITE 3     as 1
C
C         RED   4     as 1
C
C
C
C
C
C
C         A.J.Penny                RGO                            82-11-1
C
C
C--------------------------------------------------------------------------



C
C
C
      LOGICAL VALID,LOOP
      CHARACTER*72 TEXT
      INTEGER TEXTA(15)
      EQUIVALENCE (TEXTA(1),TEXT)
      REAL SIZE(2)
      CHARACTER CVAL*1
C
C
C
      VALID = .TRUE.
C
C Open input files
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEMA,LSTLA,IPINA,IERRXY)
      IF (IERRA.NE.0) THEN
         CALL WRUSER('BAD FILE',ISTAT)
         VALID = .FALSE.
      ENDIF
      IF (VALID) THEN
         ALIMIT = NITEMA - 5
         CALL GETPAR('PARNOA','INTEGER',1,1.0,ALIMIT,.FALSE.,KPARA,
     +               RVAL,IERRB)
          IF (IERRB.NE.0) THEN
             CALL WRUSER('BAD PARAMETER',ISTAT)
             VALID = .FALSE.
          ENDIF
      ENDIF
      IF (VALID) THEN
         CALL GTXYLR('INPUTB',.FALSE.,NITEMB,LSTLB,IPINB,IERRC)
         IF (IERRC.NE.0) THEN
            CALL WRUSER('BAD FILE',ISTAT)
            VALID = .FALSE.
         ENDIF
         IF (LSTLB.NE.LSTLA) THEN
            CALL WRUSER('FILES DIFFERENT LENGTHS',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
      IF (VALID) THEN
         ALIMIT = NITEMB - 5
         CALL GETPAR('PARNOB','INTEGER',1,1.0,ALIMIT,.FALSE.,KPARB,
     +               RVAL,IERRD)
         IF (IERRD.NE.0) THEN
            CALL WRUSER('BAD PARAMETER',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Extract data
C
      IF (VALID) THEN
         CALL GETDYN('IWX',204,LSTLA,IPX,IERR)
         IF (IERR.NE.0) THEN
             CALL WRUSER('CANT GET WORKING SPACE',ISTAT)
             VALID = .FALSE.
         ELSE
            CALL GETDYN('IWY',204,LSTLA,IPY,IERRA)
            IF (IERRA.NE.0) THEN
               CALL WRUSER('CANT GET WORKING SPACE',ISTAT)
               VALID = .FALSE.
            ELSE
               CALL GETDAT(%VAL(IPINA),NITEMA,LSTLA,%VAL(IPINB),
     +                     NITEMB,KPARA,KPARB,%VAL(IPX),%VAL(IPY))
            ENDIF
         ENDIF
      ENDIF
C
C
C
      IF (VALID) THEN
C
C  Type no of stars
C
         WRITE(TEXT,905)LSTLA
  905    FORMAT(1H ,'NUMBER OF STARS = ',I6)
         CALL WRUSER(TEXT,JSTAT)
C
C  Display HR diagram
C
         LOOP = .TRUE.
         DO WHILE (LOOP)
C
C  Plot HR diagram
C
            CALL DEVOPN(IDEV,SIZE)
            IF (IDEV.NE.1) THEN
               CALL DODIAG(SIZE,%VAL(IPX),%VAL(IPY),LSTLA,AMIN)
C
C  Draw a wanted line
C
               CALL PUTLIN(IDEV,SIZE,AMIN)
C
C  Use a wanted cursor (if can) to get posns on diag
C
               IF (IDEV.EQ.2.OR.IDEV.EQ.3.OR.IDEV.EQ.4) THEN
                  CALL USECUR(IDEV,AMIN)
               ENDIF
C
C  If outputting to hard copy, write a caption
C
               IF (IDEV.EQ.5.OR.IDEV.EQ.6.OR.IDEV.EQ.8) THEN
                  CALL WRUSER('TEXT TO WRITE BELOW DIAG IS ?',ISTAT)
                  TEXT = ' '
                  CALL RDKEYC('TEXT',.TRUE.,1,TEXT,I,ISTAT)
                  CALL CNPAR('TEXT',ISTAT)
                  CALL TITLE(2,2,TEXTA,60)
               ENDIF
               CALL DEVCLS(IDEV)
            ENDIF
C
C  See if display again
C
            KW = 2
            CALL GETCMD('AGAIN','YES,NO.',1,KW,TEXT,KTEXT,IERR)
            CALL CNPAR('AGAIN',ISTAT)
            IF (KW.EQ.2) LOOP = .FALSE.
C
C
C
         ENDDO
C
C
C
      ENDIF
C
C  Free data areas
C
      CALL FRDATA(' ',JSTAT)
C
C
C
	END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DODIAG *
C      *            *
C      **************
C
C
C
C -------------------------------------------------------------
C
C
C
       SUBROUTINE DODIAG(SIZE,XX,YY,LSTLEN,AMIN)
C
C
C
      CHARACTER*1 DOTS
      CHARACTER*72 TEXT
      REAL XX(LSTLEN),YY(LSTLEN),AX(2),AY(2),SIZE(2)
      CHARACTER*12 TEXTC
      CHARACTER*12 TEXTA(1),TEXTB(1)
      INTEGER TIA,TIB
      EQUIVALENCE (TEXTB(1),TIA)
      EQUIVALENCE (TEXTA(1),TIB)
C
C  Get ranges of values
C
      XMIN = XX(1)
      XMAX = XX(1)
      YMIN = YY(1)
      YMAX = YY(1)
      DO L = 1,LSTLEN
         IF(XX(L).GT.XMAX) XMAX = XX(L)
         IF(XX(L).LT.XMIN) XMIN = XX(L)
         IF(YY(L).GT.YMAX) YMAX = YY(L)
         IF(YY(L).LT.YMIN) YMIN = YY(L)
      ENDDO
      WRITE(TEXT,920)XMIN,XMAX
  920 FORMAT(' ','X RANGE = ',2G13.6)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER('ENTER X PLOT LIMITS',ISTAT)
      AX(1) = XMIN - 0.05*(XMAX-XMIN)
      AX(2) = XMAX + 0.05*(XMAX-XMIN)
      IF (AX(1).EQ.AX(2)) THEN
         AX(1) = AX(1) - 1.0
         AX(2) = AX(2) + 1.0
      ENDIF
      CALL RDKEYR('DEVLIMX',.TRUE.,2,AX,I,ISTAT)
      CALL CNPAR('DEVLIMX',ISTAT)
      WRITE(TEXT,921)YMIN,YMAX
  921 FORMAT(' ','Y RANGE = ',2G13.6)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER('ENTER Y PLOT LIMITS',ISTAT)
      AY(1) = YMIN - 0.05*(YMAX-YMIN)
      AY(2) = YMAX + 0.05*(YMAX-YMIN)
      IF (AY(1).EQ.AY(2)) THEN
         AY(1) = AY(1) - 1.0
         AY(2) = AY(2) + 1.0
      ENDIF
      CALL RDKEYR('DEVLIMY',.TRUE.,2,AY,I,ISTAT)
      CALL CNPAR('DEVLIMY',ISTAT)
      KW = 2
      CALL GETCMD('DOTNUM','YES,NO.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('DOTNUM',ISTAT)
      IF (KW.EQ.1) THEN
         DOTS = 'Y'
      ELSE
         DOTS = 'N'
      ENDIF
      XL = SIZE(1)
      YL = SIZE(2)
C
C
      CALL NEWPLT(AX(1),AX(2),XL,AY(1),AY(2),YL)
      CALL WRUSER(' CAPTION FOR X AXIS IS ?',ISTAT)
      TEXTC = 'X'
      CALL RDKEYC('TEXTX',.TRUE.,1,TEXTC,I,ISTAT)
      CALL CNPAR('TEXTX',ISTAT)
      WRITE (TEXTA(1),902)TEXTC
  902 FORMAT(A12)
      CALL WRUSER(' CAPTION FOR Y AXIS IS ?',ISTAT)
      TEXTC = 'Y'
      CALL RDKEYC('TEXTY',.TRUE.,1,TEXTC,I,ISTAT)
      CALL CNPAR('TEXTY',ISTAT)
      WRITE (TEXTB(1),902)TEXTC
      CALL DRAW AX(TIA,12,AX(1),90.0)
      CALL DRAW AX(TIB,12,AY(1),0.0)
C
      DO L = 1,LSTLEN
         X = XX(L)
         Y = YY(L)
         IF(DOTS.EQ.'N') THEN
            CALL MARK PT(X,Y,3)
         ELSE
            CALL NUMB PT(X,Y,3,L)
         ENDIF
      ENDDO
      CALL BREAK
      CALL JOIN PT(AX(2),AY(1))
      CALL JOIN PT(AX(2),AY(2))
      CALL JOIN PT(AX(1),AY(2))
C
C
C
      AMIN = AX(1)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R USECUR *
C      *            *
C      **************
C
C --------------------------------------------------------------
C
C
C
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
C        Get posns and put out as values, until posn to left of diag picked
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
C      * S/R PUTLIN *
C      *            *
C      **************
C
C
C      This s/r puts lines on the display
C
C      STARLINK PARAMETERS
C        LINE
C           Flag for type of posn input
C        FILE
C           Name of file for XY file input
C        XY
C           XY offset for file input, or XY posn for keyboard input
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE PUTLIN(IDEV,SIZE,AMIN)
C
C
C
      REAL SIZE(2),XY(2,2),XYA(2)
      LOGICAL LOOP,LOOPA
      CHARACTER*72 TEXT
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      LOOPA = .TRUE.
      KLOOP = 0
      DO WHILE (LOOPA)
         KLOOP = KLOOP + 1
         IF (KLOOP.EQ.1) THEN
            KW = 4
            CALL GETCMD('LINE1','CURSOR,FILE,KEYBOARD,NONE.',1,KW,
     +                  TEXT,KTEXT,IERR)
            CALL CNPAR('LINE1',ISTAT)
         ELSE
            KW = 4
            CALL GETCMD('LINE2','CURSOR,FILE,KEYBOARD,NONE.',1,KW,
     +                  TEXT,KTEXT,IERR)
            CALL CNPAR('LINE2',ISTAT)
         ENDIF
C
C  Exit
C
         IF (KW.EQ.4) THEN
            LOOPA = .FALSE.
         ENDIF
C
C  Use cursor
C
         IF (KW.EQ.1) THEN
            WRITE(TEXT,900)AMIN
  900       FORMAT(' ','PLACE TO LESS THAN ',G14.4,'  TO EXIT')
            CALL WRUSER(TEXT,ISTAT)
            CALL BREAK
            LOOP = .TRUE.
         DO WHILE (LOOP)
               CALL CURSOR(X,Y)
               IF (X.LT.AMIN) THEN
                  LOOP = .FALSE.
               ELSE
                  CALL JOIN PT(X,Y)
               ENDIF
            ENDDO
         ENDIF
C
C  Use file, adding offset and checking for breaks in the line
C  and setting up break check for 1st point in list
C
         IF (KW.EQ.2) THEN
            CALL WRUSER('INPUT FILE',ISTAT)
            CALL GTXYLR('FILE',.TRUE.,NITEM,LSTLEN,IPIN,IERR)
            CALL CNPAR('FILE',ISTAT)
            CALL WRUSER('INPUT X,Y OFFSETS TO FILE',ISTAT)
            XYA(1) = 0.0
            XYA(2) = 0.0
            CALL RDKEYR('XY',.TRUE.,2,XYA,NVAL,IERR)
            CALL CNPAR('XY',ISTAT)
            CALL BREAK
            DO K = 1,LSTLEN
               CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,K,K,6,7,
     +                     XY,2,2,1,1)
               XN = XY(1,1)
               YN = XY(2,1)
               IF (K.EQ.1) THEN
                  XP = XN - 1.0
                  YP = YN - 1.0
               ENDIF
               IF (XN.EQ.XP.AND.YN.EQ.YP) THEN
                  CALL BREAK
               ELSE
                  X = XN + XYA(1)
                  Y = YN + XYA(2)
                  CALL JOIN PT(X,Y)
                  XP = XN
                  YP = YN
               ENDIF
            ENDDO
            CALL FRDATA('FILE',ISTAT)
         ENDIF
C
C  Use keyboard
C
         IF (KW.EQ.3) THEN
            CALL WRUSER('INPUT XY POSNS',ISTAT)
            CALL WRUSER('PUT NULL ENTRY TO STOP',ISTAT)
            CALL WRUSER('A REPEAT ENTRY DRAWS NO LINE TO NEXT POSN',
     +                  ISTAT)
            LOOP = .TRUE.
            CALL BREAK
            KK = 0
            DO WHILE (LOOP)
               KK = KK + 1
               CALL RDKEYR('XY',.FALSE.,2,XYA,NVAL,ISTAT)
               IF (KK.EQ.1) THEN
                   XP = XYA(1) - 1.0
                   YP = XYA(2) - 1.0
               ENDIF
               IF (ISTAT.EQ.ERR_PARNUL) THEN
                  LOOP = .FALSE.
               ELSE
                  IF (NVAL.EQ.2) THEN
                     X = XYA(1)
                     Y = XYA(2)
                     IF (X.EQ.XP.AND.Y.EQ.YP) THEN
                        CALL BREAK
                     ELSE
                        XP = X
                        YP = Y
                        CALL JOIN PT(X,Y)
                     ENDIF
                  ELSE
                     CALL WRUSER('NEED 2 VALUES',ISTAT)
                  ENDIF
               ENDIF
               CALL CNPAR('XY',ISTAT)
            ENDDO
         ENDIF
C
C
C
      ENDDO
C
C
C
      END


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GETDAT *
C      *            *
C      **************
C
C
C  This s/r extracts data from two arrays and puts into two others
C
C  AJPENNY               RGO                         83-1-11
C --------------------------------------------------------------
C
C
C
      SUBROUTINE GETDAT(DA,NITEMA,LSTLA,DB,NITEMB,KPARA,KPARB,X,Y)
C
C
C
      REAL DA(NITEMA,LSTLA),DB(NITEMB,LSTLA),X(LSTLA),Y(LSTLA)
C
C
C
      DO K = 1,LSTLA
         KX = 5 + KPARA
         X(K) = DA(KX,K)
         KY = 5 + KPARB
         Y(K) = DB(KY,K)
      ENDDO
C
C
C
      END



