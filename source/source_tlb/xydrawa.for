C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   XYDRAWA *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               XYDRAWA
C
C
C          FUNCTION:-
C              Draws out curves from XYlists. Takes two paramters from
C              one or two XYlists and plots out a smooth curve joining
C              the points. The interpolation is by cubic splines.
C
C              The numbers are taken from file A, parameter no A1 and file
C              B, parameter no B1, where A,A1,B,B1 are input by the user.
C
C              More than one line may be drawn on a graph.
C
C              The  output  can be put out on the Args, the Tek, or the
C              Versatec, the Calcomp, the GOC, or the CC81.
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
C         TEXTX                               Text to write on X axis.
C
C         TEXTY          As for TEXTX         Text to write on Y axis.
C
C         TEXT                                For hard-copy output,this
C                                             is some text to be put below
C                                             the diagram.
C
C         ANOTHER         No                  Flag to add another line
C                                             Choices are YES/NO.
C
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C
C
C
C
C
C
C         A.J.Penny                RGO                            83-2-16
C
C
C--------------------------------------------------------------------------



C
C
C
      LOGICAL VALID,LOOP
      CHARACTER*72 TEXT
      CHARACTER TEXTH*7,XHEAD*30,YHEAD*30
      INTEGER TEXTA(15)
      EQUIVALENCE (TEXTA(1),TEXT)
      REAL SIZE(2)
      CHARACTER CVAL*1
C
C
C
      VALID = .TRUE.
C
C
C
      KLOOP = 0
      LOOP = .TRUE.
      DO WHILE (LOOP)
         KLOOP = KLOOP + 1
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
     +                  RVAL,IERRB)
             IF (IERRB.NE.0) THEN
                CALL WRUSER('BAD PARAMETER',ISTAT)
                VALID = .FALSE.
             ELSE
               WRITE(TEXTH,930)KPARA
  930          FORMAT('HEAD',I3.3)
               CALL RDDSCR('INPUTA',TEXTH,1,XHEAD,KC,IERRH)
               IF (IERRH.NE.0) XHEAD = 'X'
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
     +                  RVAL,IERRD)
            IF (IERRD.NE.0) THEN
               CALL WRUSER('BAD PARAMETER',ISTAT)
               VALID = .FALSE.
            ELSE
               WRITE(TEXTH,930)KPARB
               CALL RDDSCR('INPUTB',TEXTH,1,YHEAD,KC,IERRH)
               IF (IERRH.NE.0) YHEAD = 'Y'
            ENDIF
         ENDIF
         CALL CNPAR('INPUTA',ISTAT)
         CALL CNPAR('PARNOA',ISTAT)
         CALL CNPAR('INPUTB',ISTAT)
         CALL CNPAR('PARNOB',ISTAT)
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
C  Open device if first time round
C
            IF (KLOOP.EQ.1) THEN
               CALL DEVOPN(IDEV,SIZE)
               IF (IDEV.NE.1) THEN
                  CALL DOAXE(SIZE,%VAL(IPX),%VAL(IPY),LSTLA,XHEAD,
     +                       YHEAD)
               ENDIF
            ENDIF
C
C  Display graph
C
            IF (IDEV.NE.1) THEN
               CALL DOLINE(%VAL(IPX),%VAL(IPY),LSTLA,KLOOP)
               CALL FRDATA('IWX',ISTAT)
               CALL FRDATA('IWY',ISTAT)
               CALL FRDATA('INPUTA',ISTAT)
               CALL FRDATA('INPUTB',ISTAT)
            ENDIF
         ENDIF
C
C  See if display another
C
         KW = 2
         CALL GETCMD('ANOTHER','YES,NO.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('ANOTHER',ISTAT)
         IF (KW.EQ.2) LOOP = .FALSE.
C
C
C
      ENDDO
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
C
C  Close device
C
      IF (IDEV.NE.1) CALL DEVCLS(IDEV)
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
C      * S/R DOAXE  *
C      *            *
C      **************
C
C
C
C -------------------------------------------------------------
C
C
C
       SUBROUTINE DOAXE(SIZE,XX,YY,LSTLEN,XHEAD,YHEAD)
C
C
C
      CHARACTER*72 TEXT
      CHARACTER*30 XHEAD,YHEAD
      REAL XX(LSTLEN),YY(LSTLEN),AX(2),AY(2),SIZE(2)
      CHARACTER*30 TEXTC
      CHARACTER*30 TEXTA(1),TEXTB(1)
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
      XL = SIZE(1)
      YL = SIZE(2)
C
C
      CALL NEWPLT(AX(1),AX(2),XL,AY(1),AY(2),YL)
      CALL WRUSER(' CAPTION FOR X AXIS IS ?',ISTAT)
      TEXTC = XHEAD
      CALL RDKEYC('TEXTX',.TRUE.,1,TEXTC,I,ISTAT)
      CALL CNPAR('TEXTX',ISTAT)
      WRITE (TEXTA(1),902)TEXTC
  902 FORMAT(A30)
      CALL CHARLN(TEXTC,KLENX)
      CALL WRUSER(' CAPTION FOR Y AXIS IS ?',ISTAT)
      TEXTC = YHEAD
      CALL RDKEYC('TEXTY',.TRUE.,1,TEXTC,I,ISTAT)
      CALL CNPAR('TEXTY',ISTAT)
      WRITE (TEXTB(1),902)TEXTC
      CALL CHARLN(TEXTC,KLENY)
      CALL DRAW AX(TIA,KLENX,AX(1),90.0)
      CALL DRAW AX(TIB,KLENX,AY(1),0.0)
      CALL BREAK
      CALL JOIN PT(AX(2),AY(1))
      CALL JOIN PT(AX(2),AY(2))
      CALL JOIN PT(AX(1),AY(2))
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
      SUBROUTINE DOLINE(X,Y,NUM,KLOOP)
C
C
C
      REAL X(NUM),Y(NUM)
C
C
C
      CALL BREAK
      KK = -2*(KLOOP-1)
      IF (KLOOP.GT.4) KK = 2*(KLOOP-1)
      IF (KLOOP.GT.7) KK = 0
      CALL BRKN CV(X,Y,NUM,KK)
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



