C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   MAGDIAG *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               MAGDIAG
C
C
C          FUNCTION:-
C               This plots out an HR or Two-Colour diagram  from  the  star
C               magnitude  lists.  These lists must all
C               contain the same stars in the same  order.  But  additional
C               lists  in  the  respective wavelength bands may be added to
C               make a grand diagram.   Conditions may be imposed on  which
C               stars  are plotted out, namely the number of estimates that
C               have gone into making the magnitudes in the MAGAV  output
C               lists.  Thus 1 or more; 2 or more; etc; estimates may be
C               required for a star. The fainter or brighter stars in a file
C               may be excluded. The limit is asked for in terms of the
C               input mags, not the plotted ones.
C                  The plotting out is done as col2 vs col3-col4
C               or col1-col2 vs col3-col4, that is as V vs  B-V
C               or  R  vs  R-I  or U-B vs B-V or any other single character
C               descriptors which  are  assigned  by  you  in  response  to
C               requests  during  this  program.  If a waveband is required
C               twice (i.e. V in V vs B-V) the file is only asked for once.
C                  In making the diagrams, zero points and colour equations
C               are input by you.
C                  The  output  can be put out on the Args, the Tek, or the
C               GOC or the CALCOMP81 or the CALCOMP or the
C               Versatec.   A file containing the results is also made.
C                  Lines can also be drawn on the diagram from keyboard,
C               cursor or file posns.
C                  Up to 2000 stars can be handled
C
C
C          USE:-
C
C
C
C         USER PARAMETERS:-
C
C         TYPE            HR                  Flag  for  choosing   HR   or
C                                             Two-Colour    diagram    type
C                                             output
C
C         COLCODE1,2,3,4                      A single character for
C                                             labelling each file for dialogue
C                                             and for output.
C                                             Reply V or U etc.
C
C         ZEROP1,2,3,4    0.0                 The zero point corrections to
C                                             be added to the input mags in
C                                             each file.
C
C         COLCOR1,2,3,4   0.0                 The colour equations to be
C                                             applied to each input mag
C
C         INPUT1,2,3,4                        The  input  magnitudes  files
C
C
C         MAGAV           NO                  Flag for files made with
C                                             the MAGAV program which
C                                             have the no of estimates
C                                             per star in.
C                                             Choices are YES,NO
C
C         NUMEST1,2,3,4   1                   If MAGAV files, this is
C                                             the minimum no of estimates
C                                             used in making the star mag
C                                             for a star in the input file
C                                             that is acceptable. Any star
C                                             with fewer that that for the
C                                             mag in this file is ignored.
C
C         HILIM1,2,3,4    40.0                Stars in this file with
C                                             input mag fainter than this
C                                             are not plotted out.
C
C         LOLIM1,2,3,4    0.1                 Stars in this file with
C                                             input mag brighter than this
C                                             are not plotted out.
C
C         AGAIN          NO                   Flag to input another set
C                                             of files. Choices are YES,NO.
C
C         DEVICE         ARGS                 The display device for the
C                                             graph.
C                                             Choices are ARGS,GOC,TEKTRONIX,
C                                             VERSATEC,CALCOMP,CC81,NONE.
C
C         DEVSIZE        Various              For graphs,the
C                                             size of the picture.
C
C         DEVLIMX,Y      Min,Max              For graphs, the range of mags
C                                             and cols to be plotted out in
C                                             X and Y
C
C         DOTNUM         No                   Flag for points plotted out
C                                             to have the number of the star
C                                             in the output list to be
C                                             plotted by the point. Choices
C                                             are YES,NO.
C
C         LINE           None                 Flag for method of putting
C                                             line on diagram. Choices are
C                                             CURSOR,FILE,KEYBOARD,NONE.
C                                             XY points are joined
C                                             together with straight lines
C
C         FILE                                If FILE is chosen, this is
C                                             the EDRS format XY file
C                                             containing the posns.
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
C         DAGAIN         No                   Flag to output the display
C                                             with different parameters
C                                             again. Choices are YES,NO.
C
C         OUTPUT                              The Output file  for  storing
C                                             the results
C
C         OUTTITLE    Output from MAGDIAG     The Title to be  inserted  in
C                                             the Output file
C
C
C         OUTPUT FILE CONTENTS
C            Identifier; X; Y; col(2) or col(1)-col(2); col(3)-col(4);
C            number of ests in col(n), where n is for all the diff cols.
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Request for magnitudes at that position to  be  typed
C                     out.
C                     If in LINe-CURSOR mode, it requests a line to
C                     drawn from the last cursor defined position. (For
C                     the first one, no line is drawn.
C                     In both modes, putting to left of diagram exits.
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
C         A.J.Penny                RGO                            15-SEP-82
C
C
C--------------------------------------------------------------------------



C
C
C
      REAL ZEROP(4),COLCOR(4),AML(4),RRVAL(1,1)
      REAL Y(2000),X(2000),XYID(7,2000)
      LOGICAL USE,LOOP
      CHARACTER*72 TEXT
      CHARACTER*30 HVAL(8)
      INTEGER TEXTA(15)
      EQUIVALENCE (TEXTA(1),TEXT)
      CHARACTER*1 COL(4)
      REAL AX(2),AY(2),SIZE(2),AMLIM(4,2),AMT(4)
      CHARACTER ATITLE*30,CVAL*1
      INTEGER IPR(5),KN(4),KNA(2000,4),KFILE(4),KG(4),KCOLC(4)
      INTEGER NITEM(4)
C
C  Get wether HR or Two-colour diagram
C
      LTYPE = 1
      CALL GETCMD('TYPE','HR,TWOCOL.',1,LTYPE,TEXT,KTXT,IERR)
      KSTART = 1
      IF (LTYPE.NE.2) KSTART = 2
C
C  Get colour codes
C
      DO K = KSTART,4
         KA = K + 1 - KSTART
         WRITE(TEXT,900)KA
  900    FORMAT(1H ,'COLOUR ',I1,' HAS CHARACTER CODE = ? (A1) ')
         CALL WRUSER(TEXT,ISTAT)
         CVAL = 'V'
         IF (KSTART.EQ.1) THEN
            IF (K.EQ.1) CVAL = 'B'
            IF (K.EQ.2) CVAL = 'V'
            IF (K.EQ.3) CVAL = 'U'
            IF (K.EQ.4) CVAL = 'B'
         ELSE
            IF (K.EQ.2) CVAL = 'V'
            IF (K.EQ.3) CVAL = 'B'
            IF (K.EQ.4) CVAL = 'V'
         ENDIF
         IF (K.EQ.1) TEXT = 'COLCODE1'
         IF (K.EQ.2) TEXT = 'COLCODE2'
         IF (K.EQ.3) TEXT = 'COLCODE3'
         IF (K.EQ.4) TEXT = 'COLCODE4'
         CALL RDKEYC(TEXT,.TRUE.,1,CVAL,I,ISTAT)
         CALL CNPAR(TEXT,JSTAT)
         COL(K) = CVAL
         KFILE(K) = 0
         DO J = KSTART,K
            IF(COL(K).EQ.COL(J).AND.KFILE(K).EQ.0) KFILE(K) = J
         ENDDO
      ENDDO
C
C  Get wether files are MAGAV type
C
      CALL WRUSER('TREAT FILES AS MAGAV OUTPUT ?',ISTAT)
      KW = 1
      CALL GETCMD('MAGAV','NO,YES.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('MAGAV',JSTAT)
      KFLNUM = 0
      IF (KW.EQ.2) KFLNUM = 1
C
C -----------------------------------------------------------------
C
C  Get the input files
C
      L = 0
      LOOP = .TRUE.
      DO WHILE (LOOP)
C
C  Open input files
C
      DO K = KSTART,4
         KA = K + 1 - KSTART
         IF(KFILE(K).EQ.K) THEN
            CALL WRUSER(' ',ISTAT)
            WRITE(TEXT,906)COL(K)
  906       FORMAT(' ','INPUT FILE FOR COLOUR ',A1)
            CALL WRUSER(TEXT,ISTAT)
    1       CONTINUE
            WRITE(TEXT,976)K
  976       FORMAT('INPUT',I1)
            CALL GTXYLR(TEXT,.FALSE.,NITEM(K),LSTLNA,IPR(K),IXY)
            IF (K.EQ.KSTART) THEN
               CALL GTDSCR(TEXT,'TITLE','CHARACTER',IVAL,RVAL,
     +                      ATITLE,IERR)
            ENDIF
            CALL CNPAR(TEXT,JSTAT)
            IF (IXY.NE.0.OR.(NITEM(K)-KFLNUM).LT.8) THEN
               CALL WRUSER('BAD FILE, TRY AGAIN',JSTAT)
               GO TO 1
            ENDIF
            IF(K.EQ.KSTART) THEN
               LSTLEN = LSTLNA
            ELSE
               IF(LSTLNA.NE.LSTLEN) THEN
                  CALL WRUSER('FILE WRONG LENGTH, TRY AGAIN',JSTAT)
                  GO TO 1
               ENDIF
            ENDIF
            IF (KFLNUM.EQ.1) THEN
               CALL WRUSER('MIN NO OF ESTS/STAR IN THIS FILE?',ISTAT)
               KGA = 1
               WRITE(TEXT,963)K
  963          FORMAT('NUMEST',I1)
               CALL RDKEYI(TEXT,.TRUE.,1,KGA,I,ISTAT)
               CALL CNPAR(TEXT,JSTAT)
               KG(K) = KGA
            ELSE
               KG(K) = 1
            ENDIF
            CALL WRUSER('FAINTEST MAGNITUDE ACCEPTABLE IN THIS FILE?',
     +                  ISTAT)
            AMLI = 40.0
            WRITE(TEXT,964)K
  964       FORMAT('HILIM',I1)
            CALL RDKEYR(TEXT,.TRUE.,1,AMLI,I,ISTAT)
            CALL CNPAR(TEXT,JSTAT)
            AMLIM(K,1) = AMLI
            CALL WRUSER('BRIGHTEST MAGNITUDE ACCEPTABLE IN THIS FILE?',
     +                  ISTAT)
            AMLI = 0.1
            WRITE(TEXT,965)K
  965       FORMAT('LOLIM',I1)
            CALL RDKEYR(TEXT,.TRUE.,1,AMLI,I,ISTAT)
            CALL CNPAR(TEXT,JSTAT)
            AMLIM(K,2) = AMLI
         ELSE
            IPR(K) = IPR(KFILE(K))
            NITEM(K) = NITEM(KFILE(K))
            KG(K) = KG(KFILE(K))
            AMLIM(K,1) = AMLIM(KFILE(K),1)
            AMLIM(K,2) = AMLIM(KFILE(K),2)
         ENDIF
      ENDDO
C
C  Get zero points
C
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('ZERO POINT CORRNS ARE:-',ISTAT)
      DO K = KSTART,4
         IF(KFILE(K).EQ.K) THEN
            CALL WRUSER(' ',ISTAT)
            WRITE(TEXT,901)COL(K),COL(K),COL(K)
  901       FORMAT(1H ,A1,'1 = ',A1,' + D',A1)
            CALL WRUSER(TEXT,ISTAT)
            WRITE(TEXT,902)COL(K)
  902       FORMAT(1H ,'D',A1,' IS = ? ')
            CALL WRUSER(TEXT,ISTAT)
            RVAL = 0.0
            WRITE(TEXT,962)K
  962       FORMAT('ZEROP',I1)
            CALL RDKEYR(TEXT,.TRUE.,1,RVAL,I,ISTAT)
            CALL CNPAR(TEXT,JSTAT)
            ZEROP(K) = RVAL
         ELSE
            ZEROP(K) = ZEROP(KFILE(K))
         ENDIF
      ENDDO
C
C  Get colour corrections
C
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('COLOUR CORRN EQNS ARE:-',ISTAT)
      DO K = KSTART,4
         IF(KFILE(K).EQ.K) THEN
            CALL WRUSER(' ',ISTAT)
            WRITE(TEXT,903)COL(K),COL(K),COL(K),COL(3),COL(4)
  903       FORMAT(1H ,A1,'2 = ',A1,'1 + C',A1,'*(',A1,'1-',A1,'1)')
            CALL WRUSER(TEXT,ISTAT)
            WRITE(TEXT,904)COL(K)
  904       FORMAT(1H ,'C',A1,' IS = ?')
            CALL WRUSER(TEXT,ISTAT)
            RVAL = 0.0
            WRITE(TEXT,961)K
  961       FORMAT('COLCOR',I1)
            CALL RDKEYR(TEXT,.TRUE.,1,RVAL,I,ISTAT)
            CALL CNPAR(TEXT,JSTAT)
            COLCOR(K) = RVAL
         ELSE
            COLCOR(K) = COLCOR(KFILE(K))
         ENDIF
      ENDDO
C
C  Extract the data from these files
C
      DO JL = 1,LSTLEN
         IF (L.LT.2000) THEN
C
C  See if this star is acceptable
C
            DO K = KSTART,4
               IF (KFILE(K).EQ.K) THEN
                  IF (KFLNUM.EQ.1) THEN
                     CALL EXTLSA(%VAL(IPR(K)),NITEM(K),LSTLEN,
     +                           JL,JL,9,9,RRVAL,1,1,1,1)
                     KN(K) = INT(RRVAL(1,1)+0.01)
                  ELSE
                     KN(K) = 1
                  ENDIF
                  CALL EXTLSA(%VAL(IPR(K)),NITEM(K),LSTLEN,JL,
     +                        JL,8,8,RRVAL,1,1,1,1)
                  AMT(K) = RRVAL(1,1)
               ELSE
                  KN(K) = KN(KFILE(K))
                  AMT(K) = AMT(KFILE(K))
               ENDIF
            ENDDO
            USE = .TRUE.
            DO K = KSTART,4
               IF (KN(K).LT.KG(K)) USE = .FALSE.
               IF (AMT(K).GT.AMLIM(K,1)) USE = .FALSE.
               IF (AMT(K).LT.AMLIM(K,2)) USE = .FALSE.
            ENDDO
C
C  If so, get its data
C
            IF (USE) THEN
               L = L + 1
               DO K = KSTART,4
                  CALL EXTLSA(%VAL(IPR(K)),NITEM(K),LSTLEN,JL,
     +                        JL,8,8,RRVAL,1,1,1,1)
                  AML(K) = RRVAL(1,1)
                  AML(K) = AML(K) + ZEROP(K)
               ENDDO
               DO K = KSTART,4
                  AML(K) = AML(K) + COLCOR(K)*(AML(3)-AML(4))
               ENDDO
C
               IF (KSTART.EQ.1) THEN
                  Y(L) = AML(1) - AML(2)
               ELSE
                  Y(L) = AML(2)
               ENDIF
               X(L) = AML(3) - AML(4)
C
               DO K = KSTART,4
                  KNA(L,K) = KN(K)
               ENDDO
C
               CALL EXTLSA(%VAL(IPR(KSTART)),NITEM(KSTART),
     +                     LSTLEN,JL,JL,1,7,XYID,7,2000,L,1)
            ENDIF
         ENDIF
C
      ENDDO
C
C  Add data from another set of files ?
C
      IF(L.NE.2000) THEN
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('INPUT AN ADDITIONAL SET OF FILES ?',ISTAT)
         KW = 1
         CALL GETCMD('AGAIN','NO,YES.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('AGAIN',JSTAT)
         IF (KW.EQ.1) LOOP = .FALSE.
      ELSE
         LOOP = .FALSE.
      ENDIF
C
C
C
      ENDDO
C
C  Total no of accepted stars
C
      LTOT = L
      CALL WRUSER(' ',JSTAT)
      WRITE(TEXT,905)LTOT
  905 FORMAT(1H ,'NUMBER OF STARS = ',I6)
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
            CALL DOPLT(SIZE,COL,X,Y,LTOT,KSTART,AMIN)
C
C  Draw a wanted line
C
            CALL PUTLIN(IDEV,SIZE,AMIN)
C
C  Use a wanted cursor (if can) to get posns on diag
C
            IF (IDEV.EQ.2.OR.IDEV.EQ.3.OR.IDEV.EQ.4) THEN
               CALL DOCUR(IDEV,COL,KSTART,AMIN)
            ENDIF
C
C  If outputting to hard copy, write a caption
C
            IF (IDEV.EQ.5.OR.IDEV.EQ.6.OR.IDEV.EQ.8) THEN
               CALL WRUSER('TEXT TO WRITE BELOW DIAG IS ?',ISTAT)
               CALL RDKEYC('TEXT',.FALSE.,1,TEXT,I,ISTAT)
               CALL CNPAR('TEXT',JSTAT)
               CALL TITLE(2,2,TEXTA,60)
            ENDIF
            CALL DEVCLS(IDEV)
         ENDIF
C
C  See if display again
C
         CALL WRUSER('DISPLAY AGAIN ? ',ISTAT)
         KW = 1
         CALL GETCMD('DAGAIN','NO,YES.',1,KW,TEXT,KTEXT,IERR)
         CALL CNPAR('DAGAIN',JSTAT)
         IF (KW.EQ.1) LOOP = .FALSE.
C
C
C
      ENDDO
C
C  Find number of separate colours
C
      NUMC = 1
      KCOLC(KSTART) = 1
      KJ = KSTART + 1
      DO K = KJ,4
         KL = 0
         KJA = KJ - 1
         DO J = KSTART,KJA
            IF (COL(K).EQ.COL(J)) KL = 1
         ENDDO
         IF (KL.EQ.0) THEN
            NUMC = NUMC + 1
            KCOLC(K) = 1
         ELSE
            KCOLC(K) = 0
         ENDIF
      ENDDO
      NUMOUT = 9 + NUMC
C
C  Open Output file
C
      CALL GTXYLW('OUTPUT',.FALSE.,NUMOUT,LTOT,IPO,JOUTST)
      IF(JOUTST.EQ.ERR_NORMAL) THEN
C
C  Insert descriptors
C
         CALL PTDSCR('OUTPUT','NITEM','INTEGER',NUMOUT,RVAL,CVAL,IERR)
         CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LTOT,RVAL,CVAL,IERR)
         CALL CHARLN(ATITLE,KLEN)
         IF (KLEN.EQ.0) THEN
            ATITLE = 'Output from MAGDIAG'
         ENDIF
         CALL RDKEYC('OUTTITLE',.TRUE.,1,ATITLE,NVAL,IERR)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +               ATITLE,IERR)
         HVAL(1) = 'X'
         HVAL(2) = 'Y'
         IF (KSTART.EQ.1) THEN
            WRITE(HVAL(3),980)COL(1),COL(2)
  980       FORMAT(A1,'-',A1)
         ELSE
            WRITE(HVAL(3),981)COL(2)
  981       FORMAT(A1)
         ENDIF
         WRITE(HVAL(4),980)COL(3),COL(4)
         KP = 4
         DO K = KSTART,4
            IF (KCOLC(K).EQ.1) THEN
               KP = KP + 1
               WRITE(HVAL(KP),982)COL(K)
  982          FORMAT('N',A1)
            ENDIF
         ENDDO
         CALL PTHEAD('OUTPUT',KP,HVAL,IERR)
C
C  Store results
C
         CALL EXTLSA(XYID,7,2000,1,LTOT,1,7,%VAL(IPO),NUMOUT,LTOT,
     +               1,1)
         DO L = 1,LTOT
            RRVAL(1,1) = Y(L)
            CALL EXTLSA(RRVAL,1,1,1,1,1,1,%VAL(IPO),NUMOUT,LTOT,
     +                  L,8)
            RRVAL(1,1) = X(L)
            CALL EXTLSA(RRVAL,1,1,1,1,1,1,%VAL(IPO),NUMOUT,LTOT,
     +                  L,9)
            KP = 9
            DO K = KSTART,4
               IF (KCOLC(K).EQ.1) THEN
                  KP = KP + 1
                  RRVAL(1,1) = KNA(L,K)
                  CALL EXTLSA(RRVAL,1,1,1,1,1,1,%VAL(IPO),NUMOUT,
     +                        LTOT,L,KP)
               ENDIF
            ENDDO
         ENDDO
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
C      *************
C      *           *
C      * S/R DOPLT *
C      *           *
C      *************
C
C
C
C -------------------------------------------------------------
C
C
C
       SUBROUTINE DOPLT(SIZE,COL,X,Y,LTOT,KSTART,ALHD)
C
C
C
      CHARACTER*1 COL(4),DOTS
      CHARACTER*72 TEXT
      REAL X(2000),Y(2000),AX(2),AY(2),SIZE(2)
      CHARACTER*4 TEXTB(2)
      INTEGER TI(2)
      EQUIVALENCE (TEXTB(1),TI(1))
      EQUIVALENCE (TEXTB(2),TI(2))
C
C  Get ranges of mags
C
      AMINA = 30000.0
      AMAXA = -30000.0
      DO L = 1,LTOT
         IF(Y(L).GT.AMAXA) AMAXA = Y(L)
         IF(Y(L).LT.AMINA) AMINA = Y(L)
      ENDDO
      IF (KSTART.EQ.1) THEN
         WRITE(TEXT,900)COL(1),COL(2)
  900    FORMAT(1H ,'ENTER ',A1,'-',A1,' PLOT LIMITS')
         CALL WRUSER(TEXT,ISTAT)
         ALIM = AMINA - 0.15
         AY(1) = REAL(INT(ABS(ALIM*10.0)))*SIGN(1.0,ALIM)/10.0
         ALIM = AMAXA + 0.15
         AY(2) = REAL(INT(ABS(ALIM*10.0)))*SIGN(1.0,ALIM)/10.0
      ELSE
         WRITE(TEXT,901)COL(2)
  901    FORMAT(1H ,'ENTER ',A1,' PLOT LIMITS')
         CALL WRUSER(TEXT,ISTAT)
         ALIM = AMINA - 0.5
         AY(1) = REAL(INT(ALIM))
         ALIM = AMAXA + 1.5
         AY(2) = REAL(INT(ALIM))
      ENDIF
      WRITE(TEXT,908)AMINA,AMAXA
  908 FORMAT(' ',' MIN= ',F6.2,'  MAX= ',F6.2)
      CALL WRUSER(TEXT,ISTAT)
      CALL RDKEYR('DEVLIMY',.TRUE.,2,AY,I,ISTAT)
      CALL CNPAR('DEVLIMY',JSTAT)
      TEMP = AY(1)
      AY(1) = AY(2)
      AY(2) = TEMP
      WRITE(TEXT,900)COL(3),COL(4)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      AMINB = 30000.0
      AMAXB = -30000.0
      DO L = 1,LTOT
         IF(X(L).GT.AMAXB) AMAXB = X(L)
         IF(X(L).LT.AMINB) AMINB = X(L)
      ENDDO
      ALIM = AMINB - 0.15
      AX(1) = REAL(INT(ABS(ALIM*10.0)))*SIGN(1.0,ALIM)/10.0
      ALIM = AMAXB + 0.15
      AX(2) = REAL(INT(ABS(ALIM*10.0)))*SIGN(1.0,ALIM)/10.0
      WRITE(TEXT,908)AMINB,AMAXB
      CALL WRUSER(TEXT,ISTAT)
      CALL RDKEYR('DEVLIMX',.TRUE.,2,AX,I,ISTAT)
      CALL CNPAR('DEVLIMX',JSTAT)
      ALHD = AX(1)
C
C
C
      KW = 1
      CALL GETCMD('DOTNUM','NO,YES.',1,KW,TEXT,KTEXT,IERR)
      CALL CNPAR('DOTNUM',JSTAT)
      IF (KW.EQ.1) THEN
         DOTS = 'N'
      ELSE
         DOTS = 'Y'
      ENDIF
      XL = SIZE(1)
      YL = SIZE(2)
C
C
      CALL NEWPLT(AX(1),AX(2),XL,AY(1),AY(2),YL)
      IF(KSTART.EQ.1) THEN
         WRITE(TEXTB(1),902)COL(1),COL(2)
  902    FORMAT(A1,'-',A1,' ')
      ELSE
         WRITE(TEXTB(1),903)COL(2)
  903    FORMAT(A1,'   ')
      ENDIF
      WRITE(TEXTB(2),902)COL(3),COL(4)
      CALL DRAW AX(TI(1),4,AX(1),90.0)
      CALL DRAW AX(TI(2),4,AY(1),0.0)
C
      DO L = 1,LTOT
         XX = X(L)
         YY = Y(L)
         IF(DOTS.EQ.'N') THEN
            CALL MARK PT(XX,YY,3)
         ELSE
            CALL NUMB PT(XX,YY,3,L)
         ENDIF
      ENDDO
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
C      *************
C      *           *
C      * S/R DOCUR *
C      *           *
C      *************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE DOCUR(IDEV,COL,KSTART,AMIN)
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
      CALL CNPAR('CURSOR',JSTAT)
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
C        Get posns and put out as mags, until posn to left of diag picked
C
         IF (KSTART.EQ.1) THEN
            WRITE(TEXT,900)COL(1),COL(2),COL(3),COL(4)
  900       FORMAT(1H ,'   ',A1,'-',A1,'    ',A1,'-',A1)
         ELSE
            WRITE(TEXT,901)COL(2),COL(3),COL(4)
  901       FORMAT(1H ,'   ',A1,'     ',A1,'-',A1)
         ENDIF
         CALL WRUSER(TEXT,JSTAT)
         LOOP = .TRUE.
         DO WHILE (LOOP)
            CALL CURSOR(XA,YA)
            IF (XA.GE.AMIN) THEN
               WRITE(TEXT,902)YA,XA
  902          FORMAT(1H ,F6.2,2X,F6.2)
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
         DO WHILE (LOOPA)
         KW = 4
         CALL GETCMD('LINE','CURSOR,FILE,KEYBOARD,NONE.',1,KW,TEXT,
     +               KTEXT,IERR)
         CALL CNPAR('LINE',JSTAT)
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
            WRITE(TEXT,900),AMIN
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
C  Use file
C
         IF (KW.EQ.2) THEN
            CALL WRUSER('INPUT FILE',ISTAT)
            CALL GTXYLR('FILE',.FALSE.,NITEMA,LSTLEN,IPIN,IERR)
            CALL CNPAR('FILE',JSTAT)
            CALL WRUSER('INPUT X,Y OFFSETS TO FILE',JSTAT)
            XYA(1) = 0.0
            XYA(2) = 0.0
            CALL RDKEYR('XY',.TRUE.,2,XYA,NVAL,IERR)
            CALL CNPAR('XY',JSTAT)
            CALL BREAK
            DO K = 1,LSTLEN
               CALL EXTLSA(%VAL(IPIN),NITEMA,LSTLEN,K,K,6,7,
     +                     XY,2,2,1,1)
               X = XY(1,1) + XYA(1)
               Y = XY(2,1) + XYA(2)
               CALL JOIN PT(X,Y)
            ENDDO
            CALL FRDATA('FILE',ISTAT)
         ENDIF
C
C  Use keyboard
C
         IF (KW.EQ.3) THEN
            CALL WRUSER('INPUT XY POSNS',ISTAT)
            CALL WRUSER('PUT NULL ENTRY TO STOP',ISTAT)
            LOOP = .TRUE.
            CALL BREAK
            DO WHILE (LOOP)
               CALL RDKEYR('XY',.FALSE.,2,XYA,NVAL,ISTAT)
               CALL CNPAR('XY',JSTAT)
               IF (ISTAT.EQ.ERR_PARNUL) THEN
                  LOOP = .FALSE.
               ELSE
                  X = XYA(1)
                  Y = XYA(2)
                  CALL JOIN PT(X,Y)
               ENDIF
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


