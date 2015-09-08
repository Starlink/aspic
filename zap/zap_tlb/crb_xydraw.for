	SUBROUTINE CRB_XYDRAWA
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
C              The numbers are taken from file A, parameter no A1 and fi
C              B, parameter no B1, where A,A1,B,B1 are input by the user
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
C         PARNOB                              The parameter number in th
C                                             second file to take the Y
C                                             values from.
C
C         DEVICE         ARGS                 The display device for the
C                                             graph. Choices are ARGS,GO
C                                             TEKTRONIX,VERSATEC,CC81,
C                                             CALCOMP.
C
C         DEVSIZE        Various              For graphs,the
C                                             size of the picture.
C
C         DEVLIMX        Min,Max              For graphs, the ranges of
C         DEVLIMY                             and Ys to be plotted out.
C
C         TEXTX                               Text to write on X axis.
C
C         TEXTY          As for TEXTX         Text to write on Y axis.
C
C         TEXT                                For hard-copy output,this
C                                             is some text to be put bel
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
C         A.J.Penny                RGO                            83-2-1
C
C
C-----------------------------------------------------------------------
 
 
 
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
		CALL SRSEND
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
	CALL CNPAR('DEVICE',ISTAT)
	CALL CNPAR('DEVSIZE',ISTAT)
	CALL CNPAR('DEVLIMX',ISTAT)
	CALL CNPAR('DEVLIMY',ISTAT)
	CALL CNPAR('TEXTX',ISTAT)
	CALL CNPAR('TEXTY',ISTAT)
	CALL CNPAR('TEXT',ISTAT)
	CALL CNPAR('ANOTHER',ISTAT)
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
