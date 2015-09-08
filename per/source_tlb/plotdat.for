      PROGRAM PLOTDAT
C+
C
C   Program PLOTDAT
C
C   This is a new program incorporating some new features for
C   plotting data associated with the period-finding programs
C
C   In particular it will plot on all GKS devices.
C                         plot as lines or markers,
C                         mark any filled gaps in the data,
C                         plot two datasets and their differences.
C
C   It uses the interim Starlink environment and SIMPLEPLOT
C   on top of GKS.
C
C   Written by K.F.Hartley at RGO
C
C   Version 1 - 12-Jan-1984
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),AX2(2),STATUS
      CHARACTER*70 TEXF,TEXS
      REAL LIMX(2),LIMY(2),RANGE(2)
      REAL LIMXT(2),LIMYT(2)
C
C   This monstrous thing is to get round the fact that Simpleplot
C   works in Hollerith strings!
C
      CHARACTER*72 TEXT
      INTEGER ITEXT(18)
      EQUIVALENCE (TEXT,ITEXT)
C
C   Obtain first dataset.
C
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,IP1,STATUS)
      CALL PER_STOP(STATUS,'BADDATA')
C
C   Look for descriptors FIRST and SECOND (indicative of the
C   location of a filled gap).
C
      CALL RDDSCR('INPUT','FIRST',1,TEXF,I,ISTAT1)
      CALL RDDSCR('INPUT','SECOND',1,TEXS,I,ISTAT2)
      IF (ISTAT1.EQ.ERR_NORMAL.AND.ISTAT2.EQ.ERR_NORMAL) THEN
         CALL CTOI(TEXF,IFIRST,ISTAT)
         CALL CTOI(TEXS,ISECND,ISTAT)
      ELSE
C
C   The plotting subroutine will recognize this as a non-existent gap
C
         IFIRST=AX(2)+1
         ISECND=AX(2)+1
      END IF
C
C   Get two 1-D arrays for X and Y co-ordinates
C
      CALL GETDYN('WORK1',FMT_R,AX(2),IPW1,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
      CALL GETDYN('WORK2',FMT_R,AX(2),IPW2,STSTUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C   Fill them with data and find the min and max
C
      CALL PER_FILLMM(%VAL(IP1),%VAL(IPW1),%VAL(IPW2),AX,LIMX,LIMY)
C
C   Now obtain plotting limits in X and Y
C
      CALL RDKEYR('XLIMITS',.TRUE.,2,LIMX,I,STATUS)
      CALL RDKEYR('YLIMITS',.TRUE.,2,LIMY,I,STATUS)
C
C   Obtain type of plot
C   A response of YES means use markers, NO means plot lines.
C
      MARK=.TRUE.
      CALL RDKEYL('POINTS',.TRUE.,1,MARK,I,STATUS)
      IF (.NOT.MARK) THEN
         CALL WRUSER('OK - a line plot will be done',STATUS)
      END IF
C
C   Now find out if there is to be a second dataset.
C
      CALL RDIMAG('INPUT2',FMT_DP,2,AX2,I,IP2,STATUS)
C
C   If a good response was found (excluding a null response)
C   make room for the second plot and the differences
C
      IF (STATUS.EQ.ERR_NORMAL) THEN
         DELTAY=LIMY(2)-LIMY(1)
         LIMY(1)=LIMY(1)-2*DELTAY
      ELSE
         DELTAY=0.0
      END IF
C
C   Increase the range a little to allow room for a title.
C
      LIMY(2) = LIMY(2) + 0.1*(LIMY(2)-LIMY(1))
C
C   Now get the plotting device, open it and draw some axes
C
      CALL PER_AXES(LIMX,LIMY)
C
C   Now plot the first dataset.
C   OFF defines the offset for this plot, RANGE defines lines to
C   mark any gap.
C
      OFF=0.0
      RANGE(1)=LIMY(1)+2.0*DELTAY
      RANGE(2)=LIMY(2)
      CALL PER_PLOTL(%VAL(IPW1),%VAL(IPW2),AX(2),IFIRST,ISECND,
     :              MARK,OFF,RANGE)
C
C   Now handle a possible second dataset
C
      IF (DELTAY.NE.0.0) THEN
         CALL CNPAR('WORK1',ISTAT)
         CALL CNPAR('WORK2',ISTAT)
         CALL FRDATA('WORK1',ISTAT)
         CALL FRDATA('WORK2',ISTAT)
         CALL GETDYN('WORK1',FMT_R,AX2(2),IPW1,STATUS)
         CALL GETDYN('WORK2',FMT_R,AX2(2),IPW2,STATUS)
         CALL PER_FILLMM(%VAL(IP2),%VAL(IPW1),%VAL(IPW2),AX2,
     :                   LIMXT,LIMYT)
         CALL RDDSCR('INPUT2','FIRST',1,TEXF,I,ISTAT1)
         CALL RDDSCR('INPUT2','SECOND',1,TEXS,I,ISTAT2)
         IF (ISTAT1.EQ.ERR_NORMAL.AND.ISTAT2.EQ.ERR_NORMAL) THEN
            CALL CTOI(TEXF,IFIRST,ISTAT)
            CALL CTOI(TEXS,ISECND,ISTAT)
         ELSE
            IFIRST=AX2(2)+1
            ISECND=AX2(2)+1
         END IF
         OFF=DELTAY
         RANGE(1)=LIMY(1)+DELTAY
         RANGE(2)=LIMY(1)+2*DELTAY
         CALL PER_PLOTL(%VAL(IPW1),%VAL(IPW2),AX2(2),IFIRST,ISECND,
     :                 MARK,OFF,RANGE)
C
C      We can now try to work out the differences between the two
C
         CALL PER_DIFFER(%VAL(IP1),AX,%VAL(IP2),AX2,%VAL(IPW1),
     :                    %VAL(IPW2))
C
C      and plot them if they were found to agree.
C      (If they were found NOT to agree the value of AX2(2) is set
C      to zero.
C
         IF (AX2(2).NE.0) THEN
            OFF=2.0*DELTAY
            RANGE(1)=LIMY(1)
            RANGE(2)=LIMY(1)+DELTAY
C
C         Note that no gaps are marked in the differences.
C
            IFIRST=AX2(2)+1
            ISECND=IFIRST
            CALL PER_PLOTL(%VAL(IPW1),%VAL(IPW2),AX2(2),IFIRST,ISECND,
     :                    MARK,OFF,RANGE)
         END IF
      END IF
C
C   Add a title
C
      TEXT=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
      IF (TEXT.NE.' ') THEN
C
C      Find out how long it is
C
         DO I=70,1,-1
            IF (TEXT(I:I).NE.' ') THEN
               NCHAR=I
               GO TO 100
            END IF
         END DO
  100    CONTINUE
         CALL TITLE(1,2,ITEXT,NCHAR)
      END IF
C
C   Finally close the plotter and tidy up.
C
      CALL ENDPLT
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
