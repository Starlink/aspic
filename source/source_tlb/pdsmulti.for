C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      ********************
C      *                  *
C      * PROGRAM PDSMULTI *
C      *                  *
C      ********************
C
C
C      Calling Sequence
C         PDSMULTI
C
C
C      FUNCTION:-
C       THIS READS A NINE TRACK PDS TAPE WRITTEN IN SCANSALOT
C       FORMAT OF SEGMENTED (LARGE) SCANS WITH ALTERNATE REFERENCE AREAS,
C       THE LATTER INCLUDE THE FIRST AND LAST OF THE SEQUENCE .
C       THESE SCANS ARE NORMALLY PRODUCED BY THE 'MULTI' INSTRUCTION
C       ON THE RGO PDS
C       THE DENSITIES ARE CONCATENATED INTO TWO STARLINK IMAGES , ONE
C       CONTAINING THE IMAGE AND THE OTHER THE REFERENCE READINGS AS
C       A N BY (NX BY NY) IMAGE.
C
C
C       USER PARAMETERS
C
C       MTUNIT              0                  Number of Mag Tape unit
C                                              where PDS tape is
C
C       NUMFILE            Next                Number of the file containing
C                                              the first scan of the wanted
C                                              image.
C                                              As each image takes up 2*n+1
C                                              files, this will be 2*n+1
C                                              greater than the NUMFILE of
C                                              the preceeding image. (n=
C                                              number of segments main image
C                                              is divided into.
C
C        CORRECT           Yes                 Is this, as judged by the
C                                              header info just typed out,
C                                              the correct image? Choices
C                                              are YES,NO.
C
C        NUMSEGS                               Number of segments main image
C                                              is divided into.
C
C        XYREF             Calculated          X,Y size of reference area.
C                                              Should be as calculated.
C
C        ZFILE                                 Name of file to write reference
C                                              area scans to. (You are
C                                              advised to make this the same
C                                              as the Main image file name
C                                              with a Z added on at the end.
C                                              You will then be able to use
C                                              PDSIM to handle them.
C
C        TITLE         file header             Title to add to file
C
C        XYMAIN       Calculated               X,Y size of main image.
C                                              Should be as calculated
C
C        OUTPUT                                Name of file Main image
C                                              is to be written to.
C
C
C
C      A J Penny                 RGO                           4-SEP-82
C----------------------------------------------------------------------
C
C
C
C       IT USES THE SET OF ROUTINES FOR HANDLING FOREIGN MAGNETIC TAPES
C       DESCRIBED IN STARLINK USER NOTE 21.1
C
C
C
      PROGRAM PDSMULTI
C
C
C
      LOGICAL TIO_ERR,TIO_EOF
      CHARACTER NUNIT*5
      CHARACTER TEXT*80
      LOGICAL*1 LOOP
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      DATA NUNIT/'_MTA*'/
C
C  Get Mag Tape unit
C
      TEXT = '0'
      CALL RDKEYC('MTUNIT',.TRUE.,1,TEXT,KTEXT,IERR)
      NUNIT(5:5) = TEXT(1:1)
      CALL TIO_OPEN(NUNIT,MTRN,IERR)
      IF (TIO_ERR(IERR)) THEN
         CALL TIO_GETMSG(IERR,TEXT,LEN)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C  Skip first 'dummy' scan
C
      NSKIP = 1
      CALL TIO_SKIP(MTRN,NSKIP,IERR)
C
C ---------------------------------------------------------
C
C  Loop round reading the requested scans
C
      NFNOW = 1
      LOOP = .TRUE.
      DO WHILE (LOOP)
C
C  Get the file number of the first file of the desired scan
C
         NUMFIL = NFNOW
         CALL GETPAR('NUMFILE','INTEGER',1,-1.0,10000.0,.TRUE.,
     +               NUMFIL,RVAL,IERR)
         CALL CNPAR('NUMFILE',ISTAT)
         IF (NUMFIL.LT.1) THEN
            LOOP = .FALSE.
         ENDIF
C
C  If file wanted is before present position, rewind and move up to it
C  (Remembering the 'dummy' file at the beginning)
C
         IF (LOOP.AND.NUMFIL.LT.NFNOW) THEN
            WRITE(TEXT,900)
  900       FORMAT(' ','HANG ON - REWINDING TAPE (FILE PRECEDES',
     +             ' CURRENT TAPE POSITION)')
            CALL WRUSER(TEXT,ISTAT)
            CALL TIO_REWIND(MTRN,IERR)
            IF (TIO_ERR(IERR)) THEN
               CALL TIO_GETMSG(IERR,TEXT,LEN)
               CALL WRUSER(TEXT,ISTAT)
            ENDIF
            NSKIP = NUMFIL
            CALL TIO_SKIP(MTRN,NSKIP,IS)
            NFNOW = NUMFIL
         ENDIF
C
C  If file wanted is after present position, move up to it
C
         IF (LOOP.AND.NUMFIL.GT.NFNOW) THEN
            NSKIP = NUMFIL - NFNOW
            CALL TIO_SKIP(MTRN,NSKIP,IERR)
            NFNOW = NUMFIL
         ENDIF
C
C ----------------------------------------------------------
C
C  Read the scan
C
         IF (LOOP) THEN
            CALL RDSCAN(MTRN,NFADD)
            NFNOW = NFNOW + NFADD
         ENDIF
C
C ------------------------------------------------------------
C
C
C  Now go back and see if another pair of images wanted
C
      ENDDO
C
C ---------------------------------------------------------------
C
C  Rewind PDS Tape at end of program
C
      CALL TIO_REWIND(MTRN,IERR)
      IF (TIO_ERR(IERR)) THEN
         CALL TIO_GETMSG(IERR,TEXT,LEN)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RDSCAN *
C      *            *
C      **************
C
C       FROM THE PDS TAPE:
C
C            ID      IS A 40 CHARACTER IDENTIFIER
C
C            NDS     IS THE NUMBER OF DATA SAMPLES IN A GIVEN SCAN
C
C            NPOS(1) IS THE X START COORDINATE
C            NPOS(2) IS THE Y START COORDINATE
C            NPOS(3) IS THE SAMPLING INTERVAL IN X
C
C            NDENS   ARE THE MEASURED DENSITY DATA SAMPLES
C                    (UP TO 4096 PER LINE)
C
C
C
C
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE RDSCAN(MTRN,NFILE)
C
C
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C
      INTEGER IDIM(2),NGETS(2)
      INTEGER*2 NDENS(16384),NDS,N
      INTEGER NPOS(3)
      CHARACTER TEXT*80,NTYPE*1,ID(40)*1,TITLE*37
      LOGICAL*1 LOOP,LOOPA
C
C  Set up starting parameters
C
      N = 4096
      NFILE = 0
      NLINE = 0
      NRLINE = 0
      NMLINE = 0
      NTYPE = 'R'
      NSEGST = 0
      NMSEG = 1000
      LOOP = .TRUE.
C
C  Loop along reading line-by-line until scan read
C
      DO WHILE (LOOP)
C
C  Read a line
C
         CALL PDSNIN(ID,NDS,NPOS,NDENS,N,MTRN)
         NLINE = NLINE + 1
C
C  Type segment information if at start of a segment
C
         IF (LOOP) THEN
            IF (NSEGST.EQ.1) THEN
               WRITE(TEXT,901) (ID(I),I=1,40),NDS,(NPOS(I),I=1,3)
  901          FORMAT(' ',40A1,I6,1X,2X,2I7,1X,2X,I5)
               CALL WRUSER(TEXT,ISTAT)
            ENDIF
         ENDIF
C
C  If 1st line of Reference scan, open Reference file
C
         IF (LOOP.AND.NLINE.EQ.1) THEN
            WRITE(TEXT,900)
  900       FORMAT(' ','IDENTIFIER',30X,'X PIXS',1X,
     +             'START COORDINATES',2X,'PITCH')
            CALL WRUSER(TEXT,ISTAT)
            WRITE(TEXT,901) (ID(I),I=1,40),NDS,(NPOS(I),I=1,3)
            CALL WRUSER(TEXT,ISTAT)
C
            IF (NDS.LT.1) THEN
               CALL WRUSER('NO PIXELS. BAD FILE OR END OF FILES',ISTAT)
               LOOP = .FALSE.
            ENDIF
C
C  Get the scan paramters
C
            IF (LOOP) THEN
               CALL WRUSER(' ',ISTAT)
               CALL WRUSER('INPUT REFERENCE FILE PARAMS',ISTAT)
               K = 1
               CALL GETCMD('CORRECT','YES,NO.',1,K,TEXT,KTEXT,IERR)
               CALL CNPAR('CORRECT',ISTAT)
               IF (K.NE.1) THEN
                  LOOP = .FALSE.
               ENDIF
            ENDIF
C
            IF (LOOP) THEN
               CALL RDKEYI('NUMSEGS',.FALSE.,1,NGETS,NVAL,IERR)
               NSEGS = NGETS(1)
               CALL CNPAR('NUMSEGS',ISTAT)
C
               NGETS(1) = NDS
               NGETS(2) = NGETS(1)
               CALL RDKEYI('XYREF',.TRUE.,2,NGETS,NVAL,IERR)
               NXREF = NGETS(1)
               NYREF = NGETS(2)*(NSEGS+1)
               NRSEG = NGETS(2)
               CALL CNPAR('XYREF',ISTAT)
C  Open reference file
C
               CALL WRUSER(' GIVE NAME OF REFERENCE IMAGE FILE',ISTAT)
               CALL CNPAR('ZFILE',ISTAT)
               IDIM(1) = NXREF
               IDIM(2) = NYREF
 100           CALL WRIMAG('ZFILE',FMT_SW,IDIM,2,IPREF,ISTAT)
               IF (ISTAT.NE.ERR_NORMAL) THEN
                  CALL WRERR('BADFRAME')
                  CALL CNPAR('ZFILE',ISTAT)
                  GO TO 100
               ENDIF
C
C  Get Title and load Title and descriptors
C
               DO KK=1,37
                  TITLE(KK:KK) = ID(KK)
               ENDDO
               CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
               CALL PTDSCR('ZFILE','TITLE','CHARACTER',IVAL,RVAL,
     +                    TITLE,IERR)
               CALL CNPAR('TITLE',ISTAT)
               INVAL = -32767
               SCALE = 1.0
               ZERO = 0.0
               CALL PTDSCR('ZFILE','INVAL','INTEGER',INVAL,RVAL,CVAL,
     +                     IERR)
               CALL PTDSCR('ZFILE','BSCALE','REAL',IVAL,SCALE,CVAL,
     +                     IERR)
               CALL PTDSCR('ZFILE','BZERO','REAL',IVAL,ZERO,CVAL,IERR)
            ENDIF
C
C  Load filemark expected position from Ref file info
C
            NEXTFM = NRSEG + 1
         ENDIF
C
C  If 1st line of Main scan, open Main file
C
         IF (LOOP.AND.NLINE.EQ.(1+NRSEG+1)) THEN
            CALL WRUSER(' ',ISTAT)
            CALL WRUSER('INPUT MAIN FILE PARAMS',ISTAT)
            K = 1
            CALL GETCMD('CORRECT','YES,NO.',1,K,TEXT,KTEXT,IERR)
            CALL CNPAR('CORRECT',ISTAT)
            IF (K.NE.1) THEN
               LOOP = .FALSE.
            ELSE
               NGETS(1) = NDS
               NGETS(2) = NGETS(1)
               CALL RDKEYI('XYMAIN',.TRUE.,2,NGETS,NVAL,IERR)
               NXMAIN = NGETS(1)
               NYMAIN = NGETS(2)
               NMSEG = NYMAIN/NSEGS
               CALL CNPAR('XYMAIN',ISTAT)
C
C
C  Open main file
C
               CALL WRUSER(' GIVE NAME OF MAIN IMAGE FILE',ISTAT)
               CALL CNPAR('OUTPUT',ISTAT)
               IDIM(1) = NXMAIN
               IDIM(2) = NYMAIN
 101           CALL WRIMAG('OUTPUT',FMT_SW,IDIM,2,IPMAIN,ISTAT)
               IF (ISTAT.NE.ERR_NORMAL) THEN
                  CALL WRERR('BADFRAME')
                  CALL CNPAR('OUTPUT',ISTAT)
                  GO TO 101
               ENDIF
C
C  Get Title and load Title and descriptors
C
               DO KK = 1,37
                  TITLE(KK:KK) = ID(KK)
               ENDDO
               CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
               CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +                 TITLE,IERR)
               CALL CNPAR('TITLE',ISTAT)
               INVAL = -32767
               SCALE = 1.0
               ZERO = 0.0
               CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVAL,RVAL,CVAL,
     +                     IERR)
               CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,SCALE,CVAL,
     +                     IERR)
               CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,ZERO,CVAL,IERR)
C
            ENDIF
C
C  Load expected filemark position from Main file info
C
            NEXTFM = NRSEG + 1 + NMSEG + 1
         ENDIF
C
C  If a file mark expected, check for it and skip this line
C
         IF (LOOP) THEN
            NSEGST = 0
            LOOPA = .TRUE.
            IF (NLINE.EQ.NEXTFM) THEN
               IF (NDS.NE.0) THEN
                  CALL WRUSER('FILE MARK MISSING, ABORTED',ISTAT)
                  LOOP = .FALSE.
               ELSE
                  LOOPA = .FALSE.
                  NSEGST = 1
                  NFILE = NFILE + 1
                  IF (NTYPE.EQ.'R') THEN
                     NTYPE = 'M'
                     NEXTFM = NEXTFM + NMSEG + 1
                  ELSE
                     NTYPE = 'R'
                     NEXTFM = NEXTFM + NRSEG + 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
C  Check for unexpected file mark
C
         IF (LOOP.AND.LOOPA) THEN
            IF (NDS.EQ.0) THEN
               LOOP = .FALSE.
               NFILE = NFILE + 1
               CALL WRUSER('BAD FILE MARK FOUND, ABORTED',ISTAT)
            ENDIF
         ENDIF
C
C  Check for line correct length
C
         IF (LOOP.AND.LOOPA) THEN
            NX = NXREF
            IF (NTYPE.EQ.'M')NX = NXMAIN
            IF (NDS.NE.NX) THEN
               CALL WRUSER('WRONG LINE LENGTH,ABORTED',ISTAT)
               LOOP = .FALSE.
            ENDIF
         ENDIF
C
C  Copy line into Reference or Main image
C
         IF (LOOP.AND.LOOPA) THEN
            IF (NTYPE.EQ.'M') THEN
               NMLINE = NMLINE + 1
               CALL DATPUT(NDENS,16384,NXMAIN,
     +                     %VAL(IPMAIN),NXMAIN,NYMAIN,NMLINE)
            ELSE
               NRLINE = NRLINE + 1
               CALL DATPUT(NDENS,16384,NXREF,
     +                     %VAL(IPREF),NXREF,NYREF,NRLINE)
            ENDIF
         ENDIF
C
C  Check if reached end of segments and read last file mark
C
         IF (LOOP.AND.LOOPA) THEN
            IF(NMLINE.EQ.NYMAIN.AND.NRLINE.EQ.NYREF) THEN
               CALL PDSNIN(ID,NDS,NPOS,NDENS,N,MTRN)
               IF (NDS.EQ.0) THEN
                  NFILE = NFILE + 1
                  CALL WRUSER('READ IMAGES OK',ISTAT)
                  LOOP = .FALSE.
               ELSE
                  CALL WRUSER('LAST FILE MARK MISSING',ISTAT)
                  LOOP = .FALSE.
               ENDIF
            ENDIF
         ENDIF
C
C
C
      ENDDO
C
C  Free the output files
C
      CALL FRDATA('OUTPUT',ISTAT)
      CALL FRDATA('ZFILE',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PDSNIN *
C      *            *
C      **************
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE PDSNIN(MD,MDS,MPOS,MDENS,M,MTRN)
C
C       THIS SUBROUTINE READS AND CONVERTS THE DATA ON A 9-TRACK
C       SCANSALOT FORMAT PDS TAPE FOR ACCESS IN OTHER SOFTWARE.
C
C       IT USES THE FOREIGN MAGNETIC TAPE HANDLING ROUTINES DESCRIBED
C       IN SUN 21.1. A CALL TO TIO_OPEN MUST BE MADE BEFORE USE OF
C       THIS SUBROUTINE.
C
C
C           B      IS THE TEXT ARRAY FOR TIO_READ.
C
C           N1
C           N2     ARE 2 TO THE POWER 8,16,24 RESPECTIVELY.
C           N3
C
C           M      IS THE DUMMY ARRAY ARGUMENT FOR MDENS
C
C       ON THE PDS TAPE:
C
C           MDENS  IS USED TO CONTAIN THE COMPUTED ARRAY OF DENSITY VALUES.
C                  THESE ARE BYTES 57 UPWARD IN EACH BLOCK.
C
C           MD     IS THE 40 CHARACTER IDENTIFIER. BYTES 3 TO 42 IN EACH BLOCK.
C
C           MDS    IS THE NUMBER DATA SAMPLES PER RECORD.BYTES 43 AND 44 IN EACH BLOCK.
C
C           MPOS(1) IS THE X START COORDINATE. BYTES 45 TO 48 IN EACH BLOCK.
C           MPOS(2) IS THE Y START COORDINATE. BYTES 49 TO 52 IN EACH BLOCK.
C           MPOS(3) IS THE SAMPLING INTERVAL IN X. BYTES 52 TO 56 IN EACH BLOCK.
C
      LOGICAL*1 B(8248)
      INTEGER*2 M,MDENS(M),MDS
      CHARACTER*1 MD(40),TEXT*80
      LOGICAL TIO_EOF,TIO_ERR
      DIMENSION MPOS(3)
      DATA N1/256/,N2/65536/,N3/16777216/
C
C        READ A BLOCK FROM THE PDS TAPE
C
      CALL TIO_READ(MTRN,(M*2+56),B,LRECL,IS)
      IF(TIO_EOF(IS)) GO TO 9999
      IF (TIO_ERR(IS)) THEN
      CALL TIO_GETMSG(IS,TEXT,LEN)
      CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C       CALCULATE MDS USING BYTE ARITHMETIC.ON PDS TAPE IT IS STORED AS -MDS.
C
      MDS=B(43)*N1+B(44)
      IF (B(44).LT.0) MDS=MDS+N1
      MDS=-MDS
C
C       CALCULATE NPOS(1),NPOS(2) AND NPOS(3) ADDING EACH BYTE TIMES
C       THE APPROPRAITE FACTOR
C
      DO 100 I=1,3
      MPOS(I)=N3*B(41+4*I)+N2*B(42+4*I)+N1*B(43+4*I)+B(44+4*I)
C
C       ALLOW FOR OVERFLOWS
C
      IF (B(42+4*I).LT.0) MPOS(I)=MPOS(I)+N3
      IF (B(43+4*I).LT.0) MPOS(I)=MPOS(I)+N2
      IF (B(44+4*I).LT.0) MPOS(I)=MPOS(I)+N1
  100 CONTINUE
C
C       CALCULATE DENSITY DATA USING BYTE ARITHMETIC
C
      DO 101 I=1,MDS
      MDENS(I)=N1*B(55+2*I)+B(56+2*I)
C
C       ALLOW FOR OVERFLOWS IN NDENS
C
 101  IF (B(56+2*I).LT.0) MDENS(I)=MDENS(I)+N1
C
C       FIND IDENTIFIER CHARACTER STRING.
C
      DO 102 I=1,40
  102 MD(I)=CHAR(B(2+I))
      GO TO 9998
C
C       NDS SET TO ZERO IF A FILE MARK HAS BEEN ENCOUNTERED ON THE
C       PDS TAPE.
C
 9999 MDS=0
C
C
C
 9998 CONTINUE
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         **************
C         *            *
C         * S/R DATPUT *
C         *            *
C         **************
C
C
C -------------------------------------------------------
C
C
C
      SUBROUTINE DATPUT(KIN,KX,NL,JOUT,JX,JY,NLINE)
C
C
C
      INTEGER*2 KIN(KX),JOUT(JX,JY)
C
C
C
      DO K = 1,NL
         JOUT(K,NLINE) = KIN(K)
      ENDDO
C
C
C
      END



