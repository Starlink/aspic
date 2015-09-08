	SUBROUTINE CRB_CONTOUR
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   CONTOUR *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               CONTOUR  [PENS=TRUE BATCH=FALSE]
C
C
C          FUNCTION:-
C               It draws contours of a 2-d Starlink image on one of seve
C               graphics  devices.  Options are available to select part
C               the image only, and to smooth the data before contouring
C               done.
C
C
C          USE:-
C               It  is  most  useful  for  generating  hard  copy   as
C               alternative to VERGREY.
C
C
C
C         USER PARAMETERS:-
C
C         DEVICE                              This is the  graphics  dev
C                                             to  be used. It is  specif
C                                             by a character  string  wh
C                                             is the GKS  device  number
C                                             a    workstation    name
C                                             described in SGP 26
C
C         IN                                  This  is  the  2-D   Starl
C                                             input image.
C
C         BLOCK           1                   This is the factor to be u
C                                             for  blocking the data bef
C                                             contouring.
C
C         SIZE                                Only used for workstations
C                                             7, 9, 10  (hardcopy device
C                                             and  defines the plot size
C                                             cms..
C
C
C
C         WHOLE           Yes                 Decides if the whole or  p
C                                             of   the  picture  is  to
C                                             plotted.
C
C         SAMPLES                             If WHOLE=NO this defines
C                                             range  of pixels to be use
C                                             4 values in the order Xsta
C                                             Xend,Ystart,Yend.
C
C         HEIGHTS                             Up to 30 contour heights
C                                             be entered, separated by
C                                             commas or spaces.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         PENS            TRUE                If PENS  is  true  then  e
C                                             contour  level  will be dr
C                                             using  a  different  GKS p
C                                             The  result  will  depend
C                                             the device i.e. the ARGS w
C                                             produce different colours
C                                             a GOC will  produce differ
C                                             line types.  If PENS is fa
C                                             then the default  pen will
C                                             used for all  contour leve
C
C         BATCH           FALSE               In   interactive   mode
C                                             program will allow a user
C                                             type CONTROL_C  to  termin
C                                             plotting   of   the   curr
C                                             contour level. If CONTOUR
C                                             run in BATCH mode then  si
C                                             CONTROL_C is  not  valid
C                                             program   will   fail  unl
C                                             BATCH is set to true.
C
C
C         J V Carey                RGO                             6-JAN
C
C modified to run over GKS
C         D J King                 RGO                            26-JAN
C
C
C-----------------------------------------------------------------------
 
 
 
C    CONTOUR PLOTTING PROGRAM ADAPTED FOR STARLINK INTERIM
C      ENVIRONMENT FROM M J CURRIE'S PROGRAM BY J V CAREY
C      1980 NOV 18
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      CHARACTER*12 DEV,DEVICE,TITLE*80
      CHARACTER*4 HEDX
      INTEGER STSAM,ENSAM,ENREC,STREC
      INTEGER STAT,OUTDIM,IDIM(2),IPOINT,BLK
      INTEGER SAMPLES(4),IDIMA(2),STATUS
      INTEGER*4 RESTAT
      CHARACTER*1 WHOLE
      REAL HT(30)
      LOGICAL CURSOR,BATCH,PENS
      INTEGER ITITLE(10)
	CALL CNPAR('DEVICE',ISTAT)
      WHOLE = 'Y'
      CALL RDKEYL('PENS',.FALSE.,1,PENS,IVAL,STATUS)
      CALL RDKEYL('BATCH',.FALSE.,1,BATCH,IVAL,STATUS)
      IF (.NOT.BATCH) THEN
         CALL SETUP('TT')
         CALL ENABLE
      ENDIF
      BLK = 1
      ISTAT=-1
      DO WHILE (ISTAT.NE.0)
         CALL RDKEYC('DEVICE',.TRUE.,1,DEVICE,IVAL,STAT)
         IF (STAT.NE.ERR_NORMAL.AND.STAT.NE.ERR_PARNUL) THEN
            CALL WRERR('ERRDEV')
            CALL CRB_CONTEXIT
         ELSE
            CALL DEVTRAN(DEVICE,IDEV,ICONID,ISTAT)
            IF (ISTAT.NE.0) THEN
               CALL CNPAR('DEVICE',STAT)
               CALL WRERR('ERRDIN')
            ENDIF
	    RESTAT=STR$UPCASE(DEVICE,DEVICE)
         ENDIF
      ENDDO
C
        CALL JBDEV(DEVICE)
      CALL JBINQ(XMAX,YMAX,CURSOR)
      SIZE=MIN(XMAX,YMAX)
      IF (IDEV.EQ.9.OR.
     +    IDEV.EQ.10.OR.
     +    IDEV.EQ.5.OR.
     +    IDEV.EQ.7) THEN
         CALL RDKEYR('SIZE',.TRUE.,1,SIZE,IVAL,STAT)
         IF (STAT.NE.ERR_NORMAL.AND.STAT.NE.ERR_PARNUL) THEN
            CALL WRERR('ERRSIZ')
            CALL CRB_CONTEXIT
         ENDIF
      ENDIF
C
C     OPEN FILE
C
      CALL RDIMAG('IMAGE',FMT_R,2,IDIM,OUTDIM,IPOINT,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRFLB')
	CALL CRB_CONTEXIT
      ENDIF
      CALL RDDSCR('IMAGE','COMMENT',1,TITLE,IVAL,STATUS)
	IF(IVAL.EQ.0) THEN
	DO NNN = 1,80
	  TITLE(NNN:NNN)=' '
	ENDDO
	ENDIF
      READ(TITLE,900) ITITLE
900   FORMAT(10A4)
200   CALL RDKEYC('WHOLE',.TRUE.,1,WHOLE,IVAL,STAT)
      IF(STAT.NE.ERR_PARNUL.AND.STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRANS')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      RESTAT=STR$UPCASE(WHOLE,WHOLE)
      IF(WHOLE.NE.'Y'.AND.WHOLE.NE.'N') THEN
      CALL CNPAR('WHOLE',STAT)
      GO TO 200
      ENDIF
      IF(WHOLE.EQ.'N') THEN
      CALL WRUSER('STSAM,ENSAM,STREC,ENREC',STAT)
300   CALL RDKEYI('SAMPLES',.FALSE.,4,SAMPLES,IVAL,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRSAM')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      IF(IVAL.LT.4) THEN
      CALL WRUSER('MORE VALUES REQUIRED',STAT)
      CALL CNPAR('SAMPLES',STAT)
      GO TO 300
      ENDIF
      STSAM = SAMPLES(1)
      ENSAM = SAMPLES(2)
      STREC = SAMPLES(3)
      ENREC = SAMPLES(4)
C
C    FORM NEW FRAME FOR PLOTTING
C
      IDIMA(1) = 1 + ENSAM - STSAM
      IDIMA(2) = 1 + ENREC - STREC
      CALL GETDYN('TEMFILE',FMT_R,IDIMA(1)*IDIMA(2),IPOINTA,STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRWRIM')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      CALL COPYARRAY(%VAL(IPOINT),%VAL(IPOINTA),STSAM,ENSAM,
     1              STREC,ENREC,IDIM(1),IDIM(2),IDIMA(1)*IDIMA(2))
      IPOINT = IPOINTA
      IDIM(1) = IDIMA(1)
      IDIM(2) = IDIMA(2)
      ELSE
      STREC = 1
      STSAM = 1
      ENREC = IDIM(2)
      ENSAM = IDIM(1)
      ENDIF
C
C     OBTAIN BLOCKING FACTOR AND BLOCK IF NEEDED
C
400   CALL RDKEYI('BLOCK',.TRUE.,1,BLK,IVAL,STAT)
      IF(STAT.NE.ERR_PARNUL.AND.STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRBLK')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      IF(BLK.LT.1) THEN
      CALL CNPAR('BLOCK',STAT)
      GO TO 400
      ENDIF
      IF(BLK.GT.1) THEN
      IDIMA(1) = (ENSAM - STSAM +1)/BLK
      IDIMA(2) = (ENREC - STREC +1)/BLK
      CALL GETDYN('ZFILE',FMT_R,IDIMA(1)*IDIMA(2),IPOINTA,
     1             STAT)
      IF(STAT.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRZFIL')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      CALL BLOCKIT(%VAL(IPOINT),%VAL(IPOINTA),BLK,
     1             IDIM(1),IDIM(2),IDIMA(1)*IDIMA(2))
      IF(BLK.EQ.1) THEN
      CALL FRDATA('IN',STAT)
	CALL CNPAR('IN',STAT)
      ELSE
      CALL FRDATA('TEMFILE',STAT)
	CALL CNPAR('TEMFILE',STAT)
      ENDIF
      IPOINT = IPOINTA
      IDIM(1) = IDIMA(1)
      IDIM(2) = IDIMA(2)
      ENDIF
      CALL RDKEYR('HEIGHTS',.FALSE.,30,HT,IVAL,STAT)
      IF(STAT.NE.ERR_NORMAL)THEN
      CALL WRERR('ERRHT')
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      ENDIF
      NH = IVAL
      CALL CONTOUR(IDEV,SIZE,HT,NH,STSAM,ENSAM,
     1             STREC,ENREC,%VAL(IPOINT),IDIM(1),
     2             IDIM(2),ITITLE,BLK,PENS)
      CALL FRDATA(' ',STAT)
      CALL CRB_CONTEXIT
      END
