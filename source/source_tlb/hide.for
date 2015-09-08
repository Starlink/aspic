      PROGRAM HIDE
C+	HIDE - Hidden Line Plotting of STARLINK Images
C
C	This program uses the subroutines SOLID,SLICED and
C	SKETCH found in SIMPLEPLOT to draw hidden line images
C	on any GKS devices.
C	It is also possible to limit the range of values
C	plotted.
C
C       Argument list:
C	--------------
C	DEVICE                   A character string containing the GKS
C                                device number or a workstation name as
C                                described in SGP 26
C
C	IN			 Name of Image file
C
C	ANGLE			 Angle of view (in degrees) default 45
C
C	CORNER			 Corner to be nearest viewer. Corners
C				 are defined as follows. For a n by n
C				 array, corner 1 is 1,1
C				 corner 2 is n,1; corner 3 is n,n;
C				 corner 4 is 1,n.
C
C	TYPE			 Type of drawing required
C				 may be SOLID SLICED or SKETCH
C
C	LIMS			 Sets up ability to insert limiting
C				 values on plotting. May only be specified
C				 on the commnad line e.g.
C
C				  HIDE LIMS=Y
C
C
C
C	MOD			 Minimum and maximum values to be plotted
C
C       The program has been modified to prevent it looping back for
C       more input in accordance with the recent decision about ASPIC
C       programs.
C
C	J V Carey
C	1981 March 16 modified 1981 September 1
C
C	Modified Jan 1984 to run over GKS by D.J.King
C-
      IMPLICIT INTEGER (A-Z)
      PARAMETER (NDEV = 3 )
      CHARACTER*6 PICT,TYPE,DEVICE*12,DEV*4
      CHARACTER*1 LIMS
      INTEGER*4 RESTAT
      INTEGER DIM(2)
      REAL ANGLE,SIZER,XMAX,YMAX
      REAL LIMMOD(2)
      LOGICAL CURSOR
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      STATUS=-1
      DO WHILE(STATUS.NE.ERR_NORMAL)
         STATUS=ERR_NORMAL
         CALL RDKEYC('DEVICE',.FALSE.,1,DEVICE,AVAL,STATUS)
         IF (STATUS.NE.ERR_NORMAL) THEN
            CALL WRERR('ERRDEV')
            CALL CNPAR('DEVICE',STAT)
         ELSE
            CALL DEVTRAN(DEVICE,IDEV,ICONID,STATUS)
            IF (STATUS.NE.ERR_NORMAL) THEN
               CALL WRERR('ERRDEV')
               CALL CNPAR('DEVICE',STAT)
            ENDIF
         ENDIF
      ENDDO
      CALL JBDEV(DEVICE)
      CALL JBINQ(XMAX,YMAX,CURSOR)
      SIZER=MIN(XMAX,YMAX)
      IF (IDEV.EQ.5.OR.
     +    IDEV.EQ.7.OR.
     +    IDEV.EQ.9.OR.
     +    IDEV.EQ.10) THEN
75      CALL RDKEYR('SIZE',.TRUE.,1,SIZER,AVAL,STATUS)
        IF(STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
          CALL WRERR('ERRSIZ')
          CALL CNPAR('SIZE',STATUS)
          GO TO 75
        ENDIF
        CALL SIZE(SIZER + 8.0)
      ENDIF
C
C     OPEN IMAGE FILE
C
100   CALL RDIMAG('IN',FMT_R,2,DIM,ADIM,POINT,STATUS)
      IF(STATUS.NE.ERR_NORMAL) THEN
      CALL WRERR('ERRIN')
      CALL CNPAR('IN',STATUS)
      GO TO 100
      ENDIF
C
C       SET DEFAULTS
C
      ANGLE=45.0
      CORNER=1
      TYPE='SKETCH'
C
C      DETERMINE ANGLE OF VIEW
C
200   CALL RDKEYR('ANGLE',.TRUE.,1,ANGLE,AVAL,STATUS)
      IF(STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
      CALL WRERR('ERRANG')
      CALL CNPAR('ANGLE',STATUS)
      GO TO 200
      ENDIF
C
C     DETERMINE WHICH CORNER IS TOWARDS YOU
C
300   CALL RDKEYI('CORNER',.TRUE.,1,CORNER,AVAL,STATUS)
      IF(STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
      CALL WRERR('ERRCOR')
      CALL CNPAR('CORNER',STATUS)
      GO TO 300
      ENDIF
C
C     DETERMINE TYPE OF PICTURE TO BE DRAWN  1: SOLID
C                                            2: SLICED
C                                            3: SKETCH
C
400   CALL RDKEYC('TYPE',.TRUE.,1,TYPE,AVAL,STATUS)
      IF(STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
      CALL WRERR('ERRTYPE')
      CALL CNPAR('TYPE',STATUS)
      GO TO 400
      ENDIF
C
C      DETERMINE IF LIMITS TO BE IMPOSED
C
      CALL RDKEYC('LIMS',.FALSE.,1,LIMS,AVAL,STATUS)
      IF(LIMS.EQ.'Y')THEN
      CALL RDKEYR('MOD',.FALSE.,2,LIMMOD,AVAL,STATUS)
      CALL LIM3D(LIMMOD(1),LIMMOD(2))
      CALL CNPAR('MOD',STATUS)
      ENDIF
      RESTAT=STR$UPCASE(TYPE,TYPE)
      IF(TYPE.EQ.'SOLID'.OR.TYPE.EQ.'SLICED'.OR.TYPE.EQ.'SKETCH') THEN
      CALL DRAWIT(TYPE,%VAL(POINT),DIM(1),DIM(2),ANGLE,CORNER,SIZER)
      CALL FRDATA(' ',STATUS)
      CALL ENDPLT
      CALL EXIT
      ELSE
      CALL CNPAR('TYPE',STATUS)
      GO TO 400
      ENDIF
      CALL CNPAR('TYPE',STATUS)
      CALL CNPAR('ANGLE',STATUS)
      CALL CNPAR('CORNER',STATUS)
      GO TO 200
      END
      SUBROUTINE DRAWIT(SUBR,Z,I1,I2,A,CORNER,SIZER)
C
C     DRAWS THE CORRECT FIGURE
C
      CHARACTER*6 SUBR
      INTEGER CORNER
      REAL Z(I1,I2)
C
C     VALUE OF CORNER DETERMINES ORIENTATION OF IMAGE
C
      NX=I1
      NY=I2
      IF(CORNER.EQ.2) NX = -NX
      IF(CORNER.EQ.3) THEN
      NX=-NX
      NY=-NY
      ENDIF
      IF(CORNER.EQ.4) NY=-NY
      IF(SUBR.EQ.'SOLID') CALL SOLID(Z,NX,NY,SIZER,A)
      IF(SUBR.EQ.'SLICED') CALL SLICED(Z,NX,NY,SIZER,A)
      IF(SUBR.EQ.'SKETCH') CALL SKETCH(Z,NX,NY,SIZER,A)
      RETURN
      END
