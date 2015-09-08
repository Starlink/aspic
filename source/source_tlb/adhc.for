C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   ADHC   *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               ADHC  [SHADE=1.]
C
C
C          FUNCTION:-
C               (ADHC = Args Display High Contrast)
C               It displays an image on the  ARGS  at  a  specified  center
C               after automatic scaling.  A nonlinear scaling function is
C               computed from a histogram of image pixel values to produce a
C               very high contrast display on the ARGS.
C
C
C          USE:-
C              Useful for a first look at the image and to examine the faint
C              image structure near the background level. The SHADE parameter
C              allows the overall brightness of the ARGS image to be adjusted.
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image to  be
C                                             displayed.  It  may be of any
C                                             size,  but   no   more   than
C                                             512*512    pixels   will   be
C                                             visible.
C
C         XC              256                 The x co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         YC              256                 The y co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         SHADE           0.5                 Controls the overall brightness
C                                             of the ARGS image. Smaller value
C                                             makes image brighter. Legal
C                                             range is from 0.0 to 1.0.
C
C         PTW/WFL/KFH              RGO                             6-JAN-82
C         MODIFIED WDP             AAO                             6-JUN-82
C
C--------------------------------------------------------------------------
C

*
*  ADHC IMAGE [XCENTRE] [YCENTRE] [SHADE]
*
*  VERSION OF DISP PROGRAM BUT PERMITTING DISPLAY CENTRED
*  ON ANY ARGS COORDINATES XC, YC AND ALSO
*  UPDATING ARGS DATABASE FILE
*
*  ORIG PTW MODIF FOR DATABASE BY WFL JULY 1981
*
*
*  MODIFIED BY WDP TO AUTOMATICALLY SCALE IMAGE
*    (DOES A "HISTOGRAM EQUALIZATION") TO
*    PRODUCE VERY HIGH CONTRAST IMAGE ON ARGS.  USEFUL FOR
*    EXAMINING FAINT STRUCTURE NEAR THE BACKGROUND LEVEL
*
*   EXPERIMENTAL VERSION USING 'DATA STRUCTURE'
*
      INTEGER IDIMN(99)
      CHARACTER VALUE*80
      INCLUDE 'INTERIM(FMTPAR)'
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
      ELSE
         CALL RDIMAG('IMAGE',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
         IF (NDIMS.NE.2) THEN
            CALL WRUSER('MUST BE 2D IMAGE!',J)
         ELSE
            NELS=1
            IXC=256
            IYC=256
            DO I=1,NDIMS
               NELS=NELS*IDIMN(I)
            END DO
            CALL GETDYN('IWK',FMT_SW,NELS,IWKP,J)
            CALL RDKEYI('XC',.TRUE.,1,IXC,NVALS,JSTAT)
            CALL RDKEYI('YC',.TRUE.,1,IYC,NVALS,JSTAT)
            CALL RDKEYR('SHADE',.FALSE.,1,SHADE,NVALS,JSTAT)
            CALL ASP_SWSP(500,8)
            CALL ASP_STAT('IMAGE',%VAL(IPIN),NELS,VTOT,VMEAN,VLO,
     &            VHI,VSIG,JSTAT)
            CALL ADHC2(%VAL(IPIN),IDIMN(1),IDIMN(2),
     &                  IXC,IYC,VLO,VHI,SHADE,%VAL(IWKP))
            CALL ASP_RWSP
            CALL FRDATA(' ',JSTAT)
	    CALL ARGS_NUMIM(IDMAX)
	    IF (IDMAX.GE.1) THEN
		CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
		CALL ASP_DZTOI('ZXC',VALUE,IZXC,JSTAT)
		CALL ASP_DZTOI('ZYC',VALUE,IZYC,JSTAT)
		CALL ASP_DZTOI('ZXF',VALUE,IXF,JSTAT)
		CALL ASP_DZTOI('ZYF',VALUE,IYF,JSTAT)
		CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	    ELSE
		IZXC=256
		IZYC=256
		IXF=1
		IYF=1
	    ENDIF
            CALL ARGS_WRIM (IXC,IYC,IDIMN(1),IDIMN(2),IDIMN(1),IDIMN(2),
     &         JSTAT)
            IF(JSTAT.NE.0)THEN
               CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)
            ELSE
               CALL ARGS_RDPAR ('DISPZOOM',1,VALUE,NVALS,JSTAT)
               CALL ASP_FTODZ ('PVLO',VLO,VALUE,JSTAT)
               CALL ASP_FTODZ ('PVHI',VHI,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZXC',IZXC,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZYC',IZYC,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZXF',IXF,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZYF',IYF,VALUE,JSTAT)
               CALL ARGS_WRPAR ('DISPZOOM',VALUE,1,JSTAT)
            ENDIF
         END IF
      ENDIF
      END
C
      SUBROUTINE ADHC2(PIC,NX,NY,IXC,IYC,VLO,VHI,SHADE,IPIC)
      PARAMETER NINTER=200
      INTEGER NX,NY,IXC,IYC
      INTEGER*2 IPIC(NX*NY),IDUMMY
      REAL PIC(NX*NY),VLO,VHI,VL,D,SCALE,V
      REAL MINVAL,MAXVAL
      DIMENSION VAL(NINTER),XHIS(NINTER),SIG(NINTER),A(10),
     1 NHIS(NINTER)
      DOUBLE PRECISION VAL,XHIS,SIG,A,CHI,RMS,XVAL
      CHARACTER BUFFER*80
C
      MINVAL=VLO
      MAXVAL=VHI
C
      YZERO=SHADE*2.
      YZERO=MIN(MAX(YZERO,0.),2.)
C
C     YZERO CONTROLS SHAPE OF HISTOGRAM OF OUTPUT DENSITIES
C     YZERO=1, FLAT HISTOGRAM
C     YZERO=0, STRONGLY WEIGHTED TOWARDS WHITE
C     YZERO=2, STRONGLY WEIGHTED TOWARDS BLACK
C
C     COEFFICIENTS OF QUADRATIC EQUATION TO BE SOLVED
      B=YZERO
      AQ=1.-YZERO
      A2=2.*AQ
      A4=4.*AQ
      B2=B*B
C
C        COMPUTE 'NINTER' INTERVAL HISTOGRAM OF PIXEL VALUES
C
      NCYCLES=0
      CALL WRUSER('COMPUTING HISTOGRAM OF PIXEL VALUES...',ISTAT)
10       WRITE(BUFFER,1000)MINVAL,MAXVAL
1000     FORMAT('MIN AND MAX VALUES =',2E15.5)
         CALL WRUSER(BUFFER,ISTATUS)
         LOWPTS=0
         DO 15 I=1,NINTER
15       NHIS(I)=0
         XINTER=NINTER
         DX=(MAXVAL-MINVAL)/XINTER
         IF (DX .EQ. 0.)THEN
           CALL WRUSER('FAILED: ALL PIXELS HAVE SAME VALUE',ISTAT)
           STOP
         END IF
         DO 20 I=1,NX*NY
            V=PIC(I)
            K=(V-MINVAL)/DX+1.
            IF (K .LT. 1 )THEN
              LOWPTS=LOWPTS+1
            ELSE IF (K .LE. NINTER)THEN
              NHIS(K)=NHIS(K)+1
            END IF
20       CONTINUE
C
C        COMPUTE NORMALIZED CUMULATIVE HISTOGRAM
C
         PTS=NX*NY
         NHIS(1)=NHIS(1)+LOWPTS
         DO 30 I=2,NINTER
30       NHIS(I)=NHIS(I)+NHIS(I-1)
         DO 35 I=1,NINTER
            VAL(I)=MINVAL+I*DX
            XHIS(I)=NHIS(I)/PTS
35       CONTINUE
C
C        FIT 6 TERM POLYNOMIAL TO CUMULATIVE DIST. FUNC. IN RANGE
C         OF 2 TO 96 PERCENT
C
         J=0
         DO 40 I=1,NINTER
            IF (XHIS(I) .GT. .02 .AND. (XHIS(I) .LT. .96
     1       .OR. J .EQ. 0))THEN
               J=J+1
               XHIS(J)=XHIS(I)
               VAL(J)=VAL(I)
            END IF
40       CONTINUE
         J=MAX(1,J)
         MAXVAL=VAL(J)
         MINVAL=VAL(1)
         WRITE(BUFFER,1001)J
1001     FORMAT(I6,' HISTOGRAM BINS IN 2-96 PERCENT RANGE')
         CALL WRUSER(BUFFER,ISTATUS)
C
C        IF THERE ARE LESS THAN 20 BINS IN THE HISTOGRAM, THEN
C        REDO HISTOGRAM WITH NARROWER LIMITS
C
         IF (J .LT. 20 .AND. NCYCLES .LE. 2)THEN
            NCYCLES=NCYCLES+1
            MINVAL=MINVAL-2.*DX
            MAXVAL=MAXVAL+DX
            GO TO 10
         END IF
C
         IF (J .LT. 7)THEN
          CALL WRUSER('FAILED DUE TO PECULIAR PIXEL DISTRIBUTION.'
     1                ,ISTAT)
          CALL WRUSER(' USUALLY CAUSED IF IMAGE HAS ONLY',ISTAT)
          CALL WRUSER(' A FEW DISCRETE PIXEL VALUES',ISTAT)
          STOP
         END IF
C
C        FOR MAX. PRECISION, NORMALIZE VAL TO RANGE 0-1
C
         DELX=VAL(J)-VAL(1)
         XFST=VAL(1)
         DO 45 I=1,J
45       VAL(I)=(VAL(I)-XFST)/DELX
C
         CALL POLFIT(VAL,XHIS,SIG,J,6,0,A,CHI,RMS)
         WRITE(BUFFER,1002)MINVAL,MAXVAL
1002     FORMAT('GRAY SCALE RANGES FROM',E15.5,' TO',E15.5)
         CALL WRUSER(BUFFER,ISTATUS)
C
C     CALCULATE SCALED INTENSITY (0-255) FOR EACH PIXEL
C
      DO 100 I=1,NX*NY
         VV=PIC(I)
         IF (VV .LT. MINVAL)THEN
            IPIC(I)=0
         ELSE IF (VV .LT. MAXVAL)THEN
            XVAL=(VV-XFST)/DELX
            TEMP=((((A(6)*XVAL+A(5))*XVAL+A(4))*XVAL+A(3))*XVAL+A(2))
     1       *XVAL+A(1)
            IF (SHADE .NE. .5)TEMP=(-B+SQRT(B2+A4*TEMP))/A2
            IPIC(I)=MIN(MAX(0.,TEMP*256.),255.)
         ELSE
            IPIC(I)=255
         END IF
100   CONTINUE
C
C     DISPLAY IMAGE ON ARGS
C
      CALL SRPXI2(IPIC,NX,NX,NY,IXC-NX/2,IYC-NY/2,
     &               16,.FALSE.,IDUMMY,1)
      VLO=MINVAL
      VHI=MAXVAL
      END
      SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR,RMS)
C
C     DOES A LEAST SQUARES FIT TO DATA WITH A POLYNOMIAL CURVE,
C     Y=A(1)+A(2)*X+A(3)*X**2+...
C     X=ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE
C     Y=ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C     SIGMAY=ARRAY OF STANDARD DEV. FOR Y DATA POINTS
C     NPTS=NUMBER OF PAIRS OF DATA POINTS
C     NTERMS=NUMBER OF COEFFICINTS (DEGREE OF POLYNOMIAL+1)
C     MODE=DETERMINES METHOD OF WEIGHTING LEAST SQUARES FIT
C          +1=(INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C           0=(NO WEIGHTING)  WEIGHT(I)=1.
C          -1=(STATISTICAL)  WEIGHT(I)=1./Y(I)
C     A=ARRAY OF COEFFICIENTS OF POLYNOMIAL
C     CHISQR=REDUCED CHI SQUARE FOR FIT
C     NEEDS FUNCTION DETERM
C     VALID UP TO NTERMS=10
C
      IMPLICIT DOUBLE PRECISION (A-H),(O-Z)
      DIMENSION X(1),Y(1),A(1),SIGMAY(1)
      DIMENSION SUMX(19),SUMY(10),ARRAY(10,10)
C
C     ACCUMULATE WEIGHTED SUMS
C
      NMAX=2*NTERMS-1
      DO 13 N=1,NMAX
13    SUMX(N)=0.
      DO 15 J=1,NTERMS
15    SUMY(J)=0.
      CHISQ=0.
      DO 50 I=1,NPTS
      XI=X(I)
      YI=Y(I)
      IF (MODE)32,37,39
32    IF (YI)35,37,33
33    WEIGHT=1./YI
      GO TO 41
35    WEIGHT=-1./YI
      GO TO 41
37    WEIGHT=1.
      GO TO 41
39    WEIGHT=1./SIGMAY(I)**2
41    XTERM=WEIGHT
      DO 44 N=1,NMAX
      SUMX(N)=SUMX(N)+XTERM
44    XTERM=XTERM*XI
      YTERM=WEIGHT*YI
      DO 48 N=1,NTERMS
      SUMY(N)=SUMY(N)+YTERM
48    YTERM=YTERM*XI
      CHISQ=CHISQ+WEIGHT*YI**2
50    CONTINUE
C
C     CONSTRUCT MATRICES AND CALCULATE COEFFICIENTS
C
      DO 54 J=1,NTERMS
      DO 54 K=1,NTERMS
      N=J+K-1
54    ARRAY(J,K)=SUMX(N)
      DELTA=DETERM(ARRAY,NTERMS)
      IF(DELTA)61,57,61
57    CHISQR=0.
      DO 59 J=1,NTERMS
59    A(J)=0.
      GO TO 80
61    DO 70 L=1,NTERMS
      DO 66 J=1,NTERMS
      DO 65 K=1,NTERMS
      N=J+K-1
65    ARRAY(J,K)=SUMX(N)
66    ARRAY(J,L)=SUMY(J)
70    A(L)=DETERM(ARRAY,NTERMS)/DELTA
C
C     CALCULATE CHI SQUARE
C
      FREE=NPTS-NTERMS
      IF (FREE .LE. 0.)RETURN
      DO 75 J=1,NTERMS
      CHISQ=CHISQ-2.*A(J)*SUMY(J)
      DO 75 K=1,NTERMS
      N=J+K-1
75    CHISQ=CHISQ+A(J)*A(K)*SUMX(N)
      CHISQR=CHISQ/FREE
C
C     CALCULATE STANDARD DEVIATION
C
      RMS=0.
      DO 90 K=1,NPTS
      CALC=A(1)
      IF (NTERMS .EQ. 1)GO TO 90
      DO 95 J=2,NTERMS
95    CALC=CALC+A(J)*X(K)**(J-1)
90    RMS=RMS+(Y(K)-CALC)**2
      RMS=SQRT(RMS/FREE)
80    RETURN
      END
      FUNCTION DETERM(ARRAY,NORDER)
C
C     CALCULATES THE DETERMINANT OF A SQUARE MATRIX
C     ARRAY=MATRIX
C     NORDER=ORDER OF DETERMINANT (DEGREE OF MATRIX)
C     NOTE: THIS ROUTINE DESTROYS THE INPUT MATRIX ARRAY
C     DIMENSION VALID FOR NORDER UP TO 10
      IMPLICIT DOUBLE PRECISION (A-H),(O-Z)
      DIMENSION ARRAY(10,10)
      DETERM=1.
      DO 50 K=1,NORDER
C
C     INTERCHANGE COLUMNS IF DIAGONAL ELEMENT IS ZERO
C
      IF (ARRAY(K,K))41,21,41
21    DO 23 J=K,NORDER
      IF (ARRAY(K,J))31,23,31
23    CONTINUE
      DETERM=0.
      GO TO 60
31    DO 34 I=K,NORDER
      SAVE=ARRAY(I,J)
      ARRAY(I,J)=ARRAY(I,K)
34    ARRAY(I,K)=SAVE
      DETERM=-DETERM
C
C     SUBTRACT ROW K FROM LOWER ROWS TO GET DIAGONAL MATRIX
C
41    DETERM=DETERM*ARRAY(K,K)
      IF (K-NORDER)43,50,50
43    K1=K+1
      DO 46 I=K1,NORDER
      DO 46 J=K1,NORDER
46    ARRAY(I,J)=ARRAY(I,J)-ARRAY(I,K)*ARRAY(K,J)/ARRAY(K,K)
50    CONTINUE
60    RETURN
      END
