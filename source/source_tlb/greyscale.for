      PROGRAM GREYMAIN
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   GREYSCALE *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C               GREYSCALE
C
C
C          FUNCTION:-
C               It generates output which, when plotted on  the  Printronix
C               or  Versatec  printers, gives  a  greyscale  representation
C               of the input image.  The technique used generates a density
C               of  dots in the  output  which is proportional to the input
C               data value. The pattern of dots is random.
C
C
C          USE:-
C               It gives a hard copy of image data.
C
C		When the hard copy is  for  the  Printronix it  is  written
C               to  a  file  called  GREYPLT.LIS  which  is   automatically
C               queued  to  the  printer   when   the   program   finishes.
C               Additional  copies  may  be obtained by typing the command:
C               PRINT/QUEUE=SYS_PRINTRONIX/PASSALL/NOFEED GREYPLT
C
C               When the hard copy is for the Versatec  it  is written to a
C               file called DIT.DAT which is  automatically  queued  to the
C               Versatec when the program  finishes.  A file called DIT.LIS
C               is  also   queued  to  the   Versatec   and  this  contains
C               information on the image. Additional copies may be obtained
C               by typing the command:  
C               PRINT/QUEUE=SYS_VERSATEC/PASSALL/NOFEED DIT.DAT,DIT.LIS
C               
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This  is  the  2-D   starlink
C                                             image which is to be plotted.
C
C
C         DEVICE          PRINTRONIX          Output device.  Default is
C                                             the Printronix, otherwise the
C                                             Versatec printer.
C
C         DEFAULT         T                   If TRUE, will assume  default
C                                             values  for  rotation, window
C                                             size, pixel size, grid lines,
C                                             and intensity scaling.
C
C         ROTATE          F                   If TRUE, will rotate image 90
C                                             degrees on the printer page.
C
C         CHANGE          F                   If  TRUE,  will  change   the
C                                             default   window  size  which
C                                             defines the area of  the  2-D
C                                             array to be printed.
C
C         IVALUES         1,MAXCOLS           The first and last column  of
C                                             the  2-D starlink image to be
C                                             included in the window.
C
C         JVALUES         1,MAXROWS           The first and last row of the
C                                             2-D   starlink  image  to  be
C                                             included in the window.
C
C
C         GRID            0                   Specifies  the  spacing   (in
C                                             pixel   units)  between  grid
C                                             lines to be superposed on the
C                                             output  plot.  If  0, no grid
C                                             lines will be generated.
C
C         PIXEL           WIDTH               Specifies the height of  each
C                                             image   pixel.   The  maximum
C                                             width   output   is    always
C                                             produced      (within     the
C                                             constraint  that  each  image
C                                             pixel   must  be  an  integer
C                                             number of printer dots  wide)
C                                             and  by  default square image
C                                             pixels     are      produced.
C                                             (Actually the image height is
C                                             compressed  by  a  factor  of
C                                             60/72  due  to the spacing of
C                                             the rows on the Printronix).
C
C         MAXINF          T                   If TRUE,  will  automatically
C                                             scale   the  image,  using  a
C                                             histogram        equalization
C                                             technique,   to   convey  the
C                                             maximum amount of information
C                                             in the output image.
C
C         WHITE           MIN                 If MAXINF = FALSE, then  this
C                                             defines  the  array  value to
C                                             correspond to pure  white  on
C                                             the output plot.
C
C         BLACK           MAX                 IF MAXINF = FALSE, then  this
C                                             defines  the  array  value to
C                                             correspond to pure  black  on
C                                             the output plot.
C
C         LOG             F                   If MAXINF = FALSE, then  this
C                                             defines    whether   or   not
C                                             logarithmic intensity scaling
C                                             is to be used.
C
C         LABEL                               A title, up to 80 characters,
C                                             to  be added at the bottom of
C                                             the plot.
C
C
C
C
C         WDP                      AAO                             4-JUL-82
C
C
C--------------------------------------------------------------------------



      DIMENSION IDIMEN(2)
      CHARACTER DEVICE*10
      CALL RDIMAG('INPUT',204,2,IDIMEN,NDIM,NPOINT,ISTATUS)
      IF (ISTATUS .NE. 0)STOP
      NCOL=IDIMEN(1)
      NROW=IDIMEN(2)
      MINC=1
      MAXC=NCOL
      MINR=1
      MAXR=NROW
      CONVERT=1.
      DEVICE='PRINTRONIX'
      CALL RDKEYC('DEVICE',.TRUE.,1,DEVICE,NV,IS)
      IF (DEVICE .EQ. 'PRINTRONIX')THEN
        NUNIT=-8
      ELSE
        NUNIT=8
      END IF
      CALL GREY(%VAL(NPOINT),NCOL,NROW,NCOL,NROW,MINC,MAXC,MINR,MAXR,
     1 CONVERT,NUNIT)
      END
      SUBROUTINE GREY(IRAY,ICOL,IROW,NCOL,NROW,MINC,MAXC,
     1 MINR,MAXR,CONVERT,NUNIT)
C
C *****************************************************************
C
C     DEFINES PARAMETERS AND ARRAYS FOR MAKING A GREY SCALE PLOT
C     ON THE VERSATEC OR PRINTRONIX PRINTER/PLOTTER
C
C     WRITTEN BY W D PENCE, UNIV. OF SUSSEX, OCT 1980
C
C     IRAY = 2 DIMENSIONAL IMAGE ARRAY (R*4)
C     ICOL = SIZE OF THE FIRST DIMENSION OF IRAY (I*4)
C     IROW = SIZE OF THE SECOND DIMENSION OF IRAY (I*4)
C     NCOL = ACTUAL NUMBER OF COLUMNS IN IRAY (.LE. ICOL)
C     NROW = ACTUAL NUMBER OF ROWS (RECORDS) IN IRAY (.LE. IROW)
C   THE FOLLOWING FOUR PARAMETERS DEFINE A WINDOW AREA WITHIN THE ARRAY
C     MINC = FIRST COLUMN OF IRAY WITHIN THE WINDOW
C     MAXC = LAST COLUMN
C     MINR = FIRST ROW
C     MAXR = LAST ROW
C     CONVERT = CONVERSION FACTOR BETWEEN IRAY UNITS AND PHYSICAL
C         INTENSITY UNITS:  INTENSITY = IRAY/CONVERT
C
C     USES SUBROUTINE STOCHPN, ORIGINALLY WRITTEN BY K. HARTLEY, RGO
C     LINK WITH NAG LIBRARY.
C
C *********************************************************************
C
      PARAMETER INTICK=20
      PARAMETER  NINTER=200
      REAL*4 IRAY(ICOL,IROW)
      REAL IV,MINVAL,MAXVAL
      DIMENSION VAL(NINTER),XHIS(NINTER),SIG(NINTER),A(10),
     1 NHIS(NINTER),DAT(2200)
      DOUBLE PRECISION VAL,XHIS,SIG,A,CHI,RMS,XVAL
      CHARACTER ANS*1,TYPE*8,BUFFER*80,DEVICE,CR*1,LF*1
      CHARACTER LABEL*80
      LOGICAL DEFAULT,ROTATE,MAXINF,LOG
C
      DATA CR,LF/'0D'X,'0A'X/
C
      IF (NUNIT .LT. 0)THEN
        MAXDOTS=786
        WIDE=13.1
      ELSE
        MAXDOTS=2112
        WIDE=10.56
      END IF
C     MAXDOTS = NO. OF PRINTER PIXELS ACROSS WHOLE LENGTH OF PAGE
C     WIDE = WIDTH OF PAGE IN INCHES
C
C
17    CALL WRUSER(' ',IS)
      CALL WRUSER('ASSUME DEFAULT VALUES FOR',IS)
      CALL WRUSER('    ROTATION',IS)
      CALL WRUSER('    WINDOW SIZE',IS)
      CALL WRUSER('    PIXEL SIZE',IS)
      CALL WRUSER('      AND',IS)
      CALL WRUSER('    INTENSITY SCALING?',IS)
      DEFAULT=.TRUE.
      CALL RDKEYL('DEFAULT',.TRUE.,1,DEFAULT,NVAL,IS)
C
      IF (DEFAULT)THEN
        ROTATE=.FALSE.
      ELSE
2       CALL WRUSER('ROTATE PICTURE 90 DEGREES?  (Y/N)',IS)
        ROTATE=.FALSE.
        CALL RDKEYL('ROTATE',.TRUE.,1,ROTATE,NVAL,IS)
C
C       OPTION TO CHANGE THE WINDOW
C
        CALL WINDOWQ(MINC,MAXC,MINR,MAXR,NCOL,NROW)
      END IF
C
16    IF (ROTATE)THEN
        KWIDE=MAXR-MINR+1
      ELSE
        KWIDE=MAXC-MINC+1
      END IF
      IF (KWIDE .GT. MAXDOTS-12)THEN
        WRITE(BUFFER,3000)MAXDOTS-12
3000    FORMAT('ERROR: MAX WIDTH OF PLOT IS',I5,' PIXELS')
        CALL WRUSER(BUFFER,IS)
        RETURN
      END IF
C
C    DETERMINE LENGTH OF TICK MARKS, IN PIXEL UNITS
C
      LENTICK=6/((MAXDOTS-12)/(KWIDE))
      LENTICK=MAX(LENTICK,1)
      NSAMP=KWIDE+2*LENTICK
      IWIDTH=MAXDOTS/NSAMP
C
      IF (DEFAULT)THEN
        NGRID=1000000
        IHIGH=IWIDTH
      ELSE
        CALL RDKEYI('GRID',.TRUE.,1,NGRID,NV,IS)
        IF (NGRID .EQ. 0)NGRID=1000000
65      WRITE(BUFFER,3001)IWIDTH
3001    FORMAT('EACH PIXEL WILL BE',I4,' DOTS WIDE (ACROSS PAPER)')
        CALL WRUSER(BUFFER,IS)
        CALL WRUSER('HOW MANY DOTS HIGH SHOULD EACH PIXEL BE?',IS)
        IHIGH=IWIDTH
        CALL RDKEYI('PIXEL',.TRUE.,1,IHIGH,NVAL,IS)
        IF (IHIGH .LT. 1 .OR. IHIGH .GT. MAXDOTS)GO TO 65
      END IF
      RATIO=(IHIGH+.01)/IWIDTH
C
      CALL RDKEYC('LABEL',.FALSE.,1,LABEL,NVAL,IS)
C
C     FIND MIN AND MAX VALUES WITHIN THE WINDOW
C
      MINVAL=1000000
      MAXVAL=-1000000
      DO 10 I=MINR,MAXR
      DO 10 J=MINC,MAXC
            IV=IRAY(J,I)
            MINVAL=MIN(MINVAL,IV)
10    MAXVAL=MAX(MAXVAL,IV)
      IF (MINVAL .EQ. MAXVAL)THEN
            CALL WRUSER('ERROR: ALL THE PIXELS HAVE THE SAME VALUE',IS)
            RETURN
      END IF
      MAXVAL=MAXVAL+1
C
      MAXINF=.TRUE.
      IF (.NOT. DEFAULT)THEN
        CALL WRUSER('MAKE A "MAXIMUM INFORMATION" PICTURE?',IS)
        CALL RDKEYL('MAXINF',.TRUE.,1,MAXINF,NVAL,IS)
      END IF
      IF (MAXINF)THEN
         YZERO=1.5
C          YZERO CONTROLS SHAPE OF HISTOGRAM OF OUTPUT DENSITIES
C          YZERO=1, FLAT HISTOGRAM
C          YZERO=0, STRONGLY WEIGHTED TOWARDS BLACK
C          YZERO=2, STRONGLY WEIGHTED TOWARDS WHITE
C        COEFFICIENTS OF QUADRATIC EQUATION TO BE SOLVED:
         B=YZERO
         AQ=1.-YZERO
         A2=2.*AQ
         A4=4.*AQ
         B2=B*B
C
C        COMPUTE 200 INTERVAL HISTOGRAM WITHIN RANGE
C
         NCYCLES=0
         CALL WRUSER('COMPUTING HISTOGRAM OF PIXEL VALUES...',IS)
12       WRITE(BUFFER,3003)MINVAL/CONVERT,MAXVAL/CONVERT
3003     FORMAT('MIN AND MAX VALUES =',2F14.4)
         CALL WRUSER(BUFFER,IS)
         LOWPTS=0
         DO 15 I=1,NINTER
15       NHIS(I)=0
         XINTER=NINTER
         DX=(MAXVAL-MINVAL)/XINTER
         DO 20 I=MINR,MAXR
         DO 20 J=MINC,MAXC
            IV=IRAY(J,I)
            K=(IV-MINVAL)/DX+1.
            IF (K .LT. 1 )THEN
              LOWPTS=LOWPTS+1
            ELSE IF (K .LE. NINTER)THEN
              NHIS(K)=NHIS(K)+1
            END IF
20       CONTINUE
C
C        COMPUTE NORMALIZED CUMULATIVE HISTOGRAM
C
         PTS=(MAXR-MINR+1)*(MAXC-MINC+1)
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
         WRITE(BUFFER,3004)J
3004     FORMAT(I5,' HISTOGRAM BINS IN 2-96 PERCENT RANGE')
         CALL WRUSER(BUFFER,IS)
C
C        IF THERE ARE LESS THAN 20 BINS IN THE HISTOGRAM, THEN
C        REDO HISTOGRAM WITH NARROWER LIMITS
C
         IF (J .LT. 20 .AND. NCYCLES .LE. 2)THEN
            NCYCLES=NCYCLES+1
            MINVAL=MINVAL-2.*DX
            MAXVAL=MAXVAL+DX
            GO TO 12
         END IF
C
         IF (J .LT. 7)THEN
           CALL WRUSER('FAILED DUE TO PECULIAR PIXEL DISTRIBUTION',
     1      IS)
           RETURN
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
         XFLO=MINVAL/CONVERT
         XHI=MAXVAL/CONVERT
         FLO=0.
         HI=1.
         XMID=.5
         TYPE='MAX INFO'
         TYP=999.
         WRITE(BUFFER,3005)XFLO,XHI
3005     FORMAT('GREY SCALE RANGES FROM',F14.4,' TO',F14.4)
         CALL WRUSER(BUFFER,IS)
C
      ELSE
C
         XFLO=MINVAL/CONVERT
         XHI=MAXVAL/CONVERT
         WRITE(BUFFER,3006)XFLO,XHI
3006     FORMAT('PIXELS RANGE FROM',F14.4,' TO',F14.4)
         CALL WRUSER(BUFFER,IS)
         CALL WRUSER('LEVEL TO CORRESPOND TO PURE WHITE=?',IS)
         CALL RDKEYR('WHITE',.TRUE.,1,XFLO,NVAL,IS)
         CALL WRUSER('LEVEL TO CORRESPOND TO PURE BLACK=?',IS)
         CALL RDKEYR('BLACK',.TRUE.,1,XHI,NVAL,IS)
C
         LOG=.FALSE.
         IF (XFLO .GT. 0. .AND. XHI .GT. 0.)THEN
           CALL WRUSER('PLOT ON A LOG INTENSITY SCALE?',IS)
           CALL RDKEYL('LOG',.TRUE.,1,LOG,NVAL,IS)
         END IF
         IF (LOG)THEN
            TYPE='LOG'
            TYP=2.
         ELSE
            TYPE='LINEAR'
            TYP=1.
         END IF
C
         IF (XHI .LT. XFLO)THEN
            XTEM=XHI
            XHI=XFLO
            XFLO=XTEM
            TYP=-TYP
         END IF
C
         FLO=XFLO*CONVERT
         HI=XHI*CONVERT
         XMID=(FLO+HI)/2.
         IF (LOG)XMID=10.**(LOG10(FLO)+LOG10(HI))/2.
      END IF
C
C     PLOT FROM TOP TO BOTTOM
C
      IF (NUNIT .LT. 0)THEN
        OPEN(UNIT=(-NUNIT),NAME='GREYPLT.LIS',TYPE='NEW',
     1    CARRIAGECONTROL='NONE',RECL=134    )
      ELSE
        OPEN(UNIT=NUNIT,NAME='DIT.DAT',RECORDSIZE=265/4+1,
     1   STATUS='NEW',CARRIAGECONTROL='NONE',FORM=
     2   'UNFORMATTED',RECORDTYPE='FIXED')
      END IF
      CALL STINIT(IDUMMY)
C
      IF (ROTATE)THEN
        I1=MAXR
        I2=MINR
        ID=-1
        J1=MAXC
        J2=MINC
      ELSE
        I1=MINC
        I2=MAXC
        ID=1
        J1=MAXR
        J2=MINR
      END IF
C
C     PRINT TICK MARK AT TOP OF PICTURE
C
      K=LENTICK
      DO 80 J=I1,I2,ID
        K=K+1
        IF (MOD(J,INTICK) .EQ. 0)THEN
          DAT(K)=HI
        ELSE
          DAT(K)=FLO
        END IF
80    CONTINUE
      DO 85 J=1,LENTICK
        DAT(J)=FLO
        DAT(K+J)=FLO
85    CONTINUE
      DO 88 J=1,LENTICK
        CALL STOCHPN(DAT,FLO,HI,NSAMP,TYP,WIDE,1.,NUNIT)
88    CONTINUE
C
      DO 100 I=J1,J2,-1
      IYGRID=MOD(I,NGRID)
      K=LENTICK
      DO 90 J= I1,I2,ID
      IXGRID=MOD(J,NGRID)
      K=K+1
      IF (ROTATE)THEN
        IV=IRAY(I,J)
      ELSE
        IV=IRAY(J,I)
      END IF
      IF (TYP .EQ. 999)THEN
         IF (IV .LT. MINVAL)THEN
            DAT(K)=0.
         ELSE IF (IV .LT. MAXVAL)THEN
            XVAL=(IV-XFST)/DELX
            DAT(K)=((((A(6)*XVAL+A(5))*XVAL+A(4))*XVAL+A(3))*XVAL+A(2))
     1       *XVAL+A(1)
            IF (YZERO .NE. 1.)DAT(K)=(-B+SQRT(B2+A4*DAT(K)))/A2
         ELSE
            DAT(K)=1.
         END IF
      ELSE
        DAT(K)=IV
      END IF
C
C     ADD GRID LINES
C
      IF (IYGRID .EQ. 0 .OR. IXGRID .EQ. 0)THEN
            IF (DAT(K) .LT. XMID)THEN
              DAT(K)=HI
            ELSE
              DAT(K)=FLO
            END IF
      END IF
90    CONTINUE
C
C     ADD TICK MARKS
C
      DO 95 J=1,LENTICK
      IF (MOD(I,INTICK) .EQ. 0)THEN
        DAT(J)=HI
        DAT(K+J)=HI
      ELSE
        DAT(J)=FLO
        DAT(K+J)=FLO
      END IF
95    CONTINUE
C
      IF (MOD(I,20).EQ.0)THEN
        WRITE(BUFFER,3007)I
3007    FORMAT('LINE ',I5)
        CALL WRUSER(BUFFER,IS)
      END IF
      CALL STOCHPN(DAT,FLO,HI,NSAMP,TYP,WIDE,RATIO,NUNIT)
100   CONTINUE
C
C     PRINT TICK MARKS AT BOTTOM
C
      K=LENTICK
      DO 110 J=I1,I2,ID
        K=K+1
        IF (MOD(J,INTICK) .EQ. 0)THEN
          DAT(K)=HI
        ELSE
          DAT(K)=FLO
        END IF
110   CONTINUE
      DO 115 J=1,LENTICK
        DAT(J)=FLO
        DAT(K+J)=FLO
115   CONTINUE
      DO 120 J=1,LENTICK
        CALL STOCHPN(DAT,FLO,HI,NSAMP,TYP,WIDE,1.,NUNIT)
120   CONTINUE
C
      IF (NUNIT .LT. 0)THEN
        LUNIT=ABS(NUNIT)
      ELSE
        LUNIT=19
        OPEN(UNIT=LUNIT,NAME='DIT.LIS',TYPE='NEW',RECL=134,
     1       CARRIAGECONTROL='NONE')
      END IF
      WRITE(LUNIT,2001)CR,LF,LABEL,CR,LF
      IF (TYP .LT. 0.)THEN
        XTEM=XFLO
        XFLO=XHI
        XHI=XTEM
      END IF
      WRITE(LUNIT,1005)MINC,MAXC,CR,LF,MINR,MAXR,CR,LF,XFLO,XHI,CR,LF
      IF (ROTATE)THEN
        WRITE(LUNIT,1003)CR,LF
      END IF
      IF (NGRID .LT. 1000000)THEN
        WRITE(LUNIT,1002)NGRID,CR,LF
      END IF
      WRITE(LUNIT,1004)INTICK,CR,LF
      WRITE(LUNIT,1009)IWIDTH,IHIGH,CR,LF
      WRITE(LUNIT,1007)TYPE,CR,LF
2001  FORMAT(2A1,A80,2A1)
1005  FORMAT(
     1 '  PICTURE OF COLUMNS:',2I5,2A1,12X,'AND ROWS:',2I5,2A1,
     2 '  PURE WHITE =',F12.3,', PURE BLACK =',F12.3,2A1)
1003  FORMAT(' PICTURE ROTATED 90 DEGREES COUNTER-CLOCKWISE',2A1)
1002  FORMAT('  GRID LINES ARE DRAWN AT MULTIPLES OF',I5,' PIXELS',2A1)
1004  FORMAT('  TICK MARKS ARE DRAWN AT MULTIPLES OF',I5,' PIXELS',2A1)
1009  FORMAT('  EACH PIXEL IS',I3,' DOTS WIDE BY',I3,' DOTS HIGH',2A1)
1007  FORMAT('  PLOT IS ON A ',A,' INTENSITY SCALE.',2A1)
      IF (NUNIT .LT. 0)THEN
       CLOSE(UNIT=(-NUNIT))
       ISTATUS=LIB$DO_COMMAND('$PRINT/QUEUE=SYS_PRINTRONIX/NOFEED/PASSALL
     1  GREYPLT')
      ELSE
        CLOSE(UNIT=NUNIT)
        CLOSE(UNIT=LUNIT)
        ISTATUS=LIB$DO_COMMAND('$PRINT/QUEUE=SYS_VERSATEC/NOFEED/PASSALL
     1    DIT.DAT ,DIT.LIS')
      END IF
      END
C****************************************************************
      SUBROUTINE WINDOWQ(IWMIN,IWMAX,JWMIN,JWMAX,NCOL,NROW)
C
C     DEFINE WINDOW LIMITS
C
      CHARACTER ANS*1,BUFFER*80
C
1     WRITE(BUFFER,1001)
      CALL WRUSER(BUFFER,ISTATUS)
      WRITE(BUFFER,1002)IWMIN,IWMAX
      CALL WRUSER(BUFFER,ISTATUS)
      WRITE(BUFFER,1003)JWMIN,JWMAX
      CALL WRUSER(BUFFER,ISTATUS)
      CALL WRUSER('DO YOU WANT TO CHANGE THESE LIMITS? (Y/N)',
     1 ISTATUS)
      ANS='N'
      CALL RDKEYC('CHANGE',.TRUE.,1,ANS,NVAL,ISTATUS)
      IF (ANS(1:1) .EQ. 'N' .OR. ANS(1:1) .EQ. 'F')RETURN
      IF (ANS(1:1) .NE. 'Y' .AND. ANS(1:) .NE. 'T')GO TO 1
      GO TO 5
C
5     CALL WRUSER('MIN AND MAX COLS. (INCLUSIVE) OF WINDOW = ?'
     1 ,ISTATUS)
      WRITE(BUFFER,1005)NCOL
1005  FORMAT('   RANGE = 1 TO',I5)
      CALL PRMPT2I(BUFFER,'IVALUES',IWMIN,IWMAX,ISTATUS)
      IF (IWMAX .LT. IWMIN .OR.
     1 IWMIN .LT. 1 .OR. IWMAX .GT. NCOL)THEN
      CALL WRUSER('*** ERROR ***',ISTATUS)
      GO TO 5
      END IF
C
10    CALL WRUSER('MIN AND MAX ROWS (INCLUSIVE) OF WINDOW = ?'
     1 ,ISTATUS)
      WRITE(BUFFER,1005)NROW
      CALL PRMPT2I(BUFFER,'JVALUES',JWMIN,JWMAX,ISTATUS)
      IF (JWMAX .LT. JWMIN .OR.
     1 JWMIN .LT. 1 .OR. JWMAX .GT. NROW)THEN
      CALL WRUSER('***  ERROR ***',ISTATUS)
      GO TO 10
      END IF
C
1001  FORMAT('0WINDOW AREA IS SET TO:')
1002  FORMAT('    COLUMNS',I6,' TO',I6)
1003  FORMAT('       ROWS',I6,' TO',I6)
      END
C ***************************************************************
      SUBROUTINE PRMPT2I(PREFIX,NAME,I1,I2,ISTATUS)
      CHARACTER*(*) PREFIX,NAME
      DIMENSION IVALUE(2)
10    CALL WRUSER(PREFIX,ISTAT)
      CALL RDKEYI(NAME,.FALSE.,2,IVALUE,I,ISTATUS)
      IF (ISTATUS .EQ. 1 .OR. I .NE. 2)THEN
        CALL WRUSER('******ERROR****** MUST INPUT 2 INTEGERS',ISTAT)
        CALL CNPAR(NAME,ISTAT)
        GO TO 10
      END IF
      I1=IVALUE(1)
      I2=IVALUE(2)
      END
C *******************************************************************
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
C**************************************************************
      SUBROUTINE STINIT
C
C     INITIALIZES THE RANDOM NUMBER COMMON BLOCK FOR THE STOCHASTIC
C       GREYSCALE ROUTINE FOR THE PRINTRONIX
C
      DOUBLE PRECISION X
      COMMON /LINKSTOCH/RAND(2000)
C
      CALL G05CBF(0)
C
C     G05CAF IS A NAG LIBRARY ROUTINE FOR PRODUCING RANDOM
C     NUMBERS IN THE RANGE 0.0 TO 1.0
C
      DO 100 I=1,2000
      X=G05CAF(X)
      RAND(I)=X
100   CONTINUE
      END
C*****************************************************************
      SUBROUTINE STOCHPN(DAT,FLO,HI,NSAMP,TYP,WIDE,RATIO,UNIT)
C
C     WRITTEN BY KEN HARTLEY, RGO
C     MODIFIED FOR PRINTRONIX PRINTER BY W. PENCE, AAO
C
C ..THE COMMON BLOCK LINKSTOCH LINKS WITH INIT...
C
      PARAMETER NL=264
      DOUBLE PRECISION XXX
      LOGICAL*1 XMASK(6),FIRST,LAST,IPOW(8)
      BYTE BYTE1
      INTEGER UNIT
      COMMON /LINKSTOCH/RAND(2000)
      LOGICAL*1 IOUT(264)
      DIMENSION DAT(2112)
      DIMENSION IBIT(2112),DATA(2112)
      DATA XMASK/'01'X,'02'X,'04'X,'08'X,'10'X,'20'X/
      DATA FIRST,LAST /'05'X,'0A'X/
      DATA BYTE1/4/
      DATA IPOW/128,64,32,16,8,4,2,1/
C
      IF (UNIT .LT. 0)THEN
        NPWIDE=786
        NBPI=60
      ELSE
        NPWIDE=2112
        NBPI=200
      END IF
C ..TESTS ARE CARRIED OUT TO CHECK THAT FLO AND HI...
C ..ARE IN THE VALID RANGE PRESCRIBED...
C ..AND THAT TYP IS A VALID NUMBER CODE...
C
      IF(ABS(TYP).EQ.2.0.AND.FLO.LT.0.0.OR.HI.LT.0.0)RETURN
      IF(HI.EQ.FLO)RETURN
      IF(TYP.EQ.999.0)GOTO 999
      IF(ABS(TYP).GT.2.0)RETURN
      IF(TYP.EQ.0.0)RETURN
      IF(FLOAT(INT(TYP)).NE.TYP)RETURN
C
C ..THE VALUES OF FLO AND HI ARE SWAPPED IF...
C ..GIVEN IN REVERSE...
C
  999 IF (HI.GE.FLO) GO TO 50
      TEM=HI
      HI=FLO
      FLO=TEM
   50 CONTINUE
C
C ..IF FLO IS 0.0,FLO IS SET TO A VERY SMALL NUMBER...
C ..TO ALLOW LOGS TO BE TAKEN IF NECCESSARY...
C
      IF(FLO.EQ.0.0)FLO=1E-6
C
C      DATA ARE FIXED IF TYPE= + 2 OR - 2 AND VALUE IS NEGATIVE
C
      IF (ABS(TYP).EQ.2.0) THEN
            DO 60 I=1,NSAMP
               IF (DAT(I).LT.0.0) DAT(I)=-DAT(I)
               IF (DAT(I).EQ.0.0) DAT(I)=1.0E-6
   60       CONTINUE
                           END IF
C
C ..THE WIDTH OF THE IMAGE IS DECIDED...
C
      IWIDE=NPWIDE
      IF(WIDE.LE. FLOAT(NPWIDE)/NBPI
     1  .AND.WIDE.GE.1.0)IWIDE=WIDE*NBPI
      IF(NSAMP.LE.0.OR.NSAMP.GT.NPWIDE)RETURN
C
C ..IPIX IS THE NUMBER OF BITS PER PIXEL...
C
      IPIX=IWIDE/NSAMP
      ILENGTH=FLOAT(IPIX)*RATIO+.01
      ILENGTH=MAX(1,ILENGTH)
      ISTART=(NPWIDE-IWIDE)/2
      IF (ABS(TYP).EQ.2.0) THEN
         DIV1=ALOG(HI)-ALOG(FLO)
         DIV2=-DIV1
      ELSE
         DIV1=1
         DIV2=-DIV1
      END IF
      IF(TYP.NE.999.0)GOTO 1000
      DO 123 L=1,NSAMP
        DATA(L)=DAT(L)
  123 CONTINUE
      GOTO 321
C
C ..THIS LOOP SCALES THE DATA OF DAT INTO ARRAY DATA...
C
 1000 DO 100 I=1,NSAMP
C
C ..THE CORRECT TYP OF SCALING IS CHOSEN...
C
        IF (TYP.EQ.1.0) DATA(I)=(DAT(I)-FLO)/(HI-FLO)
        IF (TYP.EQ.-1.0) DATA(I)=(DAT(I)-HI)/(FLO-HI)
        IF (TYP.EQ.2.0) DATA(I)=(ALOG(DAT(I))-ALOG(FLO))/DIV1
        IF (TYP.EQ.-2.0) DATA(I)=(ALOG(DAT(I))-ALOG(HI))/DIV2
  100 CONTINUE
C
C ..THIS LOOP OUTPUTS THE ROW ILENGTH TIMES TO PRODUCE...
C ..PIXELS OF THE DESIRED SHAPE...
C
  321 DO 400 IROW=1,ILENGTH
C
C ..THE POSITION AT WHICH THE POINTER IS SET IS RANDOM...
C
        XXX=G05CAF(XXX)
        X=XXX
        IPOS=INT(X*1999.0+1.0)
        DO 250 ISET=1,NPWIDE
          IBIT(ISET)=0
  250   CONTINUE
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF ELEMENTS OF DAT...
C
        DO 300 ISAMP=1,NSAMP
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF BITS IN EACH PIXEL...
C
          DO 200 IBITS=1,IPIX
C
C ..THE POSITION OF THE BIT UNDER CONSIDERATION IS CALCULATED...
C
            K=(ISAMP-1)*IPIX+IBITS+ISTART
C
C ..THE VALUE OF IBIT(K) IS SET ACCORDING...
C ..TO THE PROBABILITY OF THE VALUE OF DATA BEING...
C ..GREATER THAN A RANDOM NUMBER...
C
            IF (DATA(ISAMP).GT.RAND(IPOS)) IBIT(K)=1
C
C ..THE POINTER IS INCREMENTED,AND IF THE END OF THE...
C ..RAND IS REACHED,IT IS SET TO THE START OF RAND...
C
            IPOS=IPOS+1
            IF (IPOS.GT.2000) IPOS=1
  200     CONTINUE
  300   CONTINUE
C
C ..THE WORDS CONTAINING THE BITS TO WRITE TO THE TAPE...
C ..ARE THEN ASSEMBLED FROM THE BITS ALREADY FORMED...
C
      IF (UNIT .LT. 0)THEN
      DO 255 ISET = 1,140
255   IOUT(ISET)='C0'X
      DO 370 I=1,NPWIDE
        IF (IBIT(I) .EQ. 1)THEN
          KWORD=1+((I-1)/6)
          KBIT=1+MOD(I-1,6)
          IOUT(KWORD)=IOUT(KWORD).OR.XMASK(KBIT)
        END IF
370   CONTINUE
C
C ..THE LINE IS NOW WRITTEN TO THE FILE...
C
      WRITE(-UNIT,99)FIRST,(IOUT(I),I=1,131),LAST
99    FORMAT(133A1)
C
      ELSE
C
      DO 390 IIN1=1,NL
        IOUT(IIN1)=0
        DO 380 JBIT=1,8
          K=(IIN1-1)*8+JBIT
          IF (IBIT(K) .EQ. 1)IOUT(IIN1)=IOUT(IIN1)+IPOW(JBIT)
380     CONTINUE
390   CONTINUE
C
C    THE LINE IS NOW WRITTEN OF THE FILE
C
      WRITE(UNIT)BYTE1,(IOUT(J),J=1,NL)
C
      END IF
C
  400 CONTINUE
  500 RETURN
      END
