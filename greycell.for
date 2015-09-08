C-------------------------------------------------------------------------------
C
C                           **********************
C                           *                    *
C                           *  Program GREYCELL  *
C                           *                    *
C                           **********************
C
C
C         CALLING SEQUENCE:-
C
C              GREYCELL
C
C
C         FUNCTION:-
C
C              Produces a grey scale representation of a Starlink image file 
C              on raster graphics devices known to GKS 7.2, using the GKS cell  
C              array plotting subroutine GCA.
C
C
C         USER PARAMETERS:-
C
C         WKSTN           2601                GKS 7.2 workstation number.
C
C         INPUT                               2-D Starlink image file to be 
C                                             plotted.
C
C         WHOLE           T                   If TRUE, will plot the whole 
C                                             image.
C
C         IVALUES         1,MAXC              First and last columns of the
C                                             image to be included in the 
C                                             window.
C
C         JVALUES         1,MAXR              First and last rows of the
C                                             image to be included in the
C                                             window.
C
C         SIZE            1.                  Magnification, maximum value 1. 
C                                             If 1, plot is fitted to the full
C                                             display surface.
C
C         FRAME           T                   If TRUE, plots a frame around the
C                                             picture.
C
C         LABEL                               Plot title, max. 80 characters.
C
C         MAXINF          T                   If TRUE, creates a high contrast
C                                             grey scale by histogram analysis
C                                             of the pixel values. The curvature
C                                             of the grey scale reflects the
C                                             distribution of the values after
C                                             the bottom 2% and the top 4% have
C                                             been discarded.
C
C         REVERSE         F                   If TRUE, causes the negative of 
C                                             the input image to be plotted.
C
C         BIAS            1.                  Adjusts the weighting of the grey 
C                                             scale curve. If 0, the scale will
C                                             be weighted towards white; if 1
C                                             it will not be weighted; if 2
C                                             weighted towards black. The 
C                                             opposite applies if the image is
C                                             reversed.
C
C         LOG             F                   If MAXINF is FALSE, causes 
C                                             pixel values to be converted to
C                                             their logarithms. The values
C                                             entered for WHITE and BLACK will
C                                             be taken as logarithms.
C
C         WHITE           AMIN                If MAXINF is FALSE, defines
C                                             the threshold value above which
C                                             pixel values will be plotted as
C                                             white.
C
C         BLACK           AMAX                If MAXINF is FALSE, defines
C                                             the threshold value below which
C                                             pixel values will be plotted as
C                                             black.
C
C
C         NMJ Fuller (RGO) June 1987
C
C-------------------------------------------------------------------------------
C
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER WKSTN*80,BUFFER*80
      INTEGER*4 IDIM(2)
      LOGICAL*1 WHOLE,FRAME
C
      CALL WRUSER(' ',IERR)
      WRITE(BUFFER,601)
  601 FORMAT('*** This program plots high pixel values as white on',
     &       ' all devices ***')
      CALL WRUSER(BUFFER,IERR)
      WRITE(BUFFER,602)
  602 FORMAT('*** unless otherwise instructed via REVERSE (or WHITE',
     &       ' and BLACK) ***')
      CALL WRUSER(BUFFER,IERR)
C
C     Open SGS workstation
C
      WKSTN='2601'
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Enter GKS 7.2 workstation number',IERR)
      CALL RDKEYC('WKSTN',.TRUE.,1,WKSTN,NVAL,IERR)
      CALL SGS_OPEN(WKSTN,IZB,IERR)
      IF(IERR.NE.0)GO TO 900
C
C     Read image 
C
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Enter image name',IERR)
      CALL RDIMAG('INPUT',FMT_R,2,IDIM,NDIM,IP,IERR)
      IF(IERR.NE.0)GO TO 900
      NCOL=IDIM(1)
      NROW=IDIM(2)
      MINC=1
      MAXC=NCOL
      MINR=1
      MAXR=NROW
C
C     Select rows and columns to be plotted
C
      WHOLE=.TRUE.
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Plot all rows and columns?',IERR)
      CALL RDKEYL('WHOLE',.TRUE.,1,WHOLE,NVAL,IERR)
      IF(.NOT.WHOLE)THEN
        CALL WINDOWQ(MINC,MAXC,MINR,MAXR,NCOL,NROW)
        NCOL=MAXC-MINC+1
        NROW=MAXR-MINR+1
      END IF
C
C     Get work space
C
      CALL GETDYN('IW',FMT_SL,NCOL*NROW,IW,IERR)
c
c     alternative output - create image file for final array
c     call to GCA routine must be commented
c     call wrimag('output',fmt_sl,idim,ndim,iw,ierr)
c
      IF(IERR.NE.0)GO TO 900
C
C     Set up SGS zone with the same aspect ratio as the image
C
      CALL SGS_ZSHAP(REAL(NCOL)/REAL(NROW),'CC',IZONE1,IERR)
      IF(IERR.NE.0)GO TO 900
      CALL SGS_SW(0.,1.,0.,1.,IERR)
      IF(IERR.NE.0)GO TO 900
C
C     Ask for magnification factor
C
   10 SIZE=1.
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Enter magnification factor',IERR)
      CALL WRUSER('Note: no room for a label when SIZE > 0.9',IERR)
      CALL RDKEYR('SIZE',.TRUE.,1,SIZE,NVAL,IERR)
      BORDER=(1.-SIZE)/2.
      IF(SIZE.LE.0. .OR. SIZE.GT.1.)THEN
        CALL WRERR('BADSIZE')
        CALL CNPAR('SIZE',IERR)
        GO TO 10
      END IF
C
C     Ask whether a frame is required
C
      FRAME=.TRUE.
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Draw a frame around the picture?',IERR)
      CALL RDKEYL('FRAME',.TRUE.,1,FRAME,NVAL,IERR)
C
C     Call main plotting subroutine
C
      CALL GREY(%VAL(IP),%VAL(IW),IDIM(1),IDIM(2),NCOL,NROW,
     &          MINC,MAXC,MINR,MAXR,IZONE1,BORDER)
C
C     Draw frame if required
C
      IF(FRAME)CALL SGS_BOX(BORDER,1.-BORDER,BORDER,1.-BORDER)
C
C     Release work space and close SGS
C
  900 CALL FRDATA(' ',IERR)
c
c     alternative output - release final array
c     call frdata('output',ierr)
c
      CALL SGS_CLOSE
C
      END
      SUBROUTINE GREY(ARAY,IRAY,ICOL,IROW,NCOL,NROW,
     &                MINC,MAXC,MINR,MAXR,IZONE1,BORDER)
C------------------------------------------------------------------------------
C
      PARAMETER NBINS=300
C
      CHARACTER TYPE*11,BUFFER*80,LABEL*80
      INTEGER*4 IRAY(NCOL,NROW)
      LOGICAL*1 MAXINF,REV,LOG
      REAL*4    ARAY(ICOL,IROW)
      REAL*8    DVAL,DHIS,SIG,A(10),CHI,RMS,XVAL
C
      DIMENSION DVAL(NBINS),DHIS(NBINS),SIG(NBINS),NHIS(NBINS)
C
C     Get the current workstation id
C
      CALL SGS_ICURW(IWKID)
C
C     Query GKS for the number of possible grey shades
C
      CALL GQECI(IWKID,1,IERR,NGREY,IDUM)
C
C     Load the grey scale into the colour table
C
      W1=REAL(NGREY-3)
      DO ICI=2,NGREY-1
        W2=REAL(ICI-2)/W1
        CALL GSCR(IWKID,ICI,W2,W2,W2)
      END DO
C
C     Ask for label if there is room for one 
C
      IF(BORDER.GE.0.05)THEN
        CALL WRUSER(' ',IERR)
        CALL WRUSER('Enter label (max 80 chars)',IERR)
        CALL RDUSER(LABEL,IERR)
        CALL WRKEYC('LABEL',LABEL,NELS,IERR)
        IEL=80
        DO WHILE (LABEL(IEL:IEL).EQ.' ')
          IEL=IEL-1
        END DO
C
C     - make a zone 1cm high and the same width as the image zone,
C       immediately below the image
C
        CALL SGS_ZONE(0.,1.,0.,BORDER,IZONE2,IERR)
        CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
        CALL SGS_ZSIZE(XM,0.01,'TC',IZONE3,IERR)
        CALL SGS_SW(0.,XM*100.,0.,1.,IERR)
C
C     - plot the label centrally in this zone, in characters 2.5mm high
C
        CALL SGS_SHTX(0.25)
        CALL SGS_STXJ('BC')
        CALL SGS_BTEXT(XM*50.,0.5)
        CALL SGS_ATEXT(LABEL(1:IEL))
        CALL SGS_OTEXT
        CALL SGS_SELZ(IZONE1,IERR)
      ELSE
        CALL WRUSER(' ',IERR)
        CALL WRUSER('No room for a label at this magnification',IERR)
      END IF
C
C     Scan the image to find the minimum and maximum values
C
      AMIN=1000000.
      AMAX=-1000000.
      DO J=MINR,MAXR
        DO I=MINC,MAXC
          AVAL=ARAY(I,J)
          AMIN=MIN(AMIN,AVAL)
          AMAX=MAX(AMAX,AVAL)
        END DO
      END DO
      IF(AMIN.EQ.AMAX)THEN
        CALL WRERR('BADELE')
        GO TO 9000
      END IF
      AMAX=AMAX+1
      CALL WRUSER(' ',IERR)
      WRITE(BUFFER,601)AMIN,AMAX
  601 FORMAT('Minimum pixel value = ',F14.4,' maximum = ',F14.4)
      CALL WRUSER(BUFFER,IERR)
C
C     Ask what type of grey scaling is required
C
      MAXINF=.TRUE.
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Compute high contrast grey scale?',IERR)
      CALL RDKEYL('MAXINF',.TRUE.,1,MAXINF,NVAL,IERR)
C
C     HIGH CONTRAST GREY SCALE - fitted to the cumulative distribution
C     of the pixel values which lie between the white and black extremes
C
      IF(MAXINF)THEN
        TYPE='HISTOGRAM'
        ATYP=999.
C
C     Ask whether a negative plot is required
C
        CALL WRUSER(' ',IERR)
        CALL WRUSER('Reverse the image?',IERR)
        REV=.FALSE.
        CALL RDKEYL('REVERSE',.TRUE.,1,REV,NVAL,IERR)
C     
C     Ask for the weighting of the grey scale curve
C 
        BIAS=1.
        CALL WRUSER(' ',IERR)
        CALL WRUSER('Select grey scale bias',IERR)
        CALL WRUSER(' 0.0 = weighted towards black',IERR)
        CALL WRUSER('1.0 = no weighting',IERR)
        CALL WRUSER('2.0 = weighted towards white',IERR)
        CALL WRUSER('The opposite applies for a reversed image',
     &              IERR)
        CALL RDKEYR('BIAS',.TRUE.,1,BIAS,NVAL,IERR)
        B=BIAS
        AQ=1.-BIAS
        A2=2.*AQ
        A4=4.*AQ
        B2=B*B
C
C     Construct histogram of the pixel values
C
        NTRIES=0
        CALL WRUSER(' ',IERR)
        WRITE(BUFFER,600)NGREY
  600 FORMAT('Selected device can display ',I3,' grey shades')
        CALL WRUSER(BUFFER,IERR)
        CALL WRUSER(' ',IERR)
        CALL WRUSER('Computing histogram of pixel values...',IERR)
        CALL WRUSER(' ',IERR)
C
C     Bin the pixel values
C
   10   DO I=1,NBINS
          NHIS(I)=0 
        END DO
        DX=(AMAX-AMIN)/NBINS
        LOWPTS=0
        DO J=MINR,MAXR
          DO I=MINC,MAXC
            AVAL=ARAY(I,J)
            K=(AVAL-AMIN)/DX+1
            IF(K.LT.1)LOWPTS=LOWPTS+1
            IF(K.GE.1.AND.K.LE.NBINS)NHIS(K)=NHIS(K)+1
          END DO
        END DO
C
C     Make the histogram cumulative
C
        APTS=(MAXR-MINR+1)*(MAXC-MINC+1)
        NHIS(1)=NHIS(1)+LOWPTS
        DO I=2,NBINS
          NHIS(I)=NHIS(I)+NHIS(I-1)
        END DO
C
C     Compute fraction of total pixels in each bin 
C     and pixel value for each bin
C
        DO I=1,NBINS
          DHIS(I)=NHIS(I)/APTS
          DVAL(I)=AMIN+I*DX
        END DO
C
C     Drop bins which contain bottom 2% and top 4% of the pixels
C
        N94=0
        DO I=1,NBINS
          IF(DHIS(I).GT..02 .AND. (DHIS(I).LT..96 .OR. 
     &       N94.EQ.0))THEN
            N94=N94+1
            DHIS(N94)=DHIS(I)
            DVAL(N94)=DVAL(I)
c
c      write cumulative histogram data to file
c           write(10,'(1x,i3,1x,d20.10,1x,d20.10)')n94,
c    &      dhis(n94),dval(n94)
c
          END IF
        END DO
C
        N94=MAX(1,N94)
        AMAX=DVAL(N94)
        AMIN=DVAL(1)
        WRITE(BUFFER,602)N94
  602 FORMAT(I3,' histogram bins contain values in 2%-96% range')
        CALL WRUSER(BUFFER,IERR)
C
C     If less than NGREY-2 bins are left, reconstruct the histogram  
C     from a slightly wider range of pixel values  
C
        IF(N94.LT.NGREY-2 .AND. NTRIES.LT.5)THEN
          CALL WRUSER(' ',IERR)
          WRITE(BUFFER,603)
  603 FORMAT('Recomputing histogram to make best use of the ' 
     &       'available grey shades...')
          CALL WRUSER(BUFFER,IERR)
C         AMIN=AMIN-2.*DX
          AMIN=AMIN-DX
          AMAX=AMAX+DX
          NTRIES=NTRIES+1
          GO TO 10
        END IF
C
C      Give up after 5 tries
C
        IF(N94.LT.NGREY-4)THEN
          CALL WRUSER(' ',IERR)
          WRITE(BUFFER,604)
  604 FORMAT('Pixel value range is too small to make a grey scale')
          CALL WRUSER(BUFFER,IERR)
          GO TO 9000
        END IF
C
        CALL WRUSER(' ',IERR)
        WRITE(BUFFER,605)AMIN,AMAX
  605 FORMAT('Grey scale ranges from ',F14.4,' to ',F14.4)
        CALL WRUSER(BUFFER,IERR)
C
C     Normalize the bin values
C
        DELX=DVAL(N94)-DVAL(1)
        XFST=DVAL(1)
        DO I=1,N94
          DVAL(I)=(DVAL(I)-XFST)/DELX
        END DO
C 
C     Fit a fifth order polynomial to the cumulative histogram
C
        CALL POLFIT(DVAL,DHIS,SIG,N94,6,0,A,CHI,RMS)
C
C     LINEAR OR LOGARITHMIC GREY SCALE
C
      ELSE
C
        LOG=.FALSE.
        CALL WRUSER(' ',IERR)
        CALL WRUSER('Take logarithms of pixel values?',IERR)
        CALL RDKEYL('LOG',.TRUE.,1,LOG,NVAL,IERR)
C
        IF(LOG)THEN
          TYPE='LOGARITHMIC'
          ATYP=2.
        ELSE
          TYPE='LINEAR'
          ATYP=1.
        END IF
C
C     Ask for pixel values to delimit the grey scale
C
        BLACK=AMIN
        WHITE=AMAX
        REV=.FALSE.
        CALL WRUSER(' ',IERR)
        CALL WRUSER('What value should be white?',IERR)
        CALL RDKEYR('WHITE',.TRUE.,1,WHITE,NVAL,IERR)
        CALL WRUSER(' ',IERR)
        CALL WRUSER('What value should be black?',IERR)
        CALL RDKEYR('BLACK',.TRUE.,1,BLACK,NVAL,IERR)
C
C     If reversal was requested
C
        IF(WHITE.LT.BLACK)THEN
          REV=.TRUE.
          ATYP=-ATYP
        END IF
C
      END IF
C
C     GENERATE PLOT 
C
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Generating plot...',IERR)
      CALL WRUSER(' ',IERR)
C
C     Compute factor to convert pixel value to colour index value
C
      IF(ATYP.EQ.999.)CIFACT=REAL(NGREY-2)
C
      IF(ABS(ATYP).EQ.1.)CIFACT=REAL(NGREY-2)/(WHITE-BLACK)
C          
      IF(ABS(ATYP).EQ.2.)THEN
        IF(WHITE.EQ.0.)WHITE=1E-6
        IF(BLACK.EQ.0.)BLACK=1E-6
        CIFACT=ALOG(REAL(NGREY-2))/(ALOG(WHITE)-ALOG(BLACK))
      END IF
C
C     Plot array values 
C
      DO J=MINR,MAXR
        DO I=MINC,MAXC
C
          AVAL=ARAY(I,J)
C
          II=I-MINC+1
          JJ=J-MINR+1
C
C     If high contrast grey scale
C
          IF(ATYP.EQ.999.)THEN
C
            IF(AVAL.LT.AMIN)THEN
              IF(.NOT.REV)IRAY(II,NROW-JJ+1)=2
              IF(REV)IRAY(II,NROW-JJ+1)=NGREY-1
            END IF
C 
            IF(AVAL.GE.AMAX)THEN
              IF(.NOT.REV)IRAY(II,NROW-JJ+1)=NGREY-1
              IF(REV)IRAY(II,NROW-JJ+1)=2
            END IF
C
C     - normalize pixel values and compute fifth order fit
C
            IF(AVAL.GE.AMIN .AND. AVAL.LT.AMAX)THEN
              AVAL=(AVAL-XFST)/DELX
c             asave=aval
              AVAL=(((((A(6)*AVAL+A(5))*AVAL+A(4))*AVAL+A(3))
     &             *AVAL+A(2))*AVAL+A(1))
              IF(BIAS.NE.1.)AVAL=(-B+SQRT(B2+A4*AVAL))/A2
              IF(REV)AVAL=1.-AVAL
c
c     - write original and fitted data to file  
c             write(10,'(1x,f10.8,1x,f10.8)')asave,aval
c
              IRAY(II,NROW-JJ+1)=MAX(MIN(NINT(CIFACT*AVAL),NGREY-1),2)
            END IF
C
          END IF
C
C     If linear grey scale
C
          IF(ABS(ATYP).EQ.1.)IRAY(II,NROW-JJ+1)=
     &      MAX(MIN(NINT(CIFACT*(AVAL-BLACK)),NGREY-1),2)
C
C     If logarithmic grey scale
C
          IF(ABS(ATYP).EQ.2.)THEN
            IF(AVAL.LT.0.)AVAL=-AVAL
            IF(AVAL.EQ.0.)AVAL=1.E-6
            IRAY(II,NROW-JJ+1)=
     &        MAX(MIN(NINT(CIFACT*ALOG(AVAL)-ALOG(BLACK)),NGREY-1),2)
          END IF
C
        END DO
      END DO
C
C     Plot the picture
C
      CALL GCA(BORDER,BORDER,1.-BORDER,1.-BORDER,NCOL,NROW,NCOL,
     &         IRAY)
C
 9000 RETURN
      END
      SUBROUTINE WINDOWQ(IWMIN,IWMAX,JWMIN,JWMAX,NCOL,NROW)
C------------------------------------------------------------------------------
C
C     Selection of row and column ranges
C
      CHARACTER BUFFER*80
      INTEGER*4 IVALUE(2)
C
C     Display current range
C
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Plot area is currently',IERR)
      WRITE(BUFFER,601)JWMIN,JWMAX
  601 FORMAT('rows   ',I3,' to ',I3)
      CALL WRUSER(BUFFER,IERR)
      WRITE(BUFFER,602)IWMIN,IWMAX
  602 FORMAT('columns',I3,' to ',I3)
      CALL WRUSER(BUFFER,IERR)
C
C     Ask for row range
C
   10 IVALUE(1)=JWMIN
      IVALUE(2)=JWMAX
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Enter min and max rows (inclusive)',IERR)
      CALL RDKEYI('JVALUES',.TRUE.,2,IVALUE,NVAL,IERR)
      IF(IERR.GT.1 .OR. NVAL.NE.2)THEN
        CALL WRERR('BADDAT')
        CALL CNPAR('JVALUES',IERR)
        GO TO 10
      END IF
      JWMIN=IVALUE(1)
      JWMAX=IVALUE(2)
      IF(JWMAX.LT.JWMIN .OR. JWMIN.LT.1 .OR.
     &   JWMAX.GT.NROW)THEN
        CALL WRERR('BADROW')
        CALL CNPAR('JVALUES',IERR)
        GO TO 10
      END IF
C
C     Ask for column range
C
   20 IVALUE(1)=IWMIN
      IVALUE(2)=IWMAX
      CALL WRUSER(' ',IERR)
      CALL WRUSER('Enter min and max columns (inclusive)',IERR)
      CALL RDKEYI('IVALUES',.TRUE.,2,IVALUE,NVAL,IERR)
      IF(IERR.GT.1 .OR. NVAL.NE.2)THEN
        CALL WRERR('BADDAT')
        CALL CNPAR('IVALUES',IERR)
        GO TO 20
      END IF
      IWMIN=IVALUE(1)
      IWMAX=IVALUE(2)
      IF(IWMAX.LT.IWMIN .OR. IWMIN.LT.1 .OR.
     &   IWMAX.GT.NCOL)THEN
        CALL WRERR('BADCOL')
        CALL CNPAR('IVALUES',IERR)
        GO TO 20
      END IF
C
  900 RETURN
      END
      SUBROUTINE POLFIT(X,Y,SIGMAY,NPTS,NTERMS,MODE,A,CHISQR,RMS)
C------------------------------------------------------------------------------
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
