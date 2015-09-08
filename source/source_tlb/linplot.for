C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ********************* 
C                     *                   * 
C                     * Program   LINPLOT * 
C                     *                   * 
C                     ********************* 
C
C
C
C          CALLING SEQUENCE:- 
C               LINPLOT  [AUTO=F]
C
C
C          FUNCTION:- 
C               It produces a line plot of a 1-d Starlink image, on one  of 
C               several devices. The user has the choice of the
C		form of the X and Y axes. Each axis can either be
C		linear or logarithmic.
C
C               A later enhancement allows repeated plotting of 1-d
C               sections from a 2-d image.
C
C
C          USE:- 
C               It may be used to plot the histogram created by  WRHIST  or 
C               the section found by SLICE, or any other 1- or 2-d data. 
C
C
C
C         USER PARAMETERS:- 
C
C         INPUT                               This   is   the   input   1-d 
C                                             Starlink image. 
C
C         DEVICE          GKS_1               This is the  graphics  device 
C                                             to   be   used   :   any of the
C                                             devices supported by GKS may
C                                             be used.
C
C         LIMITS          1 dime. 1           These three  numbers  specify 
C                                             the  first and last pixels to 
C                                             be plotted, and the  sampling 
C                                             interval  to  be  used.  This 
C                                             latter option may be used  to 
C                                             squash  a  long plot onto the 
C                                             ARGS. 
C
C	  RANGE	    	    min,max           This is the minimum and maximum
C                                             value of Y the program will plot.
C
C	  AXES	   	    lin,lin	      These are the forms of each axis,
C					      either linear or logarithmic.
C
C         SIZE                                This is the size of the  plot 
C                                             in  cms., but only if a plotter
C                                             was selected. 
C
C         SECTION            1                The section through the
C                                             image if it is 2-d.
C
C         NORMALLY DEFAULTED PARAMETERS
C
C         AUTO               T                After the first section (in
C                                             2-d mode) it does not prompt
C                                             for any other parameters,
C                                             apart from the section number.
C
C          K F Hartley                 RGO           13-AUG-82                 
C		Version 3
C
C
C-------------------------------------------------------------------------- 



C
C	This is a modified version of the original LINPLOT program
C	written by C D PIKE  at RGO.
C       Modified to handle 2-d images by KFH at RGO on 12-12-83
C



C
C   CDP RGO 2/4/81
C
C
      INTEGER PIN,STATUS,AXIS1,NPIX(3)
      INTEGER AX(2)
      LOGICAL*1 AUTO
      REAL SIZE(2)
      CHARACTER*12 DEV
      LOGICAL CUR
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C       GET INPUT FRAME 2 TRIES ALLOWED
C
      ITRY = 0
    1 CALL RDIMAG('INPUT',FMT_R,2,AX,IDIM,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         CALL CNPAR('INPUT',STATUS)
         IF(ITRY.EQ.0)  THEN
            ITRY = 1
            GO TO 1
         ENDIF
         CALL EXIT
      END IF
C
C   Select any GKS device
C
  100 CONTINUE
      DEV='GKS_1'
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
      CALL STR$UPCASE(DEV,DEV)
      IF (STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
         CALL WRERR('HELLDEV')
         CALL CNPAR('DEVICE',STATUS)
         GO TO 100
      END IF
      CALL DEVTRAN(DEV,IDEV,ICONID,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('Failed to recognize that device',STATUS)
         CALL CNPAR('DEVICE',STATUS)
         GO TO 100
      END IF
      CALL JBDEV(DEV)
C
C   Device dependent code to correct bug in HP 2648 driver.
C
      IF (IDEV.EQ.12) PRINT *,'+I'
      CALL JBINQ(XMAX,YMAX,CUR)
      SIZE(1)=XMAX
      SIZE(2)=YMAX
      IF (.NOT.CUR) THEN
         SIZE(1)=22.0
         SIZE(2)=15.0
         CALL RDKEYR('SIZE',.TRUE.,2,SIZE,I,STATUS)
         IF (SIZE(2).EQ.0.0) THEN
            SIZE(2)=SIZE(1)
         END IF
      END IF
      XL=SIZE(1)
      YL=SIZE(2)

C
C     Copy the image into an array which can be altered by the
C     program.
C

      CALL GETDYN('ARRAY',FMT_R,AX(1),IPIN,ISTATUS)
C
C   If the image is 2-d it loops back to here
C
      ISECT=1
      CALL RDKEYL('AUTO',.FALSE.,1,AUTO,I,ISTAT)
  200 CONTINUE
      IF (IDIM.EQ.2) THEN
         CALL RDKEYI('SECTION',.TRUE.,1,ISECT,I,STATUS)
         IF (ISECT.LE.0.OR.ISECT.GT.AX(2)) GO TO 300
      ELSE
         ISECT=1
      END IF
      CALL COPY(%VAL(PIN),%VAL(IPIN),AX,ISECT)
C
C
C   GET LIMITS OF PLOT & PLOTTING STEP IN PIXELS
C
      NPIX(1) = 1
      NPIX(2) = AX(1)
      NPIX(3) = AX(1)/512 + 1
      CALL RDKEYI('LIMITS',.TRUE.,3,NPIX,NVAL,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL.AND.ISTAT.NE.ERR_PARNUL)  THEN
         CALL CNPAR('LIMITS',ISTAT)
         CALL EXIT
      ENDIF
      IF(NPIX(3).EQ.0)  NPIX(3) = AX(1)/512 + 1
C
C  CALL PLOTTING SUBROUTINE
C
      CALL PLOTLINE(%VAL(IPIN),AX(1),NPIX,XL,YL,AUTO)
C
C   TIDY UP  AND GO HOME
C
      IF (IDIM.EQ.2) THEN
         CALL CNPAR('SECTION',ISTAT)
         ISECT=ISECT+1
         IF (.NOT.AUTO) THEN
            CALL CNPAR('LIMITS',ISTAT)
         END IF
         GO TO 200
      END IF
  300 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL ENDPLT
      CALL EXIT
      END




      SUBROUTINE PLOTLINE(LINE,NX,NPIX,XL,YL,AUTO)
      REAL LINE(NX)
      REAL AX(2),AY(2)
      LOGICAL*1 AUTO
      INTEGER NPIX(3)
      CHARACTER LL(2)*3
C
C  RUN THROUGH ARRAY FOR SCALING OF PLOT
C
      FMAX = LINE(NPIX(1))
      FMIN = LINE(NPIX(1))
      DO 100 I=NPIX(1),NPIX(2),NPIX(3)
      IF(FMAX.LT.LINE(I)) FMAX = LINE(I)
      IF(FMIN.GT.LINE(I))  FMIN = LINE(I)
  100 CONTINUE
         AX(1) = NPIX(1)
         AX(2) = NPIX(2)
         AY(1) = FMIN
         AY(2) = FMAX*1.1
       NXA=2
       NYA=2 

C
C     Ask for the range of the Y axis.
C

      CALL RDKEYR('RANGE',.TRUE.,2,AY,I,ISTAT)

C      Replace any elements in the array which lie outside
C      the range by either the upper or lower limit.
C

      DO 300 I=1,NX
      IF (LINE(I).LT.AY(1)) LINE(I)=AY(1)
      IF (LINE(I).GT.AY(2)) LINE(I)=AY(2)
300   CONTINUE
      LL(1)='LIN'
      LL(2)='LIN'

C
C     Ask for the type of axes.
C

      CALL RDKEYC('AXES',.TRUE.,2,LL,I,ISTAT)
      IF (LL(1).EQ.'LOG') NXA= -2
      IF (LL(2).EQ.'LOG') NYA= -2
      CALL JBAXES(AX,NXA,XL,'PIXEL',5,AY,NYA,YL,'COUNT',5)
C
C  HAVING DRAWN SIMPLEPLOT AXES NOW PLOT DATA REQUIRED
C
      DO 200 I=NPIX(1),NPIX(2),NPIX(3)
      CALL JOIN PT(REAL(I),LINE(I))
  200 CONTINUE
      IF (.NOT.AUTO) THEN
         CALL CNPAR('RANGE',ISTAT)
         CALL CNPAR('AXES',ISTAT)
      END IF
      END


      SUBROUTINE COPY(IN,OUT,DIM,IS)

C
C     This program takes in data from the image and stores it
C     in an array .

      INTEGER DIM(2)
      REAL IN(DIM(1),DIM(2)),OUT(DIM(1))
      DO I=1,DIM(1)
         OUT(I)=IN(I,IS)
      END DO
      END
