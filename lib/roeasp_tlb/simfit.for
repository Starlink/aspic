      SUBROUTINE SIMFIT (POINTS,RADIUS,LOGI,BMIN,BMAX,
     :                    DMIN,DMAX,FLAMDA,UNITWR,PARAM,
     :                    STATUS)
C+
C     SIMFIT.
C
C     Subroutine to fit the profile of a disk galaxy using the
C     Simultaneous least squares fitting technique of Kormendy (1977).
C
C  Given;
C   POINTS    (I)  No. of points in the input profile.
C   RADIUS    (RA) Radii of points in the input profile.
C   LOGI      (RA) Log (intensity) of points in input profile.
C   BMIN      (R)  Minimum radius of selected bulge region.
C   BMAX      (R)  Maximum   "    "     "       "     "   .
C   DMIN      (R)  Minimum radius of selected disk region.
C   DMAX      (R)  Maximum   "    "     "      "     "   .
C   FLAMDA    (R)  Non-linear least squares parameter.
C   UNITWR    (I)  Fortran unit no. for output.
C
C  Returned;
C   PARAM     (RA) Parameters found for disk and bulge components.
C   STATUS    (I)  
C
C  Subroutines called;
C   E2D:-  LINLSQ, KURFIT.
C
C  Functions called;
C   E2D:-  BULDISK, DERBD, DISK.
C
C  A C Davenhall./St Andrews/                                     Spring 81.
C  A C Davenhall./ROE/ {Modified}                                 4/8/82.
C  A C Davenhall./ROE/ {Re-written in full Starlink style}        29/6/83.
C  A C Davenhall./ROE/ {Modified}                                 13/12/83.
C-
      IMPLICIT NONE
C
      INTEGER POINTS,STATUS,UNITWR
      REAL RADIUS(POINTS),LOGI(POINTS)
      REAL PARAM(10)
      REAL BMIN,BMAX,DMIN,DMAX,FLAMDA
C
C    Declare external functions.
C
      EXTERNAL DISK,BULDISK,DERBD
      REAL     DISK,BULDISK,DERBD
C
C    Internal arrays to hold extracted bulge and disk regions;
C 
      INTEGER FITPTS,BPTS,DPTS,TOTPTS
      PARAMETER (FITPTS=50)
      REAL BRAD(FITPTS),BRQUART(FITPTS),BLOGI(FITPTS),
     :     DRAD(FITPTS),DLOGI(FITPTS),
     :     RADEXT(FITPTS),LOGIEXT(FITPTS),LOGIFIT(FITPTS)
C
      INTEGER COUNT1,COUNT2,INDEX,ITER,CONVSTAT,PRSTAT
      REAL RADPT,WORK,RATIO
C
      REAL GRAD,C,SIGG,SIGC,CHISQR,CHISQ1,AA,FLAMDW
      REAL A(10),SIGMAA(10),PREVPARM(10)
C
      INTEGER MAXITER 
      PARAMETER (MAXITER=20)
C
      REAL CONST,CONST1
      PARAMETER (CONST=4.34294E-1,
     :           CONST1=-3.3307E0)
C
C
      WRITE(UNITWR,2006)
 2006 FORMAT(1H1/41X,'Profile Decomposition by Simultaneous Least ',
     :       'Squares.'//)
C
C    Extract the points from the bulge and disk regions.
C
      COUNT1=0
      COUNT2=0
      DO INDEX=1,POINTS
        RADPT=RADIUS(INDEX)
        IF (RADPT.GT.BMIN.AND.RADPT.LE.BMAX) THEN
          COUNT1=COUNT1+1
          BRAD(COUNT1)=RADPT
          BLOGI(COUNT1)=LOGI(INDEX)
          BRQUART(COUNT1)=RADPT**2.5E-1
        ELSE IF (RADPT.GT.DMIN.AND.RADPT.LE.DMAX) THEN
          COUNT2=COUNT2+1
          DRAD(COUNT2)=RADPT
          DLOGI(COUNT2)=LOGI(INDEX)
        END IF
      END DO
      BPTS=COUNT1
      DPTS=COUNT2
      WRITE(UNITWR,2007) BMIN,BMAX,BPTS,DMIN,DMAX,DPTS
 2007 FORMAT(38X,'Bulge range = ',0PF6.2,'  -',F6.2,5X,
     :      'Number of points = ',I4/
     :       38X,'Disk range = ',F7.2,'  -',F6.2,5X,
     :      'Number of points = ',I4//)
C
C    Combine the bulge and disk points into one array.
C
      DO INDEX=1,BPTS
        RADEXT(INDEX)=BRAD(INDEX)
        LOGIEXT(INDEX)=BLOGI(INDEX)
      END DO
      DO INDEX=1,DPTS
        RADEXT(INDEX+BPTS)=DRAD(INDEX)
        LOGIEXT(INDEX+BPTS)=DLOGI(INDEX)
      END DO
      TOTPTS=BPTS+DPTS
C
C    Check that there are enough points in both the disk and bulge
C    regions to permit a least squares fit.
C
      IF (BPTS.GT.2.AND.DPTS.GT.2) THEN
C
C    Make an initial fit to the disk.
C
        CALL LINLSQ (DRAD,DLOGI,DPTS,FITPTS,.FALSE.,GRAD,C,SIGG,SIGC,
     :               CHISQR,UNITWR)
        PARAM(3)=1.0E1**C
        PARAM(4)=-GRAD/CONST
        PARAM(6)=CHISQR
C
C    Initial fit to the bulge.
C
        A(1)=PARAM(3)
        A(2)=PARAM(4)
        DO INDEX=1,BPTS
          RADPT=BRAD(INDEX)
          WORK=(1.0E1**BLOGI(INDEX))-DISK(RADPT,A,10)
          IF (WORK.GE.1.0E-6) THEN
            BLOGI(INDEX)=ALOG10(WORK)
          ELSE
            BLOGI(INDEX)=-6.0E0
          END IF
        END DO
        CALL LINLSQ (BRQUART,BLOGI,BPTS,FITPTS,.FALSE.,GRAD,C,
     :               SIGG,SIGC,CHISQR,UNITWR)
        AA=CONST1/GRAD
        PARAM(2)=AA**4.0E0
        PARAM(1)=1.0E1**(C+CONST1)
        PARAM(5)=CHISQR
        WRITE(UNITWR,2000,IOSTAT=PRSTAT) (PARAM(INDEX),INDEX=1,6)
 2000   FORMAT(1X,'Initial approximation:-',10X,'--- Bulge ---',
     :    12X,'--- disk ---',14X,'Chi Squared'/33X,'Io',11X,'Ro',
     :    10X,'Io',8X,'Alpha',10X,'Bulge',7X,'Disk'/27X,0PF10.3,
     :    3X,F10.3,2X,F10.3,1X,F10.3,5X,1PE12.3,1PE12.3//)
        WRITE(UNITWR,2001)
 2001   FORMAT(34X,'--- Bulge ---',12X,'--- Disk ---',/33X,'Io',11X,
     :    'Ro',10X,'Io',8X,'Alpha',7X,'Chi Squared',4X,'Lamda'/)
C
C    Make simultaneous least squares fit.
C
        ITER=0
        CHISQ1=-5.6E4
        CHISQR=5.6E4
        CONVSTAT=0
C
        DO WHILE (ABS(CHISQR-CHISQ1).GE.5.0E-6.AND.ITER.LE.MAXITER.
     :            AND.CONVSTAT.EQ.0)
C
          DO INDEX=1,4
            PREVPARM(INDEX)=PARAM(INDEX)
          END DO
C
          ITER=ITER+1
          CHISQ1=CHISQR
          FLAMDW=FLAMDA
          CALL KURFIT (RADEXT,LOGIEXT,LOGIFIT,TOTPTS,FITPTS,PARAM,
     :                 SIGMAA,4,10,BULDISK,DERBD,FLAMDW,CHISQR,
     :                 CONVSTAT)
          WRITE(UNITWR,2002,IOSTAT=PRSTAT) ITER,(PARAM(INDEX),INDEX=1,4)
     :                     ,CHISQR,FLAMDW
 2002     FORMAT(24X,I3,0PF10.3,3X,F10.3,2X,F10.3,1X,F10.3,5X,1PE12.3,
     :           1PE12.3)
C
C   Check whether the parameters have wildly diverged this 
C   iteration, or whether the scale lengths have become 
C   negative.
C
          DO INDEX=1,4
            IF (ABS(PARAM(INDEX)).GT.1.0E-6) THEN
              RATIO=ABS(PREVPARM(INDEX)/PARAM(INDEX))
            ELSE
              RATIO=1.0E0
              CONVSTAT=3
            END IF
            IF (RATIO.GT.1.0E2.OR.RATIO.LT.1.0E-3) CONVSTAT=3
          END DO
          IF (PARAM(2).LE.0.0E0) CONVSTAT=3
          IF (PARAM(4).LE.0.0E0) CONVSTAT=3
C
        END DO
C
C    Sort out the return status.
C
        STATUS=0
        IF (CONVSTAT.EQ.1.) THEN
          WRITE(UNITWR,2003) CONVSTAT
 2003     FORMAT(/5X,'*** ERRROR ***'//10X,
     :     'Return Status from least squares routine KURFIT = ',I2//
     :     10X,'More parameters than data points.'///)
          STATUS=1
        END IF
C
        IF (CONVSTAT.GE.2) THEN
          WRITE(UNITWR,2008)
 2008     FORMAT(/5X,'*** ERROR ***'//10X,
     :     'Solution has run wild. Fitting terminating.'///)
          STATUS=1
        END IF
C
        IF (ITER.GE.MAXITER) THEN
          WRITE(UNITWR,2004) ITER
 2004     FORMAT(/5X,'*** ERROR ***'//10X,
     :     'Convergence has not been found after ',I2,' iterations.'
     :      ///)
          STATUS=2
        END IF
C
C    Write out the final solution again.
C
        IF (STATUS.EQ.0) WRITE(UNITWR,2005) (PARAM(INDEX),INDEX=1,4)
 2005   FORMAT(/7X,'Final solution:- ',3X,
     :     0PF10.3,3X,F10.3,2X,F10.3,1X,F10.3//)
      ELSE
C
C    Case where there are not enough points in either the disk
C    or the bulge region to permit a fit.
C
        WRITE(UNITWR,2009)
 2009   FORMAT(/5X,'*** ERROR ***'//)
        IF (BPTS.LE.2) WRITE(UNITWR,2010)
 2010   FORMAT(10X,'Insufficient points in the bulge to permit a fit.')
        IF (DPTS.LE.2) WRITE(UNITWR,2011)
 2011   FORMAT(10X,'Insufficient points in the disk to permit a fit.')
        DO INDEX=1,4
          PARAM(1)=1.0E0
        END DO
C
        STATUS=1
C
      END IF
C
      IF (PARAM(1).GE.1.0E-6) THEN
        PARAM(1)=ALOG10(PARAM(1))
      ELSE
        PARAM(1)=-6.0E0
      END IF
      IF (PARAM(3).GE.1.0E-6) THEN
        PARAM(3)=ALOG10(PARAM(3))
      ELSE
        PARAM(3)=-6.0E0
      END IF
C
      END
