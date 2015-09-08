      SUBROUTINE ITERFIT (POINTS,RADIUS,LOGI,BMIN,BMAX,BULPTS,
     :                    DMIN,DMAX,DISPTS,FLAMDA,UNITWR,PARAM,
     :                    STATUS)
C+
C     ITERFIT.
C
C     Subroutine to fit the profile of a disk galaxy using the
C     Iterative fitting technique of Kormendy (1977).
C
C  Given;
C   POINTS    (I)  No. of points in the input profile.
C   RADIUS    (RA) Radii of points in the input profile.
C   LOGI      (RA) Log (intensity) of points in input profile.
C   BMIN      (R)  Minimum radius of selected bulge region.
C   BMAX      (R)  Maximum   "    "     "       "     "   .
C   BULPTS    (I)  No. of points to be interpolated in bulbe region.
C   DMIN      (R)  Minimum radius of selected disk region.
C   DMAX      (R)  Maximum   "    "     "      "     "   .
C   DISPTS    (I)  No. of points to be interpolated in disk region.
C   FLAMDA    (R)  Non-linear least squares parameter.
C   UNITWR    (I)  Fortran unit no. for output.
C
C  Returned;
C   PARAM     (RA) Parameters found for disk and bulge components.
C   STATUS    (I)  Return status. 0 for success, otherwise non-zero.
C
C  Subroutines called;
C   E2D:-    LAGIN, LINLSQ, KURFIT.
C
C  Functions used;
C   E2D:-    DISK, DERDISK, BULGE, DERBULGE.
C
C  A C Davenhall./St Andrews/                                     Spring 81.
C  A C Davenhall./ROE/ {Modified}                                 4/8/82.
C  A C Davenhall./ROE/ {Re-written in full Starlink style}        14/6/83.
C  A C Davenhall./ROE/ {Modified}                                 12/12/83.
C-
      IMPLICIT NONE
C
      INTEGER POINTS,BPTS,DPTS,STATUS,UNITWR
      INTEGER BULPTS,DISPTS
      REAL RADIUS(POINTS),LOGI(POINTS)
      REAL PARAM(10)
      REAL BMIN,BMAX,DMIN,DMAX,FLAMDA
C
C    Declare external functions.
C
      EXTERNAL DISK,DERDISK,BULGE,DERBULGE
      REAL     DISK,DERDISK,BULGE,DERBULGE
C
C    Internal arrays to hold extracted bulge and disk regions;
C 
      INTEGER FITPTS
      PARAMETER (FITPTS=50)
      REAL BRAD(FITPTS),BRQUART(FITPTS),BLOGI(FITPTS),
     :     DRAD(FITPTS),DLOGI(FITPTS),
     :     BFIT(FITPTS),FITBLOGI(FITPTS),FITDLOGI(FITPTS)
C
      REAL BDR,DDR
      INTEGER INDEX,ITER,KEEP,CONVSTAT,BULITER
C
C    Arrays for holding evaluated bulge and disk parameters.
C
      REAL A(10),SIGMAA(10),PREVPARM(10)
C
      REAL GRAD,C,SIGG,SIGC,CHISQR,CHISQ1,FLAMDW,DUMMY,AA
      REAL RATIO
C
      INTEGER MAXITER
      PARAMETER (MAXITER=15)
C
      REAL CONST,CONST1
      PARAMETER (CONST=4.34294E-1)
      PARAMETER (CONST1=-3.3307E0)
C
C
C    Force the number of bulge and disk points to
C    be within the permitted ranges.
C
      BPTS=MIN(BULPTS,FITPTS)
      DPTS=MIN(DISPTS,FITPTS)
C
C    Generate a set of equally spaced radii to fill the selected
C    bulge and disk ranges.
C    ...bulge.
C
      BDR=(BMAX-BMIN)/FLOAT(BPTS-1)
      DO INDEX=1,BPTS
        BRAD(INDEX)=BMIN+(BDR*FLOAT(INDEX-1))
        BRQUART(INDEX)=(BRAD(INDEX))**2.5E-1
      END DO
C
C    ...disk.
C
      DDR=(DMAX-DMIN)/FLOAT(DPTS-1)
      DO INDEX=1,DPTS
        DRAD(INDEX)=DMIN+(DDR*FLOAT(INDEX-1))
      END DO
C
C    Interpolate intensities for points inside the fitting ranges.
C
      CALL LAGIN (RADIUS,LOGI,BRAD,BLOGI,BPTS,POINTS,POINTS,BPTS)
      CALL LAGIN (RADIUS,LOGI,DRAD,DLOGI,DPTS,POINTS,POINTS,DPTS)
C
      WRITE(UNITWR,2000) BMIN,BMAX,BPTS
 2000 FORMAT(//49X,'Profile Decomposition by Iterative Fitting.'
     :  ///10X,'Points interpolated in the bulge;',5X,'r(min) = ',
     :  0PF6.2,2X,'r(max) = ',F6.2,2X,'No. of points = ',I3//
     :  51X,'Radius',9X,'r**1/4',10X,'Log I'//)
      WRITE(UNITWR,2001) (INDEX,BRAD(INDEX),BRQUART(INDEX),
     :                   BLOGI(INDEX),INDEX=1,BPTS)
 2001 FORMAT(44X,I3,0PF10.2,5X,F10.3,5X,F10.3)
      WRITE(UNITWR,2002) DMIN,DMAX,DPTS
 2002 FORMAT(///10X,'Points interpolated in the disk;',5X,
     :  'r(min) = ',0PF6.2,2X,'r(max) = ',F6.2,2X,
     :  'No. of points = ',I3//59X,'Radius',10X,'Log I'/)
      WRITE(UNITWR,2003) (INDEX,DRAD(INDEX),DLOGI(INDEX),
     :                   INDEX=1,DPTS)
 2003 FORMAT(52X,I3,0PF10.2,5X,F10.3)
      WRITE(UNITWR,2004)
 2004 FORMAT(///49X,'Profile Decomposition by Iterative Fitting.'
     :  //34x,'--- Bulge ---',18X,'--- Disk ---',17X,
     :  'Chi Squared'/31X,'Log Io',10X,'Ro',11X,'Log Io',10X,
     :  'Alpha',10X,'Bulge',8X,'Disk'/)
C
C    Set up for iterative fitting.
C
      DO INDEX=1,DPTS
        FITDLOGI(INDEX)=DLOGI(INDEX)
      END DO
C
C    Invent a "silly" previous solution in order to fool the
C    iteration termination criteria.
C
      DO INDEX=1,6
        PARAM(INDEX)=5.5E4
      END DO
C
      ITER=0
      KEEP=1
      CONVSTAT=0
C
      DO WHILE (KEEP.NE.0.AND.ITER.LE.MAXITER.AND.CONVSTAT.EQ.0)
        ITER=ITER+1
        DO INDEX=1,6
          PREVPARM(INDEX)=PARAM(INDEX)
        END DO
C
C    Fit the disk.
C
        CALL LINLSQ (DRAD,FITDLOGI,DPTS,POINTS,.FALSE.,GRAD,C,
     :               SIGG,SIGC,CHISQR,UNITWR)
        A(1)=1.0E1**C
        A(2)=-GRAD/CONST
        PARAM(3)=C
        PARAM(4)=A(2)
        PARAM(6)=CHISQR
C
        DO INDEX=1,DPTS
          DUMMY=(1.0E1**BLOGI(INDEX))-DISK(BRAD(INDEX),A,10)
          IF (DUMMY.GT.1.0E-6) THEN
            FITBLOGI(INDEX)=ALOG10(DUMMY)
          ELSE
            FITBLOGI(INDEX)=-6.0E0
          END IF
        END DO
C
C    Fit the bulge.
C
        CALL LINLSQ (BRQUART,FITBLOGI,BPTS,POINTS,.FALSE.,GRAD,C,
     :               SIGG,SIGC,CHISQR,UNITWR)
        IF (ABS(GRAD).GT.1.0E-6) THEN
          AA=CONST1/GRAD
        ELSE
          AA=1.0E0
          CONVSTAT=3
        END IF
        A(2)=AA**4.0E0
        A(1)=C+CONST
C
        BULITER=0
        CHISQ1=-5.6E4
        CHISQR=5.6E4
C
        DO WHILE (ABS(CHISQR-CHISQ1).GE.5.0E-4.AND.BULITER.LE.20.
     :            AND.CONVSTAT.EQ.0)
          BULITER=BULITER+1
          CHISQ1=CHISQR
          FLAMDW=FLAMDA
          CALL KURFIT (BRAD,FITBLOGI,BFIT,BPTS,POINTS,A,SIGMAA,
     :                 2,10,BULGE,DERBULGE,FLAMDW,CHISQR,
     :                 CONVSTAT)
          PARAM(1)=A(1)
          PARAM(2)=A(2)
          PARAM(5)=CHISQR
        END DO
C
        IF (ABS(A(2)).GT.1.0E-4) THEN
          DO INDEX=1,DPTS
            DUMMY=(1.0E1**DLOGI(INDEX))-(1.0E1**BULGE(DRAD(INDEX),A,10))
            IF (DUMMY.GT.1.0E-6) THEN
              FITDLOGI(INDEX)=ALOG10(DUMMY)
            ELSE
              FITDLOGI(INDEX)=-6.0E0
            END IF
          END DO
        ELSE
          CONVSTAT=2
        END IF
C
C    List the parameters found for this iteration.
C
        WRITE(UNITWR,2005) ITER,(PARAM(INDEX),INDEX=1,6),BULITER
 2005   FORMAT(23X,I3,0PF10.2,5X,F10.3,5X,F10.3,5X,F10.3,5X,
     :         1PE12.3,1PE12.3,I7)
C
        IF (CONVSTAT.EQ.0) THEN
C
C
C    Make the convergence test separately on all 6 parameters.
C
          KEEP=0
          DO INDEX=1,6
            IF (ABS(PARAM(INDEX)-PREVPARM(INDEX)).GE.1.0E-3) KEEP=1
          END DO
C
C    Check whether the parameters have wildly diverged this
C    iteration, or whether the scale lengths have become negative.
C
          IF (ITER.NE.1) THEN
            DO INDEX=1,4
              IF (ABS(PARAM(INDEX)).GT.1.0E-6) THEN
                RATIO=ABS(PREVPARM(INDEX)/PARAM(INDEX))
              ELSE
                RATIO=1.0E0
                CONVSTAT=3
              END IF
              IF (RATIO.GT.1.0E2.OR.RATIO.LT.1.0E-3) CONVSTAT=3
            END DO
            IF (PARAM(2).LE.0.0E0)  CONVSTAT=3
            IF (PARAM(4).LE.0.0E0)  CONVSTAT=3
          END IF
C
        END IF
      END DO
C
C    Sort out the return status.
C
      STATUS=0
      IF (CONVSTAT.EQ.1) THEN
        WRITE(UNITWR,2006) CONVSTAT
 2006   FORMAT(/5X,'*** ERROR ***'//10X,
     :   'Return Status from least squares routine KURFIT = ',I2//
     :   10X,'More parameters than data points.'///)
        STATUS=1
      END IF
C
      IF (CONVSTAT.GE.2) THEN
        WRITE(UNITWR,2009)
 2009   FORMAT(/5X,'*** ERROR ***'//10X,
     :   'Solution has run wild. Fitting terminating.'///)
        STATUS=1
      END IF
C
      IF (ITER.GT.MAXITER) THEN
        WRITE(UNITWR,2007) ITER
 2007   FORMAT(/5X,'*** ERROR ***'//10X,
     :   'Convergence has not been found after ',I2,' iterations.'
     :    ///)
        STATUS=2
      END IF
C
C    Write out the final solution again.
C
      IF (STATUS.EQ.0) WRITE(UNITWR,2008) (PARAM(INDEX),INDEX=1,6)
 2008 FORMAT(/10X,'Final solution:-',0PF10.2,5X,F10.3,5X,F10.3,
     :       5X,F10.3,5X,1PE12.3,1PE12.3)
C
      END
