      SUBROUTINE LISTFIT (POINTS,RADIUS,LOGI,PARAM,SKY,UNITWR,
     :                    LOGIFIT,DIFF,DIFFMU,MUFIT,RDISK,RBULG,
     :                    REMDISK,REMBULG,MU)
C+
C     LISTFIT
C
C     Subroutine to list the observed and fitted profiles for a 
C     disk galaxy after least squares fits to the bulge and
C     disk components.
C
C  Given;
C   POINTS    (I)  No. of data points in profiles.
C   RADIUS    (RA) Radii of points in profile.
C   LOGI      (RA) Log (intensity) for points in profile.
C   PARAM     (RA) Values for disk and bulge parameters.
C   SKY       (R)  Sky brightness (mag./sq.arcsec).
C   UNITWR    (I)  Fortran unit no. for output.
C
C  Used;
C   LOGIFIT   (RA) size = POINTS.
C   DIFF      (RA)      "       .
C   DIFFMU    (RA)      "       .
C   MUFIT     (RA)      "       .
C   RDISK     (RA)      "       .
C   RBULG     (RA)      "       .
C   REMDISK   (RA)      "       .
C   REMBULG   (RA)      "       .
C   MU        (RA)      "       .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   None.
C
C  Functions called;
C   E2D:-   DISK, BULGE, FCHISQ.
C
C  A C Davenhall./ROE/                                         14/6/83.
C-
      IMPLICIT NONE
C
      INTEGER POINTS,UNITWR
      REAL RADIUS(POINTS),LOGI(POINTS)
      REAL PARAM(10)
      REAL SKY
C
      REAL LOGIFIT(POINTS),DIFF(POINTS),DIFFMU(POINTS),MUFIT(POINTS),
     :     RDISK(POINTS),RBULG(POINTS),REMDISK(POINTS),
     :     REMBULG(POINTS),MU(POINTS)
C
C
C
C    Define the external functions.
C
      EXTERNAL  DISK,BULGE
      REAL      DISK,BULGE,FCHISQ
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
      REAL WORK,RIXPT,CHISQR
      REAL A(10)
      INTEGER INDEX,FREE
C
C    Decode the parameters into the work array.
C
      A(1)=1.0E1**PARAM(3)
      A(2)=PARAM(4)
C
C    Compute all the values to be output for each data point.
C
      DO INDEX=1,POINTS
        LOGIFIT(INDEX)=ALOG10(DISK(RADIUS(INDEX),A,10)+
     :                 (1.0E1**BULGE(RADIUS(INDEX),PARAM,10)))
        DIFF(INDEX)=LOGI(INDEX)-LOGIFIT(INDEX)
        DIFFMU(INDEX)=DIFF(INDEX)*POGSON
        MUFIT(INDEX)=SKY-(POGSON*LOGIFIT(INDEX))
        WORK=DISK(RADIUS(INDEX),A,10)
        RDISK(INDEX)=SKY-(POGSON*ALOG10(WORK))
        RIXPT=1.0E1**LOGI(INDEX)
        IF (RIXPT.GT.WORK) THEN
          REMBULG(INDEX)=SKY-(POGSON*ALOG10(RIXPT-WORK))
        ELSE
          REMBULG(INDEX)=-3.0E2
        END IF
        WORK=1.0E1**BULGE(RADIUS(INDEX),PARAM,10)
        RBULG(INDEX)=SKY-(POGSON*ALOG10(WORK))
        IF (RIXPT.GT.WORK) THEN                !! CHECK THIS!!
          REMDISK(INDEX)=SKY-(POGSON*ALOG10(RIXPT-WORK))
        ELSE
          REMDISK(INDEX)=-3.0E2
        END IF
        MU(INDEX)=SKY-(POGSON*LOGI(INDEX))
      END DO
C
C    Compute the residual chi-squared.
C
      FREE=POINTS-4
      IF (FREE.GT.0) THEN
        CHISQR=FCHISQ(LOGI,LOGIFIT,POINTS,POINTS,FREE)
        WRITE(UNITWR,2000) CHISQR
 2000   FORMAT(//43X,'Chi Squared for whole profile = ',1PE12.3///)
      END IF
C
C    List out the results.
C
      WRITE(UNITWR,2001)
 2001 FORMAT(33X,'Radius',6X,'Log I',4X,'Log I(fit)',4X,
     :  'Residual',6X,'Mu',7X,'Mu(fit)',6X,'Residual'/
     :  68X,'(Log I)',27X,'(magnitudes)'/)
      WRITE(UNITWR,2002) (INDEX,RADIUS(INDEX),LOGI(INDEX),
     :       LOGIFIT(INDEX),DIFF(INDEX),MU(INDEX),MUFIT(INDEX),
     :       DIFFMU(INDEX),INDEX=1,POINTS)
 2002 FORMAT(23X,I3,2X,0PF10.2,2X,F10.3,2X,F10.3,2X,F10.3,
     :       2X,F10.3,2X,F10.3,2X,F10.3)
      WRITE(UNITWR,2003) 
 2003 FORMAT(///35X,'Radius',10X,'Fitted',7X,'Residual',11X,
     :       'Fitted',8X,'Residual'/52X,'Disk',9X,'Bulge',
     :       13X,'Bulge',11X,'Disk'/)
      WRITE(UNITWR,2004) (INDEX,RADIUS(INDEX),RDISK(INDEX),
     :       REMBULG(INDEX),RBULG(INDEX),REMDISK(INDEX),
     :       INDEX=1,POINTS)
 2004 FORMAT(26X,I3,4X,0PF7.2,7X,F10.3,4X,F10.3,8X,F10.3,5X,F10.3)
C
      END
