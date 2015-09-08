      SUBROUTINE PRFDEC
C+
C     PRFDEC.
C
C     Subroutine to decompose a profile extracted from a disk 
C     galaxy into its bulge and disk components, using non-
C     linear least squares techniques. The profile is held as
C     as Starlink image and the results are sent to an output
C     VMS file called DECOMP.LIS.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-   INPICR, READR, READC, OUTPUT, CLEARIM.
C   E2D:-          KOPY, ITERFIT, SIMFIT, LISTFIT, CMPLT.
C   System:-       DATE, TIME.
C
C  Structure:-
C   Attempt to obtain a pointer to the profile.
C   If pointer obtained successfully
C     Copy the profile into work arrays.
C     Obtain values for the fitting ranges etc. from the user
C     If values obtained Ok
C       Obtain a title for the listing
C       Attempt to open output file.
C       If file opens ok
C         attempt iterative fitting
C         if fitting ok then
C           send results to file
C           print message
C         else
C           print message
C         end if
C         attempt simultaneous fitting
C         if fitting ok then
C           send results to file.
C           print messge
C         else
C           print message
C         end if
C         close file
C       else
C         print message saying unable to open file.
C       end if
C     else
C       Print message saying can't get parameters.
C     end if
C   else
C     Print message saying can't get profile.
C   end if
C   Tidy up profile image.
C
C  A C Davenhall./ROE/                                 4/8/82.
C  A C Davenhall./ROE/ {Modified}                      8/7/83.
C-
      IMPLICIT NONE
C
      INTEGER PRFPTR
      INTEGER AXISIZ(2)
C
C    Statuses.
C
      INTEGER IMSTAT,IOSTAT,OSTAT,ITERSTAT,SIMSTAT
C
      INTEGER NBULGE,NDISK
      REAL LAMDA,SKY,BMIN,BMAX,DMIN,DMAX
      CHARACTER TITLE*20
C
      INTEGER MAXPTS,NPTS
C
      PARAMETER (MAXPTS=1024)
      REAL RAD(MAXPTS),LOGI(MAXPTS),LOGIFIT(MAXPTS),DIFF(MAXPTS),
     :     DIFFMU(MAXPTS),MUFIT(MAXPTS),RDISK(MAXPTS),
     :     RBULG(MAXPTS),REMDISK(MAXPTS),REMBULG(MAXPTS),
     :     MU(MAXPTS)
      REAL PARAM(10)
C
C    Fortran unit no. for output listing.
C
      INTEGER UNITWR
      PARAMETER (UNITWR=22)
C
      CHARACTER DATEB*9, TIMEB*8
C
C
C    Attempt to obtain a pointer to the required profile.
C
      IMSTAT=0
      CALL INPICR ('PROFILE',
     :      ' Enter filename for the profile;',
     :        2,AXISIZ,PRFPTR,IMSTAT)
C
C    Proceed if the pointer has been obtained successfully.
C
      IF (IMSTAT.EQ.0) THEN
C
C    Copy the profile into the work arrays.
C
        NPTS=AXISIZ(1)
        NPTS=MIN(NPTS,MAXPTS)
        CALL KOPY (%VAL(PRFPTR),NPTS,MAXPTS,RAD,LOGI)
C
C    Obtain values for the parameters required by the fitting
C    routines from the user.
C
        IOSTAT=0
        CALL READR ('LAMDA',
     :   'Enter non-linear least squares parameter lamda',
     :    1.0E-3,1.0E-7,1.0E1,LAMDA,IOSTAT)
        CALL READR ('SKY',
     :   'Enter the sky brightness (mag./sq.arcsec);',
     :    21.55,10.0,30.0,SKY,IOSTAT)
        CALL READR ('BMIN',
     :   'Enter minimum radius for bulge fit;',
     :    0.0E0,0.0E0,3.0E2,BMIN,IOSTAT)
        CALL READR ('BMAX',
     :   'Enter maximum radius for bulge fit;',
     :    0.0E0,BMIN,3.0E2,BMAX,IOSTAT)
        CALL READR ('DMIN',
     :   'Enter minimum radius for disk fit;',
     :    0.0E0,BMAX,3.0E2,DMIN,IOSTAT)
        CALL READR ('DMAX',
     :   'Enter maximum radius for disk fit;',
     :    0.0E0,DMIN,3.0E2,DMAX,IOSTAT)
C
        NBULGE=15
        NDISK=15
C
C    Proceed if parameters obtained Ok.
C
        IF (IOSTAT.EQ.0) THEN
C
C    Obtain title.
C
          CALL READC ('TITLE',' Enter title;',' ',' ','~',
     :                 TITLE,IOSTAT)
C
C    Attempt to open output file.
C
          OPEN(UNIT=UNITWR,FILE='DECOMP.LIS',STATUS='NEW',
     :         IOSTAT=OSTAT)
          IF (OSTAT.EQ.0) THEN
            CALL OUTPUT (
     :       'Please wait. Fitting now proceeding.',IOSTAT)
            CALL DATE (DATEB)
            CALL TIME (TIMEB)
            WRITE(UNITWR,2000) TIMEB,TITLE,DATEB
 2000       FORMAT(1H1,3X,A9,44X,A20,44X,A8//)
C
C    Attempt the iterative fit.
C
            CALL ITERFIT (NPTS,RAD,LOGI,BMIN,BMAX,NBULGE,
     :                    DMIN,DMAX,NDISK,LAMDA,UNITWR,PARAM,
     :                    ITERSTAT)
            IF (ITERSTAT.EQ.0) THEN
              CALL LISTFIT (NPTS,RAD,LOGI,PARAM,SKY,UNITWR,LOGIFIT,
     :                      DIFF,DIFFMU,MUFIT,RDISK,RBULG,REMDISK,
     :                      REMBULG,MU)
              CALL CMPLT (NPTS,RAD,LOGI,PARAM,3,TITLE,
     :                    'ITERATIVE FITTING.','RADIUS',
     :                    'LOG I',RDISK,RBULG,REMDISK,DIFF,
     :                     DIFFMU,MUFIT)
              CALL OUTPUT (
     :         'Iterative fitting completed successfully.',IOSTAT)
            ELSE
              CALL OUTPUT (
     :         '***ERROR iterative fitting not successfull.',IOSTAT)
            END IF
C
C    Attempt simultaneous fit.
C
            CALL SIMFIT (NPTS,RAD,LOGI,BMIN,BMAX,DMIN,DMAX,LAMDA,
     :                   UNITWR,PARAM,SIMSTAT)
            IF (SIMSTAT.EQ.0) THEN
              CALL LISTFIT (NPTS,RAD,LOGI,PARAM,SKY,UNITWR,LOGIFIT,
     :                      DIFF,DIFFMU,MUFIT,RDISK,RBULG,REMDISK,
     :                      REMBULG,MU)
              CALL CMPLT (NPTS,RAD,LOGI,PARAM,3,TITLE,
     :                      'SIMULTANEOUS LEAST SQUARES','RADIUS',
     :                      'LOG I',RDISK,RBULG,REMDISK,DIFF,
     :                       DIFFMU,MUFIT)
              CALL OUTPUT (
     :         'Simultaneous fitting completed successfully.',
     :          IOSTAT)
            ELSE
              CALL OUTPUT (
     :         '***ERROR Simultaneous fit not successfull.',IOSTAT)
            END IF
          ELSE
            CALL OUTPUT (
     :       '***ERROR Unable to open listing file successfully.',
     :           IOSTAT)
          END IF
        ELSE
          CALL OUTPUT (
     :   ' ***ERROR Unable to obtain parameters correctly.',IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain profile file.',IOSTAT)
      END IF
C
C    Tidy up the profile file.
C
      CALL CLEARIM ('PROFILE')
C
      END
