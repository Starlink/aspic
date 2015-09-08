        SUBROUTINE EQPROF (A,IXEXT,IYEXT,
     :                ICST,IRST,PLTSCL,LOGMIN,DELOGI,NLEVEL,
     :                N1,N2,KEEP)
C+
C       EQPROF.
C
C         Subroutine to compute an equivlent profile.
C
C
C         Define region from which eq. prof. is to be computed.
C         Compute equivalent profile in pixels.
C         Convert equivalent profile to area units.
C         If equivalent profile is to be plotted.
C           Plot profile
C
C  Given;
C   A       (RA) Image array.
C   IXEXT   (I)  X extent of image.
C   IYEXT   (I)  Y extent of image.
C   ICST    (I)  Y (columns) stepsize (micron).
C   IRST    (I)  X (rows) stepsize (micron).
C   PLTSCL  (R)  Plate scale (arcsec/mm).
C   LOGMIN  (R)  Log I of the lowest isophotal level.
C   DELOGI  (R)  Increment in Log I.
C   NLEVEL  (I)  No. of isophotal levels to be considered.
C   N1, N2  (I)  Size of array KEEP.
C
C  Returned;
C   NLEVEL  (I)  No of isophotal levels in the profile
C                 = min(no requested, max permitted). 
C   KEEP    (RA) 2D array holding the equivalent profile.
C                Column 1 = equivalent radius (arcsec).
C                Column 2 = Log I.
C
C  Subroutines called;
C   E2D:-         DEFBOX, LEVGEN.
C   Interfaces:-  OUTPUT.
C
C
C         W D Pence      /Univ. of Sussex/                Oct 1980.
C         A C Davenhall. /ROE/                                10/11/81.
C         A C Davenhall. /ROE/   {Modified}                   23/7/82.
C         A C Davenhall. /ROE/   {   "    }                   14/3/83.
C-
        REAL A(IXEXT,IYEXT)
        REAL KEEP(N1,N2)
        INTEGER IXEXT,IYEXT,N1,N2,ICST,IRST,NLEVEL
        REAL LOGMIN,DELOGI,PLTSCL
        REAL LOGI(100),AREA(100),REQ(100),INT(100)
        INTEGER PIXEL(100)
        REAL VALUE,INTMIN,TEN,PI,SKY,
     :             UNITAIR
        INTEGER MINCOL,MINROW,MAXCOL,MAXROW,
     :                ISTAT,NPTS,LEVEL,MAXLEV
        LOGICAL OK,BATCHT
        CHARACTER BUFF*80
C
        PARAMETER (TEN=1.0E1)
        PARAMETER (PI=3.1415927E0)
        PARAMETER (MAXLEV=100)
        PARAMETER (SKY=1.0E0)
C
C         Obtain the region from which the eq. prof. is to be computed.
C
        CALL DEFBOX (IXEXT,IYEXT,MINROW,MAXROW,MINCOL,MAXCOL,ISTAT)
C
C         If there are more th n the permitted no. of levels set
C         no. equal to max. permitted & print message.
C
        IF (NLEVEL.GT.MAXLEV) THEN
          NLEVEL=MAXLEV
          WRITE(BUFF,2000) NLEVEL
 2000     FORMAT(1X,'Warning. More than permitted no. of levels, ',
     :    'Set equal to ',I3)
          CALL OUTPUT (BUFF,ISTAT)
        END IF
C
        CALL OUTPUT (
     :  ' Please wait; equivalent profile being calculated;',
     :     ISTAT)

C
C         Now compute the equivalent profile.
C
C         Setup array of isophotal levels in Log I and I.
C
        CALL LEVGEN (MAXLEV,LOGMIN,DELOGI,NLEVEL,LOGI,INT)
C
C         Initialise the number of pixels in all levels to zero.
C
        DO I=1,MAXLEV
          PIXEL(I)=0
        END DO
C*
C         Actually evaluate the profile.
C
C         Examine each point;
C          If pt. is greater than minimum value
C            Do while (pt. is greater than table entry & table entry
C                      is less than no. of entries)
C              increment entry
C              increment no. of pixels in entry by +1
C            end do
C          end if
C*
        NPTS=0
        INTMIN=INT(1)
        DO J=MINCOL,MAXCOL
          DO I=MINROW,MAXROW
            VALUE=A(I,J)
            IF (VALUE.GT.INTMIN) THEN
              LEVEL=0
              DO WHILE (VALUE.GT.INT(LEVEL+1).AND.LEVEL+1.LT.NLEVEL)
                LEVEL=LEVEL+1
                PIXEL(LEVEL)=PIXEL(LEVEL)+1
              END DO
              NPTS=MAX(NPTS,LEVEL)
            END IF
          END DO
        END DO
C
C         Locate & take care of the case where isophotal levels
C         with an equal no. of pixels have occurred. Squash the
C         profile to remove them.
C
        I=1
        DO WHILE (I.LT.NPTS)
          I=I+1
          IF (PIXEL(I).EQ.PIXEL(I-1)) THEN
            DO K=I,NPTS
              PIXEL(K-1)=PIXEL(K)
              LOGI(K-1)=LOGI(K)
              INT(K-1)=INT(K)
            END DO
            PIXEL(NPTS)=0
            LOGI(NPTS)=0.0E0
            INT(NPTS)=0.0E0
            I=I-1
            NPTS=NPTS-1
          END IF
        END DO
C
C         UNITAIR - Area of each pixel (square arcsec).
C
        UNITAIR=(FLOAT(IRST*ICST)/1.0E6)*(PLTSCL**2)
C
C         Compute the area in square arcsec & the equivalent profile
C         in arcsec.
C
        DO I=1,NPTS
          AREA(I)=UNITAIR*FLOAT(PIXEL(I))
          REQ(I)=SQRT(AREA(I)/PI)
        END DO
C
C         Write to 2D array for storage as a Starlink image.
C
        DO I=1,MAXLEV
          KEEP(1,I)=0.0E0
          KEEP(2,I)=0.0E0
        END DO
        DO I=1,NPTS
          KEEP(1,I)=REQ(NPTS+1-I)
          KEEP(2,I)=LOGI(NPTS+1-I)
        END DO
        NLEVEL=NPTS
C
        END
