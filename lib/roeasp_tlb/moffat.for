      SUBROUTINE MOFFAT (MAXSTR,NSTAR,XCORD,YCORD,APMAG,BETA,R,PIXSIZ,
     :    SKYTHR,SKYBRI,PHOTON,IXEXT,IYEXT,IMAGE,
     :    TOTPHOT,LIMPHOT,MAGAPP,FAISTR)
C+
C      MOFFAT.
C
C      Subroutine to add a set of Moffat profiles to a 2D
C     image array given a list of star images of known positions
C     and apparent magnitudes and appropriate parameters for
C     the Moffat profile. The profiles generated are terminated
C     when the intensity falls to a specified fraction of 
C     the sky background.
C
C     Reference; Moffat A. F. J. (1969) A&A Vol.3 pp.455-461.
C
C  Given;
C  MAXSTR  (I)  Max. no. of permitted stars (= size of arrays
C               XCORD, YCORD & APMAG, below).
C  NSTAR   (I)  No. of star images.
C  XCORD   (RA) X coords. of stars.
C  YCORD   (RA) Y   "   . "    "  .
C  APMAG   (RA) Apparent magnitudes of stars.
C  BETA    (R)  Moffat's quantity Beta.
C  R       (R)  Moffat's quantity R (arcseconds).
C  PIXSIZ  (R)  Size of a (square) pixel side (arcsec).
C  SKYTHR  (R)  The fraction of the night sky to which the profile
C               is to be evaluated.
C  SKYBRI  (R)  Sky brightness (magnitudes/sq. arcsec).
C  PHOTON  (R)  No. of photons corresponding to a zero magnitude
C               star of this exposure time, telescope aperture,
C               passband etc.
C  IXEXT   (I)  X size of image array.
C  IYEXT   (I)  Y  "   "    "     "  .
C  IMAGE   (RA) Image array.
C
C  Returned;
C  IMAGE   (RA) Image array modified by adding profiles.
C  TOTPHOT (RA) Total intensity of each star image.
C  LIMPHOT (RA) No. of photons falling inside the truncation boundary
C               for each star image.
C  MAGAPP  (RA) Magnitude applied to the image frame for each star image.
C  FAISTR  (I)  No. of stars not applied to the image frame because they
C               fall completely below the truncation boundary.
C
C  Subroutines called;  None.
C
C  Structure:-
C   Force no. of stars to be less than array size.
C   Compute intensity corresponding to sky.
C   Do for all stars in catalogue.
C     compute total luminosity corresponding to apparent magnitude.
C     compute radius corresponding to fraction of sky being 
C       integrated to.
C     generate box around star corresponding to this radius.
C     compute intensity for all pts. inside this box.
C   end do.
C
C   A C Davenhall./ROE/                                  3/3/82.
C   A C Davenhall./ROE/  {Modified}                      22/4/83.
C   based on a routine by R. S. Stobie./ROE/
C-
      INTEGER MAXSTR,NSTAR,IXEXT,IYEXT,FAISTR
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR)
      REAL IMAGE(IXEXT,IYEXT)
      REAL TOTPHOT(MAXSTR),LIMPHOT(MAXSTR),MAGAPP(MAXSTR)
      REAL SKYTHR,SKYBRI,PHOTON,BETA,R,PIXSIZ
C
C
      INTEGER ISTAR,X1,X2,Y1,Y2
      REAL POGSON,SKYPHO,ITOT,RBOUND,PI,RADIUS,PIXINT,FRAC,IO
      REAL RADPIX,SUMPIX
      PARAMETER (POGSON=2.5E0)
      PARAMETER (PI=3.14159265E0)
C
C      Force the no. of stars to be less than the array size.
C
      ISTAR=MIN(NSTAR,MAXSTR)
C
C      Compute the no. of photons corresponding to the sky
C      intensity.
C
      SKYPHO=PHOTON*(1.0E1**(-SKYBRI/POGSON))
C
C      Compute profiles for all the stars.
C
      FAISTR=0
C
      DO I=1,ISTAR
C
C      Compute the total intensity corresponding to the given
C      apparent magnitude.
C
        ITOT=PHOTON*(1.0E1**(-APMAG(I)/POGSON))
        TOTPHOT(I)=ITOT
C
C      Compute the central intensity and proceed if it is above the
C      threshold.
C
        IO=ITOT*(BETA-1.0E0)/(PI*R*R)
        IF (IO.GT.SKYTHR*SKYPHO) THEN
C
C      Compute radius corresponding to fraction of sky to which
C      the profile is to be evaluated.
C
          RBOUND=R*SQRT(((IO/(SKYTHR*SKYPHO))**(1.0E0/BETA))-1.0E0)
C
C      Convert RBOUND from arcsec. to pixels.
C
      RBOUND=RBOUND/PIXSIZ
C
C      Generate a box around each image in which the profile is
C      to be evaluated, forcing it to lie inside the array.
C
          X1=NINT(XCORD(I)-RBOUND)
          X1=MAX(1,X1)
          X2=NINT(XCORD(I)+RBOUND)
          X2=MIN(X2,IXEXT)
          Y1=NINT(YCORD(I)-RBOUND)
          Y1=MAX(1,Y1)
          Y2=NINT(YCORD(I)+RBOUND)
          Y2=MIN(Y2,IYEXT)
C
C      Generate star profile.
C
          SUMPIX=0.0E0
          DO JJ=Y1,Y2
            DO II=X1,X2
              RADPIX=SQRT(((XCORD(I)-FLOAT(II))**2)+
     :                      ((YCORD(I)-FLOAT(JJ))**2))
              RADIUS=RADPIX*PIXSIZ
              PIXINT=IO/((1.0E0+((RADIUS/R)**2))**BETA)
              IMAGE(II,JJ)=IMAGE(II,JJ)+PIXINT
              SUMPIX=SUMPIX+PIXINT
            END DO
          END DO
C
C      Compute the number of photons falling inside the boundary
C      and the consequent apparent magnitude appied to the image.
C
          LIMPHOT(I)=SUMPIX
          MAGAPP(I)=APMAG(I)-(POGSON*ALOG10(SUMPIX/ITOT))
        ELSE
C
C      Star is sufficiently faint that it nowhere is brighter than the
C      truncation threshold.
C
          LIMPHOT(I)=0.0E0
          MAGAPP(I)=0.0E0
          FAISTR=FAISTR+1
        END IF
      END DO
      END
