      SUBROUTINE EQPC1 (XEXT,YEXT,IMAGE,MINLGI,DELOGI,MAXLEV,
     :                  NLEVEL,MAXBOX,MAXPAR,NBOX,CORBOX,
     :                  DAREA,N1,N2,PIXCT,NPIX,FPIX,
     :                  WORKF,WORKN,WORKA,LOGI,INTEN,
     :                  PROFIL)
C+
C     EQPC1.
C
C     Compute the equivalent profile from an image, using the
C     technique for correcting for contamination by sky
C     noise due to C. P. Blackman.
C
C  Reference;
C   C. P. Blackman, 1979, in "Photometry, kinematics and
C     dynamics of galaxies" ed. D. S. Evans, p135,
C     Dept. Astr. Univ. of Texas at Austin, Austin.
C
C  Given;
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Image array.
C   MINLGI (R)  Minimum Log I in the equivalent profile.
C   DELOGI (R)  Increment in Log I in the equivalent profile.
C   MAXLEV (I)  Max. permitted no. of levels in the profile.
C   NLEVEL (I)  Actual no. of levels in the equivalent profile.
C   MAXBOX (I)  Max. permitted no. of apertures.
C   MAXPAR (I)  Max. permitted no. of parameters for each aperture
C               (must be at least 4).
C   NBOX   (I)  No. of apertures requested.
C   CORBOX (IA) Coords. of apertures.
C   DAREA  (R)  Area of each pixel.
C   N1,N2  (I)  Size of output array PROFIL below.
C
C  Used;
C   PIXCT  (IA) Work array.
C   NPIX   (RA)  "     "  .
C   FPIX   (RA)  "     "  .
C   WORKF  (RA)  "     "  .
C   WORKN  (RA)  "     "  .
C   WORKA  (RA)  "     "  .
C   LOGI   (RA)  "     "  .
C   INTEN  (RA)  "     "  .
C
C  Returned;
C   NLEVEL (I)  No. of levels found in the equivalent profile.
C   PROFIL (RA) Computed equivalent profile.
C
C  Subroutines called;
C   E2D:-  LEVGEN, ISOBOX, APERNF, FNPLOT.
C
C  Structure:-
C   Generate a set of isophotal levels to compare pixels against.
C   Extract the equivalent profile from the image.
C   Compute the mean N and F for each smallest aperture.
C   Plot mean N and F against smallest aperture.
C   Determine min. aperture.
C   Copy to output arrays, converting pixels to R*.
C
C  A C Davenhall./ROE/                                25/8/82.
C-
      INTEGER XEXT,YEXT,MAXLEV,MAXBOX,MAXPAR,NBOX,N1,N2
     :        NLEVEL
      REAL IMAGE(XEXT,YEXT)
      REAL NPIX(MAXLEV,MAXBOX),FPIX(MAXLEV,MAXBOX),
     :     WORKF(MAXBOX),WORKN(MAXBOX),
     :     WORKA(MAXBOX),LOGI(MAXLEV),INTEN(MAXLEV),
     :     PROFIL(N1,N2)
      INTEGER PIXCT(MAXLEV,MAXBOX),CORBOX(MAXBOX,MAXPAR)
      REAL MINLGI,DELOGI,DAREA
C
      INTEGER MINBOX,LEVEL,NH
C
      REAL PI
      PARAMETER (PI=3.1415927E0)
C
      REAL DUMMY
C
C
C    Generate a set of intensity levels equally spaced
C    in Log I above the sky.
C
      CALL LEVGEN (MAXLEV,MINLGI,DELOGI,NLEVEL,LOGI,
     :             INTEN)
C
C    Extract the equivalent profile, in pixels, for each
C    isophotal level and aperture.
C
      CALL ISOBOX (IMAGE,XEXT,YEXT,MAXLEV,NLEVEL,INTEN,
     :             MAXBOX,MAXPAR,NBOX,CORBOX,PIXCT)
C
C    Compute the mean N and F, varying the minimum aperture
C    used.
C
      CALL APERNF (MAXLEV,MAXBOX,PIXCT,NLEVEL,NBOX,MAXPAR,
     :             CORBOX,NPIX,FPIX)
C
C    Plot the mean values of N and F against the smallest
C    aperture used in their computation.
C    Allow the user to select the minimum aperture to be
C    used in defining the final equivalent profile.
C
      LEVEL=1
      CALL FNPLOT (MAXLEV,MAXBOX,NBOX,NPIX,FPIX,LEVEL,
     :             WORKF,WORKN,WORKA,MINBOX)
C
C    Erase any previous profile in the output array.
C
      DO I=1,N2
        PROFIL(1,I)=0.0E0
        PROFIL(2,I)=0.0E0
      END DO
C
C    Copy the equivalent profile, in pixels, corresponding
C    to the selected smallest aperture into the output
C    array, replacing any erroneous negative values with half a pixel.
C
      DO I=1,NLEVEL
        PROFIL(1,I)=NPIX(I,MINBOX)
        PROFIL(1,I)=MAX(PROFIL(1,I),5.0E-1)
      END DO
C
C    Convert the equivalent profile in pixels into
C    radial units (probably arcsec, but the actaul
C    units depend on the units of DAREA the 
C    area of each pixel). Also copy the set of generated
C    Log I values into the output array.
C
      DO I=1,NLEVEL
        PROFIL(1,I)=SQRT(PROFIL(1,I)*DAREA/PI)
        PROFIL(2,I)=LOGI(I)
      END DO
C
C    Due to the effects of digitisation some of the isophotal
C    levels may have the same area. Compress the profile
C    to remove such levels (they are physically unrealistic).
C
      COUNT=0
      DO WHILE (COUNT.LT.NLEVEL)
        COUNT=COUNT+1
        IF (ABS(PROFIL(1,COUNT)-PROFIL(1,COUNT+1)).LT.1.0E-3) THEN
          DO I=COUNT,NLEVEL-1
            PROFIL(1,I)=PROFIL(1,I+1)
            PROFIL(2,I)=PROFIL(2,I+1)
          END DO
          PROFIL(1,NLEVEL)=0.0E0
          PROFIL(2,NLEVEL)=0.0E0
          NLEVEL=NLEVEL-1
          COUNT=COUNT-1
        END IF
      END DO
C
C    The profile is currently held upside down in the
C    array, ie. subsequent routines all expect it
C    the other way up. Invert it.
C
      NH=NLEVEL/2
      DO J=1,2
        DO I=1,NH
          DUMMY=PROFIL(J,I)
          PROFIL(J,I)=PROFIL(J,NLEVEL+1-I)
          PROFIL(J,NLEVEL+1-I)=DUMMY
        END DO
      END DO
      END
