      SUBROUTINE EQPC (IMAGE,XEXT,YEXT,ICST,IRST,
     :                 PLTSCL,MINLGI,DELOGI,NLEVEL,
     :                 N1,N2,PROFIL,STATUS)
C+
C     EQPC.
C
C     Subroutine to interactively define a set of apertures
C     for computing the equivalent profile from a galaxy
C     image using the sky correction technique due
C     to C. P. Blackman. The profile is then computed
C     using these apertures.
C
C  Reference;
C   C. P. Blackman, 1979, in "Photometry, kinematics and
C     dynamics of galaxies" ed. D. S. Evans, p135,
C     Dept. Astr. Univ. of Texas at Austin, Austin.
C
C  Given;
C   IMAGE  (RA) Image array.
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   ICST   (I)  Y (column) stepsize (micron).
C   IRST   (I)  X (row) stepsize (micron).
C   PLTSCL (R)  Plate scale, arcsec/mm.
C   MINLGI (R)  Minimum value of Log I above the sky at which the
C               equivalent profile is to start.
C   DELOGI (R)  Increment in Log I in the equivalent profile.
C   NLEVEL (I)  Required no of levels in the equivalent profile
C               look-up-table.
C   N1,N2  (I)  Size of array PROFIL, below.
C
C  Returned;
C   PROFIL (RA)  Computed equivalent profile.
C   STATUS (I)  return status. = max(input status, internal status)
C               internal status = 0 for successful completion, 
C               otherwise non-zero.
C
C  Subroutines called;
C   Interfaces:-  OUTPUT.
C   E2D:-         GBOX, EQPC1.
C
C  Structure:-
C   Attempt to define a set of apertures.
C   If apertures defined Ok.
C     compute the pixel area.
C     compute the equivalent profile.
C   end if
C   return status=max(input status,aperture box status)
C
C  A C Davenhall./ROE/                                      25/8/82.
C-
      INTEGER XEXT,YEXT,N1,N2
      REAL IMAGE(XEXT,YEXT), PROFIL(N1,N2)
      INTEGER ICST,IRST,NLEVEL,STATUS
      REAL PLTSCL,MINLGI,DELOGI
C
      INTEGER MAXLEV,MAXBOX,MAXPAR
      PARAMETER (MAXLEV=50)
      PARAMETER (MAXBOX=20)
      PARAMETER (MAXPAR=4)
C
      REAL NPIX(MAXLEV,MAXBOX), FPIX(MAXLEV,MAXBOX),
     :     WORKF(MAXBOX), WORKN(MAXBOX), WORKA(MAXBOX),
     :     LOGI(MAXLEV), INTEN(MAXLEV)
      INTEGER PIXCT(MAXLEV,MAXBOX), CORBOX(MAXBOX,MAXPAR)
C
      REAL DAREA
      INTEGER INSTAT,NBOX,IOSTAT
C
C
C    Attempt to define a set of apertures.
C
      INSTAT=0
      NBOX=10
      CALL GBOX (XEXT,YEXT,MAXBOX,MAXPAR,NBOX,CORBOX,INSTAT)
C
C    Proceed to compute the equivalent profile if obtained Ok.
C
      IF (INSTAT.EQ.0) THEN
C
C    Compute the area of each pixel.
C
        DAREA=(FLOAT(IRST*ICST)/1.0E6)*(PLTSCL**2)
C
C    Compute the sky corrected equivalent profile.
C
      CALL OUTPUT (' Please wait. Profile being computed now.',
     :               IOSTAT)
      CALL EQPC1 (XEXT,YEXT,IMAGE,MINLGI,DELOGI,MAXLEV,NLEVEL,
     :            MAXBOX,MAXPAR,NBOX,CORBOX,DAREA,N1,N2,
     :            PIXCT,NPIX,FPIX,WORKF,WORKN,WORKA,LOGI,INTEN,
     :            PROFIL)
      END IF
C
C    Set the return status.
C
      STATUS=MAX(STATUS,INSTAT)
      END
