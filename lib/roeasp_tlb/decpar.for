      SUBROUTINE DECPAR (PARSIZ,PARAM,STNDRD,XEXT,YEXT,
     :                   PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,
     :                   BETA,R,ATMABS,EFFIC,THRESH,GFLAG,
     :                   NGLOB,DIST,GABSOR,RCORE,RTIDAL)
C+
C     DECPAR.
C 
C     Subroutine to decode the Starfield Image Simulation
C     program parameter file.
C
C  Given;
C   PARSIZ (I)  Size of parameter file.
C   PARAM  (RA) Array holding parameter array.
C
C  Returned;
C   STNDRD (I)  Flag for standard or random image.
C   XEXT   (I)  X size of image frame to be generated.
C   YEXT   (I)  Y  "   "    "     "   "  "      "    .
C   PIXSIZ (R)  Pixel size (arcsec).
C   APERTR (R)  Aperture of telescope (cm).
C   EXPOSE (R)  Exposure time (sec).
C   SKYBRI (R)  Sky brightness (mag./sq.arcsec).
C   STMULT (R)  Star density/galactic latitude factor.
C   BETA   (R)  Moffat's parameter beta.
C   R      (R)  Moffat's parameter R (arcsec).
C   ATMABS (R)  Atmospheric absorption (magnitudes).
C   EFFIC  (R)  Combined telescope and detector efficiency.
C   THRESH (R)  Plotting threshold for finding chart (magnitudes).
C   GFLAG  (L)  Flag indicating whether a globular cluster is to
C               be included in the field.
C   NGLOB  (I)  No. of stars to be included in the globular cluster.
C   DIST   (R)  distance to the globular cluster (pc).
C   GABSOR (R)  Total absorption along the l-o-s to the cluster.
C   RCORE  (R)  King's core radius for the cluster (pc).
C   RTIDAL (R)  King's tidal radius for the cluster (pc).
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                  22/12/82.
C-
      INTEGER PARSIZ
      REAL PARAM(PARSIZ)
C
      INTEGER STNDRD,XEXT,YEXT,NGLOB
      REAL PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,BETA,R,ATMABS,
     :     EFFIC,THRESH,DIST,GABSOR,RCORE,RTIDAL
      LOGICAL GFLAG
C
C
      STNDRD=NINT(PARAM(1))
      XEXT=NINT(PARAM(2))
      YEXT=NINT(PARAM(3))
      PIXSIZ=PARAM(4)
      APERTR=PARAM(5)
      EXPOSE=PARAM(6)
      SKYBRI=PARAM(7)
      STMULT=PARAM(8)
      BETA=PARAM(9)
      R=PARAM(10)
      ATMABS=PARAM(11)
      EFFIC=PARAM(12)
      THRESH=PARAM(13)
C
      IF (PARAM(14).GT.1.0E0.AND.PARSIZ.GE.19) THEN
        GFLAG=.TRUE.
      ELSE
        GFLAG=.FALSE.
      END IF
C
      IF (GFLAG) THEN
        NGLOB=NINT(PARAM(15))
        DIST=PARAM(16)
        GABSOR=PARAM(17)
        RCORE=PARAM(18)
        RTIDAL=PARAM(19)
      END IF
C
      END
