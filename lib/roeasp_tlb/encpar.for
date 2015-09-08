      SUBROUTINE ENCPAR (STNDRD,XEXT,YEXT,
     :                   PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,
     :                   BETA,R,ATMABS,EFFIC,THRESH,GFLAG,
     :                   NGLOB,DIST,GABSOR,RCORE,RTIDAL,
     :                   PARSIZ,PARAM)
C+
C     ENCPAR.
C 
C     Subroutine to encode the selected values into the Starfield
C     Image Simulation program parameter array.
C
C  Given;
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
C   PARSIZ (I)  Size of parameter array.
C
C  Returned;
C   PARAM  (RA) Parameter array.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                  30/12/82.
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
      PARAM(1)=FLOAT(STNDRD)
      PARAM(2)=FLOAT(XEXT)
      PARAM(3)=FLOAT(YEXT)
      PARAM(4)=PIXSIZ
      PARAM(5)=APERTR
      PARAM(6)=EXPOSE
      PARAM(7)=SKYBRI
      PARAM(8)=STMULT
      PARAM(9)=BETA
      PARAM(10)=R
      PARAM(11)=ATMABS
      PARAM(12)=EFFIC
      PARAM(13)=THRESH
      IF (GFLAG.AND.PARSIZ.GE.19) THEN
        PARAM(14)=2.0E0
        PARAM(15)=FLOAT(NGLOB)
        PARAM(16)=DIST
        PARAM(17)=GABSOR
        PARAM(18)=RCORE
        PARAM(19)=RTIDAL
      ELSE
        PARAM(14)=0.0E0
      END IF
      END
