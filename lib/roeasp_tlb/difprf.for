      SUBROUTINE DIFPRF (CL1PTS,C1RAD,C1INT,CL2PTS,C2RAD,C2INT,
     :                   CL1SKY,CL2SKY,IFLAG,DIFPTS,
     :                   C2IINT,DIFRAD,DIFMAG,OUTPTS)
C+
C     DIFPRF.
C
C     Subroutine to compute a colour difference profile along 
C     some axis of a nebular image, given arrays representing
C     the profile in two different colours. The input profiles
C     may be either in sky normalised intensity or
C     log10(intensity above sky).
C
C  Given;
C   CL1PTS (I)  No. of points in first colour.
C   C1RAD  (RA) Array of radii for first colour.
C   C1INT  (RA) Array of "intensities" for first colour.
C   CL2PTS (I)  No. pof points in second colour.
C   C2RAD  (RA) Array of radii for second colour.
C   C2INT  (RA) Array of "intensities" for second colour.
C   CL1SKY (R)  Sky brightness of first colour (mag./sq.arcsec).
C   CL2SKY (R)   "      "      "  second  "    ( " ./" .  "   ).
C   IFLAG  (L)  Flag showing whether "intensities" are sky normalised
C               intensities or log10(intensity above sky)
C               - true  - log10(intensity above sky).
C               - false - sky normalised intensity (1.0 NOT subtracted).
C   DIFPTS (I)  Expected no. of points in the output profile.
C   
C  Used;
C   C2IINT (RA) Array, size=DIFPTS.
C
C  Returned;
C   DIFRAD (RA) Array of radii for colour differences.
C   DIFMAG (RA) Array of colour differences (,ag./sq.arcsec).
C   OUTPTS (I)  No. of points in the colour profile.
C
C  Subroutines called;
C   E2D:-    LAGIN.
C
C  A C Davenhall./ROE/                                         2/4/83.
C-
      INTEGER CL1PTS,CL2PTS,DIFPTS,OUTPTS
      REAL C1RAD(CL1PTS), C1INT(CL1PTS),
     :     C2RAD(CL2PTS), C2INT(CL2PTS),
     :     C2IINT(DIFPTS), DIFRAD(DIFPTS), DIFMAG(DIFPTS)
      REAL CL1SKY,CL2SKY
      LOGICAL IFLAG
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
      REAL DUMMY
C
C
C    Interpolate a set of values for the second colour at the
C    radii of the first colour.
C
      CALL LAGIN (C2RAD,C2INT,C1RAD,C2IINT,DIFPTS,CL2PTS,CL2PTS,
     :            DIFPTS)
C
C    Proceed to generate colour difference profile.
C
      IF (IFLAG) THEN
C
C    Input profiles are in logI; compute the colour difference directly.
C
        DO I=1,DIFPTS
          DIFRAD(I)=C1RAD(I)
          DIFMAG(I)=CL1SKY-CL2SKY+(POGSON*(C2IINT(I)-C1INT(I)))
        END DO
        OUTPTS=DIFPTS
      ELSE
C
C    Input profiles are in sky normalised intensity;
C    compute the colour checking that the logarithm is defined.
C
        OUTPTS=0
        DO I=1,DIFPTS
          DUMMY=(C2IINT(I)-1.0E0)/(C1INT(I)-1.0E0)
          IF (DUMMY.GT.1.0E-5) THEN
            OUTPTS=OUTPTS+1
            DIFRAD(OUTPTS)=C1RAD(I)
            DIFMAG(OUTPTS)=CL1SKY-CL2SKY+(POGSON*ALOG10(DUMMY))
          END IF
        END DO
      END IF
C
      END
