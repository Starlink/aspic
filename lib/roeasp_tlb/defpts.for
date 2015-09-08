      SUBROUTINE DEFPTS (CL1PTS,C1RAD,CL2PTS,C2RAD,DIFPTS)
C+
C     DEFPTS.
C
C     Subroutine to define the number of points in a colour difference
C     array given 2 arrays holding the radii of the points from which
C     the colour differences are to be computed.
C
C  Given;
C   CL1PTS (I)  No. of points in the 1st profile.
C   C1RAD  (RA) Radii of points in 1st profile.
C   CL2PTS (I)  No. of points in the 2nd profile.
C   C2RAD  (RA) Radii of points in 2nd profile.
C
C  Returned;
C   DIFPTS (I)  No. of points to be included in colour difference
C               profile.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                            2/4/83.
C-
      INTEGER CL1PTS,CL2PTS,DIFPTS
      REAL C1RAD(CL1PTS), C2RAD(CL2PTS)
C
      IF (C1RAD(CL1PTS).LE.C2RAD(CL2PTS)) THEN
        DIFPTS=CL1PTS
      ELSE
        DIFPTS=0
        DO I=1,CL1PTS
          IF (C1RAD(I).LE.C2RAD(CL2PTS)) DIFPTS=I
        END DO
      END IF
C
      END
