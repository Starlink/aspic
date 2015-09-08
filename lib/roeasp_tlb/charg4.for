      SUBROUTINE CHARG4 (PLANE)
C+
C     CHARG4.
C
C     Subroutine to clear a specified overlay plane on the Args.
C     (removes all Fings graphics in this plane).
C
C  Given;
C   PLANE (I) Overlay plane to be cleared.
C
C  returned;
C   None.
C
C  Subroutines called;
C   ARGS:   SRINIT, ARGS_OVWRT, ARGS_OVCLR.
C
C  A C Davenhall./ROE/                                    4/6/82.
C  A C Davenhall./ROE/   {Modified}                      28/7/82.
C-
      INTEGER PLANE
C
      INTEGER IFAIL
C
C
C    Initialise the ARGS.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Check that the plane being requested is a valid plane.
C
      IF (PLANE.GE.8.AND.PLANE.LE.15) THEN
C
C    Enable the chosen plane.
C
        CALL ARGS_OVWRT (PLANE)
C
C    Clear the plane.
C
        CALL ARGS_OVCLR (PLANE)
      END IF
      END
