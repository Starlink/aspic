      SUBROUTINE SELDE1 (DEVICE,CLEAR,PLANE,COLOUR)
C+
C     SELDE1.
C
C       Subroutine to select a graphics device as a target
C       (victim) for Fings graphics under programmer
C       control & to optionally clear the screen.
C
C       Given;
C       DEVICE - Device to be selected (integer).
C              = 1 - T4010 (default).
C              = 2 - ARGS.
C              = 3 - Versatec.
C              = 4 - Cifer.
C       CLEAR - Clear the screen? (Boolean).
C             = .TRUE. - clear screen.
C             = .FALSE. - Do not clear screen.
C       PLANE - Args overlay plane into which the graphics are
C               to be written if the Args is selected.
C       COLOUR - Colour of the graphics if the Args is selected.
C
C       Subroutines called;
C       FINCOL.
C       Fings; T4010,ARGS,ARGS_PICWRT,DEVSPE,PICCLE,SRCOLS,SRINIT.
C              ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL, ARGS_OVCLR,
C              ARGS_OVGEN.
C
C       A C Davenhall. /ROE/                        27/11/81.
C       A C Davenhall. /ROE/  (modified, from GDEVI1) 6/7/82.
C-
      COMMON /SELARG/ ARG,CIFER
C
      COMMON /VARSYM/ VSYM,SYMRAD,DOTTY,PLTSYM,RATIO
      LOGICAL  VYSM,DOTTY
      REAL     SYMRAD,RATIO
      INTEGER  PLTSYM
C
      INTEGER DEVICE,PLANE
      LOGICAL CLEAR
      INTEGER DEV,PLANEU
      INTEGER COL_L(3),COL_B(3)
      LOGICAL ARG,CIFER
      CHARACTER COLOUR*1
      ARG=.FALSE.
      CIFER=.FALSE.
C
C       Terminate any previous plotting.
C
      CALL STOPLOT
C
C       Check that the value for device selection is in range &
C       if not default to tektronics.
C
      IF (DEVICE.LT.1.OR.DEVICE.GT.3) THEN
        DEV=1
      ELSE
        DEV=DEVICE
      END IF
C
C       T4010.
C
      IF (DEV.EQ.1) THEN
        CALL T4010
        CALL VUPORT (0.0,1.3,0.0,1.0)
      END IF
C
C       Cifer.
C
      IF (DEV.EQ.4) THEN
        CALL T4010
        CALL VUPORT (0.0,1.3,0.0,1.0)
        CALL GRAF
        CIFER=.TRUE.
      END IF
C
C       Args.
C
      IF (DEV.EQ.2) THEN
        CALL SRINIT (0,.FALSE.,IFAIL)
        CALL ARGS
        CALL ARGS_PICWRT
        CALL DEVSPE (9600)
        CALL VUPORT (0.0,1.0,0.0,1.0)
        ARG=.TRUE.
C
C    Force the overlay plane to be within the permitted range.
C
        IF (PLANE.GE.8.AND.PLANE.LE.15) THEN
          PLANEU=PLANE
        ELSE
          PLANEU=8
        END IF
C
C    Setup for plotting to the selected plane.
C
        CALL ARGS_OVWRT (PLANEU)
        CALL ARGS_OVCLR (PLANEU)
        CALL ARGS_OVCOL (PLANEU,COLOUR)
        CALL ARGS_OVGEN ('W')
      END IF
C
C       Versatec.
C
      IF (DEV.EQ.3) THEN
        CALL VERSA
        CALL VUPORT (0.0,1.0,0.0,1.0)
      END IF
C
C       Clear the screen if required.
C
      IF (CLEAR) CALL PICCLE
C
C      Set up the variables associated with automatically
C      scaling the plotting symbol size.
C      The default is the symbols are not
C      to be scaled.
C
      VSYM=.FALSE.
      DOTTY=.FALSE.
      SYMRAD=0.0E0
      RATIO=1.0E0
      PLTSYM=1
      END
