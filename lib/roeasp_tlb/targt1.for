      SUBROUTINE TARGT1 (CATPAR,STARS,CATALG,THRESH,PLANE,COLOUR,
     :                   STATUS)
C+
C     TARGT1.
C
C     Subroutine to draw a "gun-sight" marker around the positions
C     of an array of target objects. A corresponding image frame
C     is assumed to be displayed on the Args and the Args data-base
C     is used to locate the markers corretly over the objects.
C     Only objects brighter than a specified threshold are plotted.
C
C  Given;
C   CATPAR (I)  No. of parameters for each object in the catalogue,
C               must be at least 3.
C   STARS  (I)  No. of stars in the catalogue.
C   CATALG (RA) Star catalogue.
C   THRESH (R)  Plotting threshold (magnitudes).
C   PLANE  (I)  Args overlay plane in which the display is to be
C               plotted.
C   COLOUR (C)  Colour of the display.
C
C  Returned;
C   STATUS (I)  Max(input status, internal status)
C               internal status = 0 - successfull return.
C                               = 1 - Error plotting more than 10% of
C                                     the objects in the catalogue.
C                               = 2 - No image frame in the Args
C                                     database.
C
C  Subroutines called;
C   Args database:-  ARGS_NUMIM, ARGS_PTOA.
C   Args:-           SRINIT, ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN,
C                    ARGS_OVCLR.
C   Fings:-          ARGS, DEVSPE, VUPORT, WINDOL, CLICTL, CHAINT,
C                    DEVEND.
C   SIMIM:-          GNSGHT.
C
C  Structure:-
C   Obtain the no. of the last image in the database.
C   if (num.ge.1) then
C     Setup for overlay graphics
C     Set up for using Fings.
C     Do for all stars
C       If star brighter than threshold
C         Attempt to convert pixel --> args coords.
C         If convert Ok
C           plot gunsight
C         else
C           increment counter for no. of bad points
C         end if
C       end if
C     end do
C     If more than 10% of stars were not converted correctly
C       set return status
C     end if
C   else
C     set return status.
C   end if
C   stop plotting.
C
C  A C Davenhall./ROE/                                   3/1/83.
C-
      INTEGER CATPAR,STARS,PLANE,STATUS
      REAL CATALG(CATPAR,STARS)
      REAL THRESH
      CHARACTER COLOUR*(*)
C
      REAL CPOS,YPOS,APMAG
      INTEGER BADCNT,INTSTT,ID,CONSTT,IFAIL
      INTEGER XPIX,YPIX,XARG,YARG
      REAL RADIUS
      PARAMETER (RADIUS=2.0E1)
C
C
      INTSTT=0
C
C    Initialise the Args.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Obtain the number of the last image in the Args database,
C    and proceed if this is greater than 0.
C
      CALL ARGS_NUMIM (ID)
      IF (ID.GE.1) THEN
C
C    Setup for overlay graphics.
C
        CALL ARGS_PICWRT
        CALL ARGS_OVWRT (PLANE)
        CALL ARGS_OVCOL (PLANE,COLOUR)
        CALL ARGS_OVGEN ('W')
C
C    Remove any previous graphics from this plane.
C
        CALL ARGS_OVCLR (PLANE)
C
C    Setup for Fings.
C
        CALL ARGS
        CALL DEVSPE (9600)
        CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
        CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Enable clipping.
C
        CALL CLICTL (1)
C
C    Plot the stars in the catalogue that are brighter than the
C    threshold.
C
        BADCNT=0
        DO I=1,STARS
          XPIX=NINT(CATALG(1,I))
          YPIX=NINT(CATALG(2,I))
          APMAG=CATALG(3,I)
          IF (APMAG.LE.THRESH) THEN
C
C    Attempt to convert from pixel to Args coord.
C
            CALL ARGS_PTOA (ID,XPIX,YPIX,XARG,YARG,CONSTT)
C
C    Plot a gunsight centred on the position if the conversion was Ok.
C
            IF (CONSTT.EQ.0) THEN
              XPOS=FLOAT(XARG)
              YPOS=FLOAT(YARG)
              CALL GNSGHT (XPOS,YPOS,RADIUS)
              CALL CHAINT (I,3)
            ELSE
              BADCNT=BADCNT+1
            END IF
          END IF
        END DO
C
C    Set the return status if there was an error in plotting more
C    than 10% of the stars in the catalogue.
C
        IF (BADCNT.GT.0) THEN
          IF (FLOAT(BADCNT)/FLOAT(STARS).GT.1.0E-1) INTSTT=1
        END IF
      ELSE
C
C    No image was located in the Args database.
C
        INTSTT=2
      END IF
C
C    Terminate plotting.
C
      CALL CLICTL (0)
      CALL DEVEND
C
C    Set the return status.
C
      STATUS=MAX(STATUS,INTSTT)
      END
