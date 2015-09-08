      SUBROUTINE ISOBOX (IMAGE,XEXT,YEXT,MAXLEV,NLEVEL,LEVELS,
     :                   MAXBOX,MAXPAR,NBOX,CORBOX,
     :                   PIXCT)
C+
C     ISOBOX.
C
C     Subroutine to compute the equivalent profile, in pixels,
C     of a galaxy image inside each of a series of predefined
C     square apertures. Each equivalent profile is computed
C     for a set range of intensity levels.
C
C  Given;
C   IMAGE  (RA) Image array to be analysed.
C   XEXT   (I)  X extent of image array.
C   YEXT   (I)  Y   "    "    "     "  .
C   MAXLEV (I)  Maximum permitted no. of levels in equivalent profile.
C   NLEVEL (I)  Actual no. of levels in the equivalent profile.
C   LEVELS (RA) Set of intesnity levels for the equivalent profile,
C               ordered in increasing intensity.
C   MAXBOX (I)  Maximum permitted no. of apertures.
C   MAXPAR (I)  Maximum permitted no. of parameters for each aperture,
C               must be at least 4.
C   NBOX   (I)  No. of apertures.
C   CORBOX (IA) Coordinates for each the apertures. These must be non-
C               overlapping and ordered in increasing aperture size.
C
C  Returned;
C   NLEVEL (I)  Number of isophotal levels actually detected in the
C               image.
C   PIXCT  (IA) Two dimensional array holding the number of pixels
C               counted for each isophotal level, for each aperture.
C               The elements are held; PIXCT(level,aperture).
C
C  Subroutines called;
C   E2D:-  FORBID.
C
C  Structure:-
C   Set all elements of pixel counting array PIXCT to zero.
C   Do for (all pixels inside the largest box)
C     If the pixel is above the detection threshold
C       Find the smallest aperture which includes the pixel.
C       Find the range of intensity levels which include the pixel.
C       For (all intensity levels and apertures which include the pixel)
C         Increment no. of pixels in this cell by +1.
C       end for
C     end if
C   end for
C
C  A C Davenhall./ROE/                                     24/8/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
C
      INTEGER MAXLEV,NLEVEL
      REAL LEVELS(MAXLEV)
C
      INTEGER MAXBOX,MAXPAR,NBOX
      INTEGER CORBOX(MAXBOX,MAXPAR), PIXCT(MAXLEV,MAXBOX)
C
      INTEGER JSTRT,ISTRT,JSTOP,ISTOP,MINBOX,MAXINT,NPTS
      REAL PIXTHR
      LOGICAL BOX
C
C
C    Initialise the pixel count array to zero.
C
      DO J=1,MAXBOX
        DO I=1,MAXLEV
          PIXCT(I,J)=0
        END DO
      END DO
C
C    Set limits for the portion of the image array to be examined
C    from the coords. of the largest box.
C
      ISTRT=CORBOX(NBOX,1)
      JSTRT=CORBOX(NBOX,2)
      ISTOP=CORBOX(NBOX,3)
      JSTOP=CORBOX(NBOX,4)
C
C    Set the detection threshold for examining a pixel from the
C    lowest entry in the table of isophotal levels.
C
      PIXTHR=LEVELS(1)
C
      NPTS=0
C
C    Examine all pixels inside the largest aperture.
C
      DO J=JSTRT,JSTOP
        DO I=ISTRT,ISTOP
C
C    Check whether the pixel is above the detection threshold.
C
          IF (IMAGE(I,J).GT.PIXTHR) THEN
C
C    Locate the smallest aperture within which the pixel lies.
C
            BOX=.FALSE.
            MINBOX=0
            DO WHILE (.NOT.BOX)
              MINBOX=MINBOX+1
              CALL FORBID (J,I,CORBOX,MINBOX,MAXBOX,BOX)
            END DO
C
C    Locate the intensity level below which the pixel lies.
C
            MAXINT=1
            DO WHILE (LEVELS(MAXINT).LE.IMAGE(I,J).AND.
     :                MAXINT.LT.MAXLEV)
              MAXINT=MAXINT+1
            END DO
            MAXINT=MAXINT-1
            NPTS=MAX(NPTS,MAXINT)
C
C    Increment the number of pixels for each intensity level
C    and each aperture in which this pixel is contained.
C
            DO JJ=MINBOX,MAXBOX
              DO II=1,MAXINT
                PIXCT(II,JJ)=PIXCT(II,JJ)+1
              END DO
            END DO
          END IF
C
        END DO
      END DO
C
C    Set the return no. of levels.
C
      NLEVEL=NPTS
C
      END
