      SUBROUTINE JONES (THRESH1,THRESH2,IXEXT,IYEXT,IMAGE1,
     :                  IMAGE2,STAT)
C+
C     JONES.
C
C     Subroutine to smooth a 2 dimensional image, following
C     the procedure given by Jones et al (1967). The
C     procedure is intended for use on bright galaxies
C     and subjects each pixel to a degree of smoothing
C     appropriate to its intensity (dim pixels are 5
C     point smoothed, moderate pixels 3 pt. smoothed & 
C     bright pixels not smoothed at all).
C
C  Given;
C   THRESH1  (R)   Threshold below which pixels are 5 pt. smoothed.
C   THRESH2  (R)       "       "     "     "     "  3 " .    "    .
C   IXEXT    (I)   X extent of image.
C   IYEXT    (I)   Y   "    "    "  .
C   IMAGE1   (RA)  Original unsmoothed image.
C
C  Returned;
C   IMAGE2   (RA)  Smoothed image.
C   STAT     (I)   Return status.
C                  = 0 - Successful smooth.
C                  = 1 - Image too small to be smoothed (copied
C                        without smoothing).
C  
C  Subroutine called;
C   BOSMO.
C
C  Reference;
C   Jones et al (1967) Pub. Dept. Astr. Univ. of Texas at Austin,
C               Ser II, Vol.1, No.8.
C
C  Structure;
C   Set return status = 0.
C   If  (image more than 5 pixels in both dimensions)
C     Do for all pixels outside the boundary
C       if (pixel value .lt. thresh1)
C         5 pt. smooth.
C       else if (pixel value .ge. thresh1 & .lt. thresh2)
C         3 pt. smooth.
C       else if .ge. thresh2
C         no smoothing
C       end if
C     end do
C     Take care of the boundary pixels.
C   else
C     copy image without modifying
C     return status = 1
C   end if
C
C  A C Davenhall./ROE/                                 26/5/82.
C-
      REAL THRESH1,THRESH2
      INTEGER IXEXT,IYEXT,STAT
      REAL IMAGE1(IXEXT,IYEXT),IMAGE2(IXEXT,IYEXT)
C
      REAL GRID5(5,5),GRID3(3,3)
      REAL PIXVAL
C
C      Save the smoothing grids, ready for the next invocation
C      of the routine.
C
      SAVE GRID3,GRID5
C
C      Initialise smoothing grids.
C
      DATA GRID3/6.25E-2, 1.25E-1, 6.25E-2,
     :           1.25E-1, 2.5E-1,  1.25E-1,
     :           6.25E-2, 1.25E-1, 6.25E-2/
      DATA GRID5/
     :  3.90625E-3, 1.5625E-2, 2.34375E-2, 1.5625E-2, 3.90625E-3,
     :  1.5625E-2,  6.25E-2,   9.375E-2,   6.25E-2,   1.5625E-2,
     :  2.34375E-2, 9.375E-2,  1.40625E-1, 9.375E-2,  2.34375E-2,
     :  1.5625E-2,  6.25E-2,   9.375E-2,   6.25E-2,   1.5625E-2,
     :  3.90625E-3, 1.5625E-2, 2.34375E-2, 1.5625E-2, 3.90625E-3/
C
      STAT=0
C
C
C      Check that the image size exceeds 5 pixels in both
C      dimensions.
C
      IF (IXEXT.GT.5.AND.IYEXT.GT.5) THEN
C
C      Examine all pixels in the image to determine the
C      appropriate degree of smoothing, avoiding the
C      boundary.
C
        DO J=3,IYEXT-2
          DO I=3,IXEXT-2
            IF (IMAGE1(I,J).LT.THRESH1) THEN
C
C      Pixel is to be 5 pt. smoothed.
C
              PIXVAL=0.0E0
              DO JJ=1,5
                DO II=1,5
                  PIXVAL=PIXVAL+(IMAGE1(I-3+II,J-3+JJ)*GRID5(II,JJ))
                END DO
              END DO
              IMAGE2(I,J)=PIXVAL
            ELSE IF (IMAGE1(I,J).GE.THRESH1.AND.
     :               IMAGE1(I,J).LT.THRESH2) THEN
C
C     Pixel is to be 3 pt. smoothed.
C
              PIXVAL=0.0E0
              DO JJ=1,3
                DO II=1,3
                  PIXVAL=PIXVAL+(IMAGE1(I-2+II,J-2+JJ)*GRID3(II,JJ))
                END DO
              END DO     
              IMAGE2(I,J)=PIXVAL
            ELSE
C
C     Image not to be smoothed.
C
              IMAGE2(I,J)=IMAGE1(I,J)
            END IF
          END DO
        END DO
C
C     Take care of the boundary.
C
        DO J=1,2
          DO I=1,IXEXT
            CALL BOSMO (2,I,J,IXEXT,IYEXT,IMAGE1,IMAGE2(I,J))
          END DO
        END DO
        DO J=3,IYEXT-2
          DO I=1,2
            CALL BOSMO (2,I,J,IXEXT,IYEXT,IMAGE1,IMAGE2(I,J))
          END DO
          DO I=IXEXT-1,IXEXT
            CALL BOSMO (2,I,J,IXEXT,IYEXT,IMAGE1,IMAGE2(I,J))
          END DO
        END DO
        DO J=IYEXT-1,IYEXT
          DO I=1,IXEXT
            CALL BOSMO (2,I,J,IXEXT,IYEXT,IMAGE1,IMAGE2(I,J))
          END DO
        END DO
      ELSE
C
C      One or both of the array dimensions have less than 5
C      pixels.
C
C      Copy the original array into the new array unsmoothed.
C
        DO J=1,IYEXT
          DO I=1,IXEXT
            IMAGE2(I,J)=IMAGE1(I,J)
          END DO
        END DO
C
C      Set return status.
C
        STAT=1
      END IF
      END
