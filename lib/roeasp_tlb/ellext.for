      SUBROUTINE ELLEXT (XEXT,YEXT,IMAGE,XBASE,YBASE,XTOP,YTOP,
     :                   MAXLEV,MAXPTS,MAXPAR,NLEVEL,LOGI,INTEN,
     :                   ELLARR,LEVPT,STATUS)
C+
C     ELLEXT.
C
C     subroutine to extract the positions of all the points
C     lying on each of a set of isophotal levels in a 
C     region of an array.
C
C  Given;
C   XEXT   (I) X size of image array.
C   YEXT   (I)  Y  "   "    "     "  .
C   IMAGE  (RA) Image array.
C   XBASE  (I)  Lower X coord. of region to be examined.
C   YBASE  (I)    "   Y   "  . "    "    "  "     "    .
C   XTOP   (I)  Upper X   "  . "    "    "  "     "    .
C   YTOP   (I)    "   Y   "  . "    "    "  "     "    .
C   MAXLEV (I)  Max permitted no. of isophotal levels
C               (= size of arrays INTEN, LEVPT and ELLAR, below).
C   MAXPTS (I)  Max. permitted no. of pts. in a given contour
C               (= size of array ELLARR below).
C   MAXPAR (I)  Max. permitted no. of parameters recorded for
C               each pixel (must be .ge.2)
C   NLEVEL (I)  Actual no. of isophotal intensities.
C   LOGI   (RA) Array of Log I values.
C   INTEN  (RA) Array of isophotal intensities.
C
C  Returned;
C   NLEVEL (I)  No. of contours extracted from the array.
C   LOGI   (RA) Array of log I values modified by removing
C               sparsely populated contours.
C   ELLARR (RA) Array holding positions extracted for each level.
C   LEVPT  (IA) Array holding the number of points in each level.
C   STATUS (I)  return status. 0 for succesful completion, 1
C               any of the levels have more points than can be a
C               accommodated (in this case the level will be
C               incomplete.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                13/9/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
C
      INTEGER XBASE,YBASE,XTOP,YTOP
C
      INTEGER MAXLEV,MAXPTS,NLEVEL
      REAL LOGI(MAXLEV),INTEN(MAXPAR,MAXLEV),
     :     ELLARR(MAXPAR,MAXPTS,MAXLEV)
      INTEGER LEVPT(MAXLEV)
C
      INTEGER RLEVEL,PTS,COUNT
      REAL PIXTHR,PIXVAL
      LOGICAL FULFLG
C
C
C
C    Initialise the output array to contain termination markers.
C
      DO K=1,MAXLEV
        DO J=1,MAXPTS
          DO I=1,MAXPAR
            ELLARR(I,J,K)=-9.99E3
          END DO
        END DO
        LEVPT(K)=0
      END DO
C
      PIXTHR=INTEN(1,1)
      FULFLG=.FALSE.
      RLEVEL=0
C
C    Examine each pixel in the selected region and add it
C    to the appropriate isophote.
C
      DO J=YBASE,YTOP
        DO I=XBASE,XTOP
          PIXVAL=IMAGE(I,J)
C
C    Only examine pixels above the faintest level.
C
          IF (PIXVAL.GE.PIXTHR) THEN
C
C    Examine each band to check whether the pixel lies within it.
C
            DO K=1,NLEVEL
              IF (PIXVAL.GE.INTEN(1,K).AND.PIXVAL.LE.INTEN(2,K)) THEN
C
C    Check whether this contour is full and if not add the current
C    point to the list for it. If not set the full contour flag.
C
                PTS=LEVPT(K)+1
                IF (PTS.LE.MAXPTS) THEN
                  ELLARR(1,PTS,K)=FLOAT(I)
                  ELLARR(2,PTS,K)=FLOAT(J)
                  LEVPT(K)=PTS
                ELSE
                  FULFLG=.TRUE.
                END IF
C
C    Reset the counter for the no. of contours with points
C    in them, if necessary.
C
                RLEVEL=MAX(K,RLEVEL)
              END IF
            END DO
          END IF
        END DO
      END DO
C
C    Throw away any contours containing less than 20 points.
C
      COUNT=1
      DO WHILE (COUNT.LE.RLEVEL)
        IF (LEVPT(COUNT).LT.20) THEN
          DO K=COUNT+1,RLEVEL
            DO J=1,MAXPTS
              DO I=1,MAXPAR
                ELLARR(I,J,K-1)=ELLARR(I,J,K)
              END DO
            END DO
            LOGI(K-1)=LOGI(K)
          END DO
          RLEVEL=RLEVEL-1
        END IF
        COUNT=COUNT+1
      END DO
C
C    Reset the no. of contours to the number found.
C
      NLEVEL=RLEVEL
C
C    Set the return status.
C
      IF (.NOT.FULFLG) THEN
        STATUS=0
      ELSE
        STATUS=1
      END IF
      END
