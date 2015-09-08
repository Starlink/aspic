      SUBROUTINE FORBID (J,I,AVOID,NAVOID,MAXAVD,ZONE)
C+
C     FORBID.
C
C     Subroutine to check if the position of a pixel
C     lies in an exclusion zone.
C
C  Given;
C   J      (I)  Y coord. of pixel.
C   I      (I)  X   "  . "    "  .
C   AVOID  (IA) Array containing coords. of exclusion zones.
C   NAVOID (I)  No. of exclusion zones.
C   MAXAVD (I)  Max. permitted no. of exclusion zones
C               ( = size of one dimension of array AVOID).
C
C  Returned;
C   ZONE   (L)  = .TRUE.  - Pixel is inside an exclusion zone.
C               = .FALSE. - Pixel is not in an exclusion
C                           zone.
C
C  A C Davenhall./ROE/                             Sept. 81.
C  A C Davenhall./ROE/      {Modified}             23/8/82.
C-
      INTEGER J,I,NAVOID,MAXAVD
      INTEGER AVOID(MAXAVD,4)
      LOGICAL ZONE
C
C    Assume point is outside all zones.
C
      ZONE=.FALSE.
C
C    Check each zone to see whether pixel lies within it.
C
      DO K=1,NAVOID
        IF (I.GE.AVOID(K,1).AND.      
     :      I.LT.AVOID(K,3).AND.
     :      J.GE.AVOID(K,2).AND.
     :      J.LT.AVOID(K,4)) ZONE=.TRUE.
      END DO
      END
