      SUBROUTINE COPROF (AXIS,NAXIS,NPTS,
     :                   ARM,PIXSIZ,PROFIL,STATUS)
C+
C     COPROF.
C
C     Subourtine to copy a profile array from the array holding
C     all 4 profiles into an array holding a single profile.
C
C  Given;
C   AXIS   (RA) Array holding all four profiles.
C   NAXIS  (I)  Max. size of each profile in AXIS.
C   NPTS   (I)  No. of points in the input profile.
C   ARM    (I)  Selected axis (1-4).
C   PIXSIZ (R)  Pixel size.
C
C  Returned;
C   PROFIL (RA) Extracted profile.
C   STATUS (I)  Return status.
C               = 0 - Profile extracted Ok.
C               = 1 - Profile not extracted.
C
C  Subroutines called;
C   None.
C
C  Structure:-
C   If ARM lies between 1 & 4 then
C     copy intensities
C     compute positions
C     set return status
C   else
C     set return status
C   end if
C
C  A C Davenhall./ROE/                                   6/7/82.
C-
      INTEGER ARM,NAXIS,NPTS,STATUS
      REAL AXIS(NAXIS,4),PROFIL(NPTS,2)
      REAL PIXSIZ
C
      REAL PIXSIU
      INTEGER PTS
C
C
C    Force the pixel size to be sensible.
C
      IF (PIXSIZ.GE.1.0E-10) THEN
        PIXSIU=PIXSIZ
      ELSE
        PIXSIU=1.0E0
      END IF
C
C    Force the number of points not to exceed the array bound.
C
      PTS=MIN(NPTS,NAXIS)
C
C    Copy the profile if a valid arm has been selected.
C
      IF (ARM.GE.1.AND.ARM.LE.4) THEN
        DO I=1,PTS
          PROFIL(I,2)=AXIS(I,ARM)
          PROFIL(I,1)=PIXSIU*FLOAT(I-1)
        END DO
        STATUS=0
      ELSE
        STATUS=1
      END IF
      END
