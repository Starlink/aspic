      SUBROUTINE PRFLOG
C+
C     PRFLOG.
C
C     Convert a profile held as intensity converted to intensity
C     relative to a sky of 1.0, with the sky not subtracted,
C     to log intensity above the sky.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPICR, OUTPUT, CLEARIM.
C   E2D:-         KOPLOG.
C
C  Structure:-
C   Attempt to obtain pointer to the original profile.
C      "    "    "       "    "    " new         "  .
C   If the pointer obtained Ok.
C     Copy and convert the profile.
C     Print a message.
C   else
C     print message saying can't get files.
C   end if
C   tidy up the images.
C
C  A C Davenhall./ROE/                                   6/8/82.
C-
      INTEGER OLDPTR,NEWPTR
      INTEGER IOSTAT,IMSTAT
      INTEGER AXISIZ(2)
      INTEGER NPTS
C
C    When converting the profile it is necessary to fudge
C    points close to or below the sky because the logarithm
C    becomes indefinately negative. The following number
C    represents the minimum Log I permitted.
C
      REAL MINLGI
      PARAMETER (MINLGI=-6.0E0)
C
C
C    Attempt to obtain a pointer to the old profile.
C
      IMSTAT=0
      CALL INPICR ('OLDPRF',
     : ' Enter filename for the profile to be converted;',
     :   2,AXISIZ,OLDPTR,IMSTAT)
C
C    Attempt to obtain a pointer to the new profile.
C
      CALL OUTPICR ('NEWPRF',' Enter filename for the new profile;',
     :              2,AXISIZ,NEWPTR,IMSTAT)
C
C    Proceed to copy and convert the profile if the pointers have
C    been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        NPTS=AXISIZ(1)
        CALL KOPLOG (MINLGI,NPTS,%VAL(OLDPTR),%VAL(NEWPTR))
        CALL OUTPUT (' New file generated Ok.',IOSTAT)
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain files successfully.',IOSTAT)
      END IF
C
C    Tidy up the images.
C
      CALL CLEARIM ('OLDPRF')
      CALL CLEARIM ('NEWPRF')
      END
