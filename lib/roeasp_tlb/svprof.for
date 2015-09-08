      SUBROUTINE SVPROF (AXIS,NAXIS,PTS,PIXSIZ)
C+
C     SVPROF.
C
C     Subroutine to save an extracted profile as a Stralink
C     image.
C
C  Given;
C   AXIS   (RA)  Array holding the four extracted axes.
C   NAXIS  (I)   Max. no. of permitted extracted points in each
C                profile.
C   PTS    (IA)  No. of points in each profile.
C   PIXSIZ (R)   Pixel size.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-         COPROF.
C   Interfaces:-  READI, OUTPICR, OUTPUT,
C                 CLEARIM.
C
C  A C Davenhall./ROE/                                     6/7/82.
C-
      INTEGER NAXIS
      INTEGER PTS(4)
      REAL AXIS(NAXIS,4)
      REAL PIXSIZ
C
      INTEGER ARM,NPTS,PROPTR,STATUS,STAT,STAT1
      INTEGER OUTAXS(2)
C
C
C    Prompt for the number of the axis to be saved.
C
      STAT=0
      CALL READI ('ARM',
     :  ' Number of the axis to be saved?',1,1,4,ARM,STAT)
C
C    Obtain a pointer to the Starlink image that is to be output.
C
      NPTS=PTS(ARM)
      OUTAXS(1)=NPTS
      OUTAXS(2)=2
      CALL OUTPICR ('PROFILE',
     :  ' Enter filename for the profile.',2,OUTAXS,PROPTR,STAT)
C
C    Copy across the profile if the pointer has been obtained Ok.
C
*
*
*     PRINT333,STAT
 333  FORMAT(2X,'RETURN STTATUS=',I5/)
*
*
*
      IF (STAT.EQ.0) THEN
        CALL COPROF (AXIS,NAXIS,NPTS,ARM,PIXSIZ,
     :               %VAL(PROPTR),STATUS)
        IF (STATUS.EQ.0) THEN
          CALL OUTPUT (' Profile Saved Ok.',STAT1)
        ELSE
          CALL OUTPUT (' ***ERROR Unable to copy profile.',STAT1)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain pointer to image correctly.',
     :     STAT1)
      END IF
C
C    Clear image.
C
      CALL CLEARIM ('PROFILE')
      END
