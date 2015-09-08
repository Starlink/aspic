      SUBROUTINE CHARG7 (MINVAL,MAXVAL,ROTATE,PLANE,IOSTAT)
C+
C     Subroutine to determine from the user whether or not
C     subsequent graphics to the Args are to be rotated
C     through the overlay planes or are to be confined to a
C     single plane.
C
C  Given;
C   MINVAL  (I)  Min. permitted overlay plane (normaly 8).
C   MAXVAL  (I)  Max.     "        "      "   (   "   15).
C
C  Returned;
C   ROTATE  (L)  = .TRUE. - Graphics are to be rotated through planes.
C                = .FALSE. -   "     not "  "     "       "      "   .
C   PLANE   (I)  If graphics are not to be rotated through the planes
C                they will be wriiten to this plane. Otherwise unused.
C   IOSTAT  (I)  Return status from I/O.
C                = max (input status, internal status).
C
C  A C Davenhall./ROE/                                      28/7/82.
C-
      INTEGER MINVAL,MAXVAL,PLANE,IOSTAT
      LOGICAL ROTATE
C
      INTEGER STATUS
      CHARACTER REPLY*(1)
C
C    Determine whether or not the overlay planes are to be
C    rotated.
C
      STATUS=0
      CALL YESNO (
     : 'Are graphics to be rotated through the overlay planes?',
     : 'Y',REPLY,STATUS)
      IF (REPLY.EQ.'Y') THEN
        ROTATE=.TRUE.
      ELSE
        ROTATE=.FALSE.
      END IF
C
C    If the planes are not to be rotated determine which plane
C    is to be used.
C
      IF (.NOT.ROTATE) 
     :  CALL READI ('PLANE',
     :   ' Enter overlay plane to be used;',
     :   PLANE,MINVAL,MAXVAL,PLANE,STATUS)
C
C    Set the return status.
C
      IOSTAT=MAX(IOSTAT,STATUS)
      END
