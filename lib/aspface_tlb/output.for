      SUBROUTINE OUTPUT (BUFFER,STATUS)
C+
C     OUTPUT.
C
C       Output a character string to the terminal via.
C       the Starlink environment.
C      
C  Given;
C   BUFFER   (C)  Character string to be output.
C
C  Returned;
C   STATUS   (I)  Return status.
C                 = 0 for a successful return, otherwise non-zero.
C
C  Subroutine called;
C   WRUSER.
C
C  A C Davenhall./ROE/                                     31/8/81.
C  A C Davenhall./ROE/   {Modified}                        27/7/82.
C-
      CHARACTER BUFFER*(*)
      INTEGER STATUS
C
      CALL WRUSER (BUFFER,STATUS)
      END
