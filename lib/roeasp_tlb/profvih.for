      SUBROUTINE PROFVIH
C+
C     PROFVIH.
C
C     Subroutine to list commands available for the
C     visual comparison an observed and computed
C     profile (PROFVIS).
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutine called;
C   Interfaces:- OUTPUT.
C
C  A C Davenhall./ROE/                             2/8/82.
C-
      INTEGER STATUS
      CALL OUTPUT ('   ',STATUS)
      CALL OUTPUT (' Commands available:- ',STATUS)
      CALL OUTPUT ('   ',STATUS)
      CALL OUTPUT (' DEVICE  (D)  Select graphics device.',STATUS)
      CALL OUTPUT (' COMPARE (C)  Compare the calculated and ',
     :      STATUS)
      CALL OUTPUT ('              observed profiles.',STATUS)
      CALL OUTPUT (' RESID   (R)  Plot the residuals between the ',
     :      STATUS)
      CALL OUTPUT ('              calculated and observed profiles.',
     :      STATUS)
      CALL OUTPUT (' HELP    (H)  List commands available.',STATUS)   
      CALL OUTPUT (' EXIT    (E)  Terminate the program.',STATUS)
      END
