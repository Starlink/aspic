      SUBROUTINE STOPLOT
C+
C     STOPLOT.
C
C     Subroutine to terminate plotting Fings graphics.
C
C  Given;
C   None.
C  
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics; ALF.
C   Fings;    DEVEND.
C
C       A C Davenhall. /ROE/                        10/11/81.
C       Modified (ACD)                              29/6/82.
C-
      COMMON /SELARG/ ARG,CIFER
      LOGICAL ARG,CIFER
C
      CALL DEVEND
C
C    If the CIFER is selected return to alpha mode.
C
      IF (CIFER) CALL ALF
      END
