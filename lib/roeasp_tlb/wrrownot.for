      SUBROUTINE WRROWNOT (ROWNO)
C+
C    WRROWNOT
C
C	called from EDITTAB
C	part of general table editing package
C	outputs message saying what last row now is and that table renumbered
C
C    Given (arguments)
C	ROWNO (I)			last row number
C
C    D. Tudhope/ROE/Feb 1983
C-

      INTEGER ROWNO
      CHARACTER*60 MESSG
1     FORMAT (' table has been * RENUMBERED * - last row is now',I4)
      WRITE(MESSG,1) ROWNO
      CALL WRUSER(MESSG,ISTATUS)
      END
