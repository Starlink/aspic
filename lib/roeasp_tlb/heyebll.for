      SUBROUTINE HEYEBLL
C+
C      HEYEBLL.
C
C      Subroutine to list commands available in 'EYEBALL' - the
C      totally interactive object removal routines.
C
C      Subroutine called; OUTPUT.
C
C      A C Davenhall./ROE/                               21/1/82.
C-
      INTEGER STAT
      CALL OUTPUT ('  ',STAT)
      CALL OUTPUT (
     : ' Commands Available for Interactive Object Removal.',
     :               STAT)
      CALL OUTPUT ('  ',STAT)
      CALL OUTPUT (' SKY   (SK) - Determine mean of a box of sky.',
     :               STAT)
      CALL OUTPUT (' SET   (S)  - Draw a box around an object.',
     :               STAT)
      CALL OUTPUT (' ZAP   (Z)  - Replace pts. in box with sky value.',
     :               STAT)
      CALL OUTPUT (
     : ' REPLACE (R) - Replace star with a polynomial.',
     :   STAT)
      CALL OUTPUT (
     : ' CURCOL (C)  - Select a cursor colour for REPLACE.',
     :   STAT)
      CALL OUTPUT (' HELP  (H)  - List commands available.',STAT)
      CALL OUTPUT (' EXIT  (E)  - Terminate this process.',STAT)
      CALL OUTPUT ('  ',STAT)
      END
