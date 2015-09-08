      SUBROUTINE PRIAXH
C+
C     PRIAXH.
C
C     Subroutine to list the commands available for PRIAXE.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces;  OUTPUT.
C
C  A C Davenhall./ROE/                                       6/7/82.
C-
      INTEGER STAT
C
      CALL OUTPUT ('    ',STAT)
      CALL OUTPUT (' Commands available:-',STAT)
      CALL OUTPUT ('    ',STAT)
      CALL OUTPUT (' CENTRE  (C)  Automatically locate galaxy centre.',
     :               STAT)
      CALL OUTPUT (' MANCEN  (M)  Manually locate galaxy centre.',STAT)
      CALL OUTPUT (' PIXSIZ  (PI) Enter pixel size.',STAT)
      CALL OUTPUT (' AXES    (A)  Extract major and minor axes.',STAT)
      CALL OUTPUT (' PLOT    (P)  Plot selected axis.',STAT)
      CALL OUTPUT (' SAVE    (S)  Save a selected file.',STAT)
      CALL OUTPUT (' COLOUR  (CO) Change the cursor colour.',STAT)
      CALL OUTPUT (' HELP    (H)  List commands available.',STAT)
      CALL OUTPUT (' EXIT    (E)  Leave this node.',STAT)
      CALL OUTPUT ('     ',STAT)
      END
