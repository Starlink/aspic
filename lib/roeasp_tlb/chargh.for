      SUBROUTINE CHARGH
C+
C     CHARGH.
C
C     Subroutine to list, on the terminal, the commands available
C     for the interactive annotation of an image displayed
C     on the ARGS.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutine called;
C   Interfaces:  OUTPUT.
C
C  A C Davenhall./ROE/                                  4/6/82.
C-
      INTEGER STAT
C
      CALL OUTPUT ('    ',STAT)
      CALL OUTPUT (
     : ' Commands available for annotating an ARGS display.',STAT)
      CALL OUTPUT ('   ',STAT)
      CALL OUTPUT (' WRITE (W)    Enter, position & display string',
     :      STAT)
      CALL OUTPUT (' SCALE (S)    Display a scale length.',STAT)
      CALL OUTPUT (' ARROW (A)    Add an arrow to the display.',STAT)
      CALL OUTPUT (' MODE  (M)    Alter way in which routine works',
     :      STAT)
      CALL OUTPUT ('            - Position from cursor or keyboard',
     :     STAT)
      CALL OUTPUT ('            - Change character size.',STAT)
      CALL OUTPUT (' CLEAR (C)    Clear graphics from the last overlay',
     :      STAT)
      CALL OUTPUT ('              plane written to.',STAT)
      CALL OUTPUT (' CLEARALL (CA) Clear graphics from all overlay',
     :      STAT)
      CALL OUTPUT ('              planes.',STAT)
      CALL OUTPUT (' COLOUR (CO)  Change the colour of the cursor.',
     :      STAT)
      CALL OUTPUT (' ROTATE (R)   Determine whether or not graphics',
     :      STAT)
      CALL OUTPUT ('              are to be rotated through the',
     :      STAT)
      CALL OUTPUT ('              overlay planes.',STAT)
      CALL OUTPUT (' HELP  (H)    List commands available.',STAT)
      CALL OUTPUT (' EXIT  (E)    Terminate annotation.',STAT)
      CALL OUTPUT ('   ',STAT)
      END
