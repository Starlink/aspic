      SUBROUTINE SELCOL (COLOUR,IOSTAT)
C+
C     SELCOL.
C
C     Select a colour for the Args cursor.
C
C  Given;
C   COLOUR  (C)  Colour previously in use.
C
C  Returned;
C   COLOUR  (C)  Selected colour.
C   IOSTAT  (I)  Return status, 0 for a successful return, otherwise
C                non zero.
C
C  Subroutines called;
C   Interfaces;  MULREP.
C
C  A C Davenhall./ROE/                                    28/7/82.
C-
      CHARACTER COLOUR*(1)
      INTEGER IOSTAT
C
      CHARACTER DFAULT*(4),REPLY*(4)
C
C
C    Set the default to be the previous colour.
C
      DFAULT=COLOUR
      IF (COLOUR.EQ.'B') DFAULT='BLU'
      IF (COLOUR.EQ.'Z') DFAULT='BLAK'
C
C    Obtain the required colour.
C
      CALL MULREP (
     : ' Select the required colour for the cursor;',
     : 'W,R,G,BLU,Y,C,M,BLAK$',REPLY,IOSTAT)
C
C    decode the ambigous first letter colours (blue and black).
C
      IF (REPLY.NE.'BLAK') THEN
        COLOUR=REPLY(1:1)
      ELSE
        COLOUR='Z'
      END IF
      END
