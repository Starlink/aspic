      SUBROUTINE DEFBOX (IXEXT,IYEXT,MINROW,MAXROW,MINCOL,MAXCOL,
     :                   ISTAT)
C+
C     DEFBOX.
C
C     Subroutine to allow the user to define a box around a 
C     portion of an image array in a very general way. The
C     coords. may be input either from the keyboard or 
C     from a cursor. If the routine is being run in batch
C     the coords. will be obtained from the input stream
C     and the routine tries to act intelligently by avoiding
C     continual reprompting.
C
C  Given;
C   IXEXT   (I)  X size of the image array.
C   IYEXT   (I)  Y  "   "   "    "     "  .
C   
C  Returned;
C   MINROW  (I)  Minimum X coord. of the box.
C   MAXROW  (I)  Maximum "   "  . "   "   " .
C   MINCOL  (I)  Minimum Y   "  . "   "   " .
C   MAXROW  (I)  Maximum "   "  . "   "   " .
C   ISTAT   (I)  Return status, = 0 for successful return, otherwise
C                non zero.
C
C  Subroutines called;
C   Interfaces;   OUTPUT, YESNO, MULREP, READI, BATCH.
C   E2D;          ZET, ARGBOX.
C   Args;         ARGS_NUMIM, ARGS_PTOA.
C
C  A C Davenhall./ROE/                                       23/7/82.
C  A C Davenhall./ROE/ {Modified to use the Args database}   13/2/83.
C-
      INTEGER IXEXT,IYEXT,MINCOL,MAXCOL,MINROW,MAXROW,ISTAT
C
      LOGICAL BATCHT,OK
      CHARACTER REPLY*1,REPLY2*10
      INTEGER IST
      INTEGER IMGNO,PLTSTT,MINRAR,MAXRAR,MINCAR,MAXCAR
C
C
      MINCOL=1
      MAXCOL=IYEXT
      MINROW=1
      MAXROW=IXEXT
C
C       Inquire if wish to change box size.
C
      ISTAT=0
      CALL OUTPUT (
     :   ' The Profile will be computed from the entire array.',
     :        ISTAT)
      CALL YESNO (
     : 'Do you wish to change the area the profile is computed from?',
     :    'Y',REPLY,ISTAT)
      IF (REPLY.EQ.'Y') THEN
C
C       Case where the box is to be redefined.
C
C       First check for the case where the program is running
C       in batch mode.
C
        CALL BATCH (BATCHT)
        IF (.NOT.BATCHT) THEN
C
C       Running interactively.
C
C       Inquire whether new box is to be input from the cursor
C       or the keyboard.
C
          CALL MULREP (
     : ' New box to be defined from the cursor or the keyboard?',
     :             'CURSOR,C,KEYBOARD,K$',REPLY2,ISTAT)
          IF (REPLY2.EQ.'CURSOR'.OR.REPLY2.EQ.'C') THEN
C
C       New box from cursor.
C
C       Image assumed to be plotted on the Args.
C
            CALL RECUR (9,'Y',10,'G',MINROW,MINCOL,MAXROW,MAXCOL,
     :                    ISTAT)
          ELSE
C
C       Enter values from keyboard.
C
            OK=.FALSE.
            DO WHILE (.NOT.OK)
            ISTAT=0
            CALL OUTPUT (' Enter integer coords. of box.',ISTAT)
            CALL READI ('COORD',
     :            ' X Coord. of bottom left hand corner;',
     :                  1,1,IXEXT,MINROW,ISTAT)
            CALL READI ('COORD',
     :            ' Y Coord. of bottom left hand corner;',
     :                  1,1,IYEXT,MINCOL,ISTAT)
            CALL READI ('COORD',
     :            ' X Coord. of top right hand corner;',
     :                  IXEXT,1,IXEXT,MAXROW,ISTAT)
            CALL READI ('COORD',
     :            ' Y Coord. of top right hand corner;',
     :                  IYEXT,1,IYEXT,MAXCOL,ISTAT)
C
C      Convert from pixel to Args coords.
C
            CALL ARGS_NUMIM (IMGNO)
            CALL ARGS_PTOA (IMGNO,MINROW,MINCOL,MINRAR,MINCAR,PLTSTT)
            CALL ARGS_PTOA (IMGNO,MAXROW,MAXCOL,MAXRAR,MAXCAR,PLTSTT)
C
C      Draw chosen box.
C
            CALL ARGBOX (MINRAR,MINCAR,MAXRAR,MAXCAR)
            CALL YESNO (' Are these values Ok?','Y',REPLY,ISTAT)
            IF (REPLY.EQ.'Y') OK=.TRUE.
            END DO
          END IF
        ELSE
C
C
C       Case where program is running in batch mode.
C
          ISTAT=0
          CALL READI ('COORD',
     :          ' X Coord. of bottom left corner read.',
     :            1,1,IXEXT,MINROW,ISTAT)
          CALL READI ('COORD',
     :          ' Y Coord. of bottom left hand corner read.',
     :            1,1,IYEXT,MINCOL,ISTAT)
          CALL READI ('COORD',
     :          ' X Coord. of top right corner read.',
     :            IXEXT,1,IXEXT,MAXROW,ISTAT)
          CALL READI ('COORD',
     :          ' Y Coord. of top right corner read.',
     :            IYEXT,1,IYEXT,MAXCOL,ISTAT)
        END IF
      END IF
      END
