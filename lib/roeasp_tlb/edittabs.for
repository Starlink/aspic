      SUBROUTINE EDITTABS(NOLDROWS,NCOLS,OLDTAB,MAXROWS,TAB,NROWS)
C+
C    EDITTABS
C
C	called from EDITTAB
C	edits OLDTAB to TAB - general 2-d tables of reals
C	uses RDROWT as basic subroutine to read in a row.
C	able to Replace,Insert,Delete,List rows in table
C	and Change individual columns.
C	Can change number of rows in table, but not number of columns.
C	alterations take place immediately on TAB and may cause numbering of rows to change (if Insert etc)
C	Not allowed to change NCOLS but NROWS can change (up to MAXROWS)
C	dimensions of TAB are fixed at (MAXROWS,NCOLS) but actual no. of rows used NROWS is also returned.
C	specify where to Change,Insert etc by row number
C	and for Insert,Delete the rows in TAB are shifted up or down to make room/leave no gaps
C	full list of commands are Replace,Change,Delete,Insert,List,Print,Help,Quit
C
C    Given (arguments)
C	NOLDROWS,NCOLS (I)		original no. of rows and no. of cols
C	OLDTAB(NOLDROWS,NCOLS) (RA)	old table to be edited
C	MAXROWS (I)			maximum no. of rows allowed
C
C    Returned (arguments)
C	NROWS (I)			actual no. of rows in TAB
C	TAB(MAXROWS,NCOLS) (RA)		edited table
C
C    D. Tudhope/ROE/Feb 1983
C-

      INTEGER NOLDROWS,NCOLS,MAXROWS,NROWS
      REAL OLDTAB(NOLDROWS,NCOLS),TAB(MAXROWS,NCOLS)
      CHARACTER*1 COMMAND
      INTEGER I,J,IST,ROWNO,ROWST,ROWFIN

      IST=0
C*  first copy over oldtab-->tab and type out present table contents
      CALL WRHEADT
      DO J=1,NOLDROWS
        DO I=1,NCOLS
          TAB(J,I)=OLDTAB(J,I)
        ENDDO
        CALL WRROWT(J,MAXROWS,NCOLS,TAB)
      ENDDO
      NROWS=NOLDROWS
      COMMAND='P'

      CALL WRUSER('         TABLE EDITOR',IST)
      CALL WRUSER('When actually entering new rows,
     :do NOT type row number',IST)

C*  main command loop
      DO WHILE (COMMAND.NE.'Q')
       CALL READC('COMMAND','Replace,Change,Delete,Insert,List,Print,
     :Help,Quit','P','A','Z',COMMAND,IST)
C*  obey command - (test ordered by probability of command)
        IF (COMMAND.EQ.'R') GOTO 100
        IF (COMMAND.EQ.'C') GOTO 150
        IF (COMMAND.EQ.'D') GOTO 200
        IF (COMMAND.EQ.'I') GOTO 300
        IF (COMMAND.EQ.'L') GOTO 400
        IF (COMMAND.EQ.'P') GOTO 500
        IF (COMMAND.EQ.'H') GOTO 600
C*  if 'Q' go to exit and if any other letter, ignore
        GOTO 1000

C*  Replace
100     CALL READI('ROWNO','Replace which row no. (0 if none) ?',
     :             1,0,NROWS,ROWNO,IST)
        IF (ROWNO.NE.0) CALL RDROWT(ROWNO,MAXROWS,NCOLS,TAB)
        GOTO 1000

C*  Change
150     CALL READI('ROWNO','Change value in which row no. (0 if none)
     : ?',1,0,NROWS,ROWNO,IST)
        IF (ROWNO.NE.0) THEN
          CALL READI('COLNO','which column in that row ? (0 to abort)',
     :               1,0,NCOLS,I,IST)
          IF (I.NE.0)
     :    CALL READR('NEWVAL',' ',0.0,-1.0E19,1.0E19,TAB(ROWNO,I),IST)
        ENDIF
        GOTO 1000

C*  Delete
200     CALL READI('ROWNO','Delete which row no. (0 if none) ?',
     :             1,0,NROWS,ROWNO,IST)
        IF (ROWNO.NE.0) THEN
C*  copy rows in TAB above deleted row down so as to leave no gaps
          DO J=ROWNO,NROWS-1
            DO I=1,NCOLS
              TAB(J,I)=TAB(J+1,I)
            ENDDO
          ENDDO
          NROWS=NROWS-1
          IF (NROWS.EQ.0) THEN
            CALL WRERR('TOOFEW')
C*  and quit
            COMMAND='Q'
          ELSE
            CALL WRROWNOT(NROWS)
          ENDIF
        ENDIF
        GOTO 1000

C*  Insert
300     IF (NROWS.GE.MAXROWS) THEN
          CALL WRERR('TOOMANY')
C*  and quit
          COMMAND='Q'
        ELSE
          CALL READI('ROWNO','Insert BEFORE which row no. (0 if none,
     :  last row + 1 if at end) ?',
     :             1,0,NROWS+1,ROWNO,IST)
          IF (ROWNO.NE.0) THEN
C*  push TAB up to make space (unless at end of TAB)
            DO J=NROWS+1,ROWNO+1,-1
              DO I=1,NCOLS
                TAB(J,I)=TAB(J-1,I)
              ENDDO
            ENDDO
            CALL RDROWT(ROWNO,MAXROWS,NCOLS,TAB)
            NROWS=NROWS+1
            CALL WRROWNOT(NROWS)
          ENDIF
        ENDIF
        GOTO 1000

C*  List
400     CALL READI('ROWST','row to start Listing from ?',
     :             1,1,NROWS,ROWST,IST)
        CALL READI('ROWFIN','row to finish listing at ?',
     :             NROWS,ROWST,NROWS,ROWFIN,ST)
        CALL WRHEADT
        DO J=ROWST,ROWFIN
          CALL WRROWT(J,MAXROWS,NCOLS,TAB)
        ENDDO
        GOTO 1000

C*  Print
500     CALL WRHEADT
        DO J=1,NROWS
          CALL WRROWT(J,MAXROWS,NCOLS,TAB)
        ENDDO
        GOTO 1000

C*  Help
600     CALL WRHELPT
        GOTO 1000

1000    ENDDO
      END
