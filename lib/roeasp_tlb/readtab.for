      SUBROUTINE READTAB(MAXROWS,NCOLS,TAB,NROWS)

C+
C    READTAB
C
C	general subroutine to enter values into a (blank) 2-d table TAB of reals
C	values are entered from the keyboard as free format reals separated by commas
C	number of rows and columns in TAB are variable params where NCOLS is given
C	but NROWS depends on #rows typed in, so long as <MAXROWS.
C	and NCOLS must be <MAXKEY which is defined in READROW
C	So this subroutine works for entering reals into ANY 2-d table.
C	input is indicated by READROW returning TERMIN=.TRUE.
C	This is part of package of general table routines:
C		READTAB, EDITTAB, LISTTAB
C	which should be used with main programs that input/output the tables to STARLINK BULK DATA FRAMES
C	subroutine READROW inputs a row of values from keyboard
C	it will only return when user types a correct line
C
C    Given (arguments)
C       MAXROWS (I)			max. no of rows in table
C	NCOLS (I)			number of columns in table
C
C    Returned (arguments)
C	TAB(MAXROWS,NCOLS) (RA)		table of reals
C	NROWS (I)			actual no. of rows typed in
C
C    D. Tudhope/ROE/Feb 1983
C-

      INTEGER MAXROWS,NCOLS,NROWS
      REAL TAB(MAXROWS,NCOLS)
      LOGICAL TERMIN
      INTEGER ISTATUS

      NROWS=1
      TERMIN=.FALSE.
      DO WHILE ((.NOT. TERMIN) .AND. (NROWS.LE.MAXROWS))
        CALL READROW(NROWS,MAXROWS,NCOLS,TAB,TERMIN)
        IF (TERMIN) THEN
          NROWS=NROWS-1
        ELSE
          IF (TAB(NROWS,2).LT.0.0) THEN
C*  test specifically for CALIBRATION package on "intensity"
            CALL WRERR('MISINTEN')
          ELSE
            NROWS=NROWS+1
          ENDIF
        ENDIF
      ENDDO
      END
