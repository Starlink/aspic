      SUBROUTINE READTABT(MAXROWS,NCOLS,TAB,NROWS)

C+
C    READTABT
C
C	general subroutine to enter values into a (blank) 2-d table TAB of reals
C	values are entered from the keyboard as free format reals separated by commas
C	number of rows and columns in TAB are variable params where NCOLS is given
C	but NROWS depends on #rows typed in, so long as <MAXROWS.
C	and NCOLS must be <MAXKEY which is defined in READROWT
C	So this subroutine works for entering reals into ANY 2-d table.
C	end of input is indicated by READROWT returning TERMIN=.TRUE.
C	subroutine READROWT inputs a row of values from keyboard
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
C    D. Tudhope/ROE/April 1983
C-

      INTEGER MAXROWS,NCOLS,NROWS
      REAL TAB(MAXROWS,NCOLS)
      LOGICAL TERMIN
      INTEGER ISTATUS

      NROWS=1
      TERMIN=.FALSE.
      DO WHILE ((.NOT. TERMIN) .AND. (NROWS.LE.MAXROWS))
        CALL READROWT(NROWS,MAXROWS,NCOLS,TAB,TERMIN)
        IF (TERMIN) THEN
          NROWS=NROWS-1
        ELSE
          NROWS=NROWS+1
        ENDIF
      ENDDO
      END
