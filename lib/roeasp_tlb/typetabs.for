      SUBROUTINE TYPETABS(NROWS,NCOLS,TAB)
C+
C   TYPETABS
C
C	types table of real nos on terminal
C
C   Given         (arguments)
C	NROWS,NCOLS	(I)	dimensions of table rows,columns
C	TAB		(RA)	2-d table of real nos
C      
C	D. Tudhope/ROE/April 1983
C-

      INTEGER NROWS,NCOLS
      REAL TAB(NROWS,NCOLS)
      INTEGER J

      CALL WRHEADT
      DO J=1,NROWS
        CALL WRROWT(J,NROWS,NCOLS,TAB)
      ENDDO
      END
