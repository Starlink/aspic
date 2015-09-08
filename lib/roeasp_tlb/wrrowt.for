      SUBROUTINE WRROWT (ROWNO,MAXROWS,NCOLS,TAB)
C+
C    WRROWT
C
C	called from EDITTAB, TYPETAB
C	part of general table editing package
C	types a row from table on terminal (to 5 sig figs)
C
C    Given (arguments)
C	MAXROWS,NCOLS (I)		dimension of TAB
C	TAB(MAXROWS,NCOLS) (RA)		edited table
C	ROWNO (I)			row to be output
C
C    D. Tudhope/ROE/Feb 1983
C-

      INTEGER ROWNO,MAXROWS,NCOLS
      REAL TAB(MAXROWS,NCOLS)
C*  OUTPUT LINE LENGTH OF 80
      CHARACTER*80 ROW
      INTEGER I,J,ISTATUS,NEXTCOL

C*  Fits up to *6* cols onto a line - from 6 chars heading and 12 chars per field (80-6)/12=6
C*  deals with any size real nos BUT ONLY ACCURATE TO 5 SIG FIGS

      WRITE(ROW,1) (ROWNO,(TAB(ROWNO,I),I=1,MIN(NCOLS,6)))
      CALL WRUSER(ROW,ISTATUS)
C*  loop not executed if only 1 line needed per 'row'
      DO J=1,(NCOLS-1)/6
        NEXTCOL=6*J+1
        WRITE(ROW,2) (TAB(ROWNO,I),I=NEXTCOL,MIN(NCOLS,NEXTCOL+6-1))
        CALL WRUSER(ROW,ISTATUS)
      ENDDO
1     FORMAT (I3,' : ',6(1PE11.4,X))
2     FORMAT (6X,6(1PE11.4,X))
      END
