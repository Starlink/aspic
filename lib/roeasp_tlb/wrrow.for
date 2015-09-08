      SUBROUTINE WRROW (ROWNO,MAXROWS,NCOLS,TAB)
C*        &&&&&&& Application Dependant subroutines for table output &&&&&&&&&
      INTEGER ROWNO,MAXROWS,NCOLS
      REAL TAB(MAXROWS,NCOLS)
      CHARACTER*70 ROW
      WRITE(ROW,1) (ROWNO,(TAB(ROWNO,I),I=1,NCOLS))
      CALL WRUSER(ROW,ISTATUS)
1     FORMAT (I5,' : ',2F10.3)
      END
