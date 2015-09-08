      SUBROUTINE PRINTTABS(NROWS,NCOLS,TAB)
C+
C   PRINTTABS
C
C     OUTPUTS TABLE OF REAL NOS (ROW BY ROW) TO TEMP.DAT FOR LISTING
C
C   Given         (arguments)
C	NROWS,NCOLS	(I)	dimensions of table rows,columns
C	TAB		(RA)	2-d table of real nos
C      
C   Output  :  FORTRAN unit 4
C
C	D. Tudhope/ROE/April 1983
C-

      INTEGER NROWS,NCOLS
      REAL TAB(NROWS,NCOLS)
      INTEGER N,J,I,NEXTCOL

C*  Linelength of 132, header 6, field length 12, so (132-6)/12 = 10 cols per line
C*  deals with all real nos but only accurate to 5 SIG FIGS

1     FORMAT(I3,' : ',10(1PE11.4,X))
2     FORMAT(6X,10(1PE11.4,X))
3     FORMAT('                  Table')


      OPEN(UNIT=4,NAME='TEMP.DAT',TYPE='NEW',
     &     FORM='FORMATTED')

      WRITE(4,3)
      WRITE(4,*)' '
      WRITE(4,*)' '
      DO N=1,NROWS
        WRITE(4,1) (N,(TAB(N,I), I=1,MIN(NCOLS,10)))
C*  loop not executed if only 1 line output per 'row'
        DO J=1,(NCOLS-1)/10
          NEXTCOL=10*J+1
          WRITE(4,2) (TAB(N,I), I=NEXTCOL,MIN(NCOLS,NEXTCOL+10-1))
        ENDDO
      ENDDO

      CLOSE(UNIT=4,DISPOSE='PRINT/DELETE')
      END
