      SUBROUTINE LISTLEVS(NROWS,NCOLS,TAB)
C+
C   LISTLEVS
C
C     OUTPUTS TABLE OF REAL NOS (ROW BY ROW) TO TEMP.DAT FOR LISTING
C    part of calibration suite
C
C   Given         (arguments)
C	NROWS,NCOLS	(I)	dimensions of table rows,columns
C	TAB		(RA)	2-d table of real nos
C      
C   Output  :  FORTRAN unit 4
C
C	D. Tudhope/ROE/Sept 1982
C-

      INTEGER NROWS,NCOLS
      REAL TAB(NROWS,NCOLS)
      INTEGER J,I

C*  && application dependant FORMAT statements &&

11    FORMAT('      Observed VALUES            True INTENSITIES')
22    FORMAT(I5,3X,F10.3,17X,F10.3)
33    FORMAT('              Intensity Calibration Levels')

C*  &&                                         &&




      OPEN(UNIT=4,NAME='TEMP.DAT',TYPE='NEW',
     &     FORM='FORMATTED')

      WRITE(4,33)
      WRITE(4,*)' '
      WRITE(4,*)' '
      WRITE(4,11)
      WRITE(4,*)' '

      DO J=1,NROWS
        WRITE(4,22) (J,(TAB(J,I),I=1,NCOLS))
      ENDDO
      CLOSE(UNIT=4,DISPOSE='PRINT/DELETE')

      END
