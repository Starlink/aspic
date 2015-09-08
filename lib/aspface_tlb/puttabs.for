      SUBROUTINE PUTTABS(NAME,TABS,TABF,IST)
C+
C   PUTTABS
C
C   Writes start and finish values of intensity look-up
C   table to the Starlink header.
C
C   Given      (arguments)
C   NAME        parameter name of Starlink file
C   TABS        lowest data value in intensity conversion
C   TABF        highest data value in intensity conversion
C   IST         input status
C
C   Returned   (arguments)
C   IST         return status = MAX(input status,output status)
C
C   Subroutine calls :
C   WRDSCR           : STARLINK
C
C   B.D.Kelly/ROE/8.10.1981
C-

      CHARACTER*(*) NAME
      CHARACTER*20 LOWEST,HIGHEST
      REAL TABS,TABF
      INTEGER IST,IST1,IST2

      WRITE(LOWEST,'(F20.3)') TABS
      WRITE(HIGHEST,'(F20.3)') TABF

      CALL WRDSCR(NAME,'TABS',LOWEST,1,IST1)
      CALL WRDSCR(NAME,'TABF',HIGHEST,1,IST2)

      IST=MAX(IST,IST1,IST2)

      END
