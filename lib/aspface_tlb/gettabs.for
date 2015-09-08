      SUBROUTINE GETTABS(NAME,TABS,TABF,IST)
C+
C   GETTABS
C
C   Gets start and finish values of intensity look-up
C   table from the Starlink header.
C
C   Given      (arguments)
C   NAME        parameter name of Starlink file
C   IST         input status
C
C   Returned   (arguments)
C   TABS        lowest data value in intensity conversion
C   TABF        highest data value in intensity conversion
C   IST         return status = MAX(input status,output status)
C
C   Subroutine calls :
C   RDDSCR           : STARLINK
C
C   B.D.Kelly/ROE/8.10.1981
C-

      CHARACTER*(*) NAME
      CHARACTER*20 LOWEST,HIGHEST
      REAL TABS,TABF
      INTEGER IST,IST1,IST2,JDUM

      CALL RDDSCR(NAME,'TABS',1,LOWEST,JDUM,IST1)
      CALL RDDSCR(NAME,'TABF',1,HIGHEST,JDUM,IST2)
      READ(LOWEST,'(BN,F20.0)') TABS
      READ(HIGHEST,'(BN,F20.0)') TABF
      IST=MAX(IST,IST1,IST2)

      END
