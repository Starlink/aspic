      SUBROUTINE WRITER(NAME,VALUE,ISTAT)
C+
C   WRITER
C
C   Write a real variable to the environment.
C
C   ASPIC version
C
C   Given      (arguments)
C   NAME        name of parameter
C   VALUE       value of parameter
C   ISTAT       input status
C
C   Returned   (arguments)
C   ISTAT       return status = MAX(input status,output status)
C
C   Subroutine calls :
C   WRKEYR           : STARLINK
C
C   B.D.Kelly/ROE/22.9.1981
C-
      CHARACTER*(*) NAME
      REAL VALUE
      INTEGER ISTAT,IST1

        CALL WRKEYR(NAME,VALUE,1,IST1)
        ISTAT=MAX(ISTAT,IST1)

      END
