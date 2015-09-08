      SUBROUTINE WRITEC(NAME,VALUE,ISTAT)
C+
C   WRITEC
C
C   Write a character variable to the environment.
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
C   WRKEYC           : STARLINK
C
C   B.D.Kelly/ROE/22.9.1981
C-
      CHARACTER*(*) NAME,VALUE
      INTEGER ISTAT,IST1

        CALL WRKEYC(NAME,VALUE,1,IST1)
        ISTAT=MAX(ISTAT,IST1)

      END
