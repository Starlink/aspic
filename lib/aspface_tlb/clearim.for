      SUBROUTINE CLEARIM(NAME)
C+
C   CLEARIM
C
C   Clears up after an image has been accessed
C
C   ASPIC interface version
C
C   Given      (arguments)
C   NAME        Starlink parameter name
C
C   Subroutine calls :
C   FRDATA,CNPAR     : STARLINK
C
C   B.D.Kelly/ROE/21.9.1981
C-

      CHARACTER*(*) NAME

        CALL FRDATA(NAME,IST)
        CALL CNPAR(NAME,IST)

      END
