      SUBROUTINE COPBUF(STRING,NUMFOR)
*+
*   COPBUF
*
*   Writes a character string simultaneously to the user terminal
*   and to a diskfile.
*
*   Given         (arguments)
*   STRING    C    character string
*   NUMFOR    I    FORTRAN unit number
*
*   Subroutines called :
*   WRUSER             : STARLINK
*
*   B.D.Kelly/ROE/1.12.1981
*-
      CHARACTER*(*) STRING
      INTEGER NUMFOR

      CALL WRUSER(STRING,ISTAT)
      WRITE(NUMFOR,'(A)') STRING

      END
