C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CHARLN *
C      *            *
C      **************
C
C  Subroutine to see how long content of character string is. Gives no of
C  of effective characters as total minus no of contiguous null characters
C  at end
C
C   Input
C     TEXT    Input character variable (form CHAR*N)
C   Output
C     KLEN    no of chars until last non-null character
C
C   CALLS
C    None
C
C  AJPENNY      RGO                      1982 dec 22
C
C ------------------------------------------------------------
C
C
C
      SUBROUTINE CHARLN(TEXT,KLEN)
C
C
C
      CHARACTER*(*) TEXT
C
C
C
      KLEN = LEN(TEXT)
      K = 1
      DO WHILE (K.EQ.1.AND.KLEN.GT.0)
         IF (TEXT(KLEN:KLEN).EQ.' ') THEN
            KLEN = KLEN - 1
         ELSE
            K = 0
         ENDIF
      ENDDO
C
C
C
      END



