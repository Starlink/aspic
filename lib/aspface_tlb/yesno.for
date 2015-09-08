      SUBROUTINE YESNO(PROMPT,DEFAULT,CHAR,IST)
C+
C   YESNO
C
C   Obtains the reply Y or N to a question
C
C   Given      (arguments)
C   PROMPT      user prompt (the question)
C   DEFAULT     Y or N
C   IST         input status
C
C   Returned   (arguments)
C   CHAR        output character
C   IST         return status = MAX(input status,output status)
C
C   Subroutine calls :
C   READC            : E2DLIB
C
C   J.A.Cooke/UOE/1981
C   B.D.Kelly/ROE/28.9.1981
C   A C Davenhall./ROE/   {Bug fix.}                           14/8/84.
C-

      CHARACTER*(*) PROMPT,DEFAULT,CHAR
      CHARACTER*80 TBUF
      INTEGER IST,IST1

   10 CONTINUE

      CALL READC('REPLY',PROMPT,DEFAULT,'A','z',TBUF,IST1)
      CALL UPPCAS (TBUF)
      IF(IST1.NE.0) THEN
        CHAR=DEFAULT
      ELSE
        IF((TBUF(1:1).NE.'Y').AND.(TBUF(1:1).NE.'N')) GO TO 10
        CHAR=TBUF(1:1)
      ENDIF

      IST=MAX(IST,IST1)

      END
