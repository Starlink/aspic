      SUBROUTINE INITSTK(STACK,VALUE)
*+
*   INITSTK
*
*   Initialize a named stack.
*
*   Given      (arguments)
*   STACK   C   stack name
*   VALUE   I   initialization value (only used by FREE stack)
*
*   Subroutines called :
*   FREE, PSSTAK,OBSTAK       : E2DLIB
*   WRUSER             : INTERFACE
*
*   B.D.Kelly/ROE/11.3.1982
*-

      CHARACTER*(*) STACK
      INTEGER VALUE(1:*)

      IF(STACK.EQ.'FREE') THEN
         CALL FREE('INIT',VALUE)
      ELSE IF(STACK.EQ.'PSSTAK') THEN
         CALL PSSTAK('INIT',VALUE)
      ELSE IF(STACK.EQ.'OBSTAK') THEN
         CALL OBSTAK('INIT',VALUE)
      ELSE
         NUM=LEN(STACK)
         CALL WRUSER('ERROR IN INITSTK, STACK WAS '//STACK(1:NUM),IST)
      ENDIF

      END
