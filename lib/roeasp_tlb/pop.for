      SUBROUTINE POP(STACK,VALUE)
*+
*   POP
*
*   POP a given value off a named stack.
*
*   Given      (arguments)
*   STACK   C   stack name
*   VALUE   I   value to be POPed onto top of stack
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
         CALL FREE('POP',VALUE)
      ELSE IF(STACK.EQ.'PSSTAK') THEN
         CALL PSSTAK('POP',VALUE)
      ELSE IF(STACK.EQ.'OBSTAK') THEN
         CALL OBSTAK('POP',VALUE)
      ELSE
         NUM=LEN(STACK)
         CALL WRUSER('ERROR IN POP, STACK WAS '//STACK(1:NUM),IST)
      ENDIF

      END
