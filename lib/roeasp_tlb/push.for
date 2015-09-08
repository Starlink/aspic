      SUBROUTINE PUSH(STACK,VALUE)
*+
*   PUSH
*
*   Push a given value onto a named stack.
*
*   Given      (arguments)
*   STACK   C   stack name
*   VALUE   I   value to be pushed onto top of stack
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
         CALL FREE('PUSH',VALUE)
      ELSE IF(STACK.EQ.'PSSTAK') THEN
         CALL PSSTAK('PUSH',VALUE)
      ELSE IF(STACK.EQ.'OBSTAK') THEN
         CALL OBSTAK('PUSH',VALUE)
      ELSE
         NUM=LEN(STACK)
         CALL WRUSER('ERROR IN PUSH, STACK WAS '//STACK(1:NUM),IST)
      ENDIF

      END
