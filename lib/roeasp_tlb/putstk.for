      SUBROUTINE PUTSTK(STACK,VALUE)
*+
*   PUTSTK
*
*   Overwrite the top of a named stack with a given value.
*
*   Given      (arguments)
*   STACK   C   stack name
*   VALUE   I   value to be written onto top of stack
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
         CALL FREE('PUT',VALUE)
      ELSE IF(STACK.EQ.'PSSTAK') THEN
         CALL PSSTAK('PUT',VALUE)
      ELSE IF(STACK.EQ.'OBSTAK') THEN
         CALL OBSTAK('PUT',VALUE)
      ELSE
         NUM=LEN(STACK)
         CALL WRUSER('ERROR IN PUTSTK, STACK WAS '//STACK(1:NUM),IST)
      ENDIF

      END
