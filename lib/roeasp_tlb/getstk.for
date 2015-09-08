      SUBROUTINE GETSTK(STACK,VALUE)
*+
*   GETSTK
*
*   Return the top value on a named stack.
*
*   Given      (arguments)
*   STACK   C   stack name
*
*   Returned   (arguments)
*   VALUE   I   value on top of stack
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
         CALL FREE('GET',VALUE)
      ELSE IF(STACK.EQ.'PSSTAK') THEN
         CALL PSSTAK('GET',VALUE)
      ELSE IF(STACK.EQ.'OBSTAK') THEN
         CALL OBSTAK('GET',VALUE)
      ELSE
         NUM=LEN(STACK)
         CALL WRUSER('ERROR IN GETSTK, STACK WAS '//STACK(1:NUM),IST)
      ENDIF

      END
