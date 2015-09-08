      SUBROUTINE PSSTAK(FUNCT,VALUE)
*+
*   PSSTAK
*
*   Stores and manipulates the stack containing the
*   connectivity statuses for the previous scan.
*   Given      (arguments)
*   FUNCT    C  specifies PUSH, POP, PUT, GET or INIT
*   VALUE    I  given value for PUSH or PUT
*
*   Returned   (arguments)
*   VALUE    I  returned for POP or GET
*
*   The stack is remembered by the use of SAVE.
*
*   B.D.Kelly/ROE/11.3.1982
*-

      CHARACTER*(*) FUNCT
      INTEGER VALUE,STKLEN
      PARAMETER (STKLEN = 4096)
      INTEGER PSVAL(STKLEN),NPTR

      SAVE PSVAL,NPTR


      IF(FUNCT.EQ.'PUSH') THEN
         NPTR=MIN(NPTR+1,STKLEN)
         PSVAL(NPTR)=VALUE
      ELSE IF(FUNCT.EQ.'POP') THEN
         VALUE=PSVAL(NPTR)
         NPTR=MAX(NPTR-1,0)
      ELSE IF(FUNCT.EQ.'GET') THEN
         VALUE=PSVAL(MAX(NPTR,1))
      ELSE IF(FUNCT.EQ.'PUT') THEN
         PSVAL(MAX(NPTR,1))=VALUE
      ELSE IF(FUNCT.EQ.'INIT') THEN
         NPTR=0
      ENDIF

      END
