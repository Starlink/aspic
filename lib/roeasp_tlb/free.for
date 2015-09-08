      SUBROUTINE FREE(FUNCT,VALUE)
*+
*   FREE
*
*   Stores and manipulates the stack containing the pointers
*   to available data storage areas.
*   Given      (arguments)
*   FUNCT    C  specifies PUSH, POP, PUT, GET or INIT
*   VALUE    I  given value for PUSH PUT or INIT
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
      INTEGER FREESK(STKLEN),NPTR,NUMVAL

      SAVE FREESK,NPTR,NUMVAL


      IF(FUNCT.EQ.'PUSH') THEN
         NPTR=MIN(NPTR+1,NUMVAL)
         FREESK(NPTR)=VALUE
      ELSE IF(FUNCT.EQ.'POP') THEN
         VALUE=FREESK(NPTR)
         NPTR=MAX(NPTR-1,0)
      ELSE IF(FUNCT.EQ.'GET') THEN
         VALUE=FREESK(MAX(NPTR,1))
      ELSE IF(FUNCT.EQ.'PUT') THEN
         FREESK(MAX(NPTR,1))=VALUE
      ELSE IF(FUNCT.EQ.'INIT') THEN
         NUMVAL=VALUE
         DO J=1,NUMVAL
            FREESK(J)=J
         ENDDO
         NPTR=NUMVAL
      ENDIF

      END
