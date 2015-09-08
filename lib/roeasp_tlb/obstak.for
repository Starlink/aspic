      SUBROUTINE OBSTAK(FUNCT,VALUE)
*+
*   OBSTAK
*
*   Stores and manipulates the stack containing the data
*   for objects active in the current scan.
*   Given      (arguments)
*   FUNCT    C  specifies PUSH, POP, PUT, GET or INIT
*   VALUE    IA given value for PUSH or PUT
*               VALUE(1) = position of first pixel of object
*                          in current scan
*               VALUE(2) = position of last F0 marker for object
*               VALUE(3) = index of data for object in the data arrays
*
*   Returned   (arguments)
*   VALUE    IA returned for POP or GET
*
*   The stack is remembered by the use of SAVE.
*
*   B.D.Kelly/ROE/11.3.1982
*-

      CHARACTER*(*) FUNCT
      INTEGER VALUE(3),STKLEN
      PARAMETER (STKLEN = 2048)
      INTEGER OBVALS(3,STKLEN),NPTR

      SAVE OBVALS,NPTR


      IF(FUNCT.EQ.'PUSH') THEN
         NPTR=MIN(NPTR+1,STKLEN)
         DO I=1,3
            OBVALS(I,NPTR)=VALUE(I)
         ENDDO
      ELSE IF(FUNCT.EQ.'POP') THEN
         DO I=1,3
            VALUE(I)=OBVALS(I,NPTR)
         ENDDO
         NPTR=MAX(NPTR-1,0)
      ELSE IF(FUNCT.EQ.'GET') THEN
         DO I=1,3
            VALUE(I)=OBVALS(I,MAX(NPTR,1))
         ENDDO
      ELSE IF(FUNCT.EQ.'PUT') THEN
         DO I=1,3
            OBVALS(I,MAX(NPTR,1))=VALUE(I)
         ENDDO
      ELSE IF(FUNCT.EQ.'INIT') THEN
         NPTR=0
      ENDIF

      END
