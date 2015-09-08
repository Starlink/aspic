      SUBROUTINE GETMSG(STATUS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE GETMSG 
C
C
C         This routine takes an integer status value, decodes it and prints 
C         the results on the terminal. 
C
C         STATUS      I*4   IN    The status value to be decoded. 
C
C
C         W F Lupton               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 



C                              
C      ROUTINE TO DECODE THE INTEGER STATUS MESSAGE AND PRINT
C      THE INTERPRETATION ON THE TERMINAL
C
C      ARGUMENTS
C            STATUS      INTEGER      STATUS CODE
C
C      WFL RGO MAR 81
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER BUFADR*256
      GSTATUS=SYS$GETMSG(%VAL(STATUS),MSGLEN,BUFADR,%VAL(15),)
      IF (GSTATUS) THEN
            PRINT *,BUFADR(:MSGLEN)
      ELSE
            PRINT '(A,Z8)',' SYS$GETMSG STATUS IS ',GSTATUS
      ENDIF
      RETURN
      END
