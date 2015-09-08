*+  QUERY - Obtain a Y/N reply to a question.
      SUBROUTINE QUERY (NAME,PROMPT,DEFLT,REPLY,STATUS)
*    Description :
*     Obtains a Y/N reply to a question and returns the answer in a
*     a character variable, the first element of which is set to 
*     Y or N. If the user enters 'T' or 'F' these will be accepted
*     and translated into 'Y' and 'N' respectively.
*    Invocation :
*     CALL QUERY (NAME,PROMPT,DEFLT;REPLY,STATUS)
*    Parameters :
*     NAME  =  CHARACTER*(*) (READ)
*           Name of the parameter associated with the question in
*           the interim environment.
*     PROMPT  =  CHARACTER*(*) (READ)
*           Prompt string.
*     DEFLT  =  CHARACTER*(*) (READ)
*           Default reply.
*     REPLY  =  CHARACTER*(*) (WRITE)
*           Chosen reply.
*     STATUS  =  INTEGER (UPDATE)
*           Running status (sort of). May be modified by attemps to 
*           access the interim environemnt. Note; the routine attempts
*           to execute irrespective the status on input but the
*           output status is the maximum of the input and internal
*           statuses.
*    Method :
*     If (working interactively) then
*       do while (a satisfactory response has not been given)
*         push out a prompt and attempt to read a character
*         convert response to upper case.
*         if first element of response = 'T' put response = 'Y'
*         "    "      "    "     "     = 'F'  "     "     = 'N'
*         if first element of reponse = 'Y' or 'N' then
*           chosen response = first element of input buffer
*           set termination flag.
*         end if
*       end do
*     else working in batch
*       push out a prompt and attempt to read a character
*       convert response to upper case.
*       if first element of response = 'T' put response = 'Y'
*       "    "      "    "     "     = 'F'  "     "     = 'N'
*       if first element of reponse = 'Y' or 'N' then
*         chosen response = first element of input buffer
*       else
*         chosen response = default
*         display warning message about invalid input being given.
*       end if
*     end if
*     Sort out the return status.
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall.      (ROE::ACD)
*    History :
*     13/8/84:  Original version.                        (ROE::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      !INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER
     :  NAME*(*),   ! Parameter name for the interim environment.
     :  PROMPT*(*), ! Prompt string.
     :  DEFLT*(*)   ! Default response.
*    Export :
      CHARACTER
     :  REPLY*(*)   ! Chosen response.
*    Status :
      INTEGER
     :  STATUS      ! Status.
*    Local variables :
      CHARACTER
     :  INPBUF*80   ! Input buffer.
      LOGICAL
     :  BATFL,      ! Flag; working interactively or in batch.
     :  OKAY        ! Flag; was the input response ok?
      INTEGER
     :  INTSTT,     ! Internal status.
     :  INTST1      !    "       "   .
*-

      INTSTT=0
      INTST1=0
      INPBUF=' '
      REPLY=' '

*
*    Check whether running interactively or in batch.

      CALL BATCH (BATFL)
      IF (.NOT.BATFL) THEN

*
*       Running interactively. Prompt the user until a satisfactory
*       response is given.

         OKAY=.FALSE.
         DO WHILE (.NOT.OKAY)

*
*          Push out a prompt and attempt to read the input.

            CALL READC (NAME,PROMPT,DEFLT,'A','z',INPBUF,INTSTT)

*
*          Convert to upper case.

            CALL UPPCAS (INPBUF)

*
*          Translate the users response if he has given 'T' or 'F'
*          instead of 'Y' or 'N' respectively.

            IF (INPBUF(1:1) .EQ. 'T') INPBUF(1:1)='Y'
            IF (INPBUF(1:1) .EQ. 'F') INPBUF(1:1)='N'

*
*          Check if a valid response has been given.

            IF (INPBUF(1:1) .EQ. 'Y' .OR. INPBUF(1:1) .EQ. 'N') THEN
               REPLY(1:1)=INPBUF(1:1)
               OKAY=.TRUE.
            END IF
         END DO

      ELSE

*
*       Working in batch. Push out the prompt and attempt to read
*       the response.

         CALL READC (NAME,PROMPT,DEFLT,'A','z',INPBUF,INTSTT)

*
*       Convert to upper case.

         CALL UPPCAS (INPBUF)

*
*       Translate the users response if he has given 'T' or 'F'
*       instead of 'Y' or 'N' respectively.

         IF (INPBUF(1:1) .EQ. 'T') INPBUF(1:1)='Y'
         IF (INPBUF(1:1) .EQ. 'F') INPBUF(1:1)='N'

*
*       Check if a valid response has been given, if so adopt it
*       and display a message echoing the value given, if
*       not accept the default and display a warning message.

         IF (INPBUF(1:1) .EQ. 'Y' .OR. INPBUF(1:1) .EQ. 'N') THEN
            REPLY(1:1)=INPBUF(1:1)
         ELSE
            REPLY(1:1)=DEFLT(1:1)
            CALL WRUSER ('*WARNING invalid value input, default '/
     :        /'adopted = '//DEFLT(1:1),INTST1)
         END IF

      END IF

*
*    Sort out the return status.

      STATUS=MAX(STATUS,INTSTT,INTST1)

      END
