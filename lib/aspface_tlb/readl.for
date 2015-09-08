*+  READL - Obtain a logical flag in response to a Y/N question.
      SUBROUTINE READL (NAME,PROMPT,DEFLT,FLAG,STATUS)
*    Description :
*     Obtains a Y/N reply to a question and returns the answer in a
*     a logical variable. The varaible is set to .TRUE. if the
*     response was 'Y' and .FALSE. if it was 'N'. If the user enters
*     'T' or 'F' these will be accepted as equivalent to 'Y' and 'N' 
*     respectively.
*    Invocation :
*     CALL READL (NAME,PROMPT,DEFLT;FLAG,STATUS)
*    Parameters :
*     NAME  =  CHARACTER*(*) (READ)
*           Name of the parameter associated with the question in
*           the interim environment.
*     PROMPT  =  CHARACTER*(*) (READ)
*           Prompt string.
*     DEFLT  =  LOGICAL (READ)
*           Default response.
*     FLAG  =  LOGICAL (WRITE)
*           Chosen response.
*     STATUS  =  INTEGER (UPDATE)
*           Running status (sort of). May be modified by attemps to 
*           access the interim environemnt. Note; the routine attempts
*           to execute irrespective the status on input but the
*           output status is the maximum of the input and internal
*           statuses.
*    Method :
*     Compute a character default to pass to the user from the
*     given logical default.
*     If (working interactively) then
*       do while (a satisfactory response has not been given)
*         push out a prompt and attempt to read a character
*         convert response to upper case.
*         if first element of response = 'T' put response = 'Y'
*         "    "      "    "     "     = 'F'  "     "     = 'N'
*         if first element of reponse = 'Y' or 'N' then
*           if the first element of the response = 'Y' then
*             return flag = .TRUE.
*           else
*             return flag = .FALSE.
*           end if
*           set termination flag.
*         end if
*       end do
*     else working in batch
*       push out a prompt and attempt to read a character
*       convert response to upper case.
*       if first element of response = 'T' put response = 'Y'
*       "    "      "    "     "     = 'F'  "     "     = 'N'
*       if first element of reponse = 'Y' or 'N' then
*         if the first element of the response = 'Y' then
*           return flag = .TRUE.
*         else
*           return flag = .FALSE.
*         end if
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
*     14/8/84:  Original version.                        (ROE::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      !INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER
     :  NAME*(*),   ! Parameter name for the interim environment.
     :  PROMPT*(*)  ! Prompt string.
      LOGICAL
     :  DEFLT       ! Default response.
*    Export :
      LOGICAL
     :  FLAG        ! Chosen response.
*    Status :
      INTEGER
     :  STATUS      ! Status.
*    Local variables :
      CHARACTER
     :  INPBUF*80,  ! Input buffer.
     :  CHRDEF*1    ! Character default computed from logical default.
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

*
*    Compute a character default to pass to the user from the
*    given logical default.

      IF (DEFLT) THEN
         CHRDEF(1:1)='Y'
      ELSE
         CHRDEF(1:1)='N'
      END IF

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

            CALL READC (NAME,PROMPT,CHRDEF,'A','z',INPBUF,INTSTT)

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
               IF (INPBUF(1:1).EQ.'Y') THEN
                  FLAG=.TRUE.
               ELSE
                  FLAG=.FALSE.
               END IF
               OKAY=.TRUE.
            END IF
         END DO

      ELSE

*
*       Working in batch. Push out the prompt and attempt to read
*       the response.

         CALL READC (NAME,PROMPT,CHRDEF,'A','z',INPBUF,INTSTT)

*
*       Convert to upper case.

         CALL UPPCAS (INPBUF)

*
*       Translate the users response if he has given 'T' or 'F'
*       instead of 'Y' or 'N' respectively.

         IF (INPBUF(1:1) .EQ. 'T') INPBUF(1:1)='Y'
         IF (INPBUF(1:1) .EQ. 'F') INPBUF(1:1)='N'

*
*       Check if a valid response has been given, if so adopt it and
*       display a message echoing the value given, if not accept 
*       the default and display a warning message.

         IF (INPBUF(1:1) .EQ. 'Y' .OR. INPBUF(1:1) .EQ. 'N') THEN
            IF (INPBUF(1:1).EQ.'Y') THEN
               FLAG=.TRUE.
            ELSE
               FLAG=.FALSE.
            END IF
         ELSE
            FLAG=DEFLT
            CALL WRUSER ('*WARNING invalid value input, default '/
     :        /'adopted = '//CHRDEF(1:1),INTST1)
         END IF

      END IF

*
*    Sort out the return status.

      STATUS=MAX(STATUS,INTSTT,INTST1)

      END
