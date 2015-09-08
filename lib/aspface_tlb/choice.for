      SUBROUTINE CHOICE (CONID,PROMPT,NUMCHOI,DESCR,RESPS,
     :                   REPLY,STATUS)
C+
C     CHOICE.
C
C     Subroutine to prompt the user for one choice between a list of
C     possible responses. Prompting continues until a valid entry
C     is input. Abbreviated responses are permitted at the
C     programmer's discretion.
C
C  Given;
C   CONID    (C)  Conection file identifier.
C   PROMPT   (C)  Overall prompt string.
C   NUMCHOI  (I)  Number of possible responses.
C   DESCR    (CA) Explanation for each possible response.
C   RESPS    (CA) Possible responses.
C
C  Returned;
C   REPLY    (C)  Selected choice.
C   STATUS   (I)  Return status, = 0 for success.
C
C  Subroutines called;
C   Interfaces:-   BATCH, READC, UPPCAS.
C   Misc.:-        STRLEN, CHOICE1.
C
C  Notes;
C  1.   The overall prompt string (PROMPT) should fit on one line.
C  2.   The explanation for each choice must be no more than 50
C       characters.
C  3.   The response for each choice must be mo more than 10
C       characters.
C  4.   The (arbitary) maximum number of possible responses is 100.
C  5.   In interactive mode prompting continues until a valid response
C       is given. The default response is the first option given. In
C       batch mode the prompt is issued once only. If an invalid
C       response is given the default is adopted.
C  6.   At the programmer's discretion abbreviations for the input
C       responses are allowed. These are indicated by placing a
C       hash ("#") character in the given response after the last
C       character that may not be abbreviated. Care must be taken
C       that ambiguities are not introduced.
C          If abbreviations are allowed any abbreviation between the
C       minimum specified and the full response is accepted if it
C       corresponds on a character to character basis with the full
C       response.
C          When abbreviations are specified the value returned by the
C       routine is always the full respose given by the calling
C       routine WITHOUT the #.
C  7.   If the routine is input with a non-zero status it immediately
C       returns with REPLY set to blank.
C
C  Structure:-
C   Determine the length of each response in its most abbreviated
C     form
C   If running interactively.
C     do while (invalid response)
C       write overall prompt string.
C       do for all responses
C         code up the output buffer.
C         send buffer to environment.
C       end do
C       accept reply.
C       Check if reply is valid and set loop termination flag
C         appropriately.
C     end do
C   else running in batch.
C     write prompt string
C     do for all responses
C       code up the output buffer.
C       send buffer to environment.
C     end do
C     accept reply
C     check if valid
C     If response invalid take default.
C   end if
C
C  A C Davenhall./ROE/                                   21/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NUMCHOI,STATUS
      CHARACTER*(*) DESCR(NUMCHOI),RESPS(NUMCHOI)
      CHARACTER*(*) CONID,PROMPT,REPLY
C
      INTEGER IOSTAT,INTSTAT
C
      INTEGER MAXCHOI,NCHOICE
      PARAMETER (MAXCHOI=100)
      INTEGER RESLEN(MAXCHOI)
      CHARACTER RESPSW(MAXCHOI)*10
C
      INTEGER INDEX,INDEX1,INDEX2,LENGTH,LENGTH1
      LOGICAL VALID,BATCHT,ABBR
      CHARACTER REPBUFF*10,PROMPT1*65
C
      CHARACTER SHORT*1
      PARAMETER (SHORT='#')
C
C
      IF (STATUS.EQ.0) THEN
        IOSTAT=0
        NCHOICE=MIN(NUMCHOI,MAXCHOI)
C
C    Determine the length of each response in its most 
C    abbreviated form
C
        ABBR=.FALSE.
        DO INDEX=1,NCHOICE
          RESPSW(INDEX)=RESPS(INDEX)
          CALL STRLEN (1,RESPS(INDEX),RESLEN(INDEX))
          DO INDEX1=RESLEN(INDEX),2,-1
            IF (RESPS(INDEX)(INDEX1:INDEX1).EQ.SHORT) THEN
              ABBR=.TRUE.
              DO INDEX2=1+INDEX1,RESLEN(INDEX)
                RESPSW(INDEX)(INDEX2-1:INDEX2-1)=
     :                          RESPSW(INDEX)(INDEX2:INDEX2)
              END DO
              RESPSW(INDEX)(RESLEN(INDEX):RESLEN(INDEX))=' '
              RESLEN(INDEX)=INDEX1-1
            END IF
          END DO
        END DO
C
C    Check whether running in batch or interactively.
C
        CALL BATCH (BATCHT)
        IF (.NOT.BATCHT) THEN
C
C    Running interactively.
C
          VALID=.FALSE.
          DO WHILE (.NOT.VALID)
            CALL CHOICE1 (PROMPT,NCHOICE,DESCR,RESPSW,IOSTAT)
            IF (ABBR) THEN
              PROMPT1=
     :  'Enter required option (some abbreviations permitted);'
            ELSE
              PROMPT1='Enter required option;'
            END IF
            CALL READC (CONID,PROMPT1,
     :                  ' ',' ','~',REPBUFF,IOSTAT)
            CALL UPPCAS (REPBUFF)
            IF (REPBUFF.EQ.' ') REPBUFF=RESPSW(1)
            CALL STRLEN (1,REPBUFF,LENGTH1)
            DO INDEX=1,NCHOICE
              LENGTH=RESLEN(INDEX)
              LENGTH=MAX(LENGTH,LENGTH1)
              IF (REPBUFF(1:LENGTH).EQ.RESPSW(INDEX)(1:LENGTH))
     :                THEN
                VALID=.TRUE.
                REPLY=RESPSW(INDEX)
              END IF
            END DO
          END DO
          STATUS=IOSTAT
        ELSE
C
C    Running in batch.
C
          CALL CHOICE1 (PROMPT,NCHOICE,DESCR,RESPSW,IOSTAT)
          CALL READC (CONID,'Give required option;',
     :                ' ',' ','~',REPBUFF,IOSTAT)
          CALL UPPCAS (REPBUFF)
          INTSTAT=1
          CALL STRLEN (1,REPBUFF,LENGTH1)
          REPLY=RESPSW(1)
          DO INDEX=1,NCHOICE
            LENGTH=RESLEN(INDEX)
            LENGTH=MAX(LENGTH,LENGTH1)
            IF (REPBUFF(1:LENGTH).EQ.RESPSW(INDEX)(1:LENGTH))
     :                 THEN
              REPLY=RESPSW(INDEX)
              INTSTAT=0
            END IF
          END DO
          STATUS=MAX(INTSTAT,IOSTAT)
        END IF
      ELSE
        REPLY=' '
      END IF
C 
      END
