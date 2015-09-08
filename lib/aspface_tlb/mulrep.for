      SUBROUTINE MULREP (MESSG,PRMPTS,REPLY,ISTAT)
C+
C     MULREP.
C
C     Subroutine to interogate the input stream for one of a choice
C     of previously selected responses. If the program is running
C     interactively interogation continues until a valid response
C     is given (ve haf vays of making you give valid responses).
C     Alternatively if the program is running in batch mode the
C     input stream is interogated once only and if the response
C     is invalid control is returned to the calling routine with
C     a non-zero return status.
C     The first of the permitted responses is adopted as the default.
C
C  Given;
C   MESSG   (C)  Explanatory and instructional message for the user.
C   PRMPTS  (C)  List of the permitted valid responses, in contigous
C                elements, starting at the first, separated by
C                commas and terminated by a $.
C
C  Returned;
C   REPLY   (C)  The selected reply if no abnormal return status has
C                been raised, otherwise the default reply.
C   ISTAT  (I)  Status returned = max(status input, internal status)
C
C  Subroutines called;
C   Interfaces:-   BATCH, OUTPUT, READC, UPPCAS.
C
C  Structure;
C   Decode the individual valid responses from the argument list.
C   If running interactively
C     Issue a read for the parameter value repeatedly until a valid
C     response is given, or an exceptional status is raised.
C   else if running in batch
C     Issue a read once and accept the reply
C     check if valid and if not set return status
C   end if
C
C  A C Davenhall./ROE/                                       15/10/81.
C  A C Davenhall./ROE/    {Modified}                        27/7/82.
C-
      CHARACTER MESSG*(*),PRMPTS*(*),REPLY*(*)
      INTEGER   ISTAT
C
      CHARACTER PROMPT*(80),ANSWR*(20),DFAULT*(20)
      INTEGER ZTART(100),ZTOP(100)
      INTEGER NPRMPT,STATUS,NC,STAT1,IB
      LOGICAL VALID,BATCHT
C
      CHARACTER COMMA*1,SCOLON*1,DOLLAR*1
      PARAMETER (COMMA=',')
      PARAMETER (SCOLON=';')
      PARAMETER (DOLLAR='$')
C
C
C    Decode the individual valid responses from the argument list.
C
      NPRMPT=1
      NC=1
      ZTART(1)=1
      DO WHILE (PRMPTS(NC:NC).NE.DOLLAR)
        IF (PRMPTS(NC:NC).EQ.COMMA) THEN
          ZTOP(NPRMPT)=NC-1
          ZTART(NPRMPT+1)=NC+1
          NPRMPT=NPRMPT+1
        END IF
        NC=NC+1
      END DO
      ZTOP(NPRMPT)=NC-1
C
C    Set the default to be the first response in the list.
C
      DFAULT=PRMPTS(ZTART(1):ZTOP(1))
C
C    Check whether running in batch or interactive mode.
C
      CALL BATCH (BATCHT)
      IF (.NOT.BATCHT) THEN
C
C    Running interactively.
C
C    Clear the prompt buffer.
C
        PROMPT=' '
C
C    Load the prompt buffer with the list of valid responses
C    for the user to choose between. The prompts are
C    separated by commas and terminated by a semi-colon for
C    cosmetic reasons.
C
        PROMPT(1:12)=' Enter 1 of '
        PROMPT(13:NC+12)=PRMPTS(1:NC)
        PROMPT(NC+12:NC+12)=SCOLON
C
C    Write the instructional message to the user.
C
        CALL OUTPUT (MESSG,STAT1)
C
C    Issue the request for the parameter repeatedly until
C    an acceptable answer is received.
C
        VALID=.FALSE.
        STATUS=0
        DO WHILE (.NOT.VALID.AND.STATUS.EQ.0)
          CALL READC ('REPLY',PROMPT,DFAULT,' ','}',
     :                ANSWR,STATUS)
          CALL UPPCAS (ANSWR)
          DO I=1,NPRMPT
            IF (ANSWR.EQ.PRMPTS(ZTART(I):ZTOP(I))) VALID=.TRUE.
          END DO
        END DO
        IF (STATUS.EQ.0) THEN
          REPLY=ANSWR
        ELSE
          REPLY=DFAULT
        END IF
        ISTAT=MAX(ISTAT,STATUS)
      ELSE
C
C    Running in batch mode.
C
        CALL OUTPUT (MESSG,STAT1)
        CALL READC ('REPLY',PROMPT,DFAULT,' ','}',
     :              ANSWR,STATUS)
        CALL UPPCAS (ANSWR)
        VALID=.FALSE.
        DO I=1,NPRMPT
          IF (ANSWR.EQ.PRMPTS(ZTART(I):ZTOP(I))) VALID=.TRUE.
        END DO
        IF (VALID) THEN
          REPLY=ANSWR
        ELSE
          REPLY=DFAULT
          STATUS=1
        END IF
        ISTAT=MAX(ISTAT,STATUS)
      END IF
      END
