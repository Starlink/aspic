      SUBROUTINE CHOICE1 (PROMPT,NCHOICE,DESCR,RESPS,STATUS)
C+
C     CHOICE1.
C
C     Subroutine to send a series of option codes, with
C     descriptions to the environment.
C
C  Given;
C   PROMPT  (C)  Overall prompt string.
C   NCHOICE (I)  No. of choices to be output.
C   DESCR   (CA) Description for each choice.
C   RESPS   (CA) Option for each choice.
C
C  Returned;
C   STATUS  (I)  Return status, = 0 for success.
C
C  Subroutines called;
C   Interfaces:-   OUTPUT.
C   Misc.:-        STRLEN.
C
C  A C Davenhall./ROE/                             21/7/83.
C-
      IMPLICIT NONE
C
      INTEGER NCHOICE,STATUS
      CHARACTER*(*) PROMPT,DESCR(NCHOICE),RESPS(NCHOICE)
C
      CHARACTER OUTBUFF*72,WORKBUFF*7
      PARAMETER (WORKBUFF='  ...  ')
C
      INTEGER IOSTAT
      INTEGER INDEX,WORK,LENGTH,CHOICE
C
C
      IF (STATUS.EQ.0) THEN
        IOSTAT=0
        CALL OUTPUT (PROMPT,IOSTAT)
        CALL OUTPUT ('  ',IOSTAT)
        DO CHOICE=1,NCHOICE
          OUTBUFF=' '
          DO INDEX=1,10
            WORK=((INDEX-1)*6)+1
            OUTBUFF(WORK:WORK+6)=WORKBUFF
          END DO
          IF (CHOICE.EQ.1) OUTBUFF(51:59)='(default)'
          CALL STRLEN (1,DESCR(CHOICE),LENGTH)
          OUTBUFF(1:LENGTH)=DESCR(CHOICE)(1:LENGTH)
          CALL STRLEN (1,RESPS(CHOICE),LENGTH)
          OUTBUFF(61:61+LENGTH-1)=RESPS(CHOICE)(1:LENGTH)
          CALL OUTPUT (OUTBUFF,IOSTAT)
        END DO
        CALL OUTPUT ('  ',IOSTAT)
C
        STATUS=IOSTAT
C
      END IF
C
      END
