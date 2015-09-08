***************************************
*  ANSWER: Subroutine to read answers
***************************************
*
*-----Returns 1 for Yes, 0 for No
*-----and 2 for 'Return'
*-----Calls HELP for input 'h'
*
      SUBROUTINE CRB_ANSWER(STRING,IANS)

      CHARACTER*1 BLANK
      CHARACTER*1 YES(11),GOOSE
      CHARACTER*(*) STRING

      DATA BLANK/' '/
      DATA YES/'Y','y','1','S','s','R','r','O','o','P','p'/

      GOOSE=BLANK

      IANS=0
*
*-----Calculate string length
*
      CALL CRB_STRLEN(STRING,LEN)

      PRINT*,' '
      PRINT 101,STRING(1:LEN)

101   FORMAT(' ',A<LEN>,' ',$)
*
*-----Read input from terminal
*
      READ(5,102,END=103) GOOSE

102   FORMAT(A1)
*
*-----Check to see if HELP library required
*
      IF(GOOSE.EQ.'H'.OR.GOOSE.EQ.'h') THEN
         CALL CRB_HELP
         PRINT 101,STRING(1:LEN)
         READ(5,102,END=103) GOOSE
      ELSE
      ENDIF

      IF(GOOSE.EQ.BLANK) IANS=2

103   DO  I=1,11
         IF(GOOSE.EQ.YES(I)) IANS=1
      ENDDO
      END

