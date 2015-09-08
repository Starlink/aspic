      SUBROUTINE RDSTRR(STRING,VALS,MAXVAL,NVAL,ILEVEL,IERR)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO EXTRACT REAL VALUES FROM A CHARACTER STRING
*SOURCE
*       RDSTRR.FOR in BDFGEN.TLB
*
*METHOD
*       THE STRING IS EXPECTED TO HAVE ANY REAL VALUES AT THE START
*       BEFORE ANY COMMENT CHARACTER ('!'). IF A COMMENT CHARACTER
*       IS ENCOUNTERED THEN TRANSLATION STOPS AND THE REST OF THE
*       STRING IS IGNORED. ANY REAL VALUES WHICH OCCUR BEFORE A
*       COMMENT CHARACTER ARE TO BE SEPERATED BY ANY NUMBER OF
*       SEPERATOR CHARACTERS (TAB " "  ","  ";"  ":" "/"). IF A FIELD
*       IS FOUND BEFORE A COMMENT CHARACTER, WHICH CANNOT BE CONVERTED
*       INTO A REAL VALUE (UNTRANSLATABLE) THEN TRANSLATION STOPS,
*       IERR IS SET AND IF REQUIRED, A MESSAGE IS SENT TO THE
*       USER IDENTIFYING THE UNTRANSLATABLE FIELD. IF ALL FIELDS
*       ARE EXHAUSTED BEFORE MAXVAL VALUES HAVE BEEN READ THEN IERR
*       IS SET AND NVAL RETURNS THE ACTUALL NUMBER OF VALUES FOUND.
*
*ARGUMENTS
*       STRING (IN)
*       CHARACTER*(*)
*              THE STRING EITHER BLANK OR CONTAINING FIELDS
*              CORRESPONDING TO REAL VALUES AND/OR A COMMENT FIELD.
*       VALS (OUT)
*       REAL(MAXVAL)
*              THE ARRAY TO HOLD THE REAL VALUES FOUND
*       MAXVAL (IN)
*       INTEGER
*              THE MAXIMUM NUMBER OF VALUES TO TRANSLATE TO REAL
*       NVAL (OUT)
*       INTEGER
*              THE ACTUAL NUMBER OF REAL VALUES TRANSLATED
*       ILEVEL (IN)
*       INTEGER
*              IF SET TO ONE OR MORE THEN A MESSAGE IS SENT IF AN
*              UNTRANSLATABLE FIELD IS FOUND, IDENTIFYING THE FIELD.
*       IERR (OUT)
*       INTEGER
*              ERROR FLAG: 0 - SUCCESS, MAXVAL VALUES TRANSLATED
*                          1 - LESS THAN MAXVAL REAL FIELDS FOUND
*                          2 - AN UNTRANSLATABLE FIELD FOUND
*
*CALLS
*       VAX:
*              LIB$SCANC,LIB$SPANC
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
C
C
      CHARACTER STRING*(*)
      REAL VALS(MAXVAL)
      BYTE TABLE(0:255)
C
C TABLE IS A BYTE TABLE USED BY LIB$SCANC AND LIB$SPANC TO IDENTIFY
C SEPERATOR CHARACTERS. IT HAS AN ELEMENT CORRESPONDING TO ALL 255
C POSSIBLE CHARACTERS WITH VALID SEPERATOR CHARACTERS SET TO 1 AND ALL
C OTHER CHARACTERS SET TO 0. IN ADDITION THE COMMENT CHARACTER "!" IS
C SET TO 1 AND ALL NONE PRINTABLE CHARACTERS SUCH AS "END OF TEXT" ETC
C
       DATA TABLE/34*1,10*0,1,2*0,1,10*0,2*1,67*0,129*1/
C
C INITIALISE ERROR FLAG
C
      IERR=0
C
C FIND THE POSITION WITHIN THE STRING OF THE FIRST COMMENT CHARACTER
C
      NCOM=INDEX(STRING,'!')
C
C IF NO COMMENT CHARACTER WAS FOUND SET NCOM TO THE END OF THE STRING.
C NCOM IS NOW THE POSITION OF THE FIRST CHARACTER BEYOND THE LAST REAL
C FIELD I.E. THE END OF THE USEABLE STRING
C
      IF(NCOM.EQ.0) NCOM=LEN(STRING)
C
C IST IS THE START OF THE FIELD CURRENTLY BEING TRANSLATED
C
      IST=1
C
C LOOP FOR THE MAXVAL VALUES
C
      DO IVAL=1,MAXVAL
C
C CHECK THAT THE START OF THE CURRENT FIELD IS NOT AT OR BEYOND THE
C END OF THE USEABLE STRING, NCOM.
C
         IF(IST.LT.NCOM) THEN
C
C CALL LIB$SPANC TO FIND THE FIRST CHARACTER IN THE CURRENT FIELD
C WHICH IS NOT A SEPERATOR
C
            NST=LIB$SPANC(STRING(IST:NCOM),TABLE,1)
C
C IF NO MORE NON-SEPERATORS EXIST, THEN REAL VALUES HAVE BEEN EXHAUSTED
C
            IF(NST.NE.0) THEN
C
C UPDATE THE START OF THE CURRENT FIELD TO THE POSITION OF THE FIRST
C NON-SEPERATOR (IF THE FIELD IS NOT IN ERROR, THIS SHOULD BE THE FIRST
C NUMERICAL CHARACTER)
C
               IST=IST+NST-1
C
C CALL LIB$SCANC TO FIND THE POSITION OF THE NEXT SEPERATOR. THIS
C MARKS THE END OF THE CURRENT FIELD.
C
               ISEP=LIB$SCANC(STRING(IST:NCOM),TABLE,1)+IST-1
C
C IST AND ISEP MARK THE BEGINNING AND END OF THE CURRENT FIELD AND SO
C SHOULD CORRESPOND TO A REAL VALUE. CALL CTOR TO ATTEMPT TO CONVERT
C IT TO A REAL VALUE
C
               CALL CTOR(STRING(IST:ISEP-1),VALS(IVAL),IERR)
C
C IF THE CURRRENT FIELD CANNOT BE TRANSLATED TO A REAL VALUE, SET
C IERR AND IF REQUIRED, SEND MESSAGE.
C
               IF(IERR.NE.0) THEN
                  NVAL=IVAL-1
                  IERR=2
                  IF(ILEVEL.GT.0) THEN
                     CALL WRUSER('***ERROR CONVERTING "'
     +               //STRING(IST:ISEP)//'" TO REAL',ISTAT)
                  ENDIF
                  GOTO 999
               ENDIF
C
C SET IST TO THE END OF THE CURRENT FIELD. THIS BECOMES THE START
C OF THE NEXT FIELD.
               IST=ISEP
            ELSE
C
C IF ONLY SEPERATOR CHARACTERS ARE LEFT, THEN SET THE NUMBER OF VALUES
C FOUND AND SET THE ERROR FLAG
C
               NVAL=IVAL-1
               IERR=1
               GOTO 999
            ENDIF
         ELSE
C
C LIKEWISE IF THE USEABLE STRING HAS BEEN EXHAUSTED
C
            NVAL=IVAL-1
            IERR=1
            GOTO 999
         ENDIF
C
C GO ROUND FOR NEXT FIELD
C
      ENDDO
C
C IF COMPLETED SUCCESSFULLY, SET THE NUMBER OF VALUES FOUND
C
      NVAL=MAXVAL
C
C FINISH
C
  999 RETURN
      END