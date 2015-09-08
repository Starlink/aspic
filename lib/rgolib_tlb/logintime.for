      SUBROUTINE LOGINTIME (LOGINTIM)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE LOGINTIME 
C
C
C         It returns the time that the current process has been  logged-in, 
C         in standard ASCII representation. 
C
C         LOGINTIM    CHAR  OUT   This is the required time., in the format 
C                                 HH:MM:SS.SS 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                    
                                                                                
                                                                                
                                                                                
C ..RETURNS THE LOGIN TIME OF THE CURRENT PROCESS
C ..IN STANDARD ASCII SYSTEM TIME FORMAT
C
C ..ARGUMENT......CHARACTER VARIABLE
C
      INTEGER SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4),TIME(2)
      INTEGER*2 ITEMS(2)
      CHARACTER*(*) LOGINTIM
      PARAMETER(JPI$_LOGINTIM='206'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=8
      ITEMS(2)=JPI$_LOGINTIM
      ITEM(2)=%LOC(TIME)
      ITEM(3)=%LOC(LENGTH)
      ITEM(4)=0
C
C ..USER $GETJPI
C
      SUCCESS=SYS$GETJPI(,,,ITEM,,,)
      IF(.NOT.SUCCESS)THEN
      CALL SYS$EXIT(%VAL(SUCCESS))
      ELSE
C
C ..$GETJPI RETURNS THE TIME AS A 64 BIT SYSTEM TIME
C ..SO CONVERT TO ASCII TIME USING $ASCTIM
C
      SUCCESS=SYS$ASCTIM(,LOGINTIM,TIME,)
      IF((.NOT.SUCCESS).AND.(SUCCESS.NE.'00000000'X))THEN
      CALL SYS$EXIT(%VAL(SUCCESS))
      ENDIF
      ENDIF
      RETURN
      END
