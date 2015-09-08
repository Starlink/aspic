      SUBROUTINE USERNAME (USER)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE USERNAME 
C
C
C         It returns the user name of the current process. 
C
C         USER        Char  Out   The required user name. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                               
                                                                                
                                                                                
                                                                                
C ..RETURNS THE USERNAME OF THE CURRENT PROCESS
C
C ..ARGUMENT......CHARACTER VARIABLE
C
      INTEGER SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      CHARACTER*(*) USER
      PARAMETER(JPI$_USERNAME='202'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=LEN(USER)
      ITEMS(2)=JPI$_USERNAME
      ITEM(2)=%LOC(USER)
      ITEM(3)=%LOC(LENGTH)
      ITEM(4)=0
C
C ..USE $GETJPI
C
      SUCCESS=SYS$GETJPI(,,,ITEM,,,)
      IF(.NOT.SUCCESS)THEN
      CALL SYS$EXIT(%VAL(SUCCESS))
      ENDIF
      RETURN
      END
