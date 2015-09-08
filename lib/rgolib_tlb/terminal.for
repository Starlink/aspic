      SUBROUTINE TERMINAL (TERM)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE TERMINAL 
C
C
C         It returns the name of the (process) terminal. 
C
C         TERM        CHAR  OUT   The required name. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                               
                                                                                
                                                                                
                                                                                
C ..RETURNS THE NAME OF THE PROCESS INTERACTIVE TERMINAL
C
C ..ARGUMENT......CHARACTER VARIABLE
C
      INTEGER SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      CHARACTER*(*) TERM
      PARAMETER(JPI$_TERMINAL='31D'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=LEN(TERM)
      ITEMS(2)=JPI$_TERMINAL
      ITEM(2)=%LOC(TERM)
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
