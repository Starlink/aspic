      SUBROUTINE PROCESS (PRCNAM)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE PROCESS 
C
C
C         It returns the name of the current process. 
C
C         PRCNAM      CHAR  OUT   The required name. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                
                                                                                
                                                                                
                                                                                
C ..RETURNS THE CURRENT PROCESS NAME
C
C ..ARGUMENT......CHARACTER VARIABLE
C
      INTEGER SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      CHARACTER*(*) PRCNAM
      PARAMETER(JPI$_PRCNAM='31C'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=LEN(PRCNAM)
      ITEMS(2)=JPI$_PRCNAM
      ITEM(2)=%LOC(PRCNAM)
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
