      SUBROUTINE IMAGENAME (IMNAME)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE IMAGENAME 
C
C
C         It returns the name of the  (executable)  image  currently  being 
C         executed. 
C
C         IMNAME      CHAR  OUT   The required name. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 



C                                  
C ..RETURNS THE NAME OF THE IMAGE CURRENTLY BEING EXECUTED
C
      INTEGER SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      CHARACTER*(*) IMNAME
      PARAMETER(JPI$_IMAGNAME='207'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=LEN(IMNAME)
      ITEMS(2)=JPI$_IMAGNAME
      ITEM(2)=%LOC(IMNAME)
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
