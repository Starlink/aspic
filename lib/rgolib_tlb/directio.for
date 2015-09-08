      SUBROUTINE DIRECTIO (DIRECT)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE DIRECTIO 
C
C
C         It  returns  to  the  user  the  number  of  direct  input/output 
C         transfers which have taken place. 
C
C         DIRECT      I*4   OUT   The number of transfers which have  taken 
C                                 place. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 



C                                 
C ..RETURNS DIRECT INPUT OUTPUT COUNT
C
C ..ARGUMENT......INTEGER
c
      INTEGER DIRECT,SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      PARAMETER(JPI$_DIRIO='40B'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_DIRIO
      ITEM(2)=%LOC(DIRECT)
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
