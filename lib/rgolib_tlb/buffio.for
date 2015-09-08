      SUBROUTINE BUFFIO (BUFF_IO)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE BUFFIO 
C
C
C         It returns  a  count  of  the  number  of  buffered  input/output 
C         transfers performed. 
C
C
C         Parameter
C
C         BUFF_IO	I*4	OUT	The number of transfers performed.
C
C         A C Harter               RGO                             9-AUG-82 
C
C-------------------------------------------------------------------------- 



C                                
C ..RETURN THE BUFFERED INPUT OUTPUT COUNT
C
C ..ARGUMENT......INTEGER
C
      INTEGER BUFF_IO,SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      PARAMETER(JPI$_BUFIO='40C'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_BUFIO
      ITEM(2)=%LOC(BUFF_IO)
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
