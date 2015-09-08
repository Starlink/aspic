      SUBROUTINE CPUTIME (TIME)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE CPUTIME 
C
C
C         It returns the  number  of  seconds  since  the  current  process 
C         started. 
C
C         TIME        I*4   OUT   The (integer) number of seconds. 
C
C
C         A C Harter               RGO                             9-AUG-82 
C
C-------------------------------------------------------------------------- 


C                                 
C ..RETURNS THE CPUTIME USED SINCE PROCESS
C ..START-UP IN WHOLE SECOND UNITS
C
C ..ARGUMENT......INTEGER
C
      INTEGER TIME,SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      PARAMETER(JPI$_CPUTIM='407'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_CPUTIM
      ITEM(2)=%LOC(TIME)
      ITEM(3)=%LOC(LENGTH)
      ITEM(4)=0
C
C ..USE $GETJPI
C
      SUCCESS=SYS$GETJPI(,,,ITEM,,,)
      IF(.NOT.SUCCESS)THEN
      CALL SYS$EXIT(%VAL(SUCCESS))
      ELSE
C
C ..$GETPJI GIVES CPUTIME IN 10 MILLISECOND TICS
C ..SO CONVERT TO SECONDS
C
      TIME=TIME/100
      ENDIF
      RETURN
      END
