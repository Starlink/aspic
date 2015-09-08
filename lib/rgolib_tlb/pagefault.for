      SUBROUTINE PAGEFAULT (PGFLTS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE PAGEFAULT 
C
C
C         This returns the number of page faults (ie use of virtual memory) 
C
C         PGFLTS      I*4   OUT   The (integer) number of page faults which 
C                                 have occurred. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                   
                                                                                
                                                                                
                                                                                
C ..RETURNS PAGEFAULT COUNT
C
C ..ARGUMENT......INTEGER
C
      INTEGER PGFLTS,SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      PARAMETER(JPI$_PAGEFLTS='40A'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_PAGEFLTS
      ITEM(2)=%LOC(PGFLTS)
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
