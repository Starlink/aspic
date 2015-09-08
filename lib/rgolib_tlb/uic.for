      SUBROUTINE UIC (GROUP,MEMBER)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE UIC 
C
C
C         It  returns  the  two  parts  of  the  User  Identification  Code 
C         (Group,Member) as two integers. 
C
C         GROUP       I*4   OUT   The group part of the UIC. 
C
C         MEMBER      I*4   OUT   The member part of the UIC. 
C
C
C         A C Harter               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                  
                                                                                
                                                                                
                                                                                
C ..RETURNS GROUP AND UIC NUMBER AS STANDARD OCTAL VALUES
C
C ..ARGUMENT......2 INTEGERS
C
      INTEGER GROUP,MEMBER,SYS$GETJPI,SUCCESS,SYS$EXIT
      INTEGER ITEM(4)
      INTEGER*2 ITEMS(2)
      PARAMETER(JPI$_GRP='308'X)
      PARAMETER(JPI$_MEM='307'X)
      EQUIVALENCE (ITEM(1),ITEMS(1))
C
C ..SET UP GROUP ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_GRP
      ITEM(2)=%LOC(GROUP)
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
C ..SET UP MEMBER ITEM LIST
C
      ITEMS(1)=4
      ITEMS(2)=JPI$_MEM
      ITEM(2)=%LOC(MEMBER)
      ITEM(3)=%LOC(LENGTH)
      ITEM(4)=0
C
C ..USE $GETJPI
C
      SUCCESS=SYS$GETJPI(,,,ITEM,,,)
      IF(.NOT.SUCCESS)THEN
      CALL SYS$EXIT(%VAL(SUCCESS))
      ENDIF
      ENDIF
      RETURN
      END
