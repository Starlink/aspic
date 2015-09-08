      SUBROUTINE SETAST
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE SETAST 
C
C
C         This is a routine used by the Control/C package described in  LUN 
C         11. It is not normally called by the user. 
C
C
C         W F Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
                                                                                
                                                                                
                                                                                
      COMMON /INTERRUPT/ITTCHAN,INTFLAG
      INTEGER SUCCESS,SYS$QIOW
      EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,AST1
      SUCCESS=SYS$QIOW(,%VAL(ITTCHAN),%VAL(IOR(%LOC(IO$_SETMODE),
     +      %LOC(IO$M_CTRLCAST))),,,,AST1,,,,,)
      IF (.NOT.SUCCESS) CALL SYS$EXIT(%VAL(SUCCESS))
      RETURN
      END
C
