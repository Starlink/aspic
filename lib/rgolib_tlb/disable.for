      SUBROUTINE DISABLE
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE DISABLE 
C
C
C         It disables control/C interrupts. Its use is described in LUN 11. 
C
C
C         W H Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
      COMMON /INTERRUPT/ITTCHAN,INTFLAG
      INTEGER SUCCESS,SYS$CANCEL
      INTFLAG=0
      SUCCESS=SYS$CANCEL(%VAL(ITTCHAN))
      IF (.NOT.SUCCESS) CALL SYS$EXIT(%VAL(SUCCESS))
      RETURN
      END
C
