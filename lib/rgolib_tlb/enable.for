      SUBROUTINE ENABLE
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE ENABLE 
C
C
C         It enables Control/C interrupts, which can subsequently be tested 
C         by using the function CTRLC (qv). Its use is described in LUN 11. 
C
C
C         W F Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 



      COMMAN /INTERRUPT/ITTCHAN,INTFLAG
      INTFLAG=0
      CALL SETAST
      RETURN
      END
C
