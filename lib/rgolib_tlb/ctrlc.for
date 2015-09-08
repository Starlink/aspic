      LOGICAL FUNCTION CTRLC(I)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         FUNCTION CTRLC 
C
C
C         It is a LOGICAL function which returns the value TRUE if the user 
C         has  pressed   CONTROL/C  since  interrupts  were  last  enabled. 
C         Otherwise it returns the value FALSE. Its use is described in LUN 
C         11. 
C
C         I           I*4         In normal use this is a  dummy  argument, 
C                                 and a value of 0 may be saafely used. 
C
C
C         W F Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
      COMMON /INTERRUPT/ITTCHAN,INTFLAG
      IF (INTFLAG.EQ.0) THEN
            CTRLC=.FALSE.
      ELSE
            CTRLC=.TRUE.
            INTFLAG=0
      ENDIF
      RETURN
      END
