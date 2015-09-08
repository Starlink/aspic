      SUBROUTINE SETUP(DEV)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE SETUP 
C
C
C         This subroutine MUST be called before any of the other  Control/C 
C         routines  (ENABLE,CTRLC,DISABLE)  as  it sets up the mechanics of 
C         the operation. Its use is described in LUN 11. 
C
C         DEV         CHAR  IN    It  defines   the   device   from   which 
C                                 interrupts are to be accepted. 
C
C
C         W F Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
                                                                                
                                                                                
                                                                                
      COMMON /INTERRUPT/ITT
      INTEGER SUCCESS,SYS$ASSIGN
      CHARACTER*(*) DEV
      DO I=LEN(DEV),1,-1
         IF (DEV(I:I).NE.' ') GO TO 10
      END DO
   10 CONTINUE
      SUCCESS=SYS$ASSIGN(DEV(:I),ITT,,)
      IF (.NOT.SUCCESS) CALL SYS$EXIT(%VAL(SUCCESS))
      END
