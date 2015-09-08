      SUBROUTINE STOP_DCL(STATUS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE STOP_DCL 
C
C
C         It waits for completion of a sub-process (creaed  by  START_DCL). 
C         It is called by DO_DCL, and should not be used in its own right. 
C
C         STATUS      I*4   OUT   The status return. 
C
C
C         W F Lupton               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                
                                                                                
                                                                                
                                                                                
C       SUBROUTINE TO WAIT FOR THE COMPLETION OF THE SUBPROCESS
C      CREATED BY START_DCL
C
C      ARGUMENTS
C      STATUS      INTEGER      RETURN STATUS FROM THE DCL COMMAND
C
C      WFL RGO MAR 81
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 INCHAN,CHAN
      CHARACTER MAILBUFF*84
      EQUIVALENCE (MAILBUFF(5:8),MSTATUS)
      EXTERNAL IO$_READVBLK
      COMMON/DO_DCLCOMM/INCHAN,CHAN
      SAVE/DO_DCLCOMM/
C
C      NOW ISSUE A READ ON THE MAILBOX (WON'T TERMINATE UNTIL THE
C      SUBPROCESS HAS TERMINATED)
C
      STATUS=SYS$QIO(%VAL(1),%VAL(CHAN),IO$_READVBLK,,,,
     1       %REF(MAILBUFF),%VAL(84),,,,)
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','QIO STATUS IS',STATUS
            RETURN
      ENDIF
      STATUS=SYS$WAITFR(%VAL(1))
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','WAITFR STATUS IS',STATUS
            RETURN
      ENDIF
C
C      CAN NOW DE-ASSIGN THE I/O CHANNELS TO BOTH THE MAILBOX USED FOR
C      HOLDING THE DCL COMMAND (INCHAN) AND THE TERMINATION BOX (CHAN)
C
      STATUS=SYS$DASSGN(%VAL(INCHAN))
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','DASSGN STATUS IS',STATUS
            RETURN
      ENDIF
      STATUS=SYS$DASSGN(%VAL(CHAN))
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','DASSGN STATUS IS',STATUS
            RETURN
      ENDIF
      STATUS=MSTATUS
C
C      AND RETURN
C
      RETURN
      END
