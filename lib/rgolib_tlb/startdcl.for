      SUBROUTINE START_DCL(COMMAND,STATUS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C        SUBROUTINE START_DCL
C
C
C         This routine initiates a sub-process to execute a DCL command. It 
C         is called by DO_DCL, and should not be used in its own right. 
C
C         COMMAND     CHAR  IN    The command to be exectued. 
C
C         STATUS      I*4   OUT   The status return. 
C
C
C         W F Lupton et al         RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                         
                                                                                
                                                                                
                                                                                
C       ROUTINE TO CREATE A SUBPROCESS TO EXECUTE THE DCL COMMAND IN
C      'COMMAND'. THE CALLING PROCESS DOES NOT AWAIT TERMINATION OF THE
C      SUB-PROCESS.
C
C      STATUS CONTAINS THE SYSTEM SERVICE RETURN STATUS
C
C      ARGUMENTS
C            COMMAND      DCL COMMAND TO BE EXECUTED (CHARACTER)
C            STATUS      RETURN STATUS (INTEGER)
C
C      RGO WFL MAR 81
C
C      In the event of the routine being unable to complete its task, it
C      deassigns any mailbox(es) previously assigned before returning to
C      the calling routine.         B M Harris    18.10.83
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 CHAN,MBXUNIT,INCHAN
      CHARACTER*(*) COMMAND
      CHARACTER CHANBUFF*44
      EQUIVALENCE (CHANBUFF(13:14),MBXUNIT)
      COMMON/DO_DCLCOMM/INCHAN,CHAN
      SAVE/DO_DCLCOMM/
      EXTERNAL IO$_WRITEVBLK,IO$_READVBLK
C
C      FIRST OF ALL, CREATE A MAILBOX TO RECEIVE THE TERMINATION MESSAGE
C
      STATUS=SYS$CREMBX(,CHAN,,,,,'TERMBOX')
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','CREMBX STATUS IS',STATUS
            RETURN
      ENDIF
C
C      NOW GET THE CHANNEL NUMBER OF THE MAILBOX
C
      STATUS=SYS$GETCHN(%VAL(CHAN),,CHANBUFF,,)
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','GETCHN STATUS IS',STATUS
            RETURN
      ENDIF
C
C      NOW CREATE A MAILBOX TO HOLD THE INPUT COMMAND
C
      STATUS=SYS$CREMBX(,INCHAN,,,,,'INBOX')
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','CREMBX STATUS IS',STATUS
C
C      If there is an error here, deassign the mailbox previously set up
C      and assigned. Retain the original value of STATUS to return it to
C      the calling routine.
C
            STAT   = STATUS
            STATUS = SYS$DASSGN(%VAL(CHAN))
            IF (.NOT.STATUS) THEN
                  PRINT '(1X,A,Z8)','DASSGNB STATUS IS',STATUS
            ENDIF
            STATUS = STAT
            RETURN
      ENDIF
      STATUS=SYS$QIO(,%VAL(INCHAN),IO$_WRITEVBLK,,,,
     1       %REF(COMMAND),%VAL(LEN(COMMAND)),,,,)
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','QIO STATUS IS',STATUS
            RETURN
      ENDIF
      STATUS=SYS$QIO(,%VAL(INCHAN),IO$_WRITEVBLK,,,,
     1       %REF('$EXIT'),%VAL(5),,,,)
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','QIO STATUS IS',STATUS
            RETURN
      ENDIF
C
C      NOW CREATE THE SUB-PROCESS
C
      STATUS=SYS$CREPRC(,'SYS$SYSTEM:LOGINOUT','INBOX','_NL:',
     1       '_NL:',,,,%VAL(4),,%VAL(MBXUNIT),)
      IF (.NOT.STATUS) THEN
            PRINT '(1X,A,Z8)','CREPRC STATUS IS',STATUS
C
C      If there is an error here, deassign both the mailboxes previously
C      set up and assigned. Retain the value of STATUS to return  it  to
C      the calling routine.
C
            STAT   = STATUS
            STATUS = SYS$DASSGN(%VAL(INCHAN))
            IF (.NOT.STATUS) THEN
                  PRINT '(1X,A,Z8)','DASSGNB STATUS IS',STATUS
            ENDIF
            STATUS = SYS$DASSGN(%VAL(CHAN))
            IF (.NOT.STATUS) THEN
                  PRINT '(1X,A,Z8)','DASSGNB STATUS IS',STATUS
            ENDIF
            STATUS = STAT
            RETURN
      ENDIF
C
C      RETURN
C
      RETURN
      END
