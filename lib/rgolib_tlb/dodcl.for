      SUBROUTINE DO_DCL(COMMAND,STATUS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE DO_DCL
C
C
C         It creates a sub-process which executes a specified DCL  command. 
C         Control is returned to the calling process only on termination or 
C         error. 
C
C         There is an alternative - the library utility LIB$DO_COMMAND (see 
C         RUN  TIME REFERENCE Manual P3-8) but be warned that if this works 
C         it forces an unconditional exit from the calling program. 
C
C         COMMAND     CHAR  IN    This contains the command to be executed. 
C
C         STATUS      I*4   OUT   This contains the status return from  the 
C                                 command  - a zero return is "goo If there 
C                                 is an error, then the status  comes  from 
C                                 the create process comm 
C
C
C         W F Lupton               RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 



C                                      
C      ROUTINE TO CREATE A SUBPROCESS TO EXECUTE THE DCL COMMAND IN
C      'COMMAND'. THE CALLING PROCESS AWAITS TERMINATION OF THE
C      SUB-PROCESS AND THEN RETURNS THE STATUS FROM THE TERMINATION
C      MAILBOX. IF THERE IS AN ERROR IN CREATING THE SUB-PROCESS, THEN
C      STATUS CONTAINS THE $CREPRC STATUS INSTEAD
C
C
C      ARGUMENTS
C            COMMAND      DCL COMMAND TO BE EXECUTED (CHARACTER)
C            STATUS      RETURN STATUS (INTEGER)
C
C      NOV 20 1981 - ALTERED SO THAT I/O CHANNELS ARE DEASSIGNED UPON
C      RECEIPT OF THE TERMINATION MESSAGE FROM THE SUBPROCESS. THUS
C      TEMPORARY MAILBOXES WILL IMMEDIATELY BE DELETED, RATHER THAN,
C      AS PREVIOUSLY, ONLY ON IMAGE EXIT.
C
C      RGO WFL MAR 81
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) COMMAND
      CALL START_DCL(COMMAND,STATUS)
      IF (STATUS) THEN
            CALL STOP_DCL(STATUS)
      ENDIF
C
C      RETURN
C
      RETURN
      END
