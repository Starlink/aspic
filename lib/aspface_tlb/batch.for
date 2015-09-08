*+  BATCH - Determine whether running in batch or interactively.
      SUBROUTINE BATCH (VALUE)
*    Description :
*     Examines whether the process is being run in batch or
*     interactively.
*    Invocation :
*     CALL BATCH (VALUE)
*    Parameters :
*     VALUE  =  LOGICAL (WRITE)
*           Flag determining whether operating in batch or
*           interactively;
*           - .TRUE.  =  In batch.
*           - .FALSE. =  Interactively.
*    Method :
*     Interogate VMS to determine the method of working.
*    Deficiencies :
*     Uses system services calls (off necessity).
*    Bugs :
*     None known.
*    Authors :
*     B D Kelly.      (ROE::BDK)
*     S M Beard.      (ROE::SMB)
*     A C Davenhall.  (ROE::ACD)
*     A C Davenhall.  (UOE::CAST::EXPL29)
*    History :
*     16/3/83:   Original version.                          (ROE::BDK)
*     23/6/84:   Modified to include extra terminal types.  (ROE::SMB)
*     1/30/85:   Converted to "SSE" style.                  (ROE::ACD)
*     29/3/85:   The test causing the call to LIB$SIGNAL    (CAST::ACD)
*                reversed so that it is called in the
*                correct circumstances (ie. bug fix).
*    Type Definitions :
      IMPLICIT NONE
*    Export :
      LOGICAL
     :  VALUE          ! Flag; operating in batch or interactively.
*    External references :
      INTEGER
     :  SYS$TRNLOG     ! Systems service call.
*    Local variables :
      INTEGER
     :  ISTAT          ! Status from system services calls.
      CHARACTER
     :  OUTPUT*20      ! Output from system services calls.
*-

      ISTAT = SYS$TRNLOG ('SYS$COMMAND', , OUTPUT, , ,)
      IF (MOD(ISTAT, 2) .EQ. 0) CALL LIB$SIGNAL (%VAL(ISTAT))

      IF ( (OUTPUT(5:8) .EQ. '__TT') .OR.
     :     (OUTPUT(5:8) .EQ. '__RT') .OR.
     :     (OUTPUT(5:8) .EQ. '__NV')
     :   ) THEN
         VALUE = .FALSE.
      ELSE
         VALUE = .TRUE.
      END IF

      END
