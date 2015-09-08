*+  IMAGNAM - Obtain file-spec of image being executed by current process
      SUBROUTINE IMAGNAM( NAME, STATUS )
*    Description :
*     This returns the file specification of the image the current process is
*     executing.
*    Invocation :
*     CALL IMAGNAM( NAME, STATUS )
*    Parameters :
*     NAME   = CHARACTER*(*)( WRITE )
*           File specification of executable image. This can be up to
*           128 characters in size
*     STATUS = INTEGER( WRITE )
*           Status return. This will be returned zero if successful.
*    Method :
*    Deficiencies :
*    Bugs :
*     None known.
*    Authors :
*     B.V.McNally   (ROE::BMC)
*     S.M.Beard     (ROE::SMB)
*    History :
*     16/11/1984:  Original version.                        (ROE::BMC) 
*     16/11/1984:  SSE-style prologue                       (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE '($JPIDEF)'   ! JPI symbol definitions
*    Export :
      CHARACTER*(*)
     :  NAME        ! Image name
*    Status :
      INTEGER
     :  STATUS      ! Return status
*    External references :
      INTEGER
     :  SYS$GETJPI   ! System service Get Job/Process information routine
*    Global variables :
*    Local Constants :
      INTEGER MAXLEN ! Maximum possible length of NAME
      PARAMETER ( MAXLEN = 128 )
*    Local variables :
      INTEGER
     :  BUFADDR,     ! address of buffer
     :  LENADDR,     ! address of word to receive the returned buffer length
     :  LENGTH,      ! ??
     :  ISTAT        ! Local status
      INTEGER*2
     :  BUFLEN,      ! Length of buffer
     :  ITEM1,       ! item required
     :  ITEM2,       ! item terminator ( 0 )
     :  ITMLIS(7)    ! Item list for JPI

!     (Equivalence used to set up the itemlist)

      EQUIVALENCE (ITMLIS(1),BUFLEN),(ITMLIS(2),ITEM1),
     :(ITMLIS(3),BUFADDR),(ITMLIS(5),LENADDR),(ITMLIS(7),ITEM2)
*-

*   Initialise STATUS

      STATUS = 0

*  Initialize the buffer length available

      BUFLEN = MIN( MAXLEN, LEN(NAME) )

*      Initialise :-

*      the item required
         ITEM1 = JPI$_IMAGNAME

*      the item terminator ( 0 )
         ITEM2 = 0

*      the address of buffer
         BUFADDR = %LOC(NAME)

*      the address of word to receive the returned buffer length
         LENADDR = %LOC(LENGTH)

*      Obtain the item required
         ISTAT = SYS$GETJPI(,,,ITMLIS,,,)

*      Check the status return from GETJPI and return a bad status
*      if this was bad.

         IF ( (ISTAT.NE.0) .AND. (MOD(ISTAT,2).EQ.0) ) THEN

            CALL LIB$SIGNAL( %VAL(ISTAT) )
            STATUS = ISTAT

         ENDIF

      END
