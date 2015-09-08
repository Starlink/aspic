      SUBROUTINE STL_ACCFRM(NAME,ENTRY,STATUS)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     ACCFRM - Access Frame
*
*     This routine will search the Parameter Control Tables for 
*     an existing frame parameter entry of the same name. If one
*     does not exist, then it is created and the bulk data file
*     is physically opened. This operation returns the I/O channel
*     assigned to the file, the FILES-11 file identification and
*     the device name on which the volume is mounted.
*
*     CALL STL_ACCFRM(NAME,ENTRY,STATUS)
*
*     Input parameter:
*     ---------------
*     NAME:    CHARACTER expression:	Parameter name of data frame
*
*     Output parameters:
*     -----------------
*     ENTRY:   INTEGER variable:	PCT entry number
*     STATUS:  INTEGER variable:	Status return value
*
*
*     D.PEARCE  10/JUL/80  VERSION #2
*     D.BERRY   24/JAN/90  Modified so that if the parameter is active
*                          on entry but has no assigned IO channel then
*                          a BDF is created with the same name as the 
*                          one created on the first use of the 
*                          parameter. This second file will overwrite 
*	                   the first one.
*SOURCE
*      ACCFRM.FOR in UTILITIES.TLB
*----------------------------------------------------------------
C
      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) NAME
      INTEGER*4     ENTRY,STATUS
C
      CHARACTER*64  BDFNAM
C
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C     .....search for existing entry in PCT
      CALL STL_FINDPE(NAME,ENTRY)
C
C     .....if entry does not exist, then create one
      IF (ENTRY.EQ.0) CALL STL_CREPE(NAME,ENTRY)
C
C     .....store the initial parameter state
      IN_STATE=PCT_STATE(ENTRY)

C     .....check if frame is null
      IF (IN_STATE.EQ.PCT_NULL) THEN
	 STATUS=ERR_FRMNUL
	 GOTO 70
      ENDIF
C
C     .....see if frame entry is already active 
C     currently a file opened on it
      IF (IN_STATE.EQ.PCT_ACTIVE) THEN

C
C     .....if it is AND there is a file currently opened on it
C     then return 
         STATUS=ERR_NORMAL
	 IF(PCT_IOCHAN(ENTRY).NE.0) GOTO 70
      ENDIF
C
C     .....Get Program Parameter Information from the environment
      CALL STL_GETPPI(ENTRY,.FALSE.,1,BDFNAM,N,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GOTO 70

C
C     .....if the parameter was active on entry to this routine, then
C     a new BDF file will be opened with the same name as the last time.
C     (The old file must have been released by now to have got this far)
      IF(IN_STATE.EQ.PCT_ACTIVE) THEN
         VERSION=INDEX(BDFNAM,';')
         BDFNAM(VERSION:)=' '
         CALL STL_CHKBDF(PCT_TYPE(ENTRY),BDFNAM,STATUS)
         IF(STATUS.NE.ERR_NORMAL) GOTO 70
      ENDIF

C
C     .....check if bulk data frame type
      IF (PCT_TYPE(ENTRY)(1:5).NE.'FRAME') 
     +   CALL STL_ABEND('0*** PARAMETER "'//NAME//'" MUST BE A FRAME'//
     +                  '-TYPE ENTRY IN THE CONNECTION FILE')
C
C     .....open bulk data file
      CALL STL_OPNBDF(BDFNAM,PCT_IOCHAN(ENTRY),PCT_FILEID(1,ENTRY),
     +		      PCT_DEVICE(ENTRY),PCT_RDONLY(ENTRY),IOSTAT)
      CALL STL_SSERR('0***(ACCFRM/OPNBDF) ERROR',IOSTAT)
C
C
   70 RETURN
      END
