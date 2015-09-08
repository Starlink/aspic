      SUBROUTINE WRIMDF(NAME,DEFLT,FORMAT,DIMENS,NDIM,PNTR,STATUS)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     WRIMDF - WRite IMage file (with DeFault)
*SOURCE
*       WRIMDF.FOR IN UTILITIES.TLB
*
*     This routine is used to map memory, intended for image
*     output, onto a bulk data frame. The pointer returned will
*     be passed as %val(PNTR) to a lower level routine which
*     can write image data into the array argument.
*       If DEFLT is not blank then its contents are used as a default
*     file name. On exit, DEFLT always holds the name of the BDF file
*     actually used.
*
*     CALL WRIMDF(NAME,DEFLT,FORMAT,DIMENS,NDIM,PNTR,STATUS)
*
*     Input arguments:
*     ---------------
*     NAME:    CHARACTER expression:    Parameter name of data frame
*     FORMAT:  INTEGER expression:      Data format
*     DEFLT:   CHARACTER variable:      Default file name (modified on
*                                       exit).
*     DIMENS:  INTEGER array:           Array holding dimension sizes
*     NDIM:    INTEGER expression:      Number of dimensions
*
*     Output arguments:
*     ----------------
*     PNTR:    INTEGER variable:        Pointer to data in memory
*     DEFLT:   CHARACTER variable       Full file name
*     STATUS:  INTEGER variable:        Status return value
*
*
*     D.S. Berry 20/4/88
*     (Based on WRIMAG by D.PEARCE  30/JUL/80  VERSION #2)
*----------------------------------------------------------------------

      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) NAME
      INTEGER*4     FORMAT,DIMENS(*),NDIM,PNTR,STATUS
C
      CHARACTER*20  VALUE
      CHARACTER*6   AXIS
      DATA          AXIS/ 'NAXISn' /
C
      INCLUDE 'INTERIM(ERRPAR)'
C
C
      SIZE=1
      DO D=1,NDIM
         SIZE=SIZE*DIMENS(D)
      ENDDO
C
C     .....access bulk data frame using DSB's routine ACCFRM rather
C     than the official STL_ACCFRM routine.
      CALL ACCFRM(NAME,DEFLT,ENTRY,STATUS)
C
C     .....write frame if accessible
      IF (STATUS.EQ.ERR_NORMAL)
     +   CALL STL_WRFRM(ENTRY,FORMAT,'IMAGE',SIZE,PNTR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GOTO 60
C
      CALL ITOC(NDIM,VALUE,IND)
      CALL ADDSCR(NAME,'NAXIS',VALUE,1,IND)
C
      DO D=1,NDIM
         CALL ITOC(D,AXIS(6:6),IND)
         CALL ITOC(DIMENS(D),VALUE,IND)
         CALL ADDSCR(NAME,AXIS,VALUE,1,IND)
      ENDDO
C
   60 IF (STATUS.NE.ERR_NORMAL) CALL STLERR('WRIMDF',STATUS)
C
C
      RETURN
      END

      SUBROUTINE ACCFRM(NAME,DEFLT,ENTRY,STATUS)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     ACCFRM - Access Frame
C
C     This routine will search the Parameter Control Tables for
C     an existing frame parameter entry of the same name. If one
C     does not exist, then it is created and the bulk data file
C     is physically opened. This operation returns the I/O channel
C     assigned to the file, the FILES-11 file identification and
C     the device name on which the volume is mounted.
C        Modified from STL_ACCFRM by DS Berry to allow program supplied
C     default file name. Also resets CANCEL states to GROUND states,
C     before calling GETPPI, so that any value given on command line
C     or in connection file is accepted on succesive calls, rather
C     than user being automatically prompted for new values.
C
C     CALL ACCFRM(NAME,DEFLT,ENTRY,STATUS)
C
C     Input parameter:
C     ---------------
C     NAME:    CHARACTER expression:    Parameter name of data frame
C     DEFLT:   CHARACTER expression:    If not blank, this is used as
C                                       default BDF name (modified on
C                                       exit).
C
C     Output parameters:
C     -----------------
C     ENTRY:   INTEGER variable:        PCT entry number
C     DEFLT:   CHARACTER variable:      Hold full file name on exit
C     STATUS:  INTEGER variable:        Status return value
C
C
C     D.PEARCE  10/JUL/80  VERSION #2
C     (Modified by DS Berry 7/6/88)
C-----------------------------------------------------------------------
C
      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) NAME,DEFLT
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
C     .....check if frame is null
      IF (PCT_STATE(ENTRY).EQ.PCT_NULL) THEN
         STATUS=ERR_FRMNUL
         GOTO 70
      ENDIF
C
C     .....check if frame entry is already active
      IF (PCT_STATE(ENTRY).EQ.PCT_ACTIVE) THEN
         STATUS=ERR_NORMAL
         GOTO 70
      ENDIF
C
C     .....check if frame has been cancelled, and if so reset to ground
C          so that user is not automatically prompted.
      IF (PCT_STATE(ENTRY).EQ.PCT_CANCEL) THEN
         PCT_STATE(ENTRY)=PCT_GROUND
      ENDIF
C
C     .....Get Program Parameter Information from the environment
      IF(DEFLT.EQ.' ') THEN
         CALL STL_GETPPI(ENTRY,.FALSE.,1,BDFNAM,N,STATUS)
      ELSE
C
C     .....If a default has been given,offer it to the user...
         BDFNAM=DEFLT
         CALL STL_GETPPI(ENTRY,.TRUE.,1,BDFNAM,N,STATUS)
C
C     .....If a null value was given, the BDF frame will not have been
C          checked, so do it now.
         IF(PCT_STATE(ENTRY).EQ.PCT_NULL) THEN
            CALL STL_PROVAL(ENTRY,BDFNAM,1,1,BDFNAM,STATUS)
            STATUS=ERR_NORMAL
            PCT_STATE(ENTRY)=PCT_ACTIVE
         ENDIF
      ENDIF

      IF (STATUS.NE.ERR_NORMAL) GOTO 70
      DEFLT=BDFNAM
C
C     .....check if bulk data frame type
      IF (PCT_TYPE(ENTRY)(1:5).NE.'FRAME')
     +   CALL STL_ABEND('0*** PARAMETER "'//NAME//'" MUST BE A FRAME'//
     +                  '-TYPE ENTRY IN THE CONNECTION FILE')
C
C     .....open bulk data file
      CALL STL_OPNBDF(BDFNAM,PCT_IOCHAN(ENTRY),PCT_FILEID(1,ENTRY),
     +                PCT_DEVICE(ENTRY),PCT_RDONLY(ENTRY),IOSTAT)
      CALL STL_SSERR('0***(ACCFRM/OPNBDF) ERROR',IOSTAT)
C
C
   70 RETURN
      END




