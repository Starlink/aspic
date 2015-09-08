      SUBROUTINE UPDATA(NAME,STATUS)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     UPDATA - Free Data area ready for updating
*     
*     This routine performs the functions of FRDATA followed by
*     CNPAR, with the differnece that the data stored for the
*     parameter is not deleted, and is ready to be used
*     should the parameter be used again. This allows repeated
*     calls to WRDATA to update a file without prompting the user
*     for a new file name each time.
*
*     It was created directly from the INTERIM code for the following
*     routines:
*               FRDATA
*
*     CALL UPDATA(NAME,STATUS)
*
*     Input argument:
*     ---------------
*     NAME:    CHARACTER expression:  Parameter name.
*
*     Output argument:
*     ----------------
*     STATUS:  INTEGER variable:      Status return value.
*
*
*     D.BERRY 24/JAN/90  VERSION #1
*
*SOURCE
*      UPDATA.FOR in UTILITIES.TLB
*---------------------------------------------------------------------

      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) NAME
      INTEGER*4     STATUS
C
      CHARACTER*5   TYPE
C
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C
      CALL STL_FINDPE(NAME,ENTRY)
      IF (ENTRY.NE.0) THEN
         TYPE=PCT_TYPE(ENTRY)(1:5)
         IF (TYPE.EQ.'FRAME') THEN

C
C     .....if a FRAME type entry exists for the specified parameter, 
C     then free it (as in FRDATA)
            CALL STL_FRFRM(ENTRY,STATUS)
            IF(STATUS.NE.ERR_NORMAL) GOTO 10

C     .....de-assign the file and PCT entry (as in CNPAR)
            IOSTAT=SYS$DASSGN(%val(PCT_IOCHAN(ENTRY)))
	    PCT_IOCHAN(ENTRY)=0
	    PCT_FILEID(1,ENTRY)=0
	    PCT_FILEID(2,ENTRY)=0
	    PCT_FILEID(3,ENTRY)=0
	    PCT_DEVICE(ENTRY)=' '
C

         ELSE
            STATUS=ERR_NODATA
         ENDIF
      ELSE
         STATUS=ERR_NODATA
      ENDIF
C
C     .....check status
  10  IF (STATUS.NE.ERR_NORMAL) CALL STLERR('UPDATA',STATUS)
C
C
      RETURN
      END
