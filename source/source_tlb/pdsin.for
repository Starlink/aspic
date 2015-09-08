C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   PDSIN  *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               PDSIN
C
C
C          FUNCTION:-
C               To read 9-track tapes from the RGO PDS microdensitometer
C
C
C          USE:-
C               The program reads files from a PDS tape and stores them  in
C               Starlink BDF files.
C               Note that the values BSCALE=1 & BZERO=0 are set into
C               the descriptor of the output file along with a title.
C
C
C
C         USER PARAMETERS:-
C
C         MTDECK                              Specifies the device on which
C                                             the  tape is loaded, eg MTA1:
C                                             Note that th must have  first
C                                             been MOUNTED/FOREIGN
C
C         FILES                               Specifies  the  files  to  be
C                                             read from the tape (logically
C                                             these are numbe  sequentially
C                                             from  1).  Single  files or a
C                                             sequence  of  files  may   be
C                                             specifi      For     instance
C                                             FILES=4,7-10,1,2-3       will
C                                             cause  files 4,7,8,9,10,1,2,3
C                                             to be in that order.
C
C         X                                   The number  of  pixels  in  a
C                                             single scan line. The initial
C                                             default is the of  the  first
C                                             record encountered, but since
C                                             this is limited to 4096 by th
C                                             buffers   on   the   PDS  the
C                                             default  may   not   be   too
C                                             useful. After the first en of
C                                             a value  for  X  the  default
C                                             therefore  becomes  the  last
C                                             value used. The of X  may  be
C                                             in the range 1<= X <= 32768
C
C         Y                                   The number of scan  lines  in
C                                             the image. Initial default is
C
C
C                                             1 but  it  reta  user-defined
C                                             values     for     subsequent
C                                             defaults.
C
C         TITLE           PDS header          The title which will  be  put
C                                             into  the  descriptor  of the
C                                             BDF file created
C
C         OUTPUT                              The file  name  for  the  BDF
C                                             version   of  the  tape  file
C                                             requested
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         CDP   DLT                RGO MAN                        17-MAR-83
C
C
C--------------------------------------------------------------------------



      IMPLICIT INTEGER(A-Z)
      PARAMETER (LBUF=8248)
      LOGICAL TIO_ERR
      CHARACTER MSG*72,FILES(32)*16,MTDECK*16,LABEL*41,TEXT*3
      INTEGER AXIS(2)
      INTEGER*2 BUF(LBUF)
      REAL SCALE,ZERO
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
*
*  Allocate tape channel
*
      CALL RDKEYC('MTDECK',.FALSE.,1,MTDECK,I,STATUS)
      CALL STR$UPCASE(MTDECK,MTDECK)
      CALL TIO_OPEN(MTDECK,MTRN,STATUS)
      IF(TIO_ERR(STATUS)) THEN
        CALL WRERR('MTOPEN')
        STOP
      END IF
C
C       SKIP THE FIRST DUMMY FILE
C
C C
      CALL TIO_SKIP(MTRN,1,IS)
      IF(TIO_ERR(IS))  THEN
         CALL WRUSER('Error skipping first dummy file',STATUS)
         CALL TIO_GETMSG(IS,MSG,LENMES)
         CALL WRUSER(MSG(:LENMES),STATUS)
         GO TO 802
      ENDIF
*
*  Remember where we are
*  and set default Y size
*
      AXIS(2) = 1
      NLAST=1
  161 CONTINUE
*
*     .....get file list and check number of specifications
    2 CALL RDKEYC('FILES',.FALSE.,32,FILES,NFSPEC,STATUS)
      IF(STATUS.EQ.ERR_PARNUL)  GO TO 802

      IF(STATUS.NE.ERR_NORMAL)  THEN
	 CALL WRERR('NOFILES')
	 CALL CNPAR('FILES',STATUS)
	 GOTO 2
      ENDIF
*
*     .....scan thru' tape, processing each file specification
      DO 60 FS=1,NFSPEC
*
*  .....calculate file-limits
	 CALL STL_CFILIM(FILES(FS),FIRST,LAST,STATUS)
*
*  .....check file specification
	 IF (STATUS.NE.ERR_NORMAL) THEN
	    CALL WRERR('FILESBAD')
	    GOTO 802
	 ENDIF

      DO 100 IFILE=FIRST,LAST

      NFM = IFILE
*
*  calculate number of files to skip
*
      IF(IFILE.GT.FIRST)  THEN
         NLAST = IFILE-1
      ENDIF

      NSKIP = NFM - NLAST
      IF(NSKIP.LT.0)  THEN
         NSKIP = NSKIP - 1
      ENDIF


C
C       SKIP THE REQUIRED NUMBER OF FILES ON THE PDS TAPE.
C
      CALL TIO_SKIP(MTRN,NSKIP,IS)
      IF(TIO_ERR(IS))  THEN
         CALL WRUSER('Error skipping file',STATUS)
         CALL TIO_GETMSG(IS,MSG,LENMES)
         CALL WRUSER(MSG(:LENMES),STATUS)
         GO TO 802
      ENDIF
      IF(NSKIP.LT.0)  THEN
        CALL TIO_SKIP(MTRN,1,IS)
        IF(TIO_ERR(IS))  THEN
           CALL WRUSER('Error skipping file',STATUS)
           CALL TIO_GETMSG(IS,MSG,LENMES)
           CALL WRUSER(MSG(:LENMES),STATUS)
           GO TO 802
         ENDIF
      ENDIF

*
*  Read a single record to get header
*
      CALL TIO_READ(MTRN,LBUF,BUF,LEN,STATUS)
      WRITE(LABEL,'(1X,20A2)') (BUF(I),I=2,21)
      WRITE(TEXT,'(I3)') IFILE
      MSG = 'File #'//TEXT//' Scan label is ..... '//LABEL
      CALL WRUSER(' ',STATUS)
      CALL WRUSER(MSG,STATUS)

*
*    Ask for number of scans in frame
*
  162   CALL RDKEYI('Y',.TRUE.,1,AXIS(2),I,STATUS)
*
*    If the answer is silly
*
      IF(STATUS.NE.ERR_PARNUL)  THEN
         IF(STATUS.NE.ERR_NORMAL.OR.AXIS(2).LE.0)  THEN
            CALL WRUSER('Daft answer',STATUS)
            CALL CNPAR('Y',STATUS)
            GO TO 162
         ENDIF
      ENDIF

*
*    Swop the byte to get the number of points in the scan
*
      IF(NLAST.EQ.1)  THEN
        CALL GEN_REV2(BUF,LEN/2)
        AXIS(1)=-BUF(22)
      ENDIF
*
*    Suggest this as a value of X
*
   20   CALL RDKEYI('X',.TRUE.,1,AXIS(1),I,STATUS)
        IF(STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL
     :     .OR.AXIS(1).LT.1 )                          THEN
          CALL CNPAR('X',STATUS)
          GO TO 20
        END IF
*
*    Go back one record ready for PDS
*
        CALL TIO_MOVE(MTRN,-1,STATUS)
*
*
*      get the bulk data frame
*
   30      IF(AXIS(2).EQ.1)  THEN
              CALL WRIMAG('OUTPUT',FMT_SW,AXIS(1),1,FRAME,STATUS)
           ELSE
             CALL WRIMAG('OUTPUT',FMT_SW,AXIS,2,FRAME,STATUS)
          ENDIF
          IF(STATUS.EQ.ERR_FRMNUL .OR. STATUS.EQ.ERR_FRMNAC) THEN
            CALL CNPAR('OUTPUT',STATUS)
            GO TO 30
          ELSE IF (STATUS.NE.ERR_NORMAL) THEN
            CALL WRERR('BADFR',STATUS)
            STOP
          END IF
*
          CALL PDS(MTRN,BUF,LBUF,%VAL(FRAME),AXIS(1),AXIS(2),STATUS)
*
*  SET DESCRIPTOR IN FILE HEADER
*
            CALL RDKEYC('TITLE',.TRUE.,1,LABEL,NVAL,ISTAT)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +              LABEL,IERR)
            CALL CNPAR('TITLE',ISTAT)
            INVAL = -32767
            SCALE = 1.0
            ZERO = 0.0
            CALL PTDSCR('OUTPUT','INVAL','INTEGER',INVAL,RVAL,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,SCALE,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,ZERO,CVAL,IERR)
*
*      Free the data frame
*      and dimensions
*
          CALL FRDATA('OUTPUT',STATUS)
          CALL CNPAR('OUTPUT',STATUS)
          CALL CNPAR('X',STATUS)
          CALL CNPAR('Y',STATUS)


  100 CONTINUE

      CALL CNPAR('OUTPUT',STATUS)

      NLAST =  LAST

   60 CONTINUE

*
*  Go see if any more files are required
*
      CALL CNPAR('FILES',STATUS)
      GO TO 2


C
C       REWIND PDS TAPE AFTER LAST REQUIRED FILE HAS BEEN READ.
C
 802  CALL TIO_REWIND(MTRN,IS)
      CALL FRDATA(' ',STATUS)
      END
      SUBROUTINE PDS(MTRN,BUF,LBUF,ARRAY,NXAXIS,NYAXIS,STAT)
*
*  This routine reads a frame from a PDS tape and puts it in the array
*  ARRAY. If an error occurs the routine returns immediatly and STAT
*  contains the status return from the last call to MTREAD.
*
*  If the frame contains more than NYAXIS scans the excess records are
*  ignored.
*
*  The calling program must ensure that NXAXIS is large enough.
*
*  The PDS tape format is described in the PDS manual obtainable
*  from RGO
*
*  The mag tape reading routine are described in SSN7
*
*  GEN_REV2 is in the Starlink library
*
*  Input parameters:
*
*    MTRN  I*4  mag tape channel identifier - unchanged on exit
*
*    LBUF  I*4  length of array BUF               "      "   "
*
*    NXAXIS  I*4  First dimension of ARRAY        "      "   "
*
*    NYAXIS  I*4  Second dimension of ARRAY       "      "   "
*
*  Output parameters:
*
*    BUF(LBUF)  I*2  Workspace - must be as long as the largest record
*                    on the tape
*
*    ARRAY(NXAXIS,NYAXIS)  I*2  Contains PDS frame on exit
*
*    STAT  I*4  Status return - see above
*
*  Subroutines called:  MTREAD,MTERR,GEN_REV2
*
*
      INTEGER STAT,NXAXIS,NYAXIS,LBUF,NP,J,I,NACT,MTRN,LEN
      INTEGER*2  BUF(LBUF),ARRAY(NXAXIS,NYAXIS)
      LOGICAL TIO_ERR
*
*
      DO I=1,NYAXIS
*
*  NACT is the number of data points we have got for this scan so far
*
        NACT=0
        DO WHILE (NACT.LT.NXAXIS)
          CALL TIO_READ(MTRN,LBUF,BUF,LEN,STAT)
          IF (TIO_ERR(STAT)) RETURN
*
*  NP is the number of data points in this record
*
          NP=(LEN-56)/2
*
*  Copy the data into ARRAY
*
          DO J=1,NP
            ARRAY(NACT+J,I)=BUF(J+28)
          END DO
*
          NACT=NACT+NP
        END DO
      END DO
*
*  Reverse the bytes and return
*
      CALL GEN_REV2(ARRAY,NXAXIS*NYAXIS)
      RETURN
      END

