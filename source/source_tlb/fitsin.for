C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   FITSIN *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               FITSIN
C
C
C          FUNCTION:-
C               To read standard FITS format tapes.
C
C
C          USE:-
C               The program reads image data held on  a  magnetic  tape  in
C               standard  FITS  format  and  converts  the data to Starlink
C               'Image-type bulk data frames' on disk (see RGO LUN  48  for
C               details).
C
C
C
C         USER PARAMETERS:-
C
C         MTDECK                              Specifies the device on which
C                                             the  input  tape  is  loaded.
C                                             This  will  generally  be  an
C                                             explicit device name, such as
C                                             MTA1:,  although  users   can
C                                             specify  any VMS logical name
C                                             which has  been  pre-assigned
C                                             to  the device. The tape must
C                                             be mounted by the DCL command
C                                             using the /FOREIGN qualifier,
C                                             e.g. $MOUNT/FOREIGN MTA1:
C
C         FILES                               Specifies the data  files  on
C                                             the input tape that are to be
C                                             processed  (logically   these
C                                             are   numbered  consecutively
C                                             from 1). Single  files  or  a
C                                             set  of adjacent files may be
C                                             specified               (e.g.
C                                             FILES=3,7-10,20   will   read
C                                             files 3,7,8,9,10,20).
C
C         OUTPUT                              Specifies  the  name  of  the
C                                             output  'bulk  data frame' on
C                                             disk. If only one file is  to
C                                             be  processed  the  parameter
C                                             can  be  specified   on   the
C                                             command  line.  However,  for
C
C
C                                             multi-file  processing,   the
C                                             user  will  be  prompted each
C                                             time a new output frame  file
C                                             specification is required.
C
C         MORE                                If MORE=YES the program  will
C                                             loop  back  and  ask for more
C                                             data files to  be  specified.
C                                             The  files  must  not already
C                                             have been skipped over.  This
C                                             is    more   efficient   than
C                                             invoking FITSIN again as  the
C                                             program rewinds the tape each
C                                             time it is executed.
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         LABELLED        NO                  By default it is assumed that
C                                             the     input     tape     is
C                                             'non-labelled'   i.e.    each
C                                             image  data file is separated
C                                             from any adjacent files by  a
C                                             single tape mark. If the tape
C                                             is 'labelled', however,  then
C                                             the    user    must   specify
C                                             LABELLED=YES on  the  command
C                                             line  to  ensure correct tape
C                                             positioning.
C
C
C
C         D.J.Pearce & L.L.Bell    RAL & RGO                       8-MAR-83
C
C
C--------------------------------------------------------------------------


*
      IMPLICIT      INTEGER(A-Z)
*
      CHARACTER*16  MTDECK
      CHARACTER*16  FILES(32)
      LOGICAL       LABELLED,MORE
*
      LOGICAL       MTEOF,MTEOT,MTERR
*
      CHARACTER*48  TEXT
      INTEGER       AXIS(9)
      LOGICAL       SIMPLE
      LOGICAL*1     BUFFER(2880)
      CHARACTER*80  HEADER(36)
      EQUIVALENCE   (BUFFER(1),HEADER(1))
*
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
*
*
*     .....allocate tape input device
    1 CALL RDKEYC('MTDECK',.FALSE.,1,MTDECK,I,STATUS)
      CALL TIO_OPEN(MTDECK,MTRN,IOSTAT)
      IF (TIO_ERR(IOSTAT,0)) THEN
	 CALL WRERR('OPEN')
	 CALL CNPAR('MTDECK',STATUS)
	 GOTO 1
      ENDIF
*
*     .....check tape is at its load point
      CALL TIO_REWIND(MTRN,IOSTAT)
      IF (TIO_ERR(IOSTAT,0)) THEN
	 CALL WRERR('REWIND')
	 GOTO 90
      ENDIF
*
*     .....check if tape is labelled
      LABELLED=.FALSE.
      CALL RDKEYL('LABELLED',.FALSE.,1,LABELLED,N,STATUS)
*
*     .....initialise tape position
      TPOS=1
*
*     .....get file list and check number of specifications
    2 CALL RDKEYC('FILES',.FALSE.,32,FILES,NFSPEC,STATUS)
      IF (NFSPEC.EQ.0) THEN
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
	    GOTO 90
	 ENDIF
*
*  .....check for file sequence error
	 IF (FIRST.LT.TPOS) THEN
	    CALL WRERR('SEQERROR')
	    GOTO 90
	 ENDIF
*
*  .....calculate number of files to skip
	 NFSKIP=FIRST-TPOS
	 IF (LABELLED) NFSKIP=NFSKIP*3
	 CALL TIO_SKIP(MTRN,NFSKIP,IOSTAT)
	 IF (TIO_ERR(IOSTAT,0)) THEN
	    CALL WRERR('SKIP')
	    GOTO 90
	 ENDIF
*
*  .....process each frame-file in this specification
	 DO 40 FF=FIRST,LAST
*
*     .....skip file labels if necessary
	    IF (LABELLED) CALL TIO_SKIP(MTRN,1,IOSTAT)
*
*     .....read first header record and check for error
	    CALL TIO_READ(MTRN,2880,%REF(HEADER),LEN,IOSTAT)
	    IF (TIO_ERR(IOSTAT,0)) THEN
	       CALL WRERR('READ')
	       GOTO 90
	    ENDIF
*
*     .....display file number and descriptors in first block
	    WRITE (TEXT,'(''0FILE # '',I4,
     +			  ''  DESCRIPTORS FOLLOW:'')') FF
	    CALL WRUSER(TEXT,STATUS)
            CARD=1
	    DO WHILE (HEADER(CARD)(1:4).NE.'END '.AND.CARD.LE.36)
	       CALL WRUSER(' '//HEADER(CARD),STATUS)
               CARD=CARD+1
	    ENDDO
*
*     .....is file FITS standard ??
	    CALL CTOL(HEADER(1)(11:30),SIMPLE,STATUS)
	    IF (.NOT.SIMPLE) THEN
               CALL WRUSER('NOT SIMPLE???',STATUS)
     	    ENDIF
*
*     .....read BITPIX and determine data format
	    CALL CTOI(HEADER(2)(11:30),BITPIX,STATUS)
	    IF (BITPIX.EQ.08) FORMAT=FMT_UB
	    IF (BITPIX.EQ.16) FORMAT=FMT_SW
	    IF (BITPIX.EQ.32) FORMAT=FMT_SL
*
*     .....read NAXIS
	    CALL CTOI(HEADER(3)(11:30),NDIM,STATUS)
*
*     .....decode NAXISn values and calculate frame size
	    SIZE=1
	    CARD=4
	    DO N=1,NDIM
	       CALL CTOI(HEADER(CARD)(11:30),AXIS(N),STATUS)
	       CARD=CARD+1
	       SIZE=SIZE*AXIS(N)
	    ENDDO
*
*     .....check for negative or zero image size
	    IF (SIZE.LT.1) THEN
	       CALL WRERR('BADSIZE')
	       GOTO 30
	    ENDIF
*
*     .....allocate frame-data and check status
   20	    CALL WRIMAG('OUTPUT',FORMAT,AXIS,NDIM,OUTPUT,STATUS)
	    IF (STATUS.EQ.ERR_FRMNUL.OR.
     +		STATUS.EQ.ERR_FRMNAC) THEN
	       CALL WRERR('BDFERR1')
	       CALL CNPAR('OUTPUT',STATUS)
	       GOTO 20
	    ENDIF
	    IF (STATUS.NE.ERR_NORMAL) THEN
	       CALL WRERR('BDFERR2')
	       GOTO 90
	    ENDIF
*
*     .....read rest of FITS header record(s)
	    CALL FITS_RHR(MTRN,HEADER,CARD)
*
*     .....read frame data
	    CALL FITS_RFD(%val(OUTPUT),SIZE,FORMAT,MTRN,BUFFER)
*
*     .....skip to end-of-file
   30	    CALL TIO_SKIP(MTRN,1,IOSTAT)
	    IF (LABELLED) CALL TIO_SKIP(MTRN,1,IOSTAT)
*
*     .....free frame data and cancel OUTPUT association
	    CALL FRDATA('OUTPUT',STATUS)
	    CALL CNPAR('OUTPUT',STATUS)
*
   40    ENDDO
*
*  .....reset tape position
 	 TPOS=LAST+1
*
   60 ENDDO
	CALL WRUSER('ANY MORE FILES TO BE READ???',STATUS)
	MORE=.FALSE.
	CALL RDKEYL('MORE',.FALSE.,32,MORE,N,STATUS)
	IF (MORE) THEN
		CALL CNPAR('MORE',STATUS)
		CALL CNPAR('FILES',STATUS)
		GOTO 2
	ENDIF
*
   90 CALL FRDATA('  ',STATUS)
      CALL EXIT
      END
      SUBROUTINE FITS_RFD(OUTPUT,SIZE,FORMAT,MTRN,BUFFER)
*
*     FITS_RFD - Read Frame Data
*
*     This routine will transfer the image data from the FITS tape
*     file to the mapped Starlink frame. Byte reversal is performed
*     on data other than in 8-bit format as a consequence of the
*     VAX addressing techniques.
*
*
      IMPLICIT  INTEGER(A-Z)
*
      INTEGER   SIZE,FORMAT,MTRN
      LOGICAL*1 OUTPUT(*),BUFFER(*)
*
      INCLUDE 'INTERIM(FMTPAR)'
*
*
*     .....calculate Bytes-Per-Value and total size of frame in bytes
      BPV=FORMAT-(FORMAT/100)*100
      NBYTES=BPV*SIZE
*
*     .....initialise frame displacement pointer
      DISP=1
*
*     .....read frame data
      DO WHILE (DISP.LT.NBYTES)
	 BUFSIZ=MIN0(2880,NBYTES-DISP+1)
	 CALL TIO_READ(MTRN,BUFSIZ,OUTPUT(DISP),LEN,IOSTAT)
         IF (TIO_ERR(IOSTAT,0)) THEN
            CALL WRERR('MTERR')
         ENDIF
	 DISP=DISP+2880
      ENDDO
*
*     .....reverse the byte-order if necessary
      IF (FORMAT.EQ.FMT_SW) CALL GEN_REV2(OUTPUT,SIZE)
      IF (FORMAT.EQ.FMT_SL) CALL GEN_REV4(OUTPUT,SIZE)
*
*
      RETURN
      END
      SUBROUTINE FITS_RHR(MTRN,HEADER,CARD)
*
*     FITS_RHR - Read Header Record(s)
*
*
      IMPLICIT       INTEGER(A-Z)
*
      INTEGER        MTRN,CARD,BLOCK
      CHARACTER*80   HEADER(36)
*
*
*     .....transfer FITS header information to frame file
      BLOCK=1
      DO WHILE (HEADER(CARD)(1:4).NE.'END ')
         IF (BLOCK.NE.1)CALL WRUSER(HEADER(CARD)(1:80),STATUS)
	 IF ((HEADER(CARD)(1:7).EQ.'COMMENT').OR.
     +       (HEADER(CARD)(1:7).EQ.'HISTORY').OR.
     +       (HEADER(CARD)(1:8).EQ.'        ')) THEN

             CALL ADDSCR('OUTPUT',HEADER(CARD)(1:8),HEADER(CARD)(9:80),
     +                   1,STATUS)
         ELSE
	     CALL ADDSCR('OUTPUT',HEADER(CARD)(1:8),HEADER(CARD)(10:80),
     +		         1,STATUS)
         ENDIF

	 CARD=CARD+1
	 IF (CARD.GT.36) THEN
	    CALL TIO_READ(MTRN,2880,%ref(HEADER),LEN,IOSTAT)
            BLOCK=BLOCK+1
	    CARD=1
   	 ENDIF
      ENDDO
*
*
      RETURN
      END
