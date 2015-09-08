C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C
C
C                     ********************* 
C                     *                   * 
C                     * Program   FITSOUT * 
C                     *                   * 
C                     ********************* 
C
C
C
C          CALLING SEQUENCE:- 
C               FITSOUT 
C
C
C          FUNCTION:- 
C               To write standard FITS format tapes. 
C
C
C          USE:- 
C               The program converts 'Image-type bulk data frames' on  disk 
C               into  standard  FITS format image data and then writes this 
C               image data to magnetic tape. (see RGO LUN 49 for details) 
C
C
C
C         USER PARAMETERS:- 
C
C         MTDECK                              Specifies the device on which 
C                                             the     output     tape    is 
C                                             loaded.This will generally be 
C                                             an explicit device name, such 
C                                             as MTA1:, although users  can 
C                                             specify  any VMS logical name 
C                                             which has  been  pre-assigned 
C                                             to  the device. The tape must 
C                                             be mounted by the DCL command 
C                                             using the /FOREIGN qualifier, 
C                                             e.g. $MOUNT/FOREIGN MTA1: 
C
C         FILES                               Specifies the location of the 
C                                             data files on the output tape 
C                                             (logically,     these     are 
C                                             numbered  consecutively  from 
C                                             1) If more than one  file  is 
C                                             to be written then a sequence 
C                                             of files can be specified  by 
C                                             FILES=3-6  This  will write 4 
C                                             files  to  tape  starting  at 
C                                             file  3;  file  1  being  the 
C                                             physical beginning of tape. 
C
C         SIMPLE          YES                 If   the    user    specifies 
C                                             SIMPLE=YES,  he  will also be 
C                                             asked to  supply  the  BITPIX 
C                                             parameter, which dictates the 
C                                             number  of  bits  per   pixel 
C
C 
C                                             value. If SIMPLE=NO then  the 
C                                             data  will  be written in VAX 
C                                             F~floating format (REAL*4). 
C
C         BITPIX          16                  Specifies the number of  bits 
C                                             per  pixel  value.  The  only 
C                                             valid values for  BITPIX  are  
C                                             8, 16 and 32.
C
C         INPUT                               Specifies  the  name  of  the 
C                                             input  'bulk  data  frame' on 
C                                             disk. If only one file is  to 
C                                             be  processed  the  parameter 
C                                             can  be  specified   on   the 
C                                             command  line.  However,  for 
C                                             multi-file   processing   the 
C                                             user  will  be  prompted each 
C                                             time a new input  frame  file 
C                                             specification is required. 
C
C
C         NORMALLY DEFAULTED PARAMETERS:- 
C
C         EDIT            NO                  An  edit  function  has  been 
C                                             provided  for adding optional 
C                                             FITS parameters to the header 
C                                             records.  If  the user wishes 
C                                             to  take  advantage  of  this 
C                                             facility   he   must  specify 
C                                             EDIT=YES on the command line. 
C
C
C
C         D.J.Pearce & D.J.King    RAL & RGO                       8-MAR-83 
C
C
C-------------------------------------------------------------------------- 



*
      IMPLICIT      INTEGER(A-Z)
*
      CHARACTER*16  MTDECK
      CHARACTER*16  FILES
      CHARACTER*80  DESCR,ATEXT(16384)
      LOGICAL       SIMPLE,EDIT,MORE
*
      CHARACTER*48  TEXT
      INTEGER       AXIS(9),COUNT
      BYTE          BUFFER(2880)
      CHARACTER*80  HEADER(36)
      EQUIVALENCE   (BUFFER(1),HEADER(1))
*
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
*
*
*     .....allocate tape output device
      CALL RDKEYC('MTDECK',.FALSE.,1,MTDECK,I,STATUS)
      CALL TIO_OPEN(MTDECK,MTCHAN,MTSTAT)
      CALL FITS_CHECK('OPEN',MTSTAT,*90)
*
*     .....get file list and check number of specifications
      CALL RDKEYC('FILES',.FALSE.,1,FILES,NFSPEC,STATUS)
      IF (NFSPEC.NE.1) THEN
	 CALL WRERR('FILESBAD')
	 GOTO 90
      ENDIF
*
*     .....check tape is at its load point
      CALL TIO_REWIND(MTCHAN,MTSTAT)
      CALL FITS_CHECK('REWIND',MTSTAT,*90)
*
*     .....calculate file-limits
      CALL STL_CFILIM(FILES,FIRST,LAST,STATUS)
*
*     .....check file specification
      IF (STATUS.NE.ERR_NORMAL) THEN
	 CALL WRERR('FILESBAD')
	 GOTO 90
      ENDIF
*
*     .....skip over any preceding files
      CALL TIO_SKIP(MTCHAN,FIRST-1,MTSTAT)
      CALL FITS_CHECK('SKIP',MTSTAT,*90)
*
*     .....are extra FITS header records to be 'edited' ?
      EDIT=.FALSE.
      CALL RDKEYL('EDIT',.FALSE.,1,EDIT,I,STATUS)
*
*     .....assume FITS standard and 16-bit data as default
      SIMPLE=.TRUE.
      BITPIX=16
*
*     .....process each frame-file in specification
      DO 60 FF=FIRST,LAST
*
*  .....initialise FITS header record
	 HEADER(1)='SIMPLE  ='
 	 HEADER(2)='BITPIX  ='
	 HEADER(3)='NAXIS   ='
	 DO N=4,36
	    HEADER(N)=' '
	 ENDDO
*
*  .....is FITS file to be standard ??
	 CALL RDKEYL('SIMPLE',.TRUE.,1,SIMPLE,I,STATUS)
	 CALL CNPAR('SIMPLE',STATUS)
*
*  .....if so, find bits/pixel and corresponding data format
	 IF (SIMPLE) THEN
	    HEADER(1)(30:30)='T'
   10	    CALL RDKEYI('BITPIX',.TRUE.,1,BITPIX,I,STATUS)
	    CALL CNPAR('BITPIX',STATUS)
	    IF (BITPIX.EQ.8) THEN
	       FORMAT=FMT_UB
	    ELSE IF (BITPIX.EQ.16) THEN
	       FORMAT=FMT_SW
	    ELSE IF (BITPIX.EQ.32) THEN
	       FORMAT=FMT_SL
	    ELSE
	       GOTO 10
	    ENDIF
*
*  .....if not standard, assume VAX floating point
	 ELSE
	    HEADER(1)(30:30)='F'
	    BITPIX=32
	    FORMAT=FMT_R
	 ENDIF
	 CALL ITOC(BITPIX,HEADER(2)(11:30),STATUS)
*
*  .....access Starlink input image
   20    CALL RDIMAG('INPUT',FORMAT,9,AXIS,NDIM,INPUT,STATUS)
	 IF (STATUS.EQ.ERR_FRMNUL.OR.
     +	     STATUS.EQ.ERR_FRMNAC) THEN
	    CALL WRERR('BDFERR1')
	    CALL CNPAR('INPUT',STATUS)
	    GOTO 20
	 ENDIF
	 IF (STATUS.NE.ERR_NORMAL) THEN
	    CALL WRERR('BDFERR2')
	    GOTO 90
	 ENDIF
*
*        .....construct NAXIS descriptor
	 CALL ITOC(NDIM,HEADER(3)(11:30),STATUS)
*
*  .....display file number and first 3 descriptors
	 WRITE (TEXT,'('' File # '',I4,'' - Mandatory '',
     +		       ''descriptors follow:'')') FF
	 CALL WRUSER(TEXT,STATUS)
	 DO CARD=1,3
	    CALL WRUSER(' '//HEADER(CARD),STATUS)
	 ENDDO
*
*  .....construct NAXISn descriptors and calculate frame size
	 SIZE=1
	 CARD=4
	 DO N=1,NDIM
	    WRITE (HEADER(CARD),'(A,I1,A,I20)') 'NAXIS',N,'  = ',AXIS(N)
	    CALL WRUSER(' '//HEADER(CARD),STATUS)
	    CARD=CARD+1
	    SIZE=SIZE*AXIS(N)
	 ENDDO
*
*  .....construct VAX-floating descriptor if required
	 IF (.NOT.SIMPLE) THEN
	    HEADER(CARD)='FLOATING= ''VAX-F   '''
	    CARD=CARD+1
	 ENDIF
*
*  ....Skip out NAXIS and NAXISn BDF descriptors
*
	MORE=.TRUE.
	COUNT=0
	NVAL=1
 	DO WHILE (MORE)
	COUNT=COUNT+NVAL
	CALL RDDSCN('INPUT',COUNT,DESCR,STATUS)
	IF(STATUS.EQ.ERR_DSCNPR) GOTO 999
	CALL RDDSCR('INPUT',DESCR,16384,ATEXT,NVAL,STATUS)
*
*  ...Set keyword to uppercase
*
        CALL STR$UPCASE(DESCR(1:8),DESCR(1:8))
*
*  ...Reject any duplication of mandatory keywords
*
	IF (DESCR(1:5).NE.'SIMPL'.AND.
     +      DESCR(1:5).NE.'BITPI'.AND.
     +      DESCR(1:5).NE.'NAXIS') THEN
	DO I=1,NVAL
	CALL WRUSER(DESCR(1:8)//' '//ATEXT(I)(1:68),STATUS)
	HEADER(CARD)(1:8)=DESCR(1:8)
	 IF ((HEADER(CARD)(1:7).EQ.'COMMENT').OR.
     +       (HEADER(CARD)(1:7).EQ.'HISTORY').OR.
     +       (HEADER(CARD)(1:8).EQ.'        ')) THEN

             HEADER(CARD)(9:76)=ATEXT(I)(1:68)
         ELSE
            HEADER(CARD)(9:10)='= '
            HEADER(CARD)(11:78)=ATEXT(I)(1:68)
         ENDIF
        CARD=CARD+1
	IF (CARD.GT.36) THEN
		CALL TIO_WRITE(MTCHAN,BUFFER,2880,MTSTAT)
		CALL FITS_CHECK('WRITE',MTSTAT,*90)
		CARD=1
		DO N=1,36
			HEADER(N)=' '
		ENDDO
	ENDIF
	ENDDO
	ENDIF
	ENDDO
*  .....edit extra FITS header records if requested
999	 IF (EDIT) CALL FITS_EDIT(MTCHAN,HEADER,CARD,*90)
*
*  .....construct END card and write last header record
	 HEADER(CARD)='END'
	 CALL TIO_WRITE(MTCHAN,BUFFER,2880,MTSTAT)
	 CALL FITS_CHECK('WRITE',MTSTAT,*90)
*
*  .....write frame data
	 CALL FITS_WRITE(%val(INPUT),SIZE,FORMAT,MTCHAN,BUFFER,*90)
*
*  .....write tape mark
	 CALL TIO_MARK(MTCHAN,MTSTAT)
	 CALL FITS_CHECK('MARK',MTSTAT,*90)
*
*  .....free input frame
	 CALL FRDATA('INPUT',STATUS)
	 CALL CNPAR('INPUT',STATUS)
*
   60 ENDDO
*
*
   90 CALL TIO_MARK(MTCHAN,MTSTAT)
      CALL FRDATA(' ',STATUS)
      CALL EXIT
*
      END
      SUBROUTINE FITS_WRITE(INPUT,SIZE,FORMAT,MTCHAN,BUFFER,*)
*
*     FITS_WRITE - Write Frame Data
*     ----------
*     This routine will transfer the image data from the mapped
*     Starlink frame to the FITS tape file. Byte reversal is per-
*     -formed on data other than in 8-bit format as a consequence
*     of the VAX addressing technique.
*
*     Format of call:
*     --------------
*     CALL FITS_WRITE(INPUT,SIZE,FORMAT,MTCHAN,BUFFER,*<error-label>)
*
*     Input arguments:
*     ---------------
*     INPUT	is the address of start of the image-data.
*     SIZE	is the number of values in the image.
*     FORMAT	is the format of the image-data.
*     MTCHAN	is the I/O channel assigned to the output tape device.
*     BUFFER	is the address of a 2880-byte buffer.
*
*     The alternate return is taken if an error occurs during writing.
*
*
      IMPLICIT  INTEGER(A-Z)
*
      INTEGER   SIZE,FORMAT,MTCHAN
      BYTE      INPUT(*),BUFFER(*)
*
      INCLUDE 'INTERIM(FMTPAR)'
*
*
*     .....calculate bytes per value and total number of bytes in frame
      BPV=FORMAT-(FORMAT/100)*100
      NBYTES=BPV*SIZE
*
*     .....initialise frame displacement pointer
      DISP=1
*
*     .....write frame data
      DO WHILE (DISP.LT.NBYTES)
	 BUFSIZ=MIN0(2880,NBYTES-DISP+1)
	 CALL GEN_COPY(INPUT(DISP),BUFFER,BUFSIZ)
	 IF (FORMAT.EQ.FMT_SW) CALL GEN_REV2(BUFFER,BUFSIZ/2)
	 IF (FORMAT.EQ.FMT_SL) CALL GEN_REV4(BUFFER,BUFSIZ/4)
	 CALL TIO_WRITE(MTCHAN,BUFFER,2880,MTSTAT)
	 CALL FITS_CHECK('WRITE',MTSTAT,*90)
	 DISP=DISP+2880
      ENDDO
      RETURN
*
*
   90 RETURN 1
      END

      SUBROUTINE FITS_EDIT(MTCHAN,HEADER,CARD,*)
*
*     FITS_EDIT - Edit Header Record(s)
*     ---------
*
*
      IMPLICIT      INTEGER(A-Z)
*
      INTEGER       MTCHAN,CARD
      CHARACTER*80  HEADER(36),INPUT
*
      INPUT=' '
*
      CALL WRUSER(' EDIT called'//CHAR(07),STATUS)
*
*     .....only EDIT function at present is ADD
      CALL WRUSER(' Only ADD function supported',STATUS)
      CALL WRUSER(' Enter FITS card images to be added to file -'//
     +            ' terminate with END card:',STATUS)
*
*     .....add user supplied FITS card-images until 'END'
      DO WHILE (INPUT.NE.'END     ')
	CALL RDUSER(INPUT,STATUS)
*
*   ...Set first 8 characters to upper case
*
        CALL STR$UPCASE(INPUT(1:8),INPUT(1:8))
*
*   ...Reject any duplication of mandatory parameters
*
	IF (INPUT(1:5).NE.'SIMPL'.AND.
     +      INPUT(1:5).NE.'BITPI'.AND.
     +      INPUT(1:5).NE.'NAXIS') THEN
		HEADER(CARD)(1:80)=INPUT(1:80)
	        CARD=CARD+1
	ELSE
		CALL WRUSER(' Duplicate of mandatory FITS card image.',STATUS)
		CALL WRUSER(' Card image rejected!!',STATUS)
	ENDIF
	IF (CARD.GT.36) THEN
		CALL TIO_WRITE(MTCHAN,%REF(HEADER),2880,MTSTAT)
		CALL FITS_CHECK('WRITE',MTSTAT,*90)
		CARD=1
		DO N=1,36
			HEADER(N)=' '
		ENDDO
	ENDIF
      ENDDO
      CARD=CARD-1
      RETURN
*
*
   90 RETURN 1
      END
      SUBROUTINE FITS_CHECK(NAME,MTSTAT,*)
* +
*     FITS_CHECK - Check status from tape I/O operation
*     ----------
* -
      IMPLICIT      INTEGER(A-Z)
      CHARACTER*(*) NAME
      INTEGER       MTSTAT
*
      LOGICAL       TIO_ERR
      CHARACTER*76  MSGBUF
*
*
      IF (TIO_ERR(MTSTAT)) THEN
         CALL WRERR(NAME)
         CALL TIO_GETMSG(MTSTAT,MSGBUF,MSGLEN)
         CALL WRUSER(MSGBUF(:MSGLEN)//CHAR(07),STATUS)
	 RETURN 1
      ENDIF
*
*
      END
