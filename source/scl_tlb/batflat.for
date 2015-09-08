!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!      !!!!!!!!!!!!!!!
!      !             !
!      ! BATFLAT.SCL !
!      !             !
!      !!!!!!!!!!!!!!!
!
!
!
!        This Flat Fields an image.
!
!        It does it as a batch job, but is otherwise the same as FLAT.
!
!        It takes the input image and the Flat Field image and two files
!        containing an EDRS format XY list of the positions of a set
!        of reference points (more than or equal to 3) in the
!        coordinate frames of the two images. It then makes a new
!        image by taking each point in the input image and finding the
!        matching point in the Flat Field image and dividing the input
!        image value by the Flat Field value.
!        The result is stored in an output image
!
!        The images must be in the EDRS I*2 format.
!
!
!  WRITTEN BY  A.J. PENNY                              82-7-6
!---------------------------------------------------------------
!
!
WRITE SYS$OUTPUT "TYPE FULL FILE NAMES (INCLUDING DISK IF NOT DEFAULT)"
WRITE SYS$OUTPUT " "
INQUIRE P1 "INPUT IMAGE ?"
INQUIRE P2 "IMAGE REF COORDS XYLIST ?"
INQUIRE P3 "FLAT FIELD IMAGE?"
INQUIRE P4 "FLAT REF COORDS XYLIST ?"
INQUIRE P5 "OUTPUT IMAGE ?"
!
!
!
INQUIRE P6 "TYPE 3 IF USING USER3 DISK, 4 IF USING USER4, 0 IF NEITHER"
!
INQUIRE P7 "BATCH OR FASTBATCH (B/F) ?"
!
INQUIRE P8 "NAME TO BE GIVEN TO FILE EXECUTING BATCH JOB ?"
!
!
!
$ OPEN/WRITE OUTPUT_FILE 'P8'.COM
WRITE OUTPUT_FILE "$ SET NOVERIFY"
IF P6 .EQS. "3" THEN WRITE OUTPUT_FILE "$ MOUNT/SHARE DRC2: USER3"
IF P6 .EQS. "4" THEN WRITE OUTPUT_FILE "$ MOUNT/SHARE DRC2: USER4"
WRITE OUTPUT_FILE "$ DSCL"
WRITE OUTPUT_FILE "FLAT"
WRITE OUTPUT_FILE P1
WRITE OUTPUT_FILE P2
WRITE OUTPUT_FILE P3
WRITE OUTPUT_FILE P4
WRITE OUTPUT_FILE P5
WRITE OUTPUT_FILE "STOP"
WRITE OUTPUT_FILE "$ EXIT"
$ CLOSE OUTPUT_FILE
$ IF P7 .EQS. "B" THEN SUBMIT 'P8'
$ IF P7 .EQS. "F" THEN SUBMIT/QUEUE=FASTBATCH 'P8'
