!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!      !!!!!!!!!!!!
!      !          !
!      ! FLAT.SCL !
!      !          !
!      !!!!!!!!!!!!
!
!
!
!      Calling sequence
!            FLAT INPUT INPUTXY FLATIMAGE FLATXY OUTPUT
!
!
!        This Flat Fields an image.
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
IF P1 .EQS. "" THEN INQUIRE P1 "INPUT IMAGE ?"
IF P2 .EQS. "" THEN INQUIRE P2 "IMAGE REF COORDS XYLIST ?"
IF P3 .EQS. "" THEN INQUIRE P3 "FLAT FIELD IMAGE?"
IF P4 .EQS. "" THEN INQUIRE P4 "FLAT REF COORDS XYLIST ?"
IF P5 .EQS. "" THEN INQUIRE P5 "OUTPUT IMAGE ?"
!
!
!
LET XYFIT_INPUTA='P2'
LET XYFIT_INPUTB='P4'
LET XYFIT_ILEVEL=3
LET XYFIT_NSIGMA=2.5
EDRS:XYFIT
!
!
!
LET FLATTEN_IMAGE='P1'
LET FLATTEN_FLAT='P3'
LET FLATTEN_TRCOEFFS=XYFIT_TRCOEFFS
LET FLATTEN_OUTPUT='P5'
LET FLATTEN_TITLE='P5'
FLATTEN
!
!
$ CLEAR XYFIT_
$ CLEAR FLATTEN_
!
!
!
!
EXIT
!
!
!
