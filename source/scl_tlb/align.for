!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!      ALIGN.SCL
!
!
!      CALLING SEQUENCE
!             ALIGN  REFIMAGE INPUTIMAGE OUTPUTIMAGE METHOD XBOX YBOX
!
!
!
!  MAKES A NEW VERSION OF AN IMAGE ALIGNED WITH A REFERENCE
!  IMAGE, USING MATCHING STARS IN THE IMAGES TO DEFINE THE
!  TRANSFORMATION
!
!    USE:-
!        It puts up the reference image on the ARGS. The cursor
!    will then light and you go round picking off fiducial stars
!    by pressing the red button. (The white buttons will increase
!    or decrease the magnification by x2, and the green button will
!    reset the magnification to 1 and recentre the image.) To end
!    set the cursor to outside the image at the bottom lh corner
!    and press the red button. From 1 to 2000 stars can be used,
!    but 6 to 8 spread over the image is a reasonable number.
!        The input image will then be put up and the reference
!    stars marked. The program will then go through the stars,
!    placing the cursor on the marks. You move the cursor onto
!    the actual star position and press the red button. (The green
!    and white buttons have the same effect as before.)
!    These two sets of picking up stars are performed by the programs
!    XYCURA and XYCURB (qqv).
!
!        After this the program centres on your approx positions
!    to get accurate star positions.
!        The centering is done by fitting a 2D Gauss profile in a box
!    at your defined position, using the program GAUMAG (qv). You have
!    to define the X and Y size of the box round each star to use. The
!    maximum size of a side is 30.
!
!        The transformation is calculated by the program XYFIT (qv) and
!    applied using the program RESAMPLE (qv) and in its operation
!    the methods NEAREST,LINEAR, or UNIFORM can be used.
!        The transformation is done by taking each pixel position
!    in the Output image, calculating the position in the Input
!    image that corresponds to, and taking the value of the nearest
!    pixel in the Input image and puting that value in the Output image.
!
!   WARNING:-
!        It creates and destroys files TEMPXY,TEMPXYC,TEMPAXY,TEMPAXYC
!    in your present directory, so you mustnt have any of those
!    names.
!
!  A J PENNY              RGO                    OCT-82
!-----------------------------------------------------------
!
!
$ IF P1 .NES. "" THEN GOTO A
$ INQUIRE P1 "REFERENCE IMAGE ?"
A:
!
$ IF P2 .NES. "" THEN GOTO B
$ INQUIRE P2 "INPUT IMAGE ?"
B:
!
$ IF P3 .NES. "" THEN GOTO C
$ INQUIRE P3 "OUTPUT IMAGE ?"
C:
!
$ IF P4 .NES. "" THEN GOTO D
$ INQUIRE P4 "METHOD (NEAREST,LINEAR,UNIFORM) ?"
D:
!
!
!
$ IF P5 .NES. "" THEN GOTO E
$ INQUIRE P5 "X SIZE OF GAUSS BOX = ?"
E:
$ IF P6 .NES. "" THEN GOTO F
$ INQUIRE P6 "Y SIZE OF GAUSS BOX = ?"
F:
XBOX = P5
IF XBOX .GT. 30 THEN XBOX = 30
YBOX = P6
IF YBOX .GT. 30 THEN YBOX = 30
!
!
!
$ ARESET
$ ICDISP 'P1' = = MDIS=T
$ XYCURA OVERCL=Y INPUT= OUTPUT=TEMPXY TITLE=T
!
!
$ ARESET
$ ICDISP 'P2' = = MDIS=T
$ LET XYCURB_LIMITS= 
$ XYCURB OVERCL=Y INPUT=TEMPXY ALLINP=YES OUTPUT=TEMPAXY TITLE=T
!
!
!
$ LET GAUMAG_PARFIX = NO
$ LET GAUMAG_XBOX = 'XBOX'
$ LET GAUMAG_YBOX = 'YBOX'
$ GAUMAG IMAGE='P1' XYLIST=TEMPXY OUTPUT=TEMPXYC TITLE=TEMPXYC
!
!
$ LET GAUMAG_PARFIX = NO
$ LET GAUMAG_XBOX = 'XBOX'
$ LET GAUMAG_YBOX = 'YBOX'
$ GAUMAG IMAGE='P2' XYLIST=TEMPAXY OUTPUT=TEMPAXYC TITLE="T"
$ EDRS:XYFIT INPUTA=TEMPXYC INPUTB=TEMPAXYC NSIGMA=2.5
$ LET RESAMPLE_METHOD='P4'
$ LET RESAMPLE_TRCOEFFS=XYFIT_TRCOEFFS
$ LET RESAMPLE_TITLE='P3'
$ EDRS:RESAMPLE INPUT='P2' OUTPUT='P3'
!
!
$ CLEAR XYCURB_
$ CLEAR GAUMAG_
$ CLEAR RESAMPLE_
$ DELETE TEMPAXY.BDF;1,TEMPAXYC.BDF;1,TEMPXY.BDF;1,TEMPXYC.BDF;1
!
!
