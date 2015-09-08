!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  *********************
!  *                   *
!  * PROCEDURE ICBLINK *
!  *                   *
!  *********************
!
!
!      CALLING SEQUENCE
!           ICBLINK 1STIMAGE 2NDIMAGE
!
!
!	Display two images, one in bottom 8 bits of ARGS memory, the
!	other in top 8 bits, then run BLINKER program which allows
!	use of trackerball buttons to flip between the two, both
!	manually and automatically.
!         You must also alter the lookup table of each image to
!     change and, if desired, match the brightnesses of the images
!
!
!     USE:-
!        After inputting the images, you will be asked for the
!     area of the 1st image you want (Xlimits and Ylimits) and
!     then the range of the black-white contrast for the image
!     values (PVLO,PVHI). A default answer in all cases will
!     give you the complete range. The image will then be put
!     up on the ARGS and you must change the black-white limits
!     by setting new ones by using the cursor to define a
!     horizontal fraction of the image width to represent the
!     the upper (white 2 button) or lower (white 1 button) limits
!     as proportion of the input limits. Press red button to exit.
!        Repeat for image 2.
!
!        The display will then come up and you can flip between
!     the two images by pressing the white buttons (white button 1
!     gets image 1, white button 2 gets image 2). Or you can
!     flip automatically between the images by pressing the green
!     button. Subsequent pressing of the green button will halve
!     the blink rate.
!     Movement of the trackerball will shift the first image relative
!     to the second.
!        Stop by pressing the red button. You will then have the
!     chance to alter the lookup table for the image currently
!     displayed. If you do the blinking will restart afterwards. If
!     you dont the program will end.
!
!
!      A J PENNY                  RGO                   OCT-82
!--------------------------------------------------------------
!
!     ICBLINK.SCL
!
!
!
!
IF P1 .NES "" THEN GOTO A1
INQUIRE P1 "IMAGE 1 = ? "
A1:
IF P2 .NES. "" THEN GOTO A2
INQUIRE P2 "IMAGE 2 = ? "
A2:
PUSH 'P1'
PUSH 'P2'
!
!
!
ARESET
LUTGREY
HEXFILE FLNAME=MASK8
WRITE SYS$OUTPUT " "
WRITE SYS$OUTPUT "FILE 1 DATA"
ICDISP 'P1'
WRITE SYS$OUTPUT "USE LUTLIN CURSORS TO SCALE DISPLAY"
WRITE SYS$OUTPUT "YOU MUST SET AN UPPER OR LOWER LIMIT AT LEAST ONCE"
LET LUTLIN_OUTPUT=LUTTEMP1
LUTLIN
!
!
!
SWAP
HEXFILE FLNAME=ZPAFLIP
HEXFILE FLNAME=MASK8
WRITE SYS$OUTPUT " "
WRITE SYS$OUTPUT "FILE 2 DATA"
LUTGREY
ICDISP 'P2'
WRITE SYS$OUTPUT "USE LUTLIN CURSORS TO SCALE DISPLAY"
WRITE SYS$OUTPUT "YOU MUST SET AN UPPER OR LOWER LIMIT AT LEAST ONCE"
LET LUTLIN_OUTPUT=LUTTEMP2
LUTLIN
!
!
!
SWAP
HEXFILE FLNAME=ZPABACK
B1:
WRITE SYS$OUTPUT " "
WRITE SYS$OUTPUT "USE BUTTONS TO BLINK"
WRITE SYS$OUTPUT "LIT WHITE TO SWAP, GREEN TO FLICKER AND CHANGE SPEED"
WRITE SYS$OUTPUT "AND RED TO EXIT. MOVE TRACKERBALL TO SHIFT IMAGE 1"
LET GBLINKER_LUT1=LUTTEMP1
LET GBLINKER_LUT2=LUTTEMP2
GBLINKER
!
WRITE SYS$OUTPUT " "
INQUIRE B "WANT TO CHANGE LU TABLE (Y/N) ?"
IF B .NES. "Y" THEN GOTO B2
$  X='F$LOGICAL("GBLINKER_PICT")'
WRITE SYS$OUTPUT "USE LUTLIN CURSOR TO CHANGE LUT FOR IMAGE ''X' "
WRITE SYS$OUTPUT "YOU MUST SET AN UPPER OR LOWER LIMIT AT LEAST ONCE"
LUTLIN OUTPUT=LUTTEMP'X'
GOTO B1
B2:
!
!
!
WRITE SYS$OUTPUT " "
WRITE SYS$OUTPUT "DETAILS OF BLINKING"
LOOK GBLINKER
!
!
!
HEXFILE FLNAME=MASK8
HEXFILE FLNAME=ZPABACK
HEXFILE FLNAME=MASK8
!
!
!
CLEAR LUTLIN_
CLEAR GBLINKER_
!
!
!
