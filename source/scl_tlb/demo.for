!
!+	DSCL procedure DEMO
!
!	Demonstration of DSCL, prepared for release 17 November 1982
!
!			Clear ALL ARGS functions and memories
	ARESET
!			Put an image onto the stack
!			Note	a NAME is put on the stack
!				does NOT create a new image
	PUSH ASPDIR:HORSE
!			Make a copy of the top of the stack
!			(Some operations replace the top of stack)
	DUPE
!			Display the top of the stack.
!			Note	= means accept run time default
!				$ means top of the stack
	AFLASH $ = = = =
!			Zoom about the centre of the last image
!			so that it fills the screen
!			Note	default values found from the
!				ARGS database.
	AZOOM = = = =
!			Read a previously stored look-up-table
	LUTCOL
!			Draw a 0-255 ramp; location defined from
!			ARGS database.
	ABLOCK = = =
!			Return to zoom of 1,1 and centre 256,256
	UNZOOM
!			Load a previously stored greyscale 
!			look-up-table.
	LUTGREY
!			Allow cursor definition of a slice through
!			an image. The slice is defined on the ARGS
!			but the data are taken, in this case from
!			the top of the stack.
!			The = means accept default ie do not store
!			the resulting slice.
	SLICE $ =
!			Perform histogram equalization, replacing the
!			top of the stack with the results.
!			Note that in this case not all parameters
!			are given default values, so you will be
!			prompted for them.
!			Note	try INLIMS=75,125
	HISTMATCH $ $
!			Reset the ARGS again
	ARESET
!			Load the new image AND the second top of stack
!			(the one we DUPEd earlier!)
!			and allow blinking between them.
!			Note	this takes some time to set up.
!				the offsets bewteen the two images
!				in X nad Y are returned to the
!				environment for use by other
!				programs.
!
!+	procedure ABLINK (modified for the demo to get images from the stack)
!
!	display two images, one in bottom 8 bits of ARGS memory, the
!	other in top 8 bits, then run BLINKER program which allows
!	use of trackerball buttons to flip between the two, both
!	manually and automatically.
!
!
ACLEAR
HEXFILE FLNAME=MASK8
AFLASH $ = = = =
HEXFILE FLNAME=ZPAFLIP
HEXFILE FLNAME=MASK8
SWAP
AFLASH $ = = = =
HEXFILE FLNAME=ZPABACK
BLINKER
LOOK BLINKER
HEXFILE FLNAME=MASK8
HEXFILE FLNAME=ZPABACK
HEXFILE FLNAME=MASK8
!			Allow trackerball panning/zooming
	APAN
!			Delete the top of the stack
!			Note 	the name not the image.
	POP
!			Ditto
	POP
!			END OF DEMONSTRATION
!			(There could be an EXIT here if wished)
