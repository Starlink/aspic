      subroutine memcc9(sample,c1,c2,c3,c4,pixel,line)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the information required to do bi-linear interpolation
*       within a sky image at the position of any data sample centre.
*
*SOURCE
*       MEMCC9.FOR in MEMCRDD.TLB
*
*METHOD
*	Find the sky value at the current psf pixel location using 
*       bilinear interpolation among  4 adjacent pixels. NB, only
* 	samples lying at least 3 arcmins from the image edge are
*	used, therefore the indexing done here cannot go out of the 
*       declared array bounds. 
*       
*ARGUMENTS       
*   INPUT:
*	sample	integer		The sample no. within a data set
*   OUTPUTS:
*       c1	real		Contribution from bottom left pixel
*       c2	real		Contribution from bottom right pixel
*       c3	real		Contribution from top left pixel
*       c4	real		Contribution from top right pixel
*	pixel	integer		Closest pixel no. to sample centre
*	line	integer		Closest line no. to sample centre
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/
*
*SUBROUTINES CALLED
*       none
*              
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... POINTERS TO MEMCRDD INTERNAL FILES HELD IN ARRAY ME_ST
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      integer	sample,pixel,line
      real	c1,c2,c3,c4
      
*
* DECLARE LOCAL VARIABLES
*
      real	dx	! Increment in x from centre of the bottom left
		  	! of the 4 pixels closest to the data sample,
			! to the centre of the data sample
      real	dy	! Increment in y from centre of the bottom left
		  	! of the 4 pixels closest to the data sample,
			! to the centre of the data sample
      real	skyx	! X coord of centre of data sample
      real	skyy	! Y coord of centre of data sample

*
* EXTRACT THE FRACTIONAL LOCATION OF THE CENTRE OF THIS DATA SAMPLE
* WITHIN THE FRAME OF THE SKY IMAGE
*
      skyx=ME_st(B6_x+sample-1)
      skyy=ME_st(B6_y+sample-1)

*
* FIND THE BOTTOM LEFT OF THE FOUR CLOSEST PIXELS. NB, THE BLANK IMAGE
* BORDER ENSURES THAT SKYX AND SKYY ARE ALWAYS GREATER THAN ONE 
*
      line=int(skyy)
      pixel=int(skyx)

*
* FIND THE OFFSET FROM THE CENTRE OF THE BOTTOM LEFT PIXEL TO THE DATA
* SAMPLE CENTRE
*
      dx=skyx-real(pixel)
      dy=skyy-real(line)

*
* CALCULATE THE COEFFICIENT OF EACH OF THE FOUR CLOSEST PIXELS IN
* THE FINAL INTERPOLATED DATA VALUE
*
      c1=(1.0-dx)*(1.0-dy)
      c2=dx*(1.0-dy)
      c3=(1.0-dx)*dy
      c4=dx*dy

*
* FINISH
*
      end
