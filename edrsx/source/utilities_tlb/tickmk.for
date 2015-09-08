      subroutine tickmk(valhi,vallo,mntick,sbtick,nsubtk)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To calculate spacing for graph axis tick marks which gives
*       nice round numbers
*
*SOURCE
*       TICKMK.FOR in UTILITIES.TLB
*
*METHOD
*       Tick marks are of two types, main ticks which have value labels
*       and subticks which dont. This routine aims for 10 main ticks
*       between the axis limits specified, but modifies this to get nice
*       round numbers at the main tick marks.
*
*ARGUMENTS
*   INPUTS:
*       valhi   real    The upper limit of the data values to be plotted.
*       vallo   real    The lower limit of the data values to be plotted.
*   OUTPUTS:
*       mntick  real    The spacing between main tick marks. These should
*                       be labelled with their data value on the graph.
*       sbtick  real    The spacing between sub ticks. These should not be
*                       labelled.
*       nsubtk  integer The no. of sub ticks between main ticks
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       NONE
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      valhi,vallo,mntick,sbtick
      integer   nsubtk
*
* DECLARE LOCAL VARIABLES
*
      real      magtic  ! The power of 10 less than the exact main tick spacing
      integer   nsbtic(9)!The no. of sub-ticks between main ticks
      data      nsbtic  /4,4,4,4,5,5,10,5,5/
      integer   rndtic  ! The no. of magtic's in trgtic
      integer   ticfac(9)! Factors which round the tick spacing to a nice value
      data      ticfac  /2,2,4,4,5,5,10,10,10/
      real      trgtic  ! The main tick spacing which gives exactly 10 ticks
*
* CALCULATE LABELLED TICK MARK SPACING WHICH WOULD GIVE
* EXACTLY 10 TICKS
*
      trgtic=(valhi-vallo)*0.1
*
* CALCULATE THE POWER OF TEN JUST SMALLER THAN OR EQUAL TO THIS OPTIMUM SPACING
*
      magtic=10.0**(int(log10(trgtic)))
*
* TRUNCATION IN VAX FORTRAN IS ALWAYS TOWARDS ZERO, SO FOR NEGATIVE
* EXPONENTS THE ABOVE TRUNCATION WILL BE IN THE WRONG DIRECTION (UNLESS
* TRGTIC IS AN EXACT POWER OF TEN). ASSUME FOR THE MOMENT THAT TRGTIC IS
* NOT AN EXACT POWER OF TEN AND CORRECT ALL NEGATIVE EXPONENT VALUES.
*
      if(trgtic.lt.1) magtic=magtic*0.1
*
* FIND OUT HOW MANY OF THESE POWERS OF TEN FIT INTO THE OPTIMUM SPACING
*
      rndtic=int(trgtic/magtic)
*
* IF MAGTIC WAS AN EXACT POWER OF TEN, AND LESS THAN ONE, THEN THE ABOVE
* CORRECTION FOR TRUNCATION SHOULDN'T HAVE BEEN DONE. SO TAKE OUT THE
* CORRECTION.
*
      if(rndtic.eq.10) then
         rndtic=1
         magtic=10*magtic
      endif
*
* USE THE NO. OF MAGTICS IN TRGTIC TO LOOK UP A FACTOR WHICH GIVES
* NICE MULTIPLES OF MAGTIC NEAR TO THE EXACT VALUE
*
      mntick=ticfac(rndtic)*magtic
*
* CALCULATE THE SIZE OF SUB TICK MARKS (UNLABELLED)
*
      nsubtk=nsbtic(rndtic)-1
      sbtick=mntick/nsbtic(rndtic)
*
* FINISH
*
      end
