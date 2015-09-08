      subroutine lnshft(in,out,nlines,npixin,npxout,inval,shifts,
     :                  datmax,datmin,lines,sizlns,usedls)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Produces an an output image by shifting every row of the input
*       image by a specified amount in the x direction. The shift is
*       specified separately for each line and can be fractional, in
*       which case linear interpolation is perfomed among the input
*       pixel values.
*             Also, returns the max and min values found within a
*       specified set of lines.
*
*SOURCE
*       LNSHFT.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       in      integer         The input image
*       nlines  integer         The no. of lines in input and output
*       npixin  integer         The no. of pixels in each line of input
*       npxout  integer         The no. of pixels in each line of output
*       inval   integer         The invalid pixel value
*       shifts  real            The shift for each line of the input
*       lines(sizlns) integer   A set of line no.s for which the max
*                               min data values are to be found. These
*                               must be in order of increasing line no.
*       sizlns  integer         The size of array lines
*       usedls  integer         The number of entries in lines to use
*   OUTPUTS:
*       datmax  real            Maximum of output data values from
*                               selected lines
*       datmin  real            Minimum of output data values from
*                               selected lines
*       out     real            The output image
*
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               hint
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   sizlns,usedls,lines(sizlns)
      integer   npixin,nlines,npxout,in(npixin,nlines),inval
      real      shifts(nlines),out(npxout,nlines),datmax,datmin
*
* DECLARE LOCAL VARIABLES
*
      real      conthi  ! The contribution to the final pixel value
                        ! by the higher of the 2 input pixels being
                        ! interpolated
      real      contlo  ! The contribution to the final pixel value
                        ! by the lower of the 2 input pixels being
                        ! interpolated
      real      dshift  ! The fractional part of a shift
      integer   hint    ! Function giving nearest integer in +ve direction
      integer   iline   ! Pointer to next line to be monitored
                        ! for max and min values
      logical   inset   ! True if current line is to be
                        ! monitored for max and min values
      integer   ishift  ! The integer part of a shift
      real      weight  ! The sum of the weights of the 2 input pixels
                        ! being interpolated
      integer   xlo     ! The x index of the lower of the 2 input pixels
                        ! being interpolated
      integer   xhi     ! The x index of the higher of the 2 input
                        ! pixels being interpolated
      integer   yy      ! The y index of the line being processed
      integer   xx      ! The x index of the output pixel being
                        ! calculated
*
* INITIALIZE MAX AND MIN DATA VALUES AND POINTER TO CURRENT ENTRY IN
* SET OF LINES FOR WHICH MAX AND MIN VALUES ARE REQUIRED
*
      datmax=-1.0e32
      datmin=1.0e32
      iline=1
*
* LOOP THROUGH THE LINES OF THE OUTPUT IMAGE
*
      do yy=1,nlines
*
* SEE IF THE CURRENT LINE IS IN THE SET OF LINES FOR WHICH MAX AND MIN
* VALUES ARE TO BE FOUND
*
         if(iline.le.usedls.and.yy.eq.lines(iline)) then
            inset=.true.
            iline=iline+1
         else
            inset=.false.
         endif
*
* CALCULATE THE INTEGER AND FRACTIONAL PARTS OF THE SHIFT FOR THIS LINE
*
         ishift=hint(shifts(yy))
         dshift=1+shifts(yy)-ishift
*
* INITIALIZE XLO, THE POINTER TO THE LOWER OF THE TWO INTERPOLATED INPUT PIXELS
*
         xlo=-ishift
*
* LOOP THROUGH THE PIXELS OF THE CURRENT LINE OF THE OUTPUT IMAGE
*
         do xx=1,npxout
*
* CALCULATE THE INDEX OF EACH OF THE 2 CONTRIBUTING INPUT PIXELS
*
            xlo=xlo+1
            xhi=xlo+1
*
* initialise sum of weights and individual contributions to zero
*
            weight=0
            contlo=0
            conthi=0
*
* IF THE LOWER INPUT PIXEL IS VALID CALCULATE ITS CONTRIBUTION TO THE
* OUTPUT PIXEL
*
            if(xlo.ge.1.and.xlo.le.npixin) then
               if(in(xlo,yy).ne.inval) then
                  contlo=in(xlo,yy)*dshift
                  weight=dshift
               endif
            endif
*
* IF THE UPPER INPUT PIXEL IS VALID CALCULATE ITS CONTRIBUTION TO THE
* OUTPUT PIXEL
*
            if(xhi.ge.1.and.xhi.le.npixin) then
               if(in(xhi,yy).ne.inval) then
                  conthi=in(xhi,yy)*(1-dshift)
                  weight=weight+1-dshift
               endif
            endif
*
* CALCULATE THE TOTAL OUTPUT PIXEL VALUE
*
            if(weight.gt.0) then
               out(xx,yy)=(conthi+contlo)/weight
*
* UPDATE MAX AND MIN VALUES IF CURRENT LINE IS IN THE SET DESCRIBED BY
* ARRAY 'LINES'
*
               if(inset) then
                  datmax=max(datmax,out(xx,yy))
                  datmin=min(datmin,out(xx,yy))
               endif
            else
               out(xx,yy)=inval
            endif
*
* FINISH
*
         enddo
      enddo

      end
