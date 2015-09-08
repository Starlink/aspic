      subroutine scazer(scales,zeros,nval,bscale,bzero)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates a scale and zero factor which will allow all data
*       values represented with a range of different scale and zeros,
*       to be represented by one.
*
*SOURCE
*       SCAZER.FOR in UTILITIES.TLB
*
*METHOD
*       The maximum and minimum data values representable by each of
*       the input scalings are calculated, and the maximum and minimum
*       of all of them found. This gives the total data range
*       representable by all of the input images. A scale and zero
*       are then calculated which will be three quarters covered by
*       the total possible input data range.
*
*ARGUMENTS
*   INPUTS:
*       nval            integer The number of input images
*       scales(nval)    real    The scale values of the input images
*       zeros(nval)     real    The zero values of the input images
*   OUTPUTS:
*       bscale          real    The scale value for the output
*       bzero           real    The zero value for the output
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   nval
      real      scales(nval),zeros(nval),bscale,bzero
*
* DECLARE LOCAL VARIABLES
*
      integer   maxint  ! Maximum integer storeable in image files
      integer   minint  ! Minimum integer storeable in image files
      parameter (maxint=32767,minint=-32767)

      real      datmax  ! Maximum possible data value in inputs
      real      datmin  ! Minimum possible data value in inputs
      integer   i       ! Loop count
*
* FIND MAXIMUM AND MINIMUM DATA VALUES REPRESENTABLE WITH THE LIST OF
* INPUT SCALINGS
*
      datmax=maxint*scales(1)+zeros(1)
      datmin=minint*scales(1)+zeros(1)
      do i=2,nval
         datmax=max(datmax,maxint*scales(i)+zeros(i))
         datmin=min(datmin,minint*scales(i)+zeros(i))
      enddo
*
* CALCULATE A BSCALE AND BZERO WHICH WILL BE 3/4 COVERED BY THE INPUT
* DATA RANGE
*
      if((datmax-datmin).gt.1.0e-20) then
         bscale=(datmax-datmin)/(0.75*(maxint-minint))
         bzero=((datmax+datmin)-(maxint+minint)*bscale)*0.5
      else if(datmax.ge.datmin) then
         bscale=1.0
         bzero=datmin
      else
         bscale=1.0
         bzero=0.0
      endif
*
* FINISH
*
      end
