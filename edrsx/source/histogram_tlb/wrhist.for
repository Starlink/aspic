      subroutine wrhist(hist,maxint,minint,scale,zero,output)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Scale a histogram into an output I*2 array and return the
*       BSCALE and BZERO values used.
*
*SOURCE
*       WRHIST.FOR in HISTOGRAM.TLB
*
*METHOD
*       It is assumed that all bins are valid. If a bin has a negative
*       value, it is replaced by zero.
*
*ARGUMENTS
*   INPUTS:
*       hist(minint:maxint)     integer Input histogram
*       minint,maxint           integer Min and max integers allowed
*                                       in histogramed image
*   OUTPUTS:
*       scale                   real    The scale factor used to produce
*                                       output
*       zero                    real    The zero offset used to produce
*                                       output
*       output(minint:maxint)   I*2     Output I*2 image
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   maxint,minint
      integer   hist(minint:maxint)
      real      scale,zero
      integer*2 output(minint:maxint)
*
* DECLARE LOCAL VARIABLES
*
      integer   hilim   ! Highest integer value in any bin of input
      integer   i       ! Loop count
      integer   ival    ! Integer value stored in current bin
      integer   lolim   ! Lowest integer value in any bin of input
      real      rscl    ! Reciprocal of scale
*
* FIND MIN AND MAX VALUE IN ANY BIN
*
      lolim=100000000
      hilim=0
      do i=minint,maxint
         ival=max(0,hist(i))
         lolim=min(lolim,ival)
         hilim=max(hilim,ival)
      enddo
*
* CALCULATE SCALE AND ZERO FACTORS
*
      if(hilim.gt.lolim) then
         if(hilim-lolim.lt.maxint-minint) then
            scale=1.0
            zero=lolim-minint
            rscl=1.0
         else
            scale=(hilim-lolim)/(0.75*(maxint-minint))
            zero=((hilim+lolim)-(maxint+minint)*scale)*0.5
            rscl=1.0/scale
         endif
      else
         scale=1.0
         zero=0.0
         rscl=1.0
      endif
*
* COPY INPUT TO OUTPUT ADDING SCALING
*
      do i=minint,maxint
         output(i)=nint((hist(i)-zero)*rscl)
      enddo
*
* FINISH
*
      end
