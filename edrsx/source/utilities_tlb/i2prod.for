      subroutine i2prod(data,npix,nlin,rinval,scale,zero,iout,inval)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts an image stored in REAL format into one stored in
*       I*2 format, by calculating scaling factors which make the
*       total input range fit into 0.75 of the output range. The
*       input is then scaled with these factors to produce the I*2
*       output image. If the flag value for invalid pixels in the
*       input is 0.0, then all pixels are assumed to be valid.
*
*SOURCE
*       I2PROD.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) real    Input image
*       npix            integer No. of pixels in each line of input
*       nlin            integer No. of lines in input image
*       rinval          real    Flag for invalid pixels in input
*       inval           integer Flag for invalid pixels in output
*   OUTPUTS:
*       scale           real    Scale factor used for scaling output
*       zero            real    Zero offset used for scaling output
*       iout(npix,nlin) I*2     Output I*2 image
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
*       D.S. Berry (MAVAD::DSB) 29/2/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval
      real      data(npix,nlin),rinval,scale,zero
      integer*2 iout(npix,nlin)
*
* DECLARE LOCAL VARIABLES
*
      real      dmax    ! Maximum value in input image
      real      dmin    ! Minimum value in input image
      real	dynran	! Dynamic range of data
      integer   ilin    ! Line counter
      integer   ipix    ! Pixel counter
      integer   maxint  ! Max integer representable in output I*2 format
      integer   minint  ! Min integer representable in output I*2 format
      real      rscl    ! Reciprocal of scale factor

      parameter (maxint=32767,minint=-32767)

      if(rinval.ne.0.0) then
*--------------------------------------------------------------------
* THIS SECTION IS USED IF THERE COULD BE INVALID PIXELS IN INPUT
*
* FIND MIN AND MAX VALUE IN INPUT REAL IMAGE
*
         dmax=-1.0e32
         dmin=1.0e32
         do ilin=1,nlin
            do ipix=1,npix
               if(data(ipix,ilin).ne.rinval) then
                  dmax=max(dmax,data(ipix,ilin))
                  dmin=min(dmin,data(ipix,ilin))
               endif
            enddo
         enddo
*
* IF ALL PIXELS ARE INVALID THEN SET SCALE=1 ZERO=0
*
         if(dmax.eq.-1.0e32) then
            scale=1.0
            zero=0.0
         else
*
* OTHERWISE CALCULATE SCALE AND ZERO WHICH USE 0.75 OF AVAILABLE
* RANGE OF OUTPUT INTEGERS
*
            if(dmax+dmin.ne.0.0) then
               dynran=abs( (dmax-dmin)/(dmax+dmin) )
            else if(dmax-dmin.ne.0) then
               dynran=1.0e30
            else
               dynran=0
            endif

            if(dynran.gt.1.0e-6) then
               scale=(dmax-dmin)/(0.75*(maxint-minint))
               zero=((dmax+dmin)-(maxint+minint)*scale)*0.5
               rscl=1.0/scale

            else if(dmax.ge.dmin) then
               scale=1.0
               zero=dmin
               rscl=1.0

            else
               scale=1.0
               zero=0.0
               rscl=1.0
            endif
         endif
*
* CONVERT THE RESULTS TO INTEGERS, TAKING ACCOUNT OF INVALID RESULTS
*
         do ilin=1,nlin
            do ipix=1,npix
               if(data(ipix,ilin).ne.rinval) then
                  iout(ipix,ilin)=nint((data(ipix,ilin)-zero)*rscl)
               else
                  iout(ipix,ilin)=inval
               endif
            enddo
         enddo
      else
*--------------------------------------------------------------------
* THIS SECTION IS USED IF IT IS KNOWN THAT THERE ARE NO INVALID PIXELS
* IN THE INPUT
*
         dmax=-1.0e32
         dmin=1.0e32
         do ilin=1,nlin
            do ipix=1,npix
               dmax=max(dmax,data(ipix,ilin))
               dmin=min(dmin,data(ipix,ilin))
            enddo
         enddo

         if(dmax+dmin.ne.0.0) then
            dynran=(dmax-dmin)/(dmax+dmin)
         else if(dmax-dmin.ne.0) then
            dynran=1.0e30
         else
            dynran=0
         endif

         if(dynran.gt.1.0e-6) then
            scale=(dmax-dmin)/(0.75*(maxint-minint))
            zero=((dmax+dmin)-(maxint+minint)*scale)*0.5
            rscl=1.0/scale
         else if(dmax.ge.dmin) then
            scale=1.0
            zero=dmin
            rscl=1.0
         else
            scale=1.0
            zero=0.0
            rscl=1.0
         endif
         do ilin=1,nlin
            do ipix=1,npix
               iout(ipix,ilin)=nint((data(ipix,ilin)-zero)*rscl)
            enddo
         enddo
      endif
*
* FINISH
*
      end
