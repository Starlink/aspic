      subroutine convim(data,npixd,nlind,psf,npixp,nlinp,out,rinval,
     :                  outmin,outmax,psfxcn,psfycn,nout,
     :                  minpix,ninval,wout,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To convolve an entire image with a PSF defined by a second
*       image. The PSF image is assumed to be defined with the same
*       grid spacing and orientation as the data image.
*
*SOURCE
*       CONVIM.FOR in UTILITIES.TLB
*
*METHOD
*       The convolution is done by looping through every
*       pixel in the input. Each valid input pixel is weighted by
*       every valid, non-zero PSF pixel and the products summed into
*       the appropriate output pixel. A record is kept of the sum of
*       the weights for each output pixel, which is used to normalise
*       the final output pixel value. A count is also kept of the number
*       of valid input pixels which contribute to each output pixel.
*       If an output pixel has less contributions than specified by
*       argument minpix, then it is flagged as invalid.
*               If the PSF has a total sum of zero the final
*       normalisation of the output values cannot be done. In this
*       case the output values are not normalised and the user is
*       given a warning message.
*
*ARGUMENTS
*   INPUTS:
*       data(npixd,nlind)       real    The data image
*       npixd,nlind             integers The size of the data image
*       psf(npixp,nlinp)        real    The PSF image
*       npixp,nlinp             integers The size of the PSF image
*       rinval                  real    Flag for invalid pixels in PSF
*                                       and data images
*       psfxcn                  integer The location of the psf centre
*                                       within the psf image in x
*       psfxcn                  integer The location of the psf centre
*                                       within the psf image in y
*       minpix                  integer Min no. of contributions
*                                       required for an output pixel
*   OUTPUTS:
*       out(npixd,nlind)        real    The output convolved image
*       nout(npixd,nlind)       integer The no. of contributions to
*                                       each output pixel
*       wout(npixd,nlind)       real    Temp real workspace
*       outmax                  real    Max value in output image
*       outmin                  real    Min value in output image
*       ninval                  integer No. of invalid pixels in output
*       ierr                    integer Error status: 0 - Success
*                                        1 - PSF centre is outside psf
*                                            image
*                                        2 - No valid pixels in output
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr
*
*STARLINK PARAMETERS
*       ZEROFILT/error/         Accessed if the PSF has zero data sum
*       NONVAL/error/           Accessed if there are no valid pixels
*                               in the output image
*       PSFCEN/error/           Accessed if PSF centre is outside PSF
*                               image.
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npixd,nlind,npixp,nlinp,ierr,psfxcn,psfycn,minpix,ninval
      integer   nout(npixd,nlind)
      real      data(npixd,nlind),psf(npixp,nlinp),out(npixd,nlind)
      real      outmin,outmax,rinval,wout(npixd,nlind)
*
* DECLARE LOCAL VARIABLES
*
      integer   dlin    ! No. of current line in data
      integer   dpix    ! No. of current pixel in data
      real      factor  ! Factor to normalize psf to average value of 1
      integer   lin     ! Image line counter
      integer   ncont   ! No. of valid contributions to convolution
      logical   nonval  ! True if no valid output pixels yet found
      integer   nval    ! No. of valid pixels in psf
      integer   olin    ! No. of current line in output
      integer   opix    ! No. of current pixel in output
      logical   psfinv  ! True if there are any invalid psf pixels
      integer   pix     ! Image pixel counter
      real      pixval  ! The current data pixel value
      integer   psflin  ! No. of current line in psf
      integer   psfpix  ! No. of current pixel in psf
      real      psfsum  ! Sum of all data in psf image
      real      psfval  ! Current PSF pixel value
      integer   shiftx  ! Offset in x between psf image and data image
      integer   shifty  ! Offset in y between psf image and data image
*
* INITIALIZE ERROR STATUS TO SUCCESS
*
      ierr=0
*
* THE PSF CENTRE MUST LIE WITHIN THE PSF IMAGE. IF NOT END WITH IERR=1
*
      if(psfxcn.lt.1.or.psfxcn.gt.npixp.or.
     :   psfycn.lt.1.or.psfycn.gt.nlinp) then
         ierr=1
         call wrerr('PSFCEN')
         goto 999
      endif
*
* CALCULATE THE TOTAL DATA SUM IN THE PSF IMAGE.
*
      psfinv=.false.
      psfsum=0
      do lin=1,nlinp
         do pix=1,npixp
            if(psf(pix,lin).ne.rinval) then
               psfsum=psfsum+psf(pix,lin)
            else
               psfinv=.true.
            endif
         enddo
      enddo
*
* IF THE PSF HAS A TOTAL DATA SUM OF ZERO, THEN THE OUTPUT IMAGE
* CANNOT BE NORMALISED TO THE INPUT IMAGE. GIVE A WARNING MESSAGE.
*
      psfsum=abs(psfsum)
      if(psfsum.lt.1.0e-20) call wrerr('ZEROFILT')
*
* INITIALISE OUTPUT ARRAYS
*
      do olin=1,nlind
         do opix=1,npixd
            out(opix,olin)=0
            nout(opix,olin)=0
            wout(opix,olin)=0
         enddo
      enddo
*
* IF THERE ARE NO INVALID PIXELS IN PSF, THEN JUMP FORWARD AND DO
* THE CONVOLUTION WITHOUT CHECKING FOR INVALID PSF PIXELS
*
      if(psfinv) then
*
* LOOP THROUGH THE LINES OF THE INPUT IMAGE AND MULTIPLY EVERY VALID
* INPUT PIXEL BY EVERY VALID PSF PIXEL. (THE PRODUCTS
* ARE ADDED INTO THE APPROPRIATE OUTPUT PIXEL SUM AND A COUNT KEPT OF
* THE NO. OF CONTRIBUTIONS MADE TO EACH OUTPUT PIXEL)
*
         do dlin=1,nlind
            shifty=psfycn+dlin
            do dpix=1,npixd
*
* STORE THE CURRENT INPUT PIXEL TO AVOID EXCESSIVE ACCESSING OF ARRAYS
*
               pixval=data(dpix,dlin)
*
* INVALID PIXELS DONT CONTRIBUTE TO THE OUTPUT VALUE
*
               if(pixval.ne.rinval) then
                  shiftx=psfxcn+dpix
*
* LOOP THROUGH THE PSF LINES MULTIPLYING EVERY VALID PIXEL BY THE
* CURRENT INPUT PIXEL VALUE
*
                  do psflin=max(1,shifty-nlind),min(nlinp,shifty-1)
*
* CALCULATE THE OUTPUT LINE TO WHICH THE CURRENT PRODUCTS CONTRIBUTES
*
                     olin=shifty-psflin
*
* LOOP THROUGH THE PSF PIXELS
*
                     do psfpix=max(1,shiftx-npixd),min(npixp,shiftx-1)
*
* CALCULATE THE OUTPUT PIXEL TO WHICH THE CURRENT PRODUCT CONTRIBUTES
*
                        opix=shiftx-psfpix
*
* INCREMENT THE NO. OF VALID DATA PIXELS INCLUDED IN THIS OUTPUT PIXEL
*
                        nout(opix,olin)=nout(opix,olin)+1
*
* IF THE PSF PIXEL IS VALID, INCREMENT THE OUTPUT PIXEL VALUE BY THE
* PRODUCT OF THE PSF AND THE DATA, AND INCREMENT THE TOTAL WIEGHT FOR
* THIS OUTPUT PIXEL
*
                        psfval=psf(psfpix,psflin)
                        if(psfval.ne.rinval) then
                           out(opix,olin)=out(opix,olin)+pixval*psfval
                           wout(opix,olin)=wout(opix,olin)+psfval
                        endif
                     enddo
                  enddo
               endif
            enddo
         enddo
      else
*-----------------------------------------------------------------------
* DO THE SAME AGAIN OMITTING THE INVALID PSF PIXEL CHECK IF THERE ARE
* NO INVALID PSF PIXELS
*
         do dlin=1,nlind
            shifty=psfycn+dlin
            do dpix=1,npixd
               pixval=data(dpix,dlin)
               if(pixval.ne.rinval) then
                  shiftx=psfxcn+dpix
                  do psflin=max(1,shifty-nlind),min(nlinp,shifty-1)
                     olin=shifty-psflin
                     do psfpix=max(1,shiftx-npixd),min(npixp,shiftx-1)
                        opix=shiftx-psfpix
                        nout(opix,olin)=nout(opix,olin)+1
                        psfval=psf(psfpix,psflin)
                        out(opix,olin)=out(opix,olin)+pixval*psfval
                        wout(opix,olin)=wout(opix,olin)+psfval
                     enddo
                  enddo
               endif
            enddo
         enddo
      endif
*--------------------------------------------------------------------
* NORMALIZE THE OUTPUT VALUES AND NOTE THE MAXIMUM AND MINIMUM OUTPUT
* VALUES, AND THE NO. OF INVALID OUTPUT PIXELS
*
      outmax=-1.0e32
      outmin=1.0e32
      ninval=0
      nonval=.true.
      do olin=1,nlind
         do opix=1,npixd
*
* FIRST CHECK THAT SUFFICIENT DATA PIXELS HAVE BEEN INCLUDED IN THE
* OUTPUT PIXEL
*
            if(nout(opix,olin).ge.minpix) then
*
* IF THE SUM OF THE PSF IS ZERO, NO WEIGHTING ARE GIVEN TO OUTPUT PIXELS
*
               if(psfsum.lt.1.0e-20) then
                  outmax=max(outmax,out(opix,olin))
                  outmin=min(outmin,out(opix,olin))
                  nonval=.false.
               else
*
* OTHERWISE, WEIGHT EACH OUTPUT PIXEL BY THE SUM OF THE PSF VALUES
* INCLUDED IN IT
*
                  if(abs(wout(opix,olin)).ge.1.0e-20) then
                     out(opix,olin)=out(opix,olin)/wout(opix,olin)
                     outmax=max(outmax,out(opix,olin))
                     outmin=min(outmin,out(opix,olin))
                     nonval=.false.
                  else
                     out(opix,olin)=rinval
                     ninval=ninval+1
                  endif
               endif
*
* IF THERE WERE INSUFFICIENT VALID DATA PIXELS INCLUDED IN THIS OUTPUT
* PIXEL, THEN SET IT INVALID
*
            else
               out(opix,olin)=rinval
               ninval=ninval+1
            endif
         enddo
      enddo
*
* IF THERE ARE NO VALID PIXELS IN OUTPUT GIVE MESSAGE AND SET ERROR STATUS
*
      if(nonval) then
         call wrerr('NONEVAL')
         ierr=2
      endif
*
* FINISH
*
 999  continue

      end
