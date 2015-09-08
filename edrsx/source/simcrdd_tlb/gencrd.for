      subroutine gencrd(xstart,ystart,alpha,method,
     :                  cdlt1s,cdlt2s,trdtfp,psf,npixp,nlinp,
     :                  invalp,scalep,zerop,sky,npixs,nlins,scales,
     :                  zeros,invals,answer,rinval,ndets,npixps,nlinps,
     :                  size,scanrt,nsamp,crdd,outmax,sampfr,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To generate simulated crdd using the parameters and images
*       obtained by routine SIMCRDD.
*
*SOURCE
*       GENCRD.FOR in SIMCRDD.TLB
*
*METHOD
*       The transformation from PSF image pixel co-ords to sky image
*       pixel co-ords is made up of two parts:
*       1) Transformation from PSF image pixels to focal plane arcmins
*       2) Transformation from focal plane arcmins to sky image pixels
*       Each of these transformations is described by 6 co-efficients.
*       If these are C1 to C6, the transformations are effected as
*       as follows:
*                       X' = C1 + C2*X + C3*Y
*                       Y' = C4 + C5*X + C6*Y
*       Transformation 1) is fixed for each detector and does not depend
*       on sample location (the co-effs are held in the PSF stack
*       descriptor). Tranformation 2) is independant of detector but
*       does depend on sample location. For each combination of detector
*       and sample, the concatenation of the two transformations enables
*       the sky pixel corresponding to any given detector PSF pixel to
*       be found.
*               In general, this sky image location will not lie exactly
*       under the PSF pixel and some form of interpolation is required.
*       Either nearest-neighbour or linear interpolation can be used.
*       In this way each PSF pixel can be multiplied by the
*       corresponding sky pixel, and summed to get the convolution of
*       PSF with the sky, which constitutes a single CRDD sample.
*
*ARGUMENTS
*   INPUTS:
*       xstart          integer         X pixel co-ord of focal plane
*                                       centre for 1st sample in scan
*       ystart          integer         Y pixel co-ord of focal plane
*                                       centre for 1st sample in scan
*       alpha           real            Angle from sky Y axis to scan
*                                       direction (Y to X +ve)
*       method          character       Interpolation method: LINEAR
*                                       or NEAREST.
*       cdlt1s          real            X size of sky pixels in arc degs
*       cdlt2s          real            Y size of sky pixels in arc degs
*       trdtfp(6,size)  real            Array of transformation co-effs.
*                                       Element i,j is co-eff Ci for
*                                       detector j in cross-scan order.
*       psf(npixps,nlinps,ndets) integer*2  Stack of PSF images in EDRS
*                                       unscaled I*2 format. Each image
*                                       is padded with invalid pixels.
*       npixp(size)     integer         Size of each PSF image: pixels
*       nlinp(size)     integer         Size of each PSF image: lines
*       invalp(size)    integer         Invalid pixel flag for each PSF
*       scalep(size)    real            Scale factor for each PSF
*       zerop(size)     real            Zero offset for each PSF
*       sky(npixs,nlins)   integer*2    Sky image in EDRS unscaled format
*       npixs           integer         Size of sky image: pixels
*       nlins           integer         Size of sky image: lines
*       scales          real            Scale factor for sky image
*       zeros           real            Zero offset for sky image
*       invals          integer         Invalid pixel flag for sky image
*       rinval          real            Flag for invalid real pixels
*       ndets           integer         Size of psf stack: images
*       npixps          integer         Size of psf stack: pixels
*       nlinps          integer         Size of psf stack: lines
*       size            integer         Max no. of detectors in any band
*       scanrt          real            IRAS scan rate in arcmins/sec
*       nsamp           integer         No. os CRDD samples required
*       sampfr          integer         Sampling frequency for this band
*   OUTPUTS:
*       answer(size)    real            Unused
*       crdd(nsamp,ndets)       real    Calculated CRDD sample values
*       outmax          real            Highest CRDD sample value
*       ierr            integer         Error status
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               interp
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/1/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ndets,npixps,nlinps,size,nsamp,sampfr
      integer   npixp(size),nlinp(size),invalp(size)
      integer   ierr,npixs,nlins,invals,xstart,ystart
      integer*2 psf(npixps,nlinps,ndets),sky(npixs,nlins)
      real      alpha,cdlt1s,cdlt2s
      real      trdtfp(6,size),scales,zeros,answer(size)
      real      rinval,scalep(size),zerop(size)
      real      scanrt,crdd(nsamp,ndets),outmax
      character method*(*)
*
* DECLARE LOCAL VARIABLES
*
      real      deltax
      real      deltay
      integer   det     ! Current detector cross scan position
      real      factor  ! Factor which must be applied to rebinned PSF
                        ! values to ensure normalisation with sky values
      integer   inval   ! Flag for invalid pixels in current PSF image
      integer   nlin    ! No. of lines in current PSF image
      integer   npix    ! No. of pixels in each line of current PSF image
      integer   nval    ! No. of valid contributions to the o/p sample
      real      partx   ! Part of sky x co-ord value
      real      party   ! Part of sky y co-ord value
      integer   plin    ! Line within PSF image
      integer   ppix    ! Pixel within PSF image line
      integer   psfval  ! Value of integer*2 PSF image pixel
      integer   samp
      real      scale   ! Scale factor for current PSF image
      integer   skylin  ! Line within sky image
      integer   skypix  ! Pixel within sky image line
      real      skyval  ! Value of sky image
      real      skyx    ! Fractional sky pixel posn of psf pixel centre
      real      skyy    ! Fractional sky line posn of psf line centre
      real      sum     ! Sum of all valid contributions to o/p sample
      real     trdtsk(6)! Co-effs of transformation to sky (X,Y) in
                        ! pixels, from detector psf (X,Y) in pixels.
      real     trfpsk(6)! Co-effs of transformation to sky (X,Y) in
                        ! pixels, from focal plane (Z,Y) in arcmins.
      real      zero    !Zero level for current PSF image
*
* CALCULATE TRANSFORMATION FROM FOCAL PLANE CO-ORDS (IN ARCMINS) TO SKY
* CO-ORDS (IN PIXELS) FOR INITIAL FOCAL PLANE POSITION
*
      trfpsk(1)=xstart
      trfpsk(2)=-cosd(alpha)/(60*cdlt1s)
      trfpsk(3)=-sind(alpha)/(60*cdlt1s)
      trfpsk(4)=ystart
      trfpsk(5)=sind(alpha)/(60*cdlt2s)
      trfpsk(6)=-cosd(alpha)/(60*cdlt2s)
*
* CALCULATE INCREMENT IN SKY X AND Y BETWEEN FOCAL PLANE CENTRE POSITIONS
* FOR SUCESSIVE SAMPLES
*
      deltax=scanrt*sind(alpha)/(cdlt1s*60*sampfr)
      deltay=scanrt*cosd(alpha)/(cdlt2s*60*sampfr)
*
* LOOP THOUGH SAMPLES
*
      outmax=-1.0e32
      do samp=1,nsamp
*
* LOOP THROUGH THE DETECTORS IN CROSS SCAN ORDER
*
         do det=1,ndets
*
* CONCATENATE THE TRANSFORMATIONS TO GET THE TRANSFORMATION FROM
* SKY CO-ORDS IN PIXELS TO DETECTOR CO-ORDS IN PIXELS (N.B. trdtfp(3)
* AND trdtfp(5) ARE ALWAYS ZERO)
*
            trdtsk(1)=trfpsk(1)+trfpsk(2)*trdtfp(1,det)
     :                  +trfpsk(3)*trdtfp(4,det)
            trdtsk(2)=trfpsk(2)*trdtfp(2,det)
            trdtsk(3)=trfpsk(3)*trdtfp(6,det)
            trdtsk(4)=trfpsk(4)+trfpsk(5)*trdtfp(1,det)
     :                  +trfpsk(6)*trdtfp(4,det)
            trdtsk(5)=trfpsk(5)*trdtfp(2,det)
            trdtsk(6)=trfpsk(6)*trdtfp(6,det)
*
* CONVOLVE THE PSF WITH THE SKY USING THE REQUESTED METHOD OF
* INTERPOLATION
*
* SAVE PARAMETERS FOR CURRENT PSF IMAGE
*
            scale=scalep(det)
            zero=zerop(det)
            inval=invalp(det)
            nlin=nlinp(det)
            npix=npixp(det)
*
* INITIALISE SUMS FOR THE NEXT CRDD SAMPLE
*
            sum=0
            nval=0
*
* LOOP THROUGH THE LINES OF THE CURRENT PSF IMAGE AND CALCULATE THE
* PART OF THE CORRESPONDING SKY PIXEL CO-ORDS WHICH ARE INDEPENDANT OF
* THE  PSF PIXEL
*
            do plin=1,nlin
               partx=trdtsk(1)+trdtsk(3)*plin
               party=trdtsk(4)+trdtsk(6)*plin
*
* LOOP THROUGH THE PIXELS IN THE CURRENT PSF IMAGE LINE
*
               do ppix=1,npix
*
* ONLY CONTINUE IF THIS PSF PIXEL IS VALID
*
                  psfval=psf(ppix,plin,det)
                  if(psfval.ne.inval) then
*
* CALCULATE THE Y CO-ORD OF THE SKY PIXEL CORRESPONDING TO THIS PSF
* PIXEL AS FRACTIONAL PIXEL CO-ORD AND AS NEAREST INTEGER CO-ORD
*
                     skyy=party+trdtsk(5)*ppix
                     skylin=nint(skyy)
*
* ONLY CONTINUE IF THIS Y CO-ORD IS INSIDE THE SKY IMAGE
*
                     if(skylin.ge.1.and.skylin.le.nlins) then
*
* DO LIKEWISE FOR THE X CO-ORD
*
                        skyx=partx+trdtsk(2)*ppix
                        skypix=nint(skyx)
                        if(skypix.ge.1.and.skypix.le.npixs) then
*
* FIND THE SKY VALUE AT THE CURRENT PSF PIXEL LOCATION USING THE
* REQUESTED METHOD OF INTERPOLATION
*
                           if(method.eq.'LINEAR') then
                              call interp(sky,npixs,nlins,invals,
     :                                    skyx,skyy,skyval)
                           else
                              skyval=real(sky(skypix,skylin))
                           endif
*
* IF THE SKY VALUE IS NOT INVALID, MULTIPLY IT BY THE PSF VALUE AND INCREMENT
* THE PRODUCT SUM AND THE VALID CONTRIBUTION COUNT
*
                           if(skyval.ne.invals) then
                              sum=sum+(scale*psfval+zero)
     :                            *(scales*skyval+zeros)
                              nval=nval+1
                           endif
                        endif
                     endif
                  endif
               enddo
            enddo
*
* SCALE ANSWER BY NO. OF VALID CONTRIBUTIONS. (IF NONE, THEN THIS OUTPUT
* SAMPLE IS FLAGGED INVALID)
*
            if(nval.ne.0) then
               crdd(samp,det)=sum/nval
               outmax=max(outmax,crdd(samp,det))
            else
               crdd(samp,det)=rinval
            endif
*
* GO ROUND FOR NEXT DETECTOR
*
         enddo
*
* UPDATE THE FOCAL PLANE TO SKY TRANSFORMATION FOR NEXT SAMPLE
*
         trfpsk(1)=trfpsk(1)+deltax
         trfpsk(4)=trfpsk(4)+deltay
*
* GO ROUND FOR NEXT SAMPLE
*
      enddo
*
* FINISH
*
  999 continue

      end
