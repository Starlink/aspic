      subroutine csampl(smpout,nsamp,ndets,band,frmid,image,npix,nlin,
     :                  bscale,bzero,inval,fits,omega,def,usedef,mode,
     :                  coords,istat)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Samples a 2D image at the position of each of the
*	data samples in the input CRDD file. Bilinear interpolation
*	is used.
*
*SOURCE
*       CSAMPL.FOR in CRDDSAMPLE.TLB
*
*ARGUMENTS       
*   INPUT:
*	nsamp		integer	   No. of sampled per detector stream
*	ndets		integer	   No. of detector streams
*	band		integer	   IRAS band no. (1-4)
*	frmid		integer	   Pointer to CRDD descriptor values
*	npix		integer	   No. of pixels per line in input image
*	nlin		integer	   No. of lines in input image
*	image(npix,nlin)integer*2  Input image
*	bscale		real	   Scale factor for input image values
*	bzero		real	   Zero offset for input image values
*	inval		integer	   Flag for invalid pixels in input image
*	fits(7)		real	   FITS descriptors for input image:
*				      fits(1) - CRVAL1
*				      fits(2) - CRVAL2
*				      fits(3) - CRPIX1
*				      fits(4) - CRPIX2
*				      fits(5) - CDELT1
*				      fits(6) - CDELT2
*				      fits(7) - CROTA1
*	omega(16)    	real	   The effective solid angles of the 
*				   detectors in this IRAS band. Given
*				   in units of 1.0E-7 steradians
*	def		real	   A default surface brightness (Jy/st)
*	usedef	        logical    If true, then the image used is a 
*				   flat image with value given by 'DEF'.
*				   Otherwise the image given by 'IMAGE'
*				   is used.
*	mode		character  CRDD type: 'SURVEY' or 'AO'
*	coords(nsamp,ndet,2) integer AO sample coordinate data
*   OUTPUTS:
*	smpout(nsamp,ndets) integer  The output CRDD in w/m**2. Each 
*				   sample is given by the brightness of
*				   the used image at the sample centre, 
*				   scaled by the detectors solid angle. 
*	istat		integer	   Error status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              boresp,splntr,interp,rdtoxy
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*	Trig functions in degrees
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/2/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS, CRDD FILE DESCRIPTOR VALUES, AND 
* DETECTOR DATA
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DS_COM)'
      include 'UTILITIES(AO_COM)'
      include 'UTILITIES(DT_DAT)'

*
* DECLARE ARGUMENTS
*
      integer	band,frmid,npix,nlin,inval,istat,nsamp,ndets
      integer*2	image(npix,nlin)
      real	bscale,bzero,fits(7),omega(IR_dts),def
      integer	smpout(nsamp,ndets),coords(nsamp,ndets,2)
      logical	usedef
      character mode*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   cinval  ! CRDD bad sample flag.
      real	cscale  ! CRDD scale factor
      real	czero   ! CRDD zero offset
      real	cosdec	! Cosine of AO sample DEC
      real	dec	! DEC of AO sample in degrees

      integer	det	! Detector counter
      real	flux	! Sampled flux value in W/M**2
      integer	ival	! Integerised flux value
      real	lin	! Line no. (fractional) of sample centre
      real	pix	! Pixel no. (fractional) of sample centre
      real	ra	! RA of AO sample in degrees
      integer	samp	! Sample counter
      real	solang	! Detectors effective solid angle in steradians
      real      time    ! Data sample time relative to first sample.
      real	tran(6)	! Coefficients of a linear transformation 
			! relating focal plane coords (Z,Y) in arcmins
			! to image pixel coords
      real	unsval	! Unscaled sampled data value
      real	xao	! X offset of AO data sample
      real	yao	! Y offset of AO data sample

      if(mode.eq.'SURVEY') then
         cscale = DS_bsc( frmid )
         czero = DS_bze( frmid )
         cinval = DS_bpx( frmid )

      else
         cscale = AO_bsc( frmid )
         czero = AO_bze( frmid )
         cinval = AO_bpx( frmid )

      end if
      
*
* IF THE DEFAULT FLAT IMAGE IS TO BE USED, THEN ALL SAMPLED CRDD VALUES
* WILL BE JUST THE DEFAULT IMAGE VALUE TIMES THE DETECTOR SOLID ANGLE
* (A SOLID ANGLE OF ZERO IMPLIES THE DETECTOR WAS "DEAD")
*
      if(usedef) then

         do det=1,ndets

            solang=omega(det)*1.0E-7
            if(solang.gt.0.0) then
               flux=def*solang/(DT_cvf(band)*DT_o84(band))
               ival=nint((flux-czero)/cscale)
            else
               ival=cinval
            endif

            do samp=1,nsamp
               smpout(samp,det)=ival
            enddo

         enddo

      else

*
*------------------------------------------------------------------
* IF DEALING WITH SURVEY DATA....
*
         if(mode.eq.'SURVEY') then
*
* IF THE USER GAVE AN IMAGE, SET UP THE COEFFECIENTS OF A CUBIC SPLINE 
* FIT DESCRIBING THE BORESIGHT POSITION AGAINST TIME. THESE 
* COEFFICIENTS ARE STORED IN COMMON BLOCK /BORE_SP/
*
            call boresp(frmid,istat)
            if(istat.ne.0) goto 999

*
* LOOP ROUND ALL DATA SAMPLES.
*
            do samp=1,nsamp

*
* IF THE TIME OF THIS DATA SAMPLE IS OUTSIDE THE RANGE OF THE BORESIGHT 
* SAMPLES, IT IS SET INVALID IN THE OUTPUT.
*
               time=real(samp-1)/real(DT_srt(band))
               if(time.lt.DS_but(1,frmid).or.
     :            time.gt.DS_but(DS_bsa(frmid),frmid) ) then

                     do det=1,ndets
                        smpout(samp,det)=cinval
                     enddo

               else

*
* CALCULATE A LINEAR TRANSFORMATION WHICH RELATES IRAS FOCAL PLANE
* COORDINATES (Y AND Z, IN ARCMINS), TO IMAGE PIXEL COORDINATES FOR
* THE CURRENT BORESIGHT POSITION. THIS USES THE SPLINE FIT SET UP BY
* ROUTINE BORESP.
*
                  call splntr(tran,samp,frmid,fits,band,istat)
                  if(istat.ne.0) goto 999

*
* LOOP ROUND ALL THE DETECTORS IN THE BAND BEING USED
*
                  do det=1,ndets

*
* SAVE THE DETECTORS EFFECTIVE SOLID ANGLE IN STERADIANS
*
                     solang=omega(det)*1.0E-7

*
* IF THE DETECTOR IS NOT DEAD, THEN...
*
                     if(solang.gt.0.0) then

*
* CALCULATE THE PIXEL COORDS OF THE DETECTOR CENTRE WITHIN THE FRAME OF
* THE IMAGE
*
                        pix=tran(1)+tran(2)*DT_zpo(det,band)
     :                          +tran(3)*DT_ypo(det,band)
                        lin=tran(4)+tran(5)*DT_zpo(det,band)
     :                          +tran(6)*DT_ypo(det,band)

*
* SAMPLE THE IMAGE USING BILINEAR INTERPOLATION
*
                        call interp(image,npix,nlin,inval,pix,lin,
     :                              unsval)

*
* IF THE SAMPLED VALUE IS VALID, THEN SCALE IT INTO A FLUX ESTIMATE
* IN W/M**2 USING THE DETECTORS EFFECTIVE SOLID ANGLE (INCORPORATING
* THE OCTOBER 1984 CALIBRATION CORRECTION). THIS ASSUMES THE INPUT IMAGE
* IS IN UNITS OF JANSKYS PER STERADIAN.
*
                        if(nint(unsval).ne.inval) then
                           flux=(bscale*unsval+bzero)*solang
     :                       /(DT_cvf(band)*DT_o84(band))

*
* INTEGERISE AND STORE THIS FLUX VALUE USING THE SAME SCALING AS THE 
* INPUT CRDD
*
                           smpout(samp,det)=nint((flux-czero)
     :                                           /cscale)

*
* IF THE SAMPLED VALUE WAS INVALID SET THE SAMPLED OUTPUT INVALID
*
                        else
                           smpout(samp,det)=cinval
                        endif

*
* IF THIS DETECTOR IS DEAD THEN SET THE OUTPUT SAMPLE INVALID
*
                     else
                        smpout(samp,det)=cinval
                     endif

*
* DO NEXT DETECTOR AT THE CURRENT BORESIGHT POSITION
*
                  enddo

               endif

*
* DO NEXT BORESIGHT POSITION
*
            enddo

*
*-------------------------------------------------------------------
*
* NOW DEAL WITH AO FOOTPRINT DATA...
*
         else

*
* LOOP ROUND ALL THE DETECTORS IN THE BAND BEING USED
*
            do det=1,ndets

*
* SAVE THE DETECTORS EFFECTIVE SOLID ANGLE IN STERADIANS
*
               solang=omega(det)*1.0E-7

*
* LOOP ROUND EACH BORESIGHT POSITION WITHIN THE INPUT CRDD FILE
*
               do samp=1,nsamp

*
* IF THE DETECTOR IS NOT DEAD, THEN...
*
                  if(solang.gt.0.0) then

*
* GET THE DISPLACEMENT OF THE REQUIRED SAMPLE FROM THE REFERENCE 
* POSITION IN DEGREES OF ARC NORTH AND EAST (XAO AND YAO RESPECTIVELY).
* THIS INFORMATION IS STORED IN THE AO CRDD FILE.
*
                     xao=coords(samp,det,1)/3600.0
                     yao=coords(samp,det,2)/3600.0

*
* CALCULATE THE RA AND DEC OF THE SAMPLE (BOTH IN DEGREES)
*
                     dec=AO_dec(frmid)+xao
                     cosdec=cosd(dec)

                     if(cosdec.ne.0.0) then
                        ra=AO_ra(frmid)+yao/cosdec
                     else
                        ra=0.0
                     endif

*
* FIND THE FRACTIONAL PIXEL POSITION WITHIN THE INPUT IMAGE WHICH HAS
* THE SAME RA AND DEC
*
                     call rdtoxy(pix,lin,fits(1),fits(2),int(fits(3)),
     :                        int(fits(4)),fits(5),fits(6),fits(7),ra,
     :                        dec)

*
* SAMPLE THE IMAGE USING BILINEAR INTERPOLATION
*
                     call interp(image,npix,nlin,inval,pix,lin,unsval)

*
* IF THE SAMPLED VALUE IS VALID, THEN SCALE IT INTO A FLUX ESTIMATE
* IN W/M**2 USING THE DETECTORS EFFECTIVE SOLID ANGLE (INCORPORATING
* THE OCTOBER 1984 CALIBRATION CORRECTION). THIS ASSUMES THE INPUT IMAGE
* IS IN UNITS OF JANSKYS PER STERADIAN.
*
                     if(nint(unsval).ne.inval) then
                        flux=(bscale*unsval+bzero)*solang
     :                       /(DT_cvf(band)*DT_o84(band))

*
* INTEGERISE AND STORE THIS FLUX VALUE USING THE SAME SCALING AS THE 
* INPUT CRDD
*
                        smpout(samp,det)=nint((flux-czero)
     :                                           /cscale)

*
* IF THE SAMPLED VALUE WAS INVALID SET THE SAMPLED OUTPUT INVALID
*
                     else
                        smpout(samp,det)=cinval
                     endif

*
* IF THIS DETECTOR IS DEAD THEN SET THE OUTPUT SAMPLE INVALID
*
                  else
                     smpout(samp,det)=cinval
                  endif

*
* DO NEXT BORESIGHT POSITION
*
               enddo

*
* DO NEXT DETECTOR 
*
            enddo

         endif
      endif

*
* FINISH
*
  999 continue

      end
