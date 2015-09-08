      subroutine memcc4(trfpsk,nsamp,crddf,det,samp,x,y,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates the position of the centre of a data sample in
*	the pixel coordinate frame of the output image.
*
*SOURCE
*       MEMCC4.FOR in MEMCRDD.TLB
*
*METHOD
*	For SURVEY data, a linear transformation from focal plane coords
*	to sky pixel coords is set up based on the boresight pointing
*	data stored in the CRDD file, and the FITS descriptors of the 
*  	output image. This transformation is applied to the focal plane 
*	coords of the detector centre to get therequired X and Y values.
*	The transformation is different for each boresight position and
*	is set up in routine MEMCD4.
*
*	For AO data, the offset in RA and DEC of the sample from the 
*	reference point is read from the AO CRDD file. This is added
*	to the RA and DEC of the reference point to get the RA and DEC
*	of the sample. This is then converted to pixel coordinates using
*	the FITS descriptors of the output image.
*       
*ARGUMENTS       
*   INPUT:
*	trfpsk(6,nsamp) real	Coefficients of the transformation from
*				focal plane coords (in arcmins), to sky
*				coords (in pixels). This data is only
*				used when deconvolving survey CRDD.
*	nsamp	integer		No. of samples per detector
*	crddf	integer		CRDD file index
*	det	integer		Detector no. in cross scan order
*	samp	integer		Sample number
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	X	real		Fractional pixel no. of sample centre 
*				within output image frame
*       Y	real		Fractional line no. of sample centre 
*				within output image frame
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/AO_COM/,/B0_COM/,/B2_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              rdtoxy
*       THIS PACKAGE (MEMCRDD.TLB):
*              readao (source attached)
*
*VAX SPECIFICS
*       implicit none
*       %val
*       Trig functions in degrees
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/10/89
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
* INCLUDE DATA ABOUT THE IRAS FOCAL PLANE, ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... AO CRDD FILE DESCRIPTOR VALUES
      include 'UTILITIES(AO_COM)'

* ... FITS DESCRIPTORS OF THE OUTPUT FRAME
      include '(B0_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	nsamp,crddf,det,samp,ierr
      real	x,y,trfpsk(6,nsamp)

*
* DECLARE LOCAL VARIABLES
*
      real	dec	! DEC of the sample
      real	cosdec	! Cosine of the samples DEC
      real	ra	! RA of the sample
      real	xao	! "X" value from AO CRDD file
      real	yao	! "Y" value from AO CRDD file

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999


*----------------------------------------------------------------------
* FIRST DEAL WITH SURVEY DATA
*
      if(ZZ_typ.eq.'SURVEY') then
         
*
* USE THE GIVEN TRANSFORMATION TO CALCULATE THE SKY FRAME COORDS OF THE 
* CENTRE OF THE DETECTOR
*
         x=trfpsk(1,samp) + DT_zpo(det,B2_bnd)*trfpsk(2,samp)
     :                    + DT_ypo(det,B2_bnd)*trfpsk(3,samp)

         y=trfpsk(4,samp) + DT_zpo(det,B2_bnd)*trfpsk(5,samp)
     :                    + DT_ypo(det,B2_bnd)*trfpsk(6,samp)


*-------------------------------------------------------------
* NOW DEAL WITH AO DATA
*
      else

*
* GET THE DISPLACEMENT OF THE REQUIRED SAMPLE FROM THE REFERENCE 
* POSITION IN DEGREES OF ARC NORTH AND EAST (XAO AND YAO RESPECTIVELY).
* THIS INFORMATION IS STORED IN THE AO CRDD FILE.
*
         call readao(%val(B2_pin(crddf)),nsamp,B2_nde(crddf),samp,det,
     :               xao,yao)

*
* CALCULATE THE RA AND DEC OF THE SAMPLE (BOTH IN DEGREES)
*
         dec=AO_dec(B2_frm(crddf))+xao
         cosdec=cosd(dec)

         if(cosdec.ne.0.0) then
            ra=AO_ra(B2_frm(crddf))+yao/cosdec
         else
            ra=0.0
         endif

*
* FIND THE FRACTIONAL PIXEL POSITION WITHIN THE OUTPUT FRAME WHICH HAS
* THE SAME RA AND DEC
*
         call rdtoxy(x,y,B0_fit(1),B0_fit(2),int(B0_fit(3)),
     :               int(B0_fit(4)),B0_fit(5),B0_fit(6),B0_fit(7),ra,
     :               dec)

      endif

*
* FINISH
*
  999 continue

      end


C-------------------------------------------------------------------
      subroutine readao(data,nsamp,ndet,samp,det,xao,yao)
      implicit none

      integer	samp,det,nsamp,ndet,data(nsamp,ndet,3)
      real	xao,yao

      xao=data(samp,det,2)/3600.0
      yao=data(samp,det,3)/3600.0

      end
