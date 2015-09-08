      subroutine memcf1(sumsq,sum,sky,psf,scnang,det,dirn,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Adds a PSF for a single detector into running sum images of the 
*       PSFs and squared PSFs of all detectors in the same group. The 
*	images are stored with the PSF centre at pixel (1,1) and 
*	negative X and Y displacements stored in wrap-around position 
*	at high X and Y values.
*
*SOURCE
*       MEMCF1.FOR in MEMCRDD.TLB
*
*METHOD
*	For anti-survey legs of AO data, the PSF response is reversed in
*	the focal plane Y direction (in-scan), to ensure that the long
*	tail on the in-scan PSF occurs AFTER the source has passed over
*	the detector rather than before.
*
*	The running sum images into which the PSF is copied is declared 
*	with array bounds such that the middle pixel is (0,0). The 
*	position of each pixel in the PSF is transformed into a 
*	position within	the running sum image such that the detector 
*	centre is at pixel (0,0). The PSF pixel value is then projected 
*	into the running sum image by giving the 4 nearest pixels the 
*	same weight as would be used in bi-linear interpolation.
*       
*ARGUMENTS       
*   INPUT:
*	psf	integer*2       Unscaled PSF image
*	sumsq	real		Running sum image holding squared values
*	sum	real		Running sum image holding values
*	scnang	real		Clockwise angle from south to forward 
*				track direction (in degress).
*	det	integer		Detector cross-scan position no.
*	dirn	integer		+1 for with-survey scans, -1 for 
*				anti-survey scans
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	sumsq	real		Running sum image holding squared values
*	sum	real		Running sum image holding values
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/B0_COM/,/B2_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       none
*              
*VAX SPECIFICS
*       implicit none
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/1/90
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
* INCLUDE DATA ABOUT IRAS FOCAL PLANE, ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... INFORMATION ABOUT PSF STACKS
      include '(A7_COM)'

* ... INFORMATION ABOUT FITS DESCRIPTORS OF OUTPUT IMAGE
      include '(B0_COM)'

* ... INFORMATION ABOUT CRDD DATA GIVEN AS INPUT
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	det,dirn,ierr
      real	sumsq((1-B0_nps)/2:B0_nps/2,(1-B0_nls)/2:B0_nls/2),
     :          sum((1-B0_nps)/2:B0_nps/2,(1-B0_nls)/2:B0_nls/2),
     :          sky((1-B0_nps)/2:B0_nps/2,(1-B0_nls)/2:B0_nls/2),scnang
      integer*2	psf(A7_nps,A7_nls)

*
* DECLARE LOCAL VARIABLES
*
      real	c1
      real	c2
      real	c3
      real	c4
      real	cosang	
      real	datval
      real	dx
      real	dy
      integer	lhi
      integer	lin0
      real	line	! PSF line counter
      integer	lint	! Function giving next lower integer (+ve or -ve)
      integer	llo
      real	pixel	! PSF pixel counter
      integer	phi
      integer	pix0
      integer	plin
      integer	plo
      integer	ppix
      real	psfval
      real	sinang	
      real	skyval
      real	solang	! Function giving solid angle of each detector
      real	yfp
      real	zfp

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* PROJECT THE ROTATED PSF INTO THE SKY IMAGE, CENTRED AT SKY PIXEL (0,0)
*-----------------------------------------------------------------------
* STORE UPPER AND LOWER LIMITS OF PIXEL AND LINE NUMBERS
*
      phi=B0_nps/2
      plo=(1-B0_nps)/2
      lhi=B0_nls/2
      llo=(1-B0_nls)/2

*
* SET THE SKY IMAGE TO ALL ZEROS
*
      do line=llo,lhi
         do pixel=plo,phi
            sky(pixel,line)=0.0
         enddo
      enddo

*
* CALCULATE COS AND SIN OF CLOCKWISE ANGLE FROM -VE OUTPUT FRAME Y AXIS
* TO THE SCAN DIRECTION
*
      sinang=sind(B0_fit(7)-scnang)
      cosang=cosd(B0_fit(7)-scnang)

*
* IF THE DETECTOR IS DEAD SET AN ERROR STATUS AND PASS ON
*
      if(solang(det,B2_bnd).le.0.0) then
         ierr=1
         goto 999
      endif

*
* LOOP ROUND ALL THE PIXELS AND LINES IN THE PSF IMAGE
*
      do plin=1,A7_nln(det)
         do ppix=1,A7_npx(det)

*
* IF THIS PSF PIXEL IS VALID...
*
            psfval=psf(ppix,plin)
            if(psfval.ne.A7_inv(det)) then

*
* CALCULATE THE FOCAL PLANE COORDS (Z AND Y IN ARCMINS) OF THE PIXEL 
* CENTRE
*
               zfp=A7_tr(1,det) + A7_tr(2,det)*ppix + A7_tr(3,det)*plin
               yfp=A7_tr(4,det) + A7_tr(5,det)*ppix + A7_tr(6,det)*plin

*
* CALCULATE THE OFFSET FROM THE DETECTOR CENTRE PARALLEL TO Z AND Y
*
               zfp=zfp-DT_zpo(det,B2_bnd)
               yfp=yfp-DT_ypo(det,B2_bnd)

*
* IF REQUIRED REVERSE THE SENSE OF THE Y AXIS. THIS IS USED FOR 
* "ANTI-SURVEY" LEGS OF AO DATA. IT ENSURES THAT THE HYSTERISIS TAIL
* ALWAYS OCCURS AFTER THE SOURCE HAS PASSED OVER THE DETECTOR, AND NOT
* BEFORE.
*
               yfp=dirn*yfp

*
* ROTATE AND SCALE THE FOCAL PLANE COORDS TO GIVE PIXEL AND LINE COORDS
* IN THE SKY IMAGE. THE FACTOR OF DIRN USED HERE ACCOUNTS FOR 
* THE FACT THAT THE SCAN GROUP ANGLE REFERS TO THE TRACK DIRECTION, AND 
* THE TRACK DIRECTION REVERSES RELATIVE TO FOCAL PLANE Y ON ANTI-SURVEY
* LEGS
*
               pixel=dirn*(yfp*sinang-zfp*cosang)/ZZ_psz
               line=-dirn*(yfp*cosang+zfp*sinang)/ZZ_psz

*
* CALCULATE THE CONTRIBUTION WHICH THIS PSF PIXEL MAKES TO EACH OF THE 
* FOUR NEAREST SKY PIXELS.
*
               pix0=lint(pixel)
               lin0=lint(line)
               dx=pixel-pix0
               dy=line-lin0
               c1=(1.0-dx)*(1.0-dy)
               c2=dx*(1.0-dy)
               c3=(1.0-dx)*dy
               c4=dx*dy

*
* CALCULATE THE SCALED PSF VALUE, AND ADD IT INTO THE SKY IMAGE
*
               datval=A7_sca(det)*psfval+A7_zer(det)

               sky(pix0,lin0)=sky(pix0,lin0)+c1*datval
               sky(pix0+1,lin0)=sky(pix0+1,lin0)+c2*datval
               sky(pix0,lin0+1)=sky(pix0,lin0+1)+c3*datval
               sky(pix0+1,lin0+1)=sky(pix0+1,lin0+1)+c4*datval

            endif
         enddo
      enddo

*
*----------------------------------------------------------
* ADD THE PROJECTED PSF INTO THE RUNNING SUM IMAGES
*
      do line=llo,lhi
         do pixel=plo,phi
            skyval=sky(pixel,line)
            sum(pixel,line)=sum(pixel,line)+skyval
            sumsq(pixel,line)=sumsq(pixel,line)+skyval*skyval
         enddo
      enddo

*
* FINISH
*
  999 continue

      end
