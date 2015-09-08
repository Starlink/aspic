      subroutine memcf0(error,group,output,work1,work2,sum,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates an image holding the variance of the corresponding
*	PSF image for the given sample group.
*
*SOURCE
*       MEMCF0.FOR in MEMCRDD.TLB
*
*METHOD
*	The PSF image for each sample group is the average of the PSFs
*	from several detectors. The variance in the group PSF is 
*	assumed to be made up from two terms as follows:
*	1) The variance describing the spread of values between the
*	   different detector PSFs which went into the groups "average"
*	   PSF.
*	2) The mean of the intrinsic variance of each detectors PSF.
*	   Each pixel of a detector PSF is assumed to have an intrinsic
*	   uncertainty described by an error proportional to the pixel
*	   value.
*
*ARGUMENTS       
*   INPUT:
*	error		     real     Intrinsic fractional error of each
*				      detectors PSF
*	group		     integer  The sample group for the which the
*				      PSF variance is required
*	work1(ME_mj) 	     real     Work space
*	work2(ME_mj) 	     real     Work space
*	ierr		     integer  Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	sum		     real     Sum of output variances
*	output(ME_mj)        real     Image holding the group PSF 
*				      variances.
*       ierr    	     integer  Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/A8_COM/,/B0_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*            memcf1,pcorrn(source attached)
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
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
* INCLUDE COMMON BLOCKS HOLDING...

* ... INFORMATION ABOUT PSF STACKS PROVIDED BY THE USER
      include '(A7_COM)'

* ... INFORMATION ABOUT SCAN AND DETECTOR GROUPS
      include '(A8_COM)'

* ... FITS DESCRIPTORS FOR OUTPUT IMAGE
      include '(B0_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	group,ierr
      real	error,output(ME_mj),work1(ME_mj),work2(ME_mj),sum

*
* DECLARE LOCAL VARIABLES
*
      integer	det	! Current detector
      integer	detlim	! Limit of search for next detector
      integer	dgroup	! Detector group no. for current sample group
      real	factor	! Includes PSF intrinsic errors in variance
      integer	idet	! Detector loop count
      real	mean	! Mean PSF pixel value
      integer	ndused	! No. of detectors added together
      integer	pixel	! Index into 1D images
      integer	sgroup	! Scan group no. for current sample group

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE THE WORK AND OUTPUT ARRAYS TO ALL ZEROS
*
      do pixel=1,ME_mj
         output(pixel)=0.0
         work1(pixel)=0.0
      enddo
      
*
* CALCULATE THE DETECTOR AND SCAN GROUP NUMBERS
*
      sgroup=mod(group-1,A8_nsg)+1
      dgroup=int((group-1)/A8_nsg)+1
         
*
* FIND THE NEXT DETECTOR IN THIS DETECTOR GROUP
*
      detlim=A7_nds
      ndused=0

  20  continue

      det=0
      do idet=1,detlim
         if(A8_dgp(idet).eq.dgroup) det=idet
      enddo
      detlim=det-1

*
* IF THIS GROUP HAS NOT BEEN FINISHED...
*
      if(det.ne.0) then
            
*
* ADD THE PSF OF THIS DETECTOR INTO THE RUNNING SUM IMAGES
*
         call memcf1(output,work1,work2,%val(A7_psf(det)),
     :               A8_ang(sgroup),det,1,ierr)

*
* IF THE DETECTOR WAS ADDED IN SUCCESFULLY, THEN INCREMENT THE NUMBER OF
* USED DETECTORS
*
         if(ierr.ne.0) then
            ierr=0
            goto 20
         endif

         ndused=ndused+1

*
* IF USING AO DATA, ADD THE DETECTOR IN AGAIN IN THE ANTI-SURVEY 
* DIRECTION
*
         if(ZZ_typ.eq.'AO') then
            ndused=ndused+1
            call memcf1(output,work1,work2,%val(A7_psf(det)),
     :                  A8_ang(sgroup),det,-1,ierr)
         endif

*
* GO ROUND TO DO THE NEXT DETECTOR IN THIS GROUP
*
         goto 20
      endif

*
* FORM THE VARIANCE AT EACH PIXEL (INCLUDING A TERM DUE TO THE INTRINSIC
* VARIANCE OF EACH DETECTOR PSF ASSUMING SIGMA=CONSTANT*PSF VALUE).
*
      if(ndused.gt.0) then
         factor=1.0+error*error
         sum=0.0
         do pixel=1,ME_mj
            mean=work1(pixel)/ndused
            sum=sum+mean
            work1(pixel)=factor*output(pixel)/ndused-mean**2
         enddo
      else
         ierr=1
         goto 999
      endif

*
* CORRECT THE VARIANCE FOR THE EFFECT OF NORMALISING THE TOTAL DATA 
* SUM IN THE PSF TO UNITY, AND REARRANGE THE OUTPUT SO THAT CENTRE 
* PIXEL IS AT COORDINATE (1,1), WITH THE FOUR QUADRANTS IN WRAP-AROUND 
* POSITIONS. 
*
      call pcorrn(work1,output,B0_nps,B0_nls,sum)

*
* FINISH
*
  999 continue

      end



*-------------------------------------------------------------------
      subroutine pcorrn(in,out,npix,nlin,sum)

      implicit none

      integer	pixel,line,pixoff,linoff,pixout,linout,npix,nlin
      real	in(npix,nlin),out(npix,nlin),sum,factor,outval


*
* MODIFY THE SUM NOW TO REDUCE AMOUNT OF COMPUTATION WITHIN THE LOOP
*
      factor=sqrt(real(nlin*npix))/(sum**2)

*
* THE USED PSF HAS A TOTAL DATA SUM EQUAL TO THE SQUARE ROOT OF THE 
* NUMBER OF PIXELS (THIS IS DONE TO ENSURE THAT WHEN NAG ROUTINES ARE 
* USED TO TAKE 2D FFT, THE ZERO FREQUENCY PIXEL HAS VALUE 1.0). THE 
* VARIANCE IS CORRECTED FOR THIS. ALSO MOVE CENTRE TO PIXEL (1,1) AND 
* WRAP NEGATIVE COORDINATES ROUND TO THE OTHER SIDE OF THE IMAGE.
*
      pixoff=(npix-1)/2
      linoff=(nlin-1)/2

      sum=0.0
      do line=1,nlin
         do pixel=1,npix

            pixout=pixel-pixoff
            if(pixout.le.0) pixout=npix+pixout

            linout=line-linoff
            if(linout.le.0) linout=nlin+linout

            outval=in(pixel,line)*factor
            out(pixout,linout)=outval
            sum=sum+outval

         enddo
      enddo

      sum=sum/sqrt(real(nlin*npix))

*
* FINISH
*
      end
