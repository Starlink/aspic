      subroutine memcc1(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Stores the FFT of each groups PSF.
*
*SOURCE
*       MEMCC1.FOR in MEMCRDD.TLB
*
*METHOD
*
* 	For each group, identify the detectors belonging to that group.
*	Find the mean PSF of all such detectors. These mean PSFs are 
*	rotated	according to the group scan angle, and scaled so that 
*	they have a total data sum of unity.
*
*	The current version of MEMCRDD puts all legs of AO data into the
*	same scan group and uses an average of the with-survey and 
*	anti-survey PSFs to achieve the deconvolution. This results in 
*	symetric mean PSFs for each sample group.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/A8_COM/,/B0_COM/,/B2_BND//B5_COM/,/B6_COM/,/ME_COM/,
*	/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	       wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb9,memcc6,prepsf,punit(sources attached)
*	EDRS
*	       lbgone
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
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
* INCLUDE INFO ABOUT THE IRAS FOCAL PLANE ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... INFORMATION ABOUT PSF STACKS PROVIDED BY THE USER
      include '(A7_COM)'

* ... INFORMATION ABOUT SCAN AND DETECTOR GROUPS
      include '(A8_COM)'

* ... FITS DESCRIPTORS FOR OUTPUT IMAGE
      include '(B0_COM)'

* ... INFORMATION ABOUT INPUT CRDD FILES
      include '(B2_COM)'

* ... INFORMATION ABOUT USABLE SAMPLE GROUPS
      include '(B5_COM)'

* ... POINTERS FOR MEMCRDD WORK FILES HELD IN ARRAY ME_ST
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	det	! Current detector
      integer	detlim	! Limit of search for next detector
      integer	dgroup	! Detector group no. for current sample group
      integer	group	! Group number
      integer	idet	! Detector loop count
      logical	init	! True if this is 1st PSF in this detector group
      integer	istat	! Local error status
      character prbuf*80! Buffer for screen output
      integer	sgroup	! Scan group no. for current sample group

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

      if(ZZ_ilv.ge.4) call wruser(' ',istat)

*
* NORMALISE ALL DETECTOR PSFS TO A DATA SUM OF UNITY. THIS IS EFFECTED 
* BY CHANGING THE SCALE AND ZERO FACTORS FOR EACH PSF
*
      do det=1,A7_nds
         call prepsf(%val(A7_psf(det)),A7_nps,A7_nls,A7_npx(det),
     :               A7_nln(det),A7_inv(det),A7_sca(det),A7_zer(det))
      enddo

*
* LOOP ROUND EACH GROUP
*
      do group=1,A8_ngp
         init=.true.

*
* IF THIS GROUP IS EMPTY PASS ON TO THE NEXT GROUP
*
         if(B5_lgs(group).eq.0) goto 40

*
* IF REQUIRED TELL USER WHAT IS GOING ON
*
         if(ZZ_ilv.ge.4) then
            write(prbuf,10) group
  10        format('  Calculating FFT of the PSF for group ',I3,' ...')
            call wruser(prbuf,istat)
         endif

*
* CALCULATE THE DETECTOR AND SCAN GROUP NUMBERS
*
         sgroup=mod(group-1,A8_nsg)+1
         dgroup=int((group-1)/A8_nsg)+1
         
*
* FIND THE NEXT DETECTOR IN THIS DETECTOR GROUP
*
         detlim=A7_nds

  20     continue

         det=0
         do idet=1,detlim
            if(A8_dgp(idet).eq.dgroup) det=idet
         enddo
         detlim=det-1

*
* IF THIS GROUP HAS NOT BEEN FINISHED...
*
         if(det.ne.0) then

            if(ZZ_ilv.ge.4) then
               write(prbuf,30) DT_bal(det,B2_bnd)
  30           format('      Adding detector #',I2)
               call lbgone(prbuf(24:))
               call wruser(prbuf,istat)
            endif
            
*
* ADD THE PSF OF THIS DETECTOR TO THE RUNNING SUM IMAGE, WORK 1
*
            call memcb9(ME_st(B6_wk1),ME_st(B6_wk2),%val(A7_psf(det)),
     :                  A8_ang(sgroup),det,1,init,ierr)

*
* IF USING AO DATA, ADD THE DETECTOR IN AGAIN IN THE ANTI-SURVEY 
* DIRECTION
*
            if(ZZ_typ.eq.'AO') then
               call memcb9(ME_st(B6_wk1),ME_st(B6_wk2),
     :                     %val(A7_psf(det)),A8_ang(sgroup),det,-1,
     :                     init,ierr)
            endif

*
* GO ROUND TO DO THE NEXT DETECTOR IN THIS GROUP
*
            goto 20
         endif

*
* NORMALISE THE TOTAL DATA SUM TO UNITY, AND REARRANGE THE MEAN PSF
* SO THAT CENTRE PIXEL IS AT COORDINATE (1,1), WITH THE FOUR QUADRANTS
* IN WRAP-AROUND POSITIONS. STORE RESULT IN WORK 2 IMAGE.
*
         call punit(ME_st(B6_wk1),ME_st(B6_wk2),ME_st(B6_wk2),B0_nps,
     :              B0_nls)

*
* TAKE FOURIER TRANSFORM OF THIS PSF.
*
         call memcc6(PR_fwd,ME_st(B6_wk2),ME_st(B6_psf(group)),
     :               ME_st(B6_wk1),B0_nps,B0_nls,ierr)


*
* DO NEXT GROUP
*
   40    continue
      enddo

*
* FINISH
*
  999 continue

      end



*-------------------------------------------------------------------
      subroutine punit(in,weight,out,npix,nlin)

      implicit none

      integer	pixel,line,pixoff,linoff,pixout,linout,npix,nlin,nval
      real	in(npix,nlin),weight(npix,nlin),out(npix,nlin),sum,s1,
     :          s2,omax,omin,sd,mean,outval,wgt

*
* NORMALIZE THE PSF AND FIND THE RESULTING TOTAL DATA SUM
*
      sum=0.0
      do line=1,nlin
         do pixel=1,npix
            wgt=weight(pixel,line)
            if(wgt.gt.0.0) then
               in(pixel,line)=in(pixel,line)/wgt
               sum=sum+in(pixel,line)
            else
               in(pixel,line)=0.0
            endif
         enddo
      enddo

*
* MODIFY THE SUM NOW TO REDUCE AMOUNT OF COMPUTATION WITHIN THE LOOP
*
      sum=sum/sqrt(real(nlin*npix))

*
* DIVIDE PSF BY TOTAL DATA SUM TO ENSURE THAT THE NEW TOTAL DATA SUM
* IS EQUAL TO THE SQUARE ROOT OF THE NUMBER OF PIXELS. DOING THIS
* ENSURES THAT WHEN NAG ROUTINES ARE USED TO TAKE 2D FFT, THE ZERO
* FREQUENCY PIXEL HAS VALUE 1.0. ALSO MOVE CENTRE TO PIXEL (1,1) AND
* WRAP NEGATIVE COORDINATES ROUND TO THE OTHER SIDE OF THE IMAGE.
*
      s2=0
      s1=0
      omax=-1.0E32
      omin=1.0E32
      if(sum.ne.0) then

         pixoff=(npix-1)/2
         linoff=(nlin-1)/2

         do line=1,nlin
            do pixel=1,npix

               pixout=pixel-pixoff
               if(pixout.le.0) pixout=npix+pixout

               linout=line-linoff
               if(linout.le.0) linout=nlin+linout

               outval=in(pixel,line)/sum
               out(pixout,linout)=outval
               if(outval.gt.1.0E-6) then
                  s2=s2+outval**2
                  s1=s1+outval
                  omax=max(omax,outval)
                  omin=min(omin,outval)
                  nval=nval+1
               endif
            enddo
         enddo
      endif
      s1=s1/sqrt(real(npix*nlin))
      s2=s2/real(npix*nlin)
      mean=s1/nval
      sd=sqrt( s2/nval-mean**2)

*
* FINISH
*
      end



C------------------------------------------------------------------
      subroutine prepsf(psf,npixps,nlinps,npix,nlin,inval,scale,zero)
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npixps,nlinps,npix,nlin,inval
      integer*2	psf(npixps,nlinps)
      real	scale,zero

*
* DECLARE LOCAL VARIABLES
*
      integer	pix,lin,psfval
      real	sum

*
* FIND TOTAL VALID DATA SUM
*
      sum=0
      do lin=1,nlin
         do pix=1,npix
            psfval=psf(pix,lin)
            if(psfval.ne.inval) sum=scale*psfval+zero
         enddo
      enddo

*
* MODIFY SCALE AND ZERO FACTORS TO ENSURE THAT TOTAL DATA SUM IS 1.0
*
      if(sum.gt.0) then
         scale=scale/sum
         zero=zero/sum
      endif

*
* FINISH
*
      end
