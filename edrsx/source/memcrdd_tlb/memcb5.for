      subroutine memcb5(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Identifies usable sample, and for each such sample, calculates
*	and stores the following:
*
*	a) Pixel no. within output image at which the sample centre 
*	   lies (X)
*
*	b) Line no. within output image at which the sample centre 
*	   lies (Y)
*
*	c) Flux in Jy
*
*	d) The variance of the flux associated with the detector from 
*	   which the sample was taken
*
*	e) The group number to which the sample is assigned
*
*	f) An integer value into which is packed the Sample no., the
*	   Detector number and the CRDD file no. This value is referred
*	   to as the SDC identifier, and is used later in MEMCRDD to 
*	   identify where any data sample came from.
*
*	g) The effective solid angle of the sample in Steradians
* 
*	In addition to this, ME_mk is assigned the number of usable 
*	samples, the boundaries of each sample group are stored, and
*	statistics on the data values are stored.
*
*SOURCE
*       MEMCB5.FOR in MEMCRDD.TLB
*
*METHOD
*	Samples are "usable" if the following conditions apply:
*	1) The detector is not dead
*	2) The detector has not been explicitly excluded from the
*	   deconvolution by the user
*	3) The sample is not flagged as "blank" in the input data
*	4) The sample centre lies within the output image
*	5) The sample centre does not lie within the blank margin round
*	   the edge of the output frame
*	6) The sample is not flagged as a glitch, by the deglitching
*	   routine
*
*	For each sample which satisfies these conditions, the values 
*	listed under "PURPOSE"  are calculated and stored at the low 
*	index end of the array ME_st (held in common block /MECOMS/)
*	in a block of 7 values. The blocks associated with succesive
*	usable samples, are stored in the order in which the samples
*	are read from the input CRDD files. NB, these values will be
*	re-ordered into sample groups in a later routine.
*
* 	The high index end of ME_st is used as work space for 
*	storing infromation needed to calculate the X and Y values
*	(a and b above), the field noise associated with each detector
*	stream, and for storing the deglitched data. Checks are made
*	to ensure that the data stored at opposite ends of ME_st do
*	not overwrite each other.
*
*	The SDC identifiers are defined as follows:
*	
*	   SDC = (DET-1) + 16*(CRDDF-1) + 16*cfact*(SAMP-1)
*
* 	where DET is the cross scan position of the detector, CRDDF is 
*	the CRDD file index, SAMP is the sample number, and cfact is
*	a power of two representing the number of bits needed to store
*	the largest value of CRDDF. The inverse of this is:
*
*	   DET = mod( SDC , 16 ) + 1
*	   CRDDF = mod( SDC/16 , cfact ) + 1
*	   SAMP = SDC/(16*cfact) + 1
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/A8_COM/,/B0_COM/,/B2_COM/,/ZZ_COM/
*   WRITE:
*	/B5_COM/
*		B5_var		Variance of field noise
*		B5_min		Minimum surface brightness of usable samples
*		B5_max		Maximum surface brightness of usable samples
*		B5_ave		Mean surface brightness of usable samples
*		B5_cfa		cfact. Used for unpacking sdc identifiers
*		B5_fgs		Location of first sample in each group 
*				relative to the start of any data set
*		B5_lgs		Location of last sample in each group 
*				relative to the start of any data set
*	/ME_COM/
*		ME_mk		No. of samples in a data set
*		ME_st		Data as listed in "PURPOSE" and "METHOD"
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser, hint, solang
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcc3, memcc4, memcd4
*       EDRS:
*              wrerr, lbgone
*              
*STARLINK PARAMETERS
*	B5ERR1(error)	Accessed if too much CRDD given
*	B5ERR2(error)   Accessed if too much usable CRDD
*	B5ERR3(error)   Accessed if no usable CRDD found
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/10/89
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

* ... DATA RELATED TO THE PSFS
      include '(A7_COM)'

* ... DATA DESCRIBING SCAN AND DETECTOR GROUPS
      include '(A8_COM)'

* ... FITS DESCRIPTORS OF OUTPUT IMAGE
      include '(B0_COM)'

* ... DATA DESCRIBING INPUT CRDD FILES
      include '(B2_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(B5_COM)'

* ... MEMSYS3 INFORMATION AND DATA
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
      integer	cbits	! No. of bits needed to hold CRDDF
      integer	crddf	! Index for current CRDD file
      integer	data	! Pointer to start of deglitched data at end of ME_st
      integer	det	! Current detector (cross scan order)
      real	flux	! Flux in Jy of current sample
      integer	group	! Group to which the current sample is assigned
      integer	grppop(PR_grp) ! No. of samples in each group
      integer	hint	! Returns the next higher integer
      integer	istat	! Temporary status value
      real	margin	! Width of image frame margin in pixels
      integer	maxsam	! Max no. of samples before running out of bits
      real	maxsb	! Maximum surface brightness
      real	minsb	! Minimum surface brightness
      integer	ndet	! No. of detectors in current CRDD file
      integer	nsamp	! No. of samples per detector in current CRDD file
      integer	offset	! Offset into low end of ME_st at which data is stored
      real	omega	! Solid angle of current detector in steradians
      integer	pinfo	! Pointer to start of pointing info held in ME_st
      integer	point	! Pointer to the next candidate sample in ME_st
      character prbuf*80! Buffer for screen output
      integer	samp	! Index to current sample
      real	sb	! Surface brightness (Jy/st) of current sample
      real	solang	! Detector solid angle in units of 1.0E-7 Ster.
      real	sumsb	! Sum of all usable surface brightnesses
      integer	totsmp	! Total number of samples in the input CRDD files
      integer	work	! Pointer to start of work space used by MEMCC3
      real	x	! Pixel no. of current sample centre in final image
      real	y	! Line  no. of current sample centre in final image

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* TELL USER WHATS HAPPENING
*
      if(ZZ_ilv.ge.3) then
         call wruser(' ',istat)
         call wruser('  Deglitching data and finding usable samples'//
     :               ' from...',istat)
      endif

*
* STORE THE NUMBER OF BITS REQUIRED TO REPRESENT THE MAXIMUM CRDD 
* FILE NUMBER, AND THE EQUIVALENT SCALING FACTOR
*
      cbits=hint(log(real(B2_ncf))/log(2.0))
      B5_cfa=2**cbits

*
* CALCULATE THE MAXIMUM SAMPLE NUMBER WHICH CAN FIT INTO THE BITS 
* REMAINING AFTER 4 HAVE BEEN USED FOR DETECTOR NUMBER, AND CBITS HAVE
* BEEN USED FOR THE CRDD FILE NUMBER. (ONLY 31 BITS ARE AVAILABLE SINCE
* BIT 32 IS A SIGN BIT). 
*
      maxsam=2**(27-cbits)

*
* CONVERT MARGIN FROM ARCMINS TO PIXELS
*
      margin=A7_mar/ZZ_psz

*
* INITIALISE OFFSET INTO LOW END OF ARRAY ME_ST, NUMBER OF USABLE 
* SAMPLES (ME_MK), TOTAL NUMBER OF SAMPLES AND SURFACE BRIGHTNESS
* STATISTICS 
*
      offset=1
      ME_mk=0
      totsmp=0
      minsb=1.0E32
      maxsb=-1.0E32
      sumsb=0

*
* INITIALISE GROUP POPULATIONS TO ZERO
*
      do group=1,A8_ngp
         grppop(group)=0
      enddo

*
* LOOP ROUND EACH CRDD FILE, STORING COMMONLY USED VALUES
*
      do crddf=1,B2_ncf
         ndet=B2_nde(crddf)
         nsamp=B2_nys(crddf)

*
* TELL USER WHAT IS HAPPENING
*
         if(ZZ_ilv.ge.3) then
            call wruser('    '//B2_nam(crddf),istat)
         endif

*
* INCREMENT TOTAL NUMBER OF DATA SAMPLES
*
         totsmp=totsmp+ndet*nsamp

*
* CHECK THAT THE CRDD FILE IS NOT TOO BIG TO BE REPRESENTABLE IN AN SDC 
* IDENTIFIER
*
         if(nsamp.gt.maxsam) then
            call wrerr('B5ERR1')
            ierr=1
            goto 999
         endif

*
* CALCULATE POINTING INFORMATION AND STORE AT THE TOP END OF ME_ST
* THIS INFORMATION WILL BE USED LATER IN MEMCC4 TO CALCULATE THE
* COORDS OF ALL USABLE SAMPLES
*
         pinfo=PR_mem-6*nsamp+1
         call memcd4(crddf,ME_st(pinfo),nsamp,ierr)

*
* CALCULATE POINTERS TO SPACE BELOW THE POINTING INFO IN THE ME_ST 
* ARRAY, WHERE THE CURRENT CRDD FILE CAN BE STORED AFTER DEGLITCHING. 
* CHECK THIS SPACE DOES NOT OVERLAP THE SPACE ALREADY USED AT THE START
* OF ME_ST.
*
         work=pinfo-4*nsamp
         data=work-ndet*nsamp

         if(data.le.offset) then
            call wrerr('B5ERR2')
            ierr=1
            goto 999
         endif

*
* DEGLITCH THE DATA AND ESTIMATE VARIANCE OF FIELD NOISE
*
         call memcc3(%val(B2_pin(crddf)),ME_st(data),ME_st(work),ndet,
     :               nsamp,crddf,B5_var(1,crddf),ierr)

*
* LOOP ROUND EACH DETECTOR
*
         do det=1,ndet

*
* SAVE THE SOLID ANGLE FOR THIS DETECTOR
*
            omega=solang(det,B2_bnd)*1.0E-7

*
* IF DETECTOR IS NOT DEAD OR EXCLUDED, THEN...
*
            if(omega.ne.0.and.B5_var(det,crddf).gt.0.0.and.
     :         A8_dgp(det).ne.0) then

*
* INITIALISE POINTER TO THE CANDIDATE SAMPLE FROM THE DEGLITCHED DATA
*
               point=nsamp*(det-1)+data-1

*
* LOOP ROUND EACH SAMPLE FROM THIS DETECTOR
* 
               do samp=1,nsamp

*
* INCREMENT POINTER TO CANDIDATE SAMPLE
*
                  point=point+1

*
* IF THIS DEGLITCHED DATA SAMPLE IS NOT INVALID, THEN...
*
                  flux=ME_st(point)
                  if(flux.ne.PR_rin) then

*
* CALCULATE THE POSITION OF THE SAMPLE CENTRE WITHIN THE OUTPUT FRAME
*
                     call memcc4(ME_st(pinfo),nsamp,crddf,det,samp,x,y,
     :                           ierr)
                     if(ierr.ne.0) goto 999

*
* IF THE DISTANCE FROM THIS SAMPLE TO ANY IMAGE EDGE IS GREATER THAN THE
* REQUIRED BLANK MARGIN, THEN THE SAMPLE IS USABLE.
*
                     if(x-1.0.gt.margin.and.B0_nps-x.gt.margin.and.
     :                  y-1.0.gt.margin.and.B0_nls-y.gt.margin) then

*
* CHECK THAT STORING VALUES FOR THIS SAMPLE WILL NOT OVERWRITE THE
* DEGLITCHED DATA STORED AT THE END OF ME_ST
*
                        if(offset+6.ge.data) then
                           call wrerr('B5ERR2')
                           ierr=1
                           goto 999
                        endif

*
* STORE THE X AND Y VALUES FOR THIS SAMPLE CENTRE
*
                        ME_st(offset)=x
                        ME_st(offset+1)=y

*
* STORE THE FLUX AND VARIANCE FOR THIS SAMPLE 
*
                        ME_st(offset+2)=flux
                        ME_st(offset+3)=B5_var(det,crddf)

*
* STORE THE SAMPLE GROUP NO. FOR THIS SAMPLE. SEE ROUTINE MEMCA8 FOR
* DEFINITION OF SAMPLE GROUP NUMBERS. INTEGER ARRAY ME_STI OVERLAYS THE 
* REAL ARRAY ME_ST. ALSO INCREMENT THE GROUP POPULATION COUNT.
*
                        group=A8_sgp(crddf)+A8_nsg*(A8_dgp(det)-1)
                        ME_sti(offset+4)=group
                        grppop(group)=grppop(group)+1

*
* STORE THE SDC IDENTIFIER FOR THE CURRENT SAMPLE.
*
                        ME_sti(offset+5)=(det-1)+16*(crddf-1)
     :                                          +16*B5_cfa*(samp-1)

*
* STORE THE SOLID ANGLE
*
                        ME_st(offset+6)=omega

*
* INCREMENT THE OFFSET INTO THE LOW END OF ME_ST (AND ME_STI)
*
                        offset=offset+7

*
* INCREMENT NO. OF USABLE SAMPLES
*
                        ME_mk=ME_mk+1

*
* INCREMENT THE STATISTICS FOR THE MAX, MIN AND MEAN SURFACE BRIGHTNESS
* VALUE OF ALL USABLE DATA SAMPLES
*
                        sb=flux/omega
                        maxsb=max(maxsb,sb)
                        minsb=min(minsb,sb)
                        sumsb=sumsb+sb

                     endif
                  endif

*
* DO NEXT SAMPLE FROM THIS DETECTOR
*
               enddo

*
* DO NEXT DETECTOR FROM THIS CRDD FILE
*
            endif
         enddo

*
* DO NEXT CRDD FILE
*
      enddo

*
* IF NO USABLE SAMPLES FOUND, ABORT
*
      if(ME_mk.eq.0) then
         call wrerr('B5ERR3')
         ierr=1
         goto 999
      endif

*
* STORE SURFACE BRIGHTNESS STATISTICS
*
      B5_min=minsb
      B5_max=maxsb
      B5_ave=sumsb/ME_mk      

*
* SET UP POINTERS TO THE START AND END OF EACH SAMPLE GROUP.
*
      B5_fgs(1)=1
      B5_lgs(1)=grppop(1)

      do group=2,A8_ngp
         B5_fgs(group)=B5_lgs(group-1)+1
         B5_lgs(group)=B5_fgs(group)+grppop(group)-1
      enddo

*
* IF ANY GROUP HAS NO SAMPLES IN IT, SET THE END OF THE GROUP TO 
* LOCATION ZERO, TO INDICATE THAT THE GROUP NEED NOT BE PROCESSED.
* THIS WILL SAVE A LOT OF CPU.
*
      do group=1,A8_ngp
         if(grppop(group).le.0) B5_lgs(group)=0
      enddo

*
* IF REQUIRED, TELL USER ABOUT ...
*
      if(ZZ_ilv.ge.2) then

*
* NO. OF USABLE SAMPLES...
*
         write(prbuf,10) ME_mk,totsmp
  10     format('  ',I10,' samples are being used, out of a total of ',
     :          I10)
         call lbgone(prbuf(56:))
         call lbgone(prbuf(3:))
         call wruser(prbuf,istat)

*
* MEAN SURFACE BRIGHTNESS...
*
         write(prbuf,20) B5_ave
  20     format('  Mean surface brightness of data is ',G13.6,' Jy/st')
         call lbgone(prbuf(39:))
         call wruser(' ',istat)
         call wruser(prbuf,istat)

*
* MAX AND MIN SURFACE BRIGHTNESSES
*
         call wruser('  Max and min surface brightnesses are: ',
     :                  istat)
         write(prbuf,30) maxsb,minsb
  30     format('      ',G13.6,', ',G13.6,' (Jy/st)')
         call lbgone(prbuf(22:))
         call lbgone(prbuf(7:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)

* 
* BOUNDARIES OF SAMPLE GROUPS
*
         if(ZZ_ilv.ge.4) then

            do group=1,A8_ngp
               if(B5_lgs(group).gt.0) then
                  write(prbuf,40) group,B5_fgs(group),B5_lgs(group)
  40              format('  Group ',I3,' location, start: ',I8,
     :                   ' end: ',I8)
                  call wruser(prbuf,istat)
               else
                  write(prbuf,50) group
  50              format('  Group ',I3,' is empty and will not be used')
                  call wruser(prbuf,istat)
               endif
            enddo

         endif

      endif

*
* FINISH
*
  999 continue

      end
