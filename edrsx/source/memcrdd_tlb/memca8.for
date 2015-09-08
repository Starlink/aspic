      subroutine memca8(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Find out how user wants to divide the data samples up into
*	groups (each group must contain samples having a common PSF).
*
*SOURCE
*       MEMCA8.FOR in MEMCRDD.TLB
*
*METHOD
*	   First of all, the scans (a "scan" is the contents of a single
*	CRDD file) are grouped together into "scan groups", each group 
*	having a scan group number. Each scan group contains scans which
*       differ in angle by less than 3 degrees and so can use a common 
*	PSF. The figure of 3 degrees results in a displacement of about
*	1.5 pixels at the edge of a 64*64 pixel PSF (as supplied by 
*	IPAC).
*	   Next, the user is asked which detectors are to be group
*       together and processed with the same PSF. This is done by 
*	assigning a "detector group number" to each detector. A detector
*	can be omitted from the entire deconvolution process by giving
*	it a group number of zero. The supplied detector group numbers
*	are checked to ensure they are contiguous (ie there are no gaps
*	in the group numbers). The default groups are:
*
*	   100um :  1) #55,62		25um :  1) #39,46
*		    2) #4			2) #19,22
*       	    3) all others		3) all others
*
*	   60um  :  1) #31,11		12um :  1) #47,26
*		    2) #38,12		        2) #27,54
*       	    3) all others		3) all others
*
*         A "sample group" consists of a set of samples which can share
*	the same PSF. Thus the constituent samples must belong to the
*	same detector group AND the same scan group. The "group number"
*	is defined as
*
*   group no. = scan group no. +  No. of scan groups*(det. group no-1)
*
*	The inverse of this is:
*
*   det. group = INT( (group no.-1) / No. of scan groups ) + 1
*   scan group = MOD( (group no.-1) , No. of scan groups ) + 1
*
* 	The total number of sample groups is thus the product of the
*	number of scan groups and the number of detector groups.
*
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ZZ_COM/,/B2_COM/
*   WRITE:
*	/A8_COM/
*		A8_dgp - Detector group number for each detector
*		A8_ndg - Number of used detector groups
*		A8_nsg - Number of used scan groups
*		A8_sgp - Scan group number for each CRDD file
*		A8_ang - Mean scan angle for each scan group in degrees
*			 (=clockwise angle from south to scan direction)
*		A8_ngp - Total number of sample groups
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb3
*       EDRS:
*              wrerr,lbgone
*       INTERIM:
*              rdkeyi,cnpar
*
*STARLINK PARAMETERS
*	DGROUPS		List of detector group numbers (one for each detector)
*	TOOBAD(error)	Accessed if too many bad lists given
*	A8ERR1(error)	Accessed if too many detector groups specified
*
*VAX SPECIFICS
*       implicit none
*	do while
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
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
* INCLUDE FOCAL PLANE INFO ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(A8_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETERS
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	det	! Detector index
      integer	detgrp(IR_dts,IR_bns)	! Default detector groupings
      integer	dgpmax	! Maximum no. of detector groups allowed
      integer	gmin	! Minimum supplied group no. greater than LOLIM
      integer	grp     ! Contiguous group number to replace supplied value
      integer	istat	! Temporary status value
      integer	lolim   ! Highest completed output group
      character	prbuf*80! Buffer for screen output
      logical	more	! True if output groups remain to be completed
      integer	nbad	! No. of incomplete detector groups lists given
      integer	nvals	! No. of supplied detector group values

*
* DEFINE DEFAULT SCAN GROUPINGS
*
      data	detgrp/1,2,3,3,3,3,3,3,3,3,3,3,3,3,2,1,
     :                 1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,0,
     :                 1,2,3,3,3,3,3,3,3,3,3,3,3,3,2,1,
     :                 1,2,3,3,3,3,3,3,3,3,3,3,3,3,1,0/

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* CALL MEMCB3 TO GROUP PARALLEL SCANS TOGETHER. EACH SCAN GROUP CONTAINS
* SCANS WHICH DIFFER IN SCAN ANGLE BY LESS THAN 3 DEGREES
*
      call memcb3(A8_nsg,A8_ang,A8_sgp,ierr)
      if(ierr.ne.0) goto 999

*
* CALCULATE MAXIMUM NUMBER OF DETECTOR GROUPS (THE TOTAL NUMBER OF
* SAMPLE GROUPS IS THE PRODUCT OF THE SCAN GROUPS AND THE NUMBER OF
* DETECTOR GROUPS)
*
      dgpmax=PR_grp/A8_nsg

*
* IF REQUIRED, DISPLAY DETECTOR BALL NUMBERS IN THIS BAND
*
      if(ZZ_ilv.ge.4) then
         call wruser(' ',istat)
         call wruser('  This band contains the following detectors:',
     :                istat)
         write(prbuf,10) ( DT_bal(det,B2_bnd), det=1,DT_bns(B2_bnd) )
  10     format('        ',I2,<DT_bns(B2_bnd)-1>(',',I2))
         call wruser(prbuf,istat)
      endif

*
* INITIALISE TO DEFAULT GROUPING
*
      do det=1,DT_bns(B2_bnd)
         A8_dgp(det)=detgrp(det,B2_bnd)
      enddo

*
* GET GROUP NUMBERS FOR EACH DETECTOR IN ORDER OF DETECTOR LIST ABOVE
*
      nbad=0
  20  call rdkeyi('DGROUPS',.true.,DT_bns(B2_bnd),A8_dgp,nvals,istat)

*
* IF AN INCOMPLETE LIST WAS GIVEN, GET ANOTHER LIST UPTO A MAXIMUM OF
* "PR_bad" TIMES
*
      if(nvals.ne.DT_bns(B2_bnd).and.istat.ne.1) then

         call wruser('*** List is incomplete. Do again',istat)

         nbad=nbad+1
         if(nbad.gt.PR_bad) then
            call wrerr('TOOBAD')
            ierr=1
            goto 999
         endif

         call cnpar('DGROUPS',istat)
         goto 20

      endif

*
* ENSURE THE GROUP NUMBERS SPAN THE MINIMUM NUMBER OF GROUPS. I.E.
* ENSURE NO GROUP NUMBERS ARE LEFT OUT. THIS IS DONE BY REPLACING THE 
* GROUP NUMBERS SUPPLIED BY THE USER BY NEW "OUTPUT" GROUP NUMBERS 
* WHICH ARE CONTIGUOUS
*
      grp=1
      lolim=1
      more=.true.

      do while(more)

         gmin=1000000000
         do det=1,DT_bns(B2_bnd)
            if(A8_dgp(det).ge.lolim.and.A8_dgp(det).lt.gmin) then
               gmin=A8_dgp(det)
            endif
         enddo

         if(gmin.lt.1000000000) then

            do det=1,DT_bns(B2_bnd)
               if(A8_dgp(det).eq.gmin) A8_dgp(det)=grp
            enddo

            lolim=gmin+1
            grp=grp+1
            if(grp.gt.dgpmax) then
               call wrerr('A8ERR1')
               ierr=1
               goto 999
            endif

         else
            more=.false.
         endif

      enddo

*
* STORE NUMBER OF DETECTOR GROUPS 
*
      A8_ndg=grp-1

*
* STORE TOTAL NUMBER OF SAMPLE GROUPS
*
      A8_ngp=A8_ndg*A8_nsg

*
* IF REQUIRED, TELL USER TOTAL NUMBER OF SAMPLE GROUPS
*
      if(ZZ_ilv.ge.3) then
         write(prbuf,30) A8_ngp
  30     format('  Data is being split into ',I4,' groups')
         call lbgone(prbuf(29:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif
*
* FINISH
*
  999 continue

      end
