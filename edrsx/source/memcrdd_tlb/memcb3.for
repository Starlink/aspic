      subroutine memcb3(nscgrp,sang,scangp,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Divides the CRDD files up into groups. All the scans within
*       a group have the same scan orientation to within +/- 3 degrees.
*	The figure of 3 degrees is set in the parameter declarations
*	module PR_DEC.
*
*SOURCE
*       MEMCB3.FOR in MEMCRDD.TLB
*
*METHOD
*	For survey data, the scans are grouped into bins according 
*       to their scan angle in such a way as to use the minimum number
*	of bins.
*
*	For AO data, all scans are put in scan group 1 and the 
*	associated group scan angle is set to the scan angle 
*	FOR WITH-SURVEY LEGS. For anti-survey legs, the actual 
*	scan angle is the group scan angle plus 180 degrees.
*       
*ARGUMENTS       
*   INPUTS:
*	ierr		integer	Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       nscgrp		integer	The no. of scan groups used
*	sang(PR_grp)	real	The scan angle for each group. This is
*				the clockwise angle from south to the
*				"with-survey" scan direction, in degrees
*	scangp(PR_crd)  integer The scan group assigned to each CRDD file
*       ierr    	integer Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ZZ_COM/,/B2_COM/,/AO_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              borest,scan,wruser
*       EDRS:
*              wrerr,lbgone
*              
*STARLINK PARAMETERS
*	B3ERR1(error)	Accessed if AO legs have different scan angles
*
*VAX SPECIFICS
*       implicit none
*       do while
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
* INCLUDE COMMON BLOCKS HOLDING...

* ... AO CRDD FILE DESCRIPTORS
      include 'UTILITIES(AO_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

*
* DECLARE ARGUMENTS
*
      real	sang(PR_grp)
      integer	nscgrp,scangp(PR_grp),ierr

*
* DECLARE LOCAL VARIABLES
*
      real	angle(PR_crd) ! Scan angles of each scan
      integer	binpop	! No. of scans in current group if the current
			! scan is used as the centre scan
      integer	centre	! The current best scan to use as centre scan
      integer	cfile	! Pointer to the candidate centre scan
      real	cosang  ! Sum of the cosines of all AO CRDD scan angle
      real	dec	! A declination value
      character dirn*3  ! Direction of AO leg, SVY or ANT.
      real	error   ! Difference between group and CRDD scan angles
      integer	file	! A general scan counter
      integer	group	! Current group number
      integer	istat	! Temporary status value
      integer	maxpop	! Maximum no. of scans in current group so far
      logical	more	! True unless all scans have been placed
      character prbuf*80! Buffer for screen output
      real	ra	! A right ascension value
      real	sep	! Angular seperation between scan directions
      real	sinang  ! Sum of the sines of all AO CRDD scan angle

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0 ) goto 999

      if(ZZ_ilv.ge.3) call wruser(' ',ierr)

*-----------------------------------------------------------------
* FIRST DEAL WITH SURVEY DATA
*
      if(ZZ_typ.eq.'SURVEY') then

*
* FIND SCAN ANGLE AT CENTRE OF EACH SCAN
*
         do file=1,B2_ncf

            call borest(B2_frm(file),ierr)
            if(ierr.ne.0) goto 999

            call scan(B2_nys(file)/2,B2_frm(file),B2_bnd,angle(file),
     :                ra,dec)
*
* CONVERT SCAN ANGLE TO CLOCKWISE ANGLE FROM SOUTH TO THE SCAN DIRECTION
*
            angle(file)=180.0-angle(file)

         enddo

*
* INITIALISE ALL SCANS TO GROUP ZERO (I.E. "NOT YET PLACED")
*
         do file=1,B2_ncf
            scangp(file)=0
         enddo

*
* LOOP ROUND PLACING SCANS IN PARTICULAR GROUPS UNTIL NO SCANS REMAIN
* UNPLACED
*
         group=0
         more=.true.
         do while(more)
            group=group+1
         
*
* CONSIDER EACH UNPLACED SCAN IN TURN...
*
            maxpop=0
            do cfile=1,B2_ncf
               if(scangp(cfile).eq.0) then

*
* FIND HOW MANY UNPLACED SCANS ARE WITHIN +/- PR_bin (=3) DEGREES OF 
* THIS SCAN
*
                  binpop=0
                  do file=1,B2_ncf
                     if(scangp(file).eq.0) then
   
                        sep=angle(cfile)-angle(file)
                        if(sep.gt.180) sep=360-sep
                        if(sep.lt.-180) sep=360+sep

                        if(abs(sep).le.PR_bin) binpop=binpop+1

                     endif
                  enddo

*
* SEE IF USING THIS SCAN AS THE CENTRE SCAN OF THE CURRENT GROUP WOULD 
* GIVE MORE SCANS IN THE GROUP
*
                  if(binpop.gt.maxpop) then
                     maxpop=binpop
                     centre=cfile
                  endif
   
               endif
            enddo

*
* STORE THE SCAN ANGLE OF THE CENTRE FILE AS THE SCAN ANGLE OF THE
* ENTIRE SCAN GROUP
*
            if(maxpop.gt.0) then
               sang(group)=angle(centre)

               if(ZZ_ilv.ge.3) then
                  write(prbuf,5) group,sang(group)
   5              format('  Scan group ',I2,' has mean scan angle ',
     :                   F6.1,' degrees')
                  call lbgone(prbuf(37:))
                  call lbgone(prbuf(14:))
                  call wruser(prbuf,istat)
               endif


*
* HAVING FOUND THE CENTRE SCAN WHICH GIVES THE GREATEST NUMBER OF SCANS
* IN THE CURRENT GROUP, GO ROUND THE FILES AGAIN PLACING ALL SCANS
* WITHIN +/- PR_bin (=3) DEGREES OF THE CENTRE SCAN IN THE CURRENT GROUP
*
               do file=1,B2_ncf
                  if(scangp(file).eq.0) then

                     sep=angle(centre)-angle(file)
                     if(sep.gt.180) sep=360-sep
                     if(sep.lt.-180) sep=360+sep

                     if(abs(sep).le.PR_bin) then

                        scangp(file)=group

                        if(ZZ_ilv.ge.3) then
                           write(prbuf,10) B2_nam(file),group
  10                       format('  CRDD file ',A,' is in scan group ',
     :                             I2)
                           call wruser(prbuf,istat)
                        endif

                     endif
   
                  endif
               enddo
   
*
* IF THE CURRENT GROUP CONTAINS NO SCANS, THEN ALL HAVE BEEN PLACED.
*
            else
               more=.false.
            endif
         enddo

*
* CORRECT THE NUMBER OF GROUPS FOR THE LAST (EMPTY) GROUP
*
         nscgrp=group-1               

*----------------------------------------------------------------
* NOW DEAL WITH AO DATA
*
      else if(ZZ_typ.eq.'AO') then

         if(ZZ_ilv.ge.3) call wruser('  All CRDD files will be put '//
     :                               'in scan group 1',istat)

*
* SET SCAN ANG SUMS TO ZERO
*
         sinang=0
         cosang=0

*
* LOOP ROUND ALL INPUT SCANS
*
         do file=1,B2_ncf

*
* GET THE SCAN ANGLE
*
            angle(1)=AO_ang(B2_frm(file))

*
* IF THIS LEG IS ANTI-SURVEY, CONVERT IT TO THE EQUIVALENT WITH-SURVEY
* ANGLE
*
            dirn=AO_dir(B2_frm(file))(1:3)
            if(dirn.ne.'SVY') angle(1)=180.0+angle(1)

*
* INCREMENT SUMS OF SIN AND COS OF SCAN ANGLE
*
            sinang=sinang+sind(angle(1))
            cosang=cosang+cosd(angle(1))

*
* INCREMENT SUMS AND ASSIGN ALL SCANS TO SCAN GROUP 1
*
            scangp(file)=1

*
* DO NEXT SCAN
*
         enddo

*
* CALCULATE "AVERAGE" SCAN ANGLE AND SET NUMBER OF SCAN GROUPS
*
         sang(1)=atan2d(sinang,cosang)
         nscgrp=1

*
* IF REQUIRED TELL USER THE SCAN ANGLE FOR GROUP 1
*
         if(ZZ_ilv.ge.3) then
            write(prbuf,5) 1,sang(1)
            call lbgone(prbuf(37:))
            call lbgone(prbuf(14:))
            call wruser(prbuf,istat)
         endif

*
* CHECK EACH GROUP CONTAINS PARALLEL SCANS TO WITHIN +/- PR_bin
* (=3) DEGREES
*
         do file=1,B2_ncf

            error=AO_ang(B2_frm(file))-sang(1)
            do while(error.lt.-180.0)
               error=error+360.0
            enddo
            do while(error.gt.180.0)
               error=error-360.0
            enddo

            if(abs(error).gt.PR_bin.and.abs(error).lt.180.0-PR_bin) then
               call wrerr('B3ERR1')
               ierr=1
               goto 999
            endif

         enddo

*----------------------------------------------------------------------
* QUIT IF AN UNKNOWN DATA TYPE IS SPECIFIED
*
      else
         ierr=1
      endif

*
* FINISH
*
  999 continue

      end
