      subroutine memca5(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Aquire pointers to a set of AO crdd files, togther with
*	their associated descriptor values.
*
*SOURCE
*       MEMCA5.FOR in MEMCRDD.TLB
*
*METHOD
*	Loop round aquiring the names of AO CRDD files from the user,
*	getting pointers to them, and reading the descriptors into 
*	common. Files are checked to make sure they are all from the 
*	same AO and are for the same band.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ZZ_COM/
*   WRITE:
*	/A5_COM/,
*		A5_bnd - IRAS band no. of data
*		A5_frm - Descriptor frame identifiers
*		A5_nam - The names of the BDF disk files
*		A5_ncf - No. of CRDD files given as input
*		A5_pin - Pointers to the start of each CRDD file
*		A5_spd - Scan speeds in arcmins per second
*	/AO_COM/,
*		everything (see module AO_COM.INC)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtfoot,gtinam,wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*	       calspd(source attached)
*       EDRS:
*              wrerr,lbgone
*              
*STARLINK PARAMETERS
*	CRDDF1		First input CRDD file
*	CRDDF2		Second input CRDD file
*	....		....
*	CRDDFn		nth input CRDD file   (n=PR_crd)
*	A5ERR1 (error)	Accessed if max. no. of input files exceeded
*	A5ERR2 (error)  Accessed if files from different AOs are given
*	A5ERR3 (error)  Accessed if no data is given
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS ASSOCIATED WITH THIS PROGRAM
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE DATA DESCRIBING IRAS FOCAL PLANE AND CALIBRATION
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... AO CRDD FILE DESCRIPTOR VALUES
      include 'UTILITIES(AO_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE (EXCEPT FOR CRDD FILE DESCRIPTOR 
*     VALUES)
      include '(A5_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	band1	! IRAS band no. from first file
      real	dec1	! DEC descriptor of first file
      logical 	deflt	! True if a null value for a CRDD file is OK
      integer	frmid	! Descriptor frame identifier for current file
      integer	istat	! Temporary status value
      integer	ncrddf	! No. of CRDD files given as input so far
      integer	obs1	! OBS descriptor of first file
      character	pname*7 ! Name of INTERIM parameter used to get next 
			! CRDD file.
      character	prbuf*80! Buffer for screen output
      real	ra1	! RA descriptor of first file
      integer	smptot  ! Total number of samples in all CRDD files
      integer	sop1	! SOP descriptor of first file

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE THINGS
*
      deflt=.false.
      ncrddf=1
      smptot=0

*
* LOOP ROUND TO GET A SET OF CRDD FILES CONTAINING AO DATA
*

  10  continue

*
* IF MAXIMUM NO. OF ALLOWED CRDD FILES HAVE BEEN REACHED, GIVE MESSAGE
* AND CONTINUE USING THE FILES OBTAINED SO FAR.
*
      if(ncrddf.gt.PR_crd) then
         call wrerr('A5ERR1')
      else

*
* CONSTRUCT THE PARAMETER NAME FOR THIS CRDD FILE
*
         write(pname,20) ncrddf
  20     format('CRDDF',I2)
         call lbgone(pname(6:))

*
* PROMPT USER FOR THE NAME OF AN AO FOOTPRINTS FILE, AND GET DESCRIPTORS
*
         call gtfoot(pname,deflt,A5_bnd,frmid,A5_pin(ncrddf),istat)
         if(istat.eq.0) then

*
* FIND THE SCAN SPEED
*
            call calspd(%val(A5_pin(ncrddf)),AO_nys(frmid),
     :                 AO_nde(frmid),AO_bpx(frmid),A5_spd(ncrddf),
     :                 DT_srt(A5_bnd))

            if(ZZ_ilv.ge.4) then
               write(prbuf,30) A5_spd(ncrddf)
  30           format('  Scan speed for this file is ',G13.6,
     :                ' arcmins/sec')
               call lbgone(prbuf(31:))
               call wruser(prbuf,istat)
            endif

*
* IF THIS IS THE FIRST INPUT CRDD FILE, SAVE SOME SCAN PARAMETERS
*
            if(ncrddf.eq.1) then         

               ra1=AO_ra(frmid)
               dec1=AO_dec(frmid)
               obs1=AO_obs(frmid)
               sop1=AO_sop(frmid)
               band1=A5_bnd
               
*
* IF THIS IS NOT THE FIRST FILE, CHECK THAT THIS FILE IS FOR THE SAME 
* AO AND SAME BAND AS THE FIRST FILE. IF NOT, QUIT.
*
            else

               if(AO_ra(frmid).ne.ra1.or.
     :            AO_dec(frmid).ne.dec1.or.
     :            AO_obs(frmid).ne.obs1.or.
     :	          AO_sop(frmid).ne.sop1.or.
     :            A5_bnd.ne.band1) then

                  call wrerr('A5ERR2')
                  ierr=1
                  goto 999

               endif
            endif

*
* STORE THE INPUT FILE NAME AND DESCRIPTOR FRAME IDENTIFIER IN COMMON
*
            call gtinam(pname,A5_nam(ncrddf),istat)
            A5_frm(ncrddf)=frmid

*
* INCREMENT TOTAL NO. OF SAMPLES AND NUMBER OF FILES
*
            smptot=smptot+AO_nys(frmid)*AO_nde(frmid)
            ncrddf=ncrddf+1

*
* ONLY THE FIRST INPUT CRDD FILE IS MANDATORY. THE USER CAN GIVE A NULL 
* VALUE FOR SUBSEQUENT FILE NAMES
*
            deflt=.true.

*
* LOOP ROUND FOR THE NEXT CRDD FILE
*
            goto 10

*
* IF NO CRDD FILE WAS OBTAINED, SET THE ERROR FLAG, UNLESS AN ALLOWED
* NULL FILE WAS GIVEN 
*
         else
            if(istat.ne.1.or.(.not.deflt)) ierr=1
         endif
      endif

*
* CALCULATE NUMBER OF CRDD FILES SUPPLIED AND STORE IN COMMON. 
*
      A5_ncf=ncrddf-1

*
* IF NO SAMPLES WERE GIVEN, ABORT
*
      if(smptot.eq.0) then
         call wrerr('A5ERR3')
         ierr=1
         goto 999
      endif

*
* FINISH
*
  999 continue

      end



C-------------------------------------------------------------------
      subroutine calspd(data,nsamp,ndet,inval,speed,smprat)
      implicit none
      integer	nsamp,smprat,ndet,inval,data(nsamp,ndet,3),first,last,
     :          det,samp,detfst,detlst,detmax,maxlen
      real	speed,xinc,yinc,time,dist


*
* LOOP ROUND EACH DETECTOR FINDING THE ONE WHICH HAS THE LARGEST
* LENGTH OF VALID DATA
*
      maxlen=0

      do det=1,ndet

*
* FIND THE FIRST AND LAST VALID SAMPLES IN THIS DETECTOR
*
         detfst=0

         do samp=1,nsamp
            if(data(samp,det,1).ne.inval) then
               if(detfst.eq.0) detfst=samp
               detlst=samp
            endif
         enddo

*
* IF THE LENGTH OF VALID DATA FOR THIS DETECTOR IS LONGER THAN THE
* PREVIOUS MAXIMUM, THEN STORE THIS DETECTORS VALUES AS THE FINAL VALUES
*
         if(detfst.gt.0.and.detlst-detfst+1.gt.maxlen) then
            maxlen=detlst-detfst+1
            last=detlst
            first=detfst
            detmax=det
         endif

      enddo

*
* CALCULATE AVERAGE SPEED IN ARCMINS PER SECOND BETWEEN THE FIRST AND 
* LAST POINTS
*
      if(maxlen.gt.0) then

         xinc=data(last,detmax,2)-data(first,detmax,2)
         yinc=data(last,detmax,3)-data(first,detmax,3)
         dist=sqrt(xinc**2+yinc**2)/60.0

         time=real(last-first+1)/real(smprat)

         speed=dist/time

      else

         speed=1.92

      endif

*
* FINISH
*
  999 continue

      end
