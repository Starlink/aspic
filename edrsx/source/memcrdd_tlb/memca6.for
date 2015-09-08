      subroutine memca6(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Aquire pointers to a set of SURVEY crdd files, togther with
*	their associated descriptor values.
*
*SOURCE
*       MEMCA6.FOR in MEMCRDD.TLB
*
*METHOD
*	Loop round aquiring the names of SURVEY CRDD files from the 
*	user, getting pointers to them, and reading the descriptors 
*	into common. Files are check to make sure they are for the
*	same band.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	none
*   WRITE:
*	/A6_COM/,
*		A6_bnd - IRAS band no. of data
*		A6_frm - Descriptor frame identifiers
*		A6_nam - The names of the BDF disk files
*		A6_ncf - No. of CRDD files given as input
*		A6_pin - Pointers to the start of each CRDD file
*	/DS_COM/,
*		everything (see module DS_COM.INC)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtcrdd,gtinam
*       EDRS:
*              wrerr,lbgone
*              
*STARLINK PARAMETERS
*	CRDDF1		First input CRDD file
*	CRDDF2		Second input CRDD file
*	....		....
*	CRDDFn		nth input CRDD file   (n=PR_crd)
*	A6ERR1 (error)	Accessed if max. no. of input files exceeded
*	A6ERR2 (error)  Accessed if files for different bands are given
*	A6ERR3 (error)  Accessed if no data is given
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
* INCLUDE DATA DESCRIBING iras FOCAL PLANE AND CALIBRATION
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... SURVEY CRDD FILE DESCRIPTOR VALUES
      include 'UTILITIES(DS_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE (EXCEPT FOR CRDD FILE DESCRIPTOR \
*     VALUES)
      include '(A6_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	band1	! IRAS band no. from first file
      logical 	deflt	! True if a null value for a CRDD file is OK
      integer	frmid	! Descriptor frame identifier for current file
      integer	istat	! Temporary status value
      integer	ncrddf	! No. of CRDD files given as input so far
      integer	ndet	! No. of detector data streams
      integer	nsamp	! No. of samples per detector 
      character	pname*7 ! Name of INTERIM parameter used to get next 
			! CRDD file.
      integer	smptot  ! Total number of samples in all CRDD files

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
* LOOP ROUND TO GET A SET OF CRDD FILES CONTAINING SURVEY DATA
*

  10  continue

*
* IF MAXIMUM NO. OF ALLOWED CRDD FILES HAVE BEEN REACHED, GIVE MESSAGE
* AND CONTINUE USING THE FILES OBTAINED SO FAR.
*
      if(ncrddf.gt.PR_crd) then
         call wrerr('A6ERR1')
      else

*
* CONSTRUCT THE PARAMETER NAME FOR THIS CRDD FILE
*
         write(pname,20) ncrddf
  20     format('CRDDF',I2)
         call lbgone(pname(6:))

*
* PROMPT USER FOR THE NAME OF A CRDD FILE
*
         call gt2dir(pname,104,deflt,nsamp,ndet,A6_pin(ncrddf),istat)
         if(istat.eq.0) then

*
* IF INPUT CRDD FILE OBTAINED OK, GET THE ASSOCIATED DESCRIPTORS
*
            call gtcrdd(pname,A6_bnd,frmid,A6_pin(ncrddf),ierr)
            if(ierr.ne.0) goto 999

*
* IF THIS IS THE FIRST INPUT CRDD FILE, SAVE THE BAND NUMBER
*
            if(ncrddf.eq.1) then         
               band1=A6_bnd
               
*
* IF THIS IS NOT THE FIRST FILE, CHECK THAT THIS FILE IS FOR THE SAME 
* BAND AS THE FIRST FILE. IF NOT, QUIT.
*
            else

               if(A6_bnd.ne.band1) then
                  call wrerr('A6ERR2')
                  ierr=1
                  goto 999
               endif

            endif

*
* STORE THE INPUT FILE NAME AND DESCRIPTOR FRAME IDENTIFIER IN COMMON
*
            call gtinam(pname,A6_nam(ncrddf),istat)
            A6_frm(ncrddf)=frmid

*
* INCREMENT TOTAL NO. OF SAMPLES AND NUMBER OF FILES
*
            smptot=smptot+DS_nys(frmid)*DS_nde(frmid)
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
      A6_ncf=ncrddf-1

*
* IF NO SAMPLES WERE GIVEN, ABORT
*
      if(smptot.eq.0) then
         call wrerr('A6ERR3')
         ierr=1
      endif

*
* FINISH
*
  999 continue

      end
