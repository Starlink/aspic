      subroutine crddbl
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs in scan smoothing on a CRDD file, using the in-scan 
*	PSF of a specified band of IRAS survey array detectors.
*
*SOURCE
*       CRDDBL.FOR in CRDDBLUR.TLB
*
*METHOD
*	Get data sets and parameter values from the user, then call
*	subroutine BLURIT to do the smoothing.
*       
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtbool,gt2dir,wruser,gtstrn,gt2diw
*       THIS PACKAGE (CRDDBLUR.TLB):
*              blurit
*       EDRS:
*              getpar,gtdscr,wrerr,lbgone
*       INTERIM:
*              ctoi,cydscr,frdata,cnpar
*
*STARLINK PARAMETERS
*	BLURBAND	The IRAS band defining the PSF with which the
*			input CRDD will be blurred.
*	CONTINUE	If YES then the user wishes to smooth the CRDD
*			using it's own PSF.
*	ILEVEL		The level of displayed user information.
*	INPUT		The input CRDD file.
*	LOOP 		If YES then the program loops to smooth multiple
*			CRDD files using the same PSF.
*	OUTPUT		The smoothed output CRDD file.
*	INVBAND(error)	Accessed if CRDD file descriptors specify an 
*			invalid IRAS band number.
*	NONEED(error)	Accessed if the user specifies the band number
*			of the input CRDD as the smoothing band.
*	NOTCRDD(error)	Accessed if the input BDF does not contain CRDD
*	TOOBAD(error)	Accessed if too many bad replies given to a 
*			prompt.
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/3/89
*-------------------------------------------------------------------
*
      implicit none
      
*
* INCLUDE IRAS PARAMETER VALUES (VARIABLES CALLED IR_xxx), IRAS
* DETECTOR INFORMATION (VARIABLES CALLED DT_xxx)
*
      INCLUDE 'UTILITIES(IR_PAR)'
      INCLUDE 'UTILITIES(DT_DAT)'

*
* DECLARE LOCAL VARIABLES
*
      integer	banda	! IRAS band no. of input CRDD file
      integer	bandb	! IRAS band no. with which to blur the CRDD
      logical	cont    ! True if program execution is to continue
      character contnt*30 ! Value of CONTENT descriptor from input CRDD
      character	cval*1	! Dummy character argument
      logical	deflt   ! True if a null return is OK for the CRDD file
      integer	frame	! Frame identifier for input CRDD descriptors
      integer	ierr	! Error status
      integer	ilevel	! Level of info to display on users screen
      integer   inval	! Value of BLANK descriptor from input CRDD file
      integer   ipin	! Pointer to start of input CRDD file
      integer	ipout   ! Pointer to output CRDD file
      integer	ival	! Dummy integer argument
      logical   loop    ! True if more than 1 file is to be smoothed
      integer	ndet	! No. of detector streams in input CRDD file
      integer   nsamp   ! No. of samples per detector stream in CRDD
      character	prbuf*80! Buffer for text destined for users screen
      real	rval	! Dummy real argument
      real	scale	! Scale factor of input CRDD file
      integer	waves(IR_bns)! Wavelength of each IRAS band
      real	zero	! Offset of input CRDD file

      data      waves/12,25,60,100/

*
* GET LEVEL OF INFORMATION TO DISPLAY ON USERS SCREEN
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)

*
* SEE IF THE USER WANTS TO LOOP ROUND THE PROGRAM, SMOOTHING MORE THAN
* ONE FILE.
*
      loop=.false.
      call gtbool('LOOP',.true.,loop,ierr)

*
* GET INPUT CRDD FILE TO BE BLURRED AND DISPLAY ITS WAVEBAND 
*
      deflt=.false.
  10  call gt2dir('INPUT',104,deflt,nsamp,ndet,ipin,ierr)
      if(ierr.ne.0) then
         loop=.false.
         goto 999
      endif

*
* GET CONTENT DESCRIPTOR AND CHECK IT IS A CRDD FILE. ALSO EXTRACT IRAS
* BAND NUMBER.
*
      call gtdscr('INPUT','CONTENT','CHARACTER',ival,rval,contnt,ierr)
      if(ierr.ne.0.or.
     :      contnt(2:5).ne.'BAND'.or.contnt(9:14).ne.'SIGNAL') then
         call wrerr('NOTCRDD')
         goto 999
      endif

      call ctoi(contnt(6:7),banda,ierr)
      if((ierr.eq.0.and.(banda.lt.1.or.banda.gt.4)).or.ierr.eq.5) then
         call wrerr('INVBAND')
         goto 999
      endif

*
* GET THE VALUES OF THE DESCRIPTORS BSCALE,BZERO AND BLANK
*
      scale=1.0
      zero=0.0
      inval=-999999
      call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
      call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
      call gtdscr('INPUT','BLANK','INTEGER',inval,rval,cval,ierr)

*
* IF REQUIRED, DISPLAY THE WAVEBAND OF THE INPUT CRDD FILE
*
      if(ilevel.gt.1) then
         write(prbuf,20) waves(banda)
  20     format('    Input CRDD file contains ',I3,'um data')
         call lbgone(prbuf(30:))
         call wruser(' ',ierr)
         call wruser(prbuf,ierr)
         call wruser(' ',ierr)
      endif

*
* SELECT WAVEBAND WITH WHICH TO BLUR THE INPUT CRDD FILE
*
      bandb=banda+4
      call gtstrn('BLURBAND',.true.,'1,2,3,4,12,25,60,100.',1,bandb,
     :             cval,ival,ierr)
      bandb=mod(bandb-1,4)+1

*
* IF THE RESPONSE FUNCTION OF THE BLURRING BAND IS THE SAME AS THAT OF 
* THE INPUT CRDD FILE, THERE MAY BE NO NEED TO BLUR THE DATA. WARN THE 
* USER AND ASK HIM IF HE WANTS TO CONTINUE.
*
      if(banda.eq.bandb.or.(banda.eq.1.and.bandb.eq.2)
     :   .or.(banda.eq.2.and.bandb.eq.1)) then
         call wrerr('NONEED')
         cont=.false.
         call gtbool('CONTINUE',.true.,cont,ierr)
         if(.not.cont) goto 999
      endif

*
* OPEN THE OUTPUT CRDD FILE
*
      call gt2diw('OUTPUT',104,.false.,nsamp,ndet,ipout,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif

*
* CALL ROUTINE BLURIT TO DO THE BLURRING
*
      call blurit(%val(ipin),%val(ipout),nsamp,ndet,inval,scale,zero,
     :            DT_srt(banda),DT_srt(bandb))

*
* ADD THE INPUT CRDD FILE DESCRIPTORS TO THE OUTPUT CRDD FILE 
*
      call cydscr('INPUT','OUTPUT',ierr)

*
* RELEASE ALL DATA AREAS AND IF REQUIRED, LOOP ROUND FOR A NEW INPUT
* NB, GT2DID SETS UP THE ASSOCIATION BETWEEN PARAMETER AND BDF FILE AND
* HANDLES ERRORS, BUT DOES NOT MAP THE DATA. THAT IS DONE BY GTCRDD.
*
  999 call frdata(' ',ierr)
      if(loop) then
         call cnpar('OUTPUT',ierr)
         call cnpar('INPUT',ierr)
         deflt=.true.
         goto 10
      endif         

*
* FINISH
*
      end
