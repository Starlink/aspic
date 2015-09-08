      subroutine crddsa
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*    Calls routine CRDDSA to sample a 2d image at the position of each
*    sample within a specified CRDD file, and optionally to add a 
*    linear base line to the input CRDD which minimises the deviation 
*    between the input CRDD and sampled CRDD. This is the basis of 
*    iterative destriping.
*
*SOURCE
*       CRDDSA.FOR in CRDDSAMPLE.TLB
*
*ARGUMENTS       
*	none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,rdimds,gtbool,gtcrdd,gt2diw,gtwork,gtstrn,gtinam,
*	       gtbool,ncropn
*       THIS PACKAGE (CRDDSAMPLE.TLB):
*              csampl,ccorrn,copyao(source attached)
*       EDRS:
*              gtdscr,getpar,lbgone
*       INTERIM:
*              cydscr,frdata,cnpar
*	SGS:
*	       sgs_close
*
*STARLINK PARAMETERS
*	IN		Looping input CRDD file
*	IN1		The first input CRDD file
*	...
*	IN20		The twentieth input CRDD file
*	IMAGE		The input image
*	OUT		Looping output CRDD file
*	OUT1		The first output corrected CRDD file.
*	...
*	OUT20		The twentieth output corrected CRDD file.
*	SMP		Looping sampled CRDD output file
*	SMP1	 	The first sampled CRDD output.
*       ...
*	SMP20	 	The twentieth sampled CRDD output.
*	SOLANG		Identifies which set of detector solid angles
*			are to be used (Exp. Supp. values or Moshir 
*                       values).
*	BASELINE	The type of base line to add to the input CRDD
*	FILTER		The size of the box filter (in samples) used to 
*			to reject sources from the base line estimation
*	ILEVEL		Amount of info to display on the terminal screen
*	DEFAULT		The value of a flat image to use if no image is given
*	LOOP		If YES then the user is prompted for multiple 
*			CRDD input files.
*	DEVICE		Graphics device
*	MODE		Type of output required; sampled CRDD or 
*			corrected CRDD
*	DATATYPE	CRDD type: AO or SURVEY
*
*VAX SPECIFICS
*       implicit none
*	do while
*	enddo
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/2/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS, AND CRDD FILE DESCRIPTORS
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DS_COM)'
      include 'UTILITIES(AO_COM)'

*
* DECLARE LOCAL VARIABLES
*
      integer	band	! IRAS band no. (1-4) of the input CRDD
      character	base*8  ! Type of base line to add to the CRDD
      character bdfnam*40! Name of the current input crdd file
      real	bscale	! Scale factor for input image integers
      real	bzero	! Zero offset for input image integers
      integer	crddf	! CRDD file counter
      integer	crpix1	! Value of FITS descriptor CRPIX1
      integer	crpix2	! Value of FITS descriptor CRPIX2
      character	cval*1	! Dummy character argument
      real	def	! Value of default flat image
      character device*20! SGS name of graphics device
      character dtype*6 ! CRDD type: AO or SURVEY
      real      expsup(IR_dts,IR_bns)! Solid angle data from Exp.Sup.
      integer	filter	! Size of box filter in samples
      real	fits(7) ! Positional FITS descriptors from input image
      integer	frmid	! Location of input CRDD file descriptor values
      integer	ilevel	! Level of user info to display
      character incrd*4 ! Input CRDD file parameter name
      integer	inval	! Flag for invalid pixels in input image
      integer	ipcord	! Pointer to AO coordinate data
      integer	ipcout(3)! Pointer to output corrected CRDD file
      integer	ipcrin	! Pointer to input CRDD file
      integer	ipin	! Pointer to input image
      integer	ipsout(3)! Pointer to output sampled CRDD file
      integer	ipwk1	! Pointer to work space
      integer	ipwk2	! Pointer to work space
      integer	ipwk3	! Pointer to work space
      integer	ipwk4	! Pointer to work space
      integer	ipwk5	! Pointer to work space
      integer	istat	! General status value
      integer	ival	! Dummy integer argument
      logical	loop	! If loop is yes then user is prompted for more input files
      character mode*9  ! Type of output; SAMPLED or CORRECTED
      logical	more	! True if another CRDD file is to be processed
      real      moshir(IR_dts,IR_bns)! Solid angle data from Merhdad 
			! Moshir at IPAC
      integer	ndets	! No. of detector data streams in CRDD file
      integer	nlin	! No. of lines in input image
      integer	npix	! No. of pixels per line in input image
      integer	nsamp	! No. of samples per detector
      logical	null	! If true than a null input CRDD file is OK
      character outcrd*5! Output CRDD file parameter name
      integer	posn	! Position of selected entry within list
      real	rval	! Dummy real argument
      character	solang*6! Source of solid angle data, 'MOSHIR' or 'EXPSUP'
      logical	usedef	! If true, then the default flat image will be used

*
* INITIALISE THE ARRAYS CONTAINING THE TWO DIFFERENT SETS OF SOLID 
* ANGLE VALUES (IN UNITS OF 1.0E-7 STERADIANS)
*
      data     expsup/ 0.77,  2.0,  2.7,  2.9,  3.1,  3.1,  2.5,  3.0,
     :                  2.9,  2.5,  2.8,  3.2,  3.0,  2.8,  2.0,  1.2,
     :                  1.4,  2.8,  3.2,  3.5,  3.1,  0.0,  3.2,  0.0,
     :                  3.1,  2.8,  3.2,  3.6,  3.4,  3.1,  2.4,  0.0,
     :                  2.1,  4.3,  6.3,  7.2,  6.4,  6.6,  0.0,  6.7,
     :                  5.9,  6.1,  6.6,  6.6,  6.5,  6.2,  3.9,  2.8,
     :                  7.1,11.53, 11.7, 14.5, 14.0, 12.0, 13.3, 12.7,
     :                 13.2, 12.4, 13.5, 13.0, 11.2, 12.6, 10.6,  0.0/

      data     moshir/ 0.89, 2.41, 3.25, 3.21, 3.31, 2.95, 3.19, 3.22,
     :                 3.22, 3.25, 3.24, 3.17, 3.29, 3.25, 2.45, 0.92,
     :                 1.82, 3.40, 3.50, 3.39, 3.50,  0.0, 3.45,  0.0,
     :                 3.44, 3.48, 3.46, 3.46, 3.48, 3.38, 1.78,  0.0,
     :                 1.97, 4.85, 6.36, 6.10, 6.40, 6.10,  0.0, 6.04,
     :                 6.32, 6.19, 6.37, 6.07, 6.25, 6.56, 4.84, 1.92,
     :                  7.9, 13.1, 14.0, 14.1, 13.3, 13.2, 13.8, 13.7,
     :                 14.0, 13.2, 14.2, 12.9, 12.9, 13.6,  6.8,  0.0/

*
* GET THE INPUT IMAGE TO BE SAMPLED
*
      call gt2dir('IMAGE',102,.true.,npix,nlin,ipin,istat)

*
* IF AN IMAGE WAS SUCCESFULLY OBTAINED, GET ITS DESCRIPTORS
*
      if(istat.eq.0) then

         bscale=1.0
         bzero=0.0
         inval=100000
         call gtdscr('IMAGE','BSCALE','REAL',ival,bscale,cval,istat)
         call gtdscr('IMAGE','BZERO','REAL',ival,bzero,cval,istat)
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,istat)
      
         call rdimds('IMAGE',.false.,fits(1),fits(2),crpix1,crpix2,
     :               fits(5),fits(6),fits(7),cval,istat)
         if(istat.ne.0) goto 999
         fits(3)=crpix1
         fits(4)=crpix2

*
* FLAG THAT THE GIVEN IMAGE IS TO BE USED INSTEAD OF A FLAT DEFAULT 
* IMAGE
*
         usedef=.false.

*
* IF NO IMAGE WAS OBTAINED, THE PROGRAM BEHAVES AS IF A FLAT IMAGE WAS
* GIVEN WHICH HAS A CONSTANT VALUE EQUAL TO THE VALUE OF PARAMETER
* "DEFAULT" (IN JY/ST)
*
      else
         def=0.0
         call getpar('DEFAULT','REAL',1,-1.0E32,1.0E32,.true.,ival,def,
     :                istat)
         usedef=.true.

*
* GET A DUMMY IMAGE
*
         call gtwork('DUMMY','I*2',1,ipin,istat)
         npix=1
         nlin=1
               
      endif

*
* SEE IF THE USER WANTS TO BE PROMPTED FOR MULTIPLE INPUT CRDD FILES
*
      loop=.false.
      call gtbool('LOOP',.true.,loop,istat)

*
* FIND OUT WHICH SET OF DETECTOR SOLID ANGLES SHOULD BE USED; THE
* SET FROM THE EXPLANATORY SUPPLEMENT (AS USED BY I_CRDIMAGE, 
* I_CRDD_COMBINE) OR THE VALUES DERIVED BY MERHDAD MOSHIR AT IPAC
* (AS USED BY MEMCRDD).
*
      posn=1
      call gtstrn('SOLANG',.true.,'EXPSUP,MOSHIR.',1,posn,solang,ival,
     :             istat)

*
* SEE IF THE USER WANTS TO USE AO FOOTPRINTS OR SURVEY CRDD FILES
* AS INPUT
*
      posn=1
      call gtstrn('DATATYPE',.true.,'SURVEY,AO.',1,posn,dtype,ival,
     :             istat)

*
* SEE IF THE USER WANTS THE SAMPLED CRDD TO BE GIVEN AS OUTPUT, OR
* THE CORRECTED VERSION OF THE INPUT CRDD
*
      posn=1
      call gtstrn('MODE',.true.,'CORRECTED,SAMPLED.',1,posn,mode,ival,
     :             istat)

*
* SEE IF A LINEAR OR CONSTANT BASE LINE IS TO BE ADDED ONTO THE CRDD
*
      posn=1
      call gtstrn('BASELINE',.true.,'CONSTANT,LINEAR.',1,posn,base,ival,
     :             istat)

*
* GET SIZE OF BOX FILTER (IN SAMPLES) TO APPLY TO CRDD WHEN FORMING 
* BASE LINE
*
      filter=15
      call getpar('FILTER','INTEGER',1,1.0,1.0E6,.true.,filter,rval,
     :             istat)

*
* GET LEVEL OF USER INFORMATION TO DISPLAY
*
      ilevel=1
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,
     :             istat)

*
* SEE IF USER WANTS GRAPHICS PLOTS OF THE BASE LINE FIT. IF HE DOES
* THEN OPEN NCAR 
*
      call ncropn('DEVICE',device,.true.,istat)

*
* ENTER A LOOP TO PROCESS EACH CRDD FILE IN TURN
*
      null=.false.
      more=.true.
      crddf=1

      do while(more)

*
* GENERATE THE REQUIRED PARAMETER NAMES (IF THE USER IS BEING PROMPTED
* FOR MULTIPLE INPUT FILES, ALWAYS USE THE SAME PARAMETERS. OTHERWISE
* USE PARAMETER SUFFIX 1 TO 20 IN TURN UNTIL A NULL VALUE IS OBTAINED).
* THIS ARRANGEMENT IS TO MAKE IT EASIER TO SPECIFY MULTIPLE CRDD FILES
* IN A BATCH JOB, WHILE MAINTAINING FLEXIBILITY WHEN USING THE PROGRAM
* INTERACTIVELY.
*
         if(loop) then
            incrd='IN'
            outcrd='OUT'

         else

            write(incrd,10) crddf
  10        format('IN',I2)
            call lbgone(incrd(3:))

            write(outcrd,20) crddf
  20        format('OUT',I2)
            call lbgone(outcrd(4:))

         endif

*
* GET THE INPUT CRDD FILE. IF NONE IS GIVEN QUIT
*
         if(dtype.eq.'SURVEY') then
            call gt2dir(incrd,104,null,nsamp,ndets,ipcrin,istat)
            if(istat.ne.0) goto 999

            call gtcrdd(incrd,band,frmid,ipcrin,istat)

         else
            call gtfoot(incrd,null,band,frmid,ipcrin,istat)

            nsamp=AO_nys(frmid)
            ndets=AO_nde(frmid)

         endif

         if(istat.ne.0) goto 999

         call gtinam(incrd,bdfnam,istat)

*
* GET SOME WORK SPACE
*

         call gtwork('WORK1','REAL',nsamp*ndets,ipwk1,istat)
         if(istat.ne.0) goto 999

         call gtwork('WORK2','REAL',nsamp*ndets,ipwk2,istat)
         if(istat.ne.0) goto 999

         call gtwork('WORK3','INTEGER',nsamp*ndets,ipwk3,istat)
         if(istat.ne.0) goto 999

         call gtwork('WORK4','REAL',nsamp,ipwk4,istat)
         if(istat.ne.0) goto 999

         call gtwork('WORK5','INTEGER',nsamp,ipwk5,istat)
         if(istat.ne.0) goto 999

*
* IF THE USER WANTS THE SAMPLED CRDD TO BE GIVEN AS OUTPUT, GET THE
* NAME OF THE OUTPUT CRDD FILE TO HOLD IT
*
         if(mode.eq.'SAMPLED') then
            if(dtype.eq.'SURVEY') then
               call gt2diw(outcrd,104,.false.,nsamp,ndets,
     :                  ipsout(1),istat)
            else
               call gt3diw(outcrd,104,.false.,nsamp,ndets,3,
     :                  ipsout,istat)
            endif

               
*
* OTHERWISE GET SOME WORKSPACE TO HOLD THE SAMPLED CRDD IN, AND GET A
* FILE TO HOLD THE CORRECTED OUTPUT CRDD IN
*
         else
            call gtwork('SMP','INTEGER',nsamp*ndets,
     :                  ipsout(1),istat)
            if(istat.ne.0) goto 999
            if(dtype.eq.'SURVEY') then
               call gt2diw(outcrd,104,.false.,nsamp,ndets,
     :                  ipcout(1),istat)
            else
               call gt3diw(outcrd,104,.false.,nsamp,ndets,3,
     :                  ipcout,istat)
            endif
         endif
         if(istat.ne.0) goto 999

*
* COPY THE INPUT CRDD DESCRIPTORS TO THE OUTPUT CRDD
*
         call cydscr(incrd,outcrd,istat)

*
* IF DEALING WITH AO DATA, COPY THE SAMPLE COORDINATE DATA FROM INPUT TO
* OUTPUT, AND ALSO TO SOME WORK SPACE
*
         if(dtype.eq.'AO') then

            call gtwork('COORDS','INTEGER',nsamp*ndets*2,ipcord,istat)
            if(istat.ne.0) goto 999

            call copyao(%val(ipcrin),%val(ipcout(1)),%val(ipcord),nsamp,
     :                  ndets)

         else
            ipcord=ipin
         endif

*
* TELL THE USER WHAT IS HAPPENING
*
         if(ilevel.ge.2) then
            call wruser(' ',istat)
            call wruser('>>> Processing '//bdfnam,istat)
         endif

*
* CALL CSAMPL TO PRODUCE THE SAMPLED OUTPUT CRDD (USING THE APPROPRIATE
* SOLID ANGLE VALUES)
*
         if(solang.eq.'MOSHIR') then
            call csampl(%val(ipsout(1)),nsamp,ndets,
     :                  band,frmid,%val(ipin),npix,nlin,bscale,bzero,
     :                  inval,fits,moshir(1,band),def,usedef,dtype,
     :                  %val(ipcord),istat)
         else
            call csampl(%val(ipsout(1)),nsamp,ndets,
     :                  band,frmid,%val(ipin),npix,nlin,bscale,bzero,
     :                  inval,fits,expsup(1,band),def,usedef,dtype,
     :                  %val(ipcord),istat)
         endif
         if(istat.ne.0) goto 999

*
* IF REQUIRED, CALL CCORRN TO PRODUCE THE CORRECTED INPUT CRDD
*
         if(mode.eq.'CORRECTED') then
            call ccorrn(%val(ipcrin),%val(ipsout(1)),%val(ipcout(1)),
     :                  nsamp,base,ndets,band,frmid,
     :                  filter,ilevel,%val(ipwk1),%val(ipwk2),
     :                  %val(ipwk3),%val(ipwk4),%val(ipwk5),device,
     :                  dtype,istat)
            if(istat.ne.0) goto 999
         endif

*
* RELEASE THE WORK ARRAYS 
*
         call frdata('WORK1',istat)
         call frdata('WORK2',istat)
         call frdata('WORK3',istat)
         call frdata('WORK4',istat)
         call frdata('WORK5',istat)
         if(mode.ne.'SAMPLED') call frdata('SMP',istat)
         if(dtype.eq.'AO') call frdata('COORDS',istat)

*
* RELEASE DATA FILES AND CANCEL PARAMETERS
*
         call frdata(incrd,istat)
         call cnpar(incrd,istat)

         call frdata(outcrd,istat)
         call cnpar(outcrd,istat)

*
* GET NEXT INPUT CRDD FILE. IF THE LIST OF EXPLICIT (NON-LOOPING)
* PARAMETERS HAS BEEN EXHAUSTED, THEN QUIT
*
         DS_top=0
         AO_top=0
         null=.true.
         crddf=crddf+1
         if((.not.loop).and.crddf.gt.20) more=.false.

      enddo

*
* FINISH
*
  999 call frdata(' ',istat)
      if(device.ne.'NONE') call sgs_close

      end


*-----------------------------------------------------------------------
      subroutine copyao(aoin,aoout,coords,nsamp,ndets)
      implicit none

      integer	nsamp,ndets,samp,det,x,y
      integer	aoin(nsamp,ndets,3),aoout(nsamp,ndets,3),
     :          coords(nsamp,ndets,2)

      do det=1,ndets
         do samp=1,nsamp
            x=aoin(samp,det,2)
            y=aoin(samp,det,3)
            aoout(samp,det,2)=x
            aoout(samp,det,3)=y
            coords(samp,det,1)=x
            coords(samp,det,2)=y
         enddo
      enddo

      end
