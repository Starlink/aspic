      subroutine memca2(pname,file,deflt,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Writes out an internal file to disk in the form of
*	BDF files (EDRS images or IPMAF style CRDD files).
*
*SOURCE
*       MEMCA2.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUTS:
*	pname	character	INTERIM parameter name
*	file	integer		Internal MEM file number
*	deflt	logical		True if user may choose to  produce no 
*				BDFs
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/AO_COM/,/B0_COM/,/B2_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2diw,i2prod,wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcc8
*       EDRS:
*              wrerr,ptdscr,lbgone
*       INTERIM:
*              frdata,cnpar,cydscr
*
*STARLINK PARAMETERS
*	pname  		Name of output BDF image (for images)
*	CRDOUTn		Name of CRDD BDF to receive data derived
*			from input CRDD file given by CRDDFn.
*	A2ERR1(error)	Accessed if a bad file number is given as input
*
*VAX SPECIFICS
*       implicit none
*       %val
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

* ... AO DESCRIPTORS
      include 'UTILITIES(AO_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

* ... IMAGE FRAME DESCRIPTORS
      include '(B0_COM)'

* ... INFO ABOUT INPUT CRDD FILES
      include '(B2_COM)'

* ... POINTER TO COVERAGE IMAGE, ETC
      include '(B6_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	file,ierr
      character	pname*(*)
      logical	deflt

*
* DECLARE LOCAL PARAMETERS
*
      integer	inval
      parameter (inval=-32767)

*
* DECLARE LOCAL VARIABLES
*
      character	iname*7 ! Parameter name for current i/p CRDD file
      integer	crddf	! Index of current CRDD file
      character	cval*1	! Dummy character argument
      integer	dim3	! Size of 3rd CRDD file dimension
      integer	ipout	! Pointer to output image
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      character	oname*8 ! Parameter name for current o/p CRDD file
      real	rval	! Dummy real argument
      real	scale	! Output scale factor
      integer	size	! Total no. of data elements in a CRDD file
      character	title*30! Output title
      real	zero	! Output zero offset

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* CHECK FILE NUMBER IS VALID 
*
      if(file.lt.1.or.file.gt.40) then
         call wrerr('A2ERR1')
         ierr=1
         goto 999
      else
         if(ME_kb(file).le.0.or.ME_kb(file).ge.PR_mem) then
            call wrerr('A2ERR1')
            ierr=1
            goto 999
         endif
      endif

*-------------------------------------------------------------------
* FIRST DEAL WITH IMAGES...
*
      if(file.ge.1.and.file.le.20) then

*
* GET OUTPUT INTEGER*2 IMAGE
*
         call gt2diw(pname,102,deflt,B0_nps,B0_nls,ipout,istat)

*
* IF NO FILE WAS OBTAINED, SET THE ERROR FLAG, UNLESS AN ALLOWED NULL
* VALUE WAS GIVEN
*
         if(istat.ne.0) then
            if(istat.ne.1.or.(.not.deflt)) ierr=1
            goto 999
         endif

*
* COPY REAL SKY IMAGE TO THE INTEGER*2 OUTPUT IMAGE USING 75% OF THE
* AVAILABLE DYNAMIC RANGE
*
         call i2prod(ME_st(ME_kb(file)),B0_nps,B0_nls,PR_rin,scale,zero,
     :               %val(ipout),inval)

*
* ADD REQUIRED FITS DESCRIPTORS TO THE SKY IMAGE (INCLUDING DESCRIPTORS
* DEFINING RA AND DEC GRID
*
         call ptdscr(pname,'NAXIS','INTEGER',2,rval,cval,istat)
         call ptdscr(pname,'NAXIS1','INTEGER',B0_nps,rval,cval,istat)
         call ptdscr(pname,'NAXIS2','INTEGER',B0_nls,rval,cval,istat)
         call ptdscr(pname,'BSCALE','REAL',ival,scale,cval,istat)
         call ptdscr(pname,'BZERO','REAL',ival,zero,cval,istat)
         call ptdscr(pname,'CRVAL1','REAL',ival,B0_fit(1),cval,istat)
         call ptdscr(pname,'CRVAL2','REAL',ival,B0_fit(2),cval,istat)
         call ptdscr(pname,'CRPIX1','INTEGER',int(B0_fit(3)),rval,cval,
     :                istat)
         call ptdscr(pname,'CRPIX2','INTEGER',int(B0_fit(4)),rval,cval,
     :                istat)
         call ptdscr(pname,'CDELT1','REAL',ival,B0_fit(5),cval,istat)
         call ptdscr(pname,'CDELT2','REAL',ival,B0_fit(6),cval,istat)
         call ptdscr(pname,'CROTA1','REAL',ival,B0_fit(7),cval,istat)

*
* IF DUMPING FILE 1, THEN ADD A DESCRIPTOR GIVING THE FACTOR BY WHICH 
* MEMSYS3 SCALED THE NOISE VALUES
*
         if(file.eq.1) call ptdscr(pname,'SIGMA','REAL',ival,ME_sig,
     :                             cval,istat)

*
* ADD OTHER DESCRIPTORS: INVALID PIXEL VALUE, IMAGE TITLE, BRIGHTNESS
* UNITS, IRAS INSTRUMENT TYPE
*
         call ptdscr(pname,'INVAL','INTEGER',inval,rval,cval,istat)
         title=AO_obj(1)
         call ptdscr(pname,'TITLE','CHARACTER',ival,rval,title,istat)
         call ptdscr(pname,'BUNIT','CHARACTER',ival,rval,'.JY/ST',istat)
         call ptdscr(pname,'INSTRUME','CHARACTER',ival,rval,'MEMCRDD',
     :               istat)

*
* RELEASE DATA AND CANCEL PARAMETER VALUE
*
         call frdata(pname,istat)
         call cnpar(pname,istat)


*---------------------------------------------------------------------
* NOW DEAL WITH DATA SETS...
*
      else

*
* LOOP ROUND EACH INPUT CRDD FILE
*
         do crddf=1,B2_ncf

*
* CONTRUCT PARAMETER NAMES ASSOCIATED WITH THE INPUT AND OUTPUT CRDD 
* FILES
*
            write(iname,10) crddf
  10        format('CRDDF',I2)
            call lbgone(iname(6:))

            write(oname,20) crddf
  20        format('CRDOUT',I2)
            call lbgone(oname(7:))

*
* CALCULATE NO. OF DATA ELEMENTS IN OUTPUT FILE
*
            if(ZZ_typ.eq.'AO') then
               dim3=3
            else
               dim3=1
            endif
            size=B2_nde(crddf)*B2_nys(crddf)*dim3

*
* GET POINTER TO OUTPUT BDF
*
            call wruser(' ',istat)
            call wruser('  Give name of file to hold data from '
     :                   //B2_nam(crddf),istat)
            call gt2diw(oname,104,deflt,size,1,ipout,istat)

*
* CALL MEMCC8 TO COPY THE DATA FROM THE GIVEN INTERNAL FILE TO THE 
* OUTPUT BDF
*

            if(istat.eq.0) then
               call memcc8(ME_kb(file),%val(B2_pin(crddf)),
     :                     B2_nys(crddf),B2_nde(crddf),dim3,crddf,
     :                     %val(ipout))

*
* COPY DESCRIPTORS FROM INPUT FILE TO OUTPUT FILE AND RELEASE DATA
*
               call cydscr(iname,oname,istat)
               call frdata(oname,istat)
            endif

*
* CANCEL PARAMETER ASSOCIATION 
*
            call cnpar(oname,istat)

*
* DO NEXT CRDD FILE
*
         enddo

      endif

*
* FINISH
*
  999 continue

      end
