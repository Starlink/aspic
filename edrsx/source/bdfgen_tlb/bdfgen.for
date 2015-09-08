      subroutine bdfgen
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Creates an EDRS image or XY list by reading data in from a
*       text file.
*
*SOURCE
*       BDFGEN.FOR in BDFGEN.TLB
*
*METHOD
*       The user specifies which field in the text file corresponds
*       to X, Y and data value.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wrerr,gtwork,wruser,gt2diw,gtbool,gtstrn
*       THIS PACKAGE (BDFGEN.TLB):
*              gtfile,rdfile,rdfilr,copdat
*       EDRS:
*              getpar,lbgone,ptdscr
*       INTERIM:
*              rdkeyr,rdkeyc,frdata
*STARLINK PARAMETERS
*       AUTO/read/      If true then pixel co-ords are generated
*                       automatically, otherwise they are read from
*                       the input text file.
*       BDFTYPE/read/   The type of BDF to be generated; IMAGE or XYLIST
*       DFIELD/read/    Position of pixel data within record
*       FILE/read/      Name of text file containing input data
*       IDSTART/read/   The identifier no. to give to the first XY list entry
*       OUTPUT/read/    Name of output BDF image
*       NLINEOUT/read/   No. of lines in output image
*       NPIXOUT/read/   No. of pixels per line in output image
*       NREC/read/      No. of records to read from file
*       TITLE/read/     Title for output image
*       TRCOEFFS/read/  Transformation to apply to x and y data to get
*                       actual pixel co-ordinates
*       XFIELD/read/    Position of X data within record
*       YFIELD/read/    Position of Y data within record
*       NODATA/error/   Accessed if the input contains no data
*       NOXYOUT/error/  Accessed if an XYlist required but text file has
*                       less than two fields per record
*       ILEVEL/value/   User information level
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 8/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      logical   auto    ! If True then pixel coords are generated
                        ! automatically, otherwise they are read from
                        ! the input file.
      character bdftyp*6! Type of BDF required (image or XY list)
      real      c(6)    ! Transformation to apply to X and Y before use
      character cval*1  ! Dummy character argument
      integer   idstrt  ! Lowest identifier no. for XY list entries
      integer   ierr    ! Error status
      integer   ilevel  ! User information level
      integer   inval   ! Invalid pixel value
      integer   ipd     ! Pointer to workspace holding XYlist data values
      integer   ipdata  ! Pointer to data read from file
      integer   iphl    ! Pointer to array of upper limits
      integer   ipid    ! Pointer to workspace holding XYlist identifiers
      integer   ipll    ! Pointer to array of lower limits
      integer   ipout   ! Pointer to output image
      integer   ipx     ! Pointer to workspace holding XYlist x values
      integer   ipy     ! Pointer to workspace holding XYlist y values
      integer   ival    ! Dummy integer argument
      integer   npt     ! No. of records in file
      integer   maxval  ! Max no of fields allowed per record
      parameter (maxval=20)
      integer   maxf    ! Maximum no. of fields in each file record
      integer   minf    ! Minimum no. of fields in each file record
      integer   ncomm   ! Position of default option within option list
      integer   nfield  ! No. of fields per record in input file
      integer   ndf     ! Field position of pixel data
      integer   nitem   ! No. of bytes per XY list entry
      integer   nlin    ! No. of lines in output image
      integer   npix    ! No. of pixels per line in output image
      integer   nrd     ! No. of records actually read
      integer   nval    ! Actual no. of fields found in a record
      integer   nxf     ! Field position of x data
      integer   nyf     ! Field position of y data
      character prbuf*80! Buffer for output to terminal screen
      real      rval    ! Dummy real argument
      real      scale   ! BSCALE value for output
      character title*80! Title for output
      real      val(maxval)! Values read from a single record of the file
      real      zero    ! BZERO value for output

*
* SEE IF USER WANTS TO CREATE AN IMAGE OR AN XY LIST
*
      ncomm=1
      call gtstrn('BDFTYPE',.true.,'IMAGE,XYLIST.',1,ncomm,bdftyp,ival,
     :             ierr)

*
* OPEN INPUT FILE
*
      call gtfile(10,'FILE',.false.,.false.,ierr)
      if(ierr.ne.0) goto 999

*
* FIND NO. OF FIELDS IN EACH RECORD OF INPUT DATA AND NO. OF RECORDS
* IN FILE
*

      minf=1000000
      maxf=-1000000
      npt=0
  10  call rdfilr(10,val,maxval,nval,0,ierr)
      if(ierr.ne.-1) then
         minf=min(minf,nval)
         maxf=max(maxf,nval)
         npt=npt+1
         goto 10
      endif
      rewind 10
      nfield=minf

*
* IF REQUIRED, TELL USER THE SIZE OF THE INPUT DATA ARRAY
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)

      if(ilevel.gt.1) then

         if(minf.eq.maxf) then

            write(prbuf,11) npt,nfield
  11        format(4X,'Text file contains ',I4,
     :             ' records each containing ',I4,' values')
            call lbgone(prbuf(53:))

         else

            write(prbuf,12) npt,nfield
  12        format(4X,'Text file contains ',I4,
     :             ' records each containing a minimum of ',I4,
     :             ' values')
            call lbgone(prbuf(66:))

         endif

         call lbgone(prbuf(24:))
         call wruser(' ',ierr)
         call wruser(prbuf,ierr)
         call wruser(' ',ierr)

      endif

*
* IF FILE WAS EMPTY GIVE MESSAGE AND END
*
      if(npt.eq.0.or.nfield.eq.0) then
         call wrerr('NODATA')
         goto 999
      endif

*
* IF AN XY LIST OUTPUT WAS REQUESTED CHECK THAT THE INPUT TEXT FILE HAS
* AT LEAST TWO FIELDS PER RECORD
*
      if(bdftyp.eq.'XYLIST'.and.nfield.lt.2) then
         call wrerr('NOXYOUT')
         goto 999
      endif

*
* SEE IF X AND Y CO-ORDS ARE TO BE GENERATED AUTOMATICALLY BY THE
* PROGRAM OR ARE TO BE READ FROM THE FILE
*
      if(bdftyp.eq.'IMAGE') then

         if(nfield.ge.3) then
            auto=.false.
         else
            auto=.true.
         endif
         call gtbool('AUTO',.true.,auto,ierr)

      else
*
* IF AN XY LIST IS TO BE GENERATED, GET THE IDENTIFIER NUMBER TO GIVE TO
* THE FIRST ENTRY
*
         call getpar('IDSTART','INTEGER',1,0.0,1.0E6,.true.,idstrt,
     :               rval,ierr)
         auto=.false.

      endif

*
* GET NO. OF RECORDS TO READ FROM USER
*
      call getpar('NREC','INTEGER',1,1.0,real(npt),.true.,npt,rval,ierr)

*
* GET FIELDS TO USE FOR DATA VALUE, AND (IF AUTO IS FALSE) X AND Y
* VALUES. (IF NDF IS ZERO, ALL FIELDS FROM EACH RECORD WILL BE
* CONCATENATED INTO ONE ARRAY)
*
      if(bdftyp.eq.'IMAGE') then

         if(auto) then
            nxf=0
            nyf=0
            ndf=0
         else
            nxf=1
            call getpar('XFIELD','INTEGER',1,1.0,real(nfield),.true.,
     :                   nxf,rval,ierr)
            nyf=2
            call getpar('YFIELD','INTEGER',1,1.0,real(nfield),.true.,
     :                   nyf,rval,ierr)
            ndf=3
         endif
         call getpar('DFIELD','INTEGER',1,0.0,real(nfield),.true.,ndf,
     :               rval,ierr)

      else

         nxf=1
         call getpar('XFIELD','INTEGER',1,1.0,real(nfield),.true.,
     :                nxf,rval,ierr)
         nyf=2
         call getpar('YFIELD','INTEGER',1,1.0,real(nfield),.true.,
     :                nyf,rval,ierr)

         if(nfield.gt.2) then
            ndf=3
            call getpar('DFIELD','INTEGER',1,0.0,real(nfield),.true.,
     :                   ndf,rval,ierr)
         else
            ndf=0
         endif

      endif

*
* GET WORKSPACE FOR DATA
*
      call gtwork('DATA','REAL',nfield*npt,ipdata,ierr)
      if(ierr.eq.0) call gtwork('HLIM','REAL',nfield,iphl,ierr)
      if(ierr.eq.0) call gtwork('LLIM','REAL',nfield,ipll,ierr)
      if(ierr.ne.0) goto 999

*
* CALL RDFILE TO READ THE DATA FROM THE FILE
*
      call rdfile(10,%val(ipdata),npt,nfield,%val(ipll),%val(iphl),nrd,
     :            ierr)
      if(nrd.ne.npt) then
         if(nrd.eq.0) then
            call wrerr('NODATA')
            goto 999
         else
            write(prbuf,13) nrd
  13        format('*** Only ',I6,' records read')
            call lbgone(prbuf(10:))
            call wruser(prbuf,ierr)
         endif
      endif

*
* THE FOLLOWING CODE APPLIES TO IMAGE OUTPUTS ONLY
*---------------------------------------------------------------
*
      if(bdftyp.eq.'IMAGE') then

*
* GET DEFAULT SIZE OF OUTPUT IMAGE
*
         if(auto) then
            if(ndf.eq.0) then
               npix=nfield
               nlin=nrd
            else
               npix=nrd
               nlin=1
            endif
         else

*
* IF PIXEL CO-ORDS TO BE READ FROM INPUT, GET TRANSFORMATION
* CO-EFFICIENTS TO APPLY TO X AND Y BEFORE USE AND FIND RESULTING
* PIXEL LIMITS
*
            call rdkeyr('TRCOEFFS',.false.,6,c,nval,ierr)
            call ims(%val(iphl),%val(ipll),nfield,nxf,nyf,npix,nlin,c)
         endif

*
* GET ACTUAL SIZE OF OUTPUT IMAGE
*
         call getpar('NPIXOUT','INTEGER',1,1.0,1000000.0,.true.,npix,
     :               rval,ierr)
         call getpar('NLINEOUT','INTEGER',1,1.0,1000000.0,.true.,
     :               nlin,rval,ierr)

*
* GET OUTPUT IMAGE
*
         call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierr)
         if(ierr.ne.0) goto 999

*
* COPY THE DATA INTO THE OUTPUT IMAGE
*
         inval=-32767
         call copdat(%val(ipdata),nfield,npt,%val(ipout),npix,nlin,c,
     :               nxf,nyf,ndf,scale,zero,inval,%val(iphl),%val(ipll))

*
* ADD DESCRIPTORS
*
         call ptdscr('OUTPUT','NAXIS','INTEGER',2,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',npix,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',nlin,rval,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',inval,rval,cval,
     :                ierr)
         call rdkeyc('TITLE',.false.,1,title,ival,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,
     :                ierr)

*
* THE FOLLOWING CODE APPLIES TO XY LIST OUTPUTS ONLY
*---------------------------------------------------------------
*
      else


*
* OBTAIN WORKSPACE TO HOLD IDENTIFIERS, X AND Y VALUES
*
         call gtwork('ID','INTEGER',5*npt,ipid,ierr)
         if(ierr.eq.0) call gtwork('X','INTEGER',npt,ipx,ierr)
         if(ierr.eq.0) call gtwork('Y','INTEGER',npt,ipy,ierr)

*
* IF REQUIRED, GET WORKSPACE FOR DATA VALUES
*
         if(ierr.eq.0) then
            if(ndf.ne.0) then
               call gtwork('D','INTEGER',npt,ipd,ierr)
            else
               ipd=ipx
            endif
         endif

*
* ABORT IF SPACE NOT AVAILABLE
*
         if(ierr.ne.0) goto 999

*
* CALL COPXYL TO COPY THE DATA INTO THE XYLIST WORKSPACE
*
         call copxyl(%val(ipdata),nfield,npt,%val(ipid),%val(ipx),
     :               %val(ipy),%val(ipd),nxf,nyf,ndf,idstrt)

*
* SET THE NUMBER OF BYTES PER LIST ENTRY
*
         if(ndf.eq.0) then
            nitem=7
         else
            nitem=8
         endif

*
* OBTAIN THE OUTPUT XY LIST BDF
*
         call gtxylw('OUTPUT',.false.,nitem,npt,ipout,ierr)
         if(ierr.ne.0) goto 999

*
* COPY LIST FROM WORKSAPCE TO OUTPUT BDF
*
         call addlst(%val(ipout),nitem,npt,%val(ipid),1,20)
         call addlst(%val(ipout),nitem,npt,%val(ipx),21,24)
         call addlst(%val(ipout),nitem,npt,%val(ipy),25,28)

         if(ndf.ne.0) then
            call addlst(%val(ipout),nitem,npt,%val(ipd),29,32)
         endif

*
* ADD DESCRIPTORS
*
         call ptdscr('OUTPUT','NITEM','INTEGER',nitem,rval,cval,ierr)
         call ptdscr('OUTPUT','LSTLEN','INTEGER',npt,rval,cval,ierr)
         call rdkeyc('TITLE',.false.,1,title,ival,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,
     :                ierr)

*--------------------------------------------------------------------

      endif

*
* FREE DATA AREAS
*
 999  call frdata(' ',ierr)

      end



      subroutine ims(hilim,lolim,nfield,nxf,nyf,npix,nlin,c)
      real hilim(nfield),lolim(nfield),c(6)
*
* GET SIZE OF OUTPUT IMAGE
*
      t1=c(1)+c(2)*hilim(nxf)+c(3)*lolim(nyf)
      t2=c(1)+c(2)*hilim(nxf)+c(3)*hilim(nyf)
      t3=c(1)+c(2)*lolim(nxf)+c(3)*hilim(nyf)
      t4=c(1)+c(2)*lolim(nxf)+c(3)*lolim(nyf)
      npix=int(max(t1,t2,t3,t4)+0.5)
      t1=c(4)+c(5)*hilim(nxf)+c(6)*lolim(nyf)
      t2=c(4)+c(5)*hilim(nxf)+c(6)*hilim(nyf)
      t3=c(4)+c(5)*lolim(nxf)+c(6)*hilim(nyf)
      t4=c(4)+c(5)*lolim(nxf)+c(6)*lolim(nyf)
      nlin=int(max(t1,t2,t3,t4)+0.5)

      end
