      subroutine radec
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Convert a list ox XY pixel co-ords to RA and DEC using
*       descriptors in an IRAS image. Alternatively the inverse
*       operation may be done.
*
*SOURCE
*       RADEC.FOR in RADEC.TLB
*
*METHOD
*       Prompt for IRAS image to which XY co-ords refer. Then
*       prompt for an xy list, if non is given prompt user for
*       co-ords (XY or RA and DEC). Convert the co-ordinates
*       of each point display and add to output list.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               rdimds,wrerr,gtwork,gtstrn
*       THIS PACKAGE (RADEC.TLB):
*               pradec
*       EDRS:
*               gtxylr,gtxylw,extlst,addlst,getpar,lbgone,ptdscr,gtdscr
*       INTERIM:
*               frdata,wruser,rdkeyc,cydscr
*
*STARLINK PARAMETERS
*       IMAGE/read/     The IRAS image from which co-ords are required
*       INPUT/read/     An optional input XY list
*       ILEVEL/read/    User information display level
*       MAXENTRY/read/  Max number of entries in output list
*       OUTPUT/read/    Output list
*       TITLE/read/     Title for output list
*       GETID/read/     YES if user wishes to be prompted for identifiers
*       INVERSE/read/   NO gives XY to RA,DEC conversion. YES gives
*                       inverse conversion (RA,DEC to XY).
*       NOLIST/error/   Accessed if output list has no entries
*       NOCOMB/error/   Accessed if the IRAS image was produced by
*                       COMBINE but has non-zero CROTA1
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/12/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      cdelt1  ! Degrees of arc per pixel in x
      real      cdelt2  ! Degrees of arc per pixel in y
      integer   crpix1  ! X co-ord of ref pixel
      integer   crpix2  ! Y co-ord of ref pixel
      real      crota1  ! Angle from north through +ve X to -ve Y
      real      crval1  ! RA of reference pixel in degrees
      real      crval2  ! DEC of reference pixel in degrees
      character cval*1  ! Dummy character argument
      logical   gtiden  ! True if user is to be prompted for identifiers
      integer   ierr    ! Error status
      integer   ilevel  ! Interaction level
      integer   ipid    ! Pointer to input identifiers
      integer   ipido   ! Pointer to output identifiers
      integer   ipin    ! Pointer to input xy list
      integer   ipout   ! Pointer to output xy list
      integer   ipx     ! Pointer to input xy list x co-ords
      integer   ipy     ! Pointer to input xy list y co-ords
      integer   ipxo    ! Pointer to output xy list x co-ords
      integer   ipyo    ! Pointer to output xy list y co-ords
      character instra*15! INSTRUME descriptor from IRAS image
      integer   ival    ! Dummy integer argument
      integer   lenout  ! No. of records in output xy list
      integer   lstlen  ! No. of records in input xy list
      integer   ncomm   ! Position of selected option within option list
      integer   nitem   ! No. of items in each record
      integer   nout    ! Actual no. of items to be put into output list
      character prbuf*80! Buffer for user output text
      real      rval    ! Dummy real argument
      character title*30! Title for output xy list
      logical   xyinp   ! True if an input xy list was given
      logical   xytoeq  ! If true conversion is from XY to RA and DEC
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
*
* GET THE NECESSARY DESCRIPTORS FROM THE IRAS IMAGE FROM WHICH THE XY
* CO-ORDS COME
*
      call rdimds('IMAGE',.false.,crval1,crval2,crpix1,crpix2,cdelt1,
     :             cdelt2,crota1,instra,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) goto 999
*
* GET OPTIONAL INPUT XY LIST
*
      xyinp=.true.
      call gtxylr('INPUT',.true.,nitem,lstlen,ipin,ierr)
      if(ierr.ne.0) xyinp=.false.
*
* IF INPUT WAS SUPPLIED, OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD
* X AND Y VALUES, AND IDENTIFIERS
*
      if(xyinp) then
         call gtwork('ID','INTEGER',5*lstlen,ipid,ierr)
         if(ierr.eq.0) call gtwork('X','INTEGER',lstlen,ipx,ierr)
         if(ierr.eq.0) call gtwork('Y','INTEGER',lstlen,ipy,ierr)
         if(ierr.ne.0) goto 999
*
* COPY INPUT LIST DATA TO WORKSPACE
*
         call extlst(%val(ipin),nitem,lstlen,%val(ipid),1,20)
         call extlst(%val(ipin),nitem,lstlen,%val(ipx),21,24)
         call extlst(%val(ipin),nitem,lstlen,%val(ipy),25,28)
         lenout=lstlen
      else
         lstlen=1
         lenout=100
      endif
*
* OBTAIN VALUE FROM ENVIRONMENT FOR MAX NO. OF ENTRIES IN OUTPUT LIST
*
      call getpar('MAXENTRY','INTEGER',1,real(lstlen+1),1.0e6,.true
     : .,lenout,rval,ierr)
*
* SEE IF CONVERSION FROM PIXEL CO-ORDS TO EQUATORIAL CO-ORDS IS
* REQUIRED OR OTHER WAY ROUND
*
      ncomm=2
      call gtstrn('INVERSE',.true.,'YES,NO,TRUE,FALSE.',1,ncomm,cval,
     :             ival,ierr)
      if(mod(ncomm,2).eq.0) then
         xytoeq=.true.
      else
         xytoeq=.false.
      endif
*
* SEE IF USER WISHES TO BE PROMPTED FOR IDENTIFIERS TO PUT INTO
* OUTPUT XY LIST DATASET
*
      ncomm=2
      call gtstrn('GETID',.true.,'YES,NO,TRUE,FALSE.',1,ncomm,cval,
     :             ival,ierr)
      if(mod(ncomm,2).eq.0) then
         gtiden=.false.
      else
         gtiden=.true.
      endif
*
* OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND X,Y
* VALUES IN OUTPUT LIST
*
      call gtwork('OID','INTEGER',5*lenout,ipido,ierr)
      if(ierr.eq.0) call gtwork('OX','INTEGER',lenout,ipxo,ierr)
      if(ierr.eq.0) call gtwork('OY','INTEGER',lenout,ipyo,ierr)
      if(ierr.ne.0) goto 999
*
* CALL PRADEC TO PRINT THE RA AND DEC VALUES AND PUT THEM INTO THE
* OUTPUT XY LIST
*
      call pradec(crval1,crval2,crpix1,crpix2,crota1,cdelt1,cdelt2,
     :            %val(ipx),%val(ipy),%val(ipid),lstlen,
     :            %val(ipxo),%val(ipyo),%val(ipido),lenout,
     :            nout,xytoeq,gtiden,xyinp)
*
* IF NO LIST OBTAINED, GIVE ERROR MESSAGE AND ABORT... OTHERWISE
* OBTAIN OUTPUT DATA FRAME
*
      if(nout.le.0) then
         call wrerr('NOLIST')
         goto 999
      else
         call gtxylw('OUTPUT',.true.,7,nout,ipout,ierr)
         if(ierr.eq.0) then
*
* OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS FROM WORKSPACE TO
* OUTPUT DATA FRAME
*
            call addlst(%val(ipout),7,nout,%val(ipido),1,20)
            call addlst(%val(ipout),7,nout,%val(ipxo),21,24)
            call addlst(%val(ipout),7,nout,%val(ipyo),25,28)
*
* TELL USER HOW MANY ENTRIES IN OUTPUT LIST
*
            if(ilevel.ge.2) then
               write(prbuf,104) nout
               if(nout.eq.1) prbuf(28:)='ENTRY'
104            format('   OUTPUT LIST HAS ',i7,' ENTRIES')
               call lbgone(prbuf(20:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)
               call wruser(' ',ierr)
            endif
*
* DEFAULT OUTPUT TITLE IS EITHER THE INPUT TITLE, OR BLANK.
* THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
*
            title=' '
            if(xyinp) then
               call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,
     :         title,ierr)
            endif
            call rdkeyc('TITLE',.true.,1,title,ival,ierr)
*
* COPY INPUT DESCRIPTOR TO OUTPUT (IF AVAILABLE) THEN UPDATE
* DESCRIPTOR ITEMS
*
            if(xyinp) call cydscr('INPUT','OUTPUT',ierr)
            call ptdscr('OUTPUT','NITEM','INTEGER',7,rval,cval,ierr)
            call ptdscr('OUTPUT','LSTLEN','INTEGER',nout,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title
     :       ,ierr)
         endif
      endif
*
* FINISH
*
  999 call frdata(' ',ierr)

      end
