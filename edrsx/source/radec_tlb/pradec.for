      subroutine pradec(crval1,crval2,crpix1,crpix2,crota1,cdelt1,
     :                  cdelt2,x,y,id,lstlen,xo,yo,ido,lenout,nout,
     :                  xytoeq,gtiden,xyinp)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Prints the RA and DEC of specified pixels on the terminal
*       screen. The pixels are specified either by an input xy list
*       or by prompting the user. Alternatively, the inverse operation
*       can be performed (print pixel co-ords with specified RA and
*       DEC).
*
*METHOD
*       The XY to RA and DEC transformation is defined by the arguments
*       crval1,crval2,crpix1,crpix2,cdelt1,cdelt2 and crota1. These
*       are stored as descriptors in IRAS images. Algorithms from the
*       IRAS Explanatory Supplement (Page X-30) are used to implement
*       a tangent plane projection.
*
*SOURCE
*       PRADEC.FOR in RADEC.TLB
*
*ARGUMENTS
*   INPUTS:
*       real    crval1    RA of image reference pixel in degrees east
*       real    crval2    DEC of image reference pixel in degrees north
*       integer crpix1    X co-ord of image reference pixel
*       integer crpix2    Y co-ord of image reference pixel
*       real    crota1    Angle from north through +ve X axis to -ve y axis
*       real    cdelt1    Degrees of arc per pixel in X
*       real    cdelt2    Degrees of arc per pixel in Y
*       real    x(lstlen) List of input x co-ords (X in pixels, or RA
*                         in hours)
*       real    y(lstlen) List of input y co-ords (Y in pixels, or DEC
*                         in degrees)
*       byte    id(20,lstlen) 20 character identifiers for each input
*                         position.
*       integer lstlen    No. of co-ordinate pairs in input list
*       integer lenout    Declared size of output list
*       logical gtiden    True if identifiers should be prompted for
*       logical xyinp     True if values from xy lists are to be used.
*                         If false, then user is prompted for co-ords.
*       logical xytoeq    If true conversion is from pixel co-ords to
*                         equatorial co-ords (RA and DEC).
*   OUTPUTS:
*       real    xo(lenout)List of output x co-ords (X in pixels, or RA
*                         in hours)
*       real    yo(lenout)List of output y co-ords (Y in pixels, or DEC
*                         in degrees)
*       byte    ido(20,lenout) 20 character identifiers for each output
*                         position.
*       integer nout      No. of valid entries in output list
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               xytord,rdtoxy,gtradc
*       EDRS:
*               lbgone,xyprgg
*       INTERIM:
*               wruser,rdkeyr,cnpar,rdkeyc,ctoi
*
*STARLINK PARAMETERS
*       XYCOORDS/read/  The x and y pixel co-ords obtained from user
*       IDENT/read/     Identifier for XY list entry
*       RA_HRS/read/    RA hours field
*       RA_MINS/read/   RA minutes field
*       RA_SECS/read/   RA seconds field
*       DEC_DEG/read/   DEC degrees field
*       DEC_MIN/read/   DEC minutes field
*       DEC_SEC/read/   DEC seconds field
*
*VAX SPECIFICS
*       implicit none
*       byte arrays
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/12/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer lstlen,crpix1,crpix2,nout,lenout
      real x(lstlen),y(lstlen),crval1,crval2,cdelt1,cdelt2,crota1
      real xo(lenout),yo(lenout)
      logical xyinp,gtiden,xytoeq
      byte id(20,lstlen),ido(20,lenout)
*
* DECLARE LOCAL VARIABLES
*
      real      dec     ! DEC of given pixel in degrees
      integer   decdgs  ! Degrees field of DEC
      integer   decmns  ! Minutes field of DEC
      real      decsec  ! Seconds field of DEC
      character equbuf*33! Buffer for formatted RA and DEC values
      integer   i       ! Loop count
      character idbuf*20! Buffer for identifier
      character inbuf(3)*80 ! Buffer for user input
      integer   ierr    ! Error status
      integer   ival    ! Dummy integer argument
      logical   more    ! False if user gives null co-ords value
      integer   nb      ! Value of i in "#i" identifier given by user
      integer   nblank  ! Current value of i in "#i"-type identifiers
      integer   ncar    ! Length of PIXBUF minus trailing blanks
      integer   nsave   ! No. of list entries after list is purged
      logical   ok      ! True if a good set of x and y is given by user
      character pixbuf*22! Buffer for formatted pixel co-ord values
      real      ra      ! RA of given pixel in hours
      real      radgrs  ! RA of given pixel in degrees
      integer   rahrs   ! Hours field of RA
      integer   ramins  ! Minutes field of RA
      real      rasecs  ! Seconds field of RA
      integer   strlen  ! Function giving length of string minus
                        ! trailing spaces etc
      real      temp    ! Temporary storage
      real      xy(2)   ! X and Y pixel co-ords from user
*
* LOOP UNTIL INPUT LIST IS DONE OR USER GIVES A NULL ENTRY OR MAXIMUM
* OUTPUT LIST LENGTH IS EXCEEDED
*
      nblank=1
      nout=0
      more=.true.
      do while(more)
*
* IF AN INPUT LIST WAS GIVEN, USE THE VALUES FROM THE LIST
*
         if(xyinp) then
            if(xytoeq) then
               xy(1)=x(nout+1)
               xy(2)=y(nout+1)
            else
               radgrs=15*x(nout+1)
               dec=y(nout+1)
            endif
            do i=1,20
               idbuf(i:i)=char(id(i,nout+1))
            enddo
         else
*
* IF NO INPUT LIST WAS GIVEN, GET XY PAIR AND IDENTIFIER FROM USER
* FIRST SET UP THE DEFAULT IDENTIFIER
*
            write(idbuf,'(I20)') nblank
            idbuf(1:1)='#'
            call lbgone(idbuf(2:))
*
* IF THE CONVERSION IS FROM PIXEL CO-ORDS TO RA AND DEC, GET PIXEL
* CO-ORDS AND IDENTIFIER FROM USER
*
            if(xytoeq) then
               call rdkeyr('XYCOORDS',.false.,2,xy,ival,ierr)
               if(ierr.eq.0) then
                  call cnpar('XYCOORDS',ierr)
                  if(gtiden) then
                     call rdkeyc('IDENT',.true.,1,idbuf,ival,ierr)
                     call cnpar('IDENT',ierr)
                  endif
               else
                  more=.false.
               endif
*
* IF THE CONVERSION IS FROM RA AND DEC TO PIXEL CO-ORDS , GET RA,DEC
* AND IDENTIFIER FROM USER
*
            else
               call gtradc(' ',ra,dec,.true.,ierr)
               if(ierr.eq.0) then
                  radgrs=15*ra
                  if(gtiden) then
                     call rdkeyc('IDENT',.true.,1,idbuf,ival,ierr)
                     call cnpar('IDENT',ierr)
                  endif
               else
                  more=.false.
               endif
            endif
         endif

         if(more) then
*
* IF CONVERSION IS FROM (X,Y) TO (RA,DEC) CALCULATE THE RA AND DEC
* OF THE GIVEN PIXEL
*
            if(xytoeq) then
               call xytord(xy(1),xy(2),crval1,crval2,crpix1,crpix2,
     :                     cdelt1,cdelt2,crota1,radgrs,dec)
*
* IF CONVERSION IS FROM (RA,DEC) TO (X,Y) CALCULATE THE CO-ORDINATES
* WITH THE GIVEN RA AND DEC
*
            else
               call rdtoxy(xy(1),xy(2),crval1,crval2,crpix1,crpix2,
     :                     cdelt1,cdelt2,crota1,radgrs,dec)
            endif
*
* SPLIT RA AND DEC UP INTO USUAL UNITS
*
            ra=radgrs/15.0
            rahrs=int(ra)
            temp=60*(ra-rahrs)
            ramins=int(temp)
            rasecs=60*(temp-ramins)
            decdgs=int(dec)
            temp=60*abs(dec-decdgs)
            decmns=int(temp)
            decsec=60*(temp-decmns)
*
* PRINT THE VALUES ON THE TERMINAL SCREEN
*
            write(pixbuf,10) xy(1),xy(2)
   10       format('(',F9.2,', ',F9.2,')')
            call lbgone(pixbuf(12:))
            call lbgone(pixbuf(2:))
            ncar=strlen(pixbuf)
            write(equbuf,20) rahrs,ramins,rasecs,decdgs,decmns,decsec
   20       format(I2,'h ',I2,'m ',f4.1,'s, ',I3,'deg ',I2,'m ',
     :             f4.1,'s')
            call wruser(' ',ierr)
            if(xytoeq) then
               call wruser(idbuf//' '//pixbuf(:ncar)//' = '//equbuf,
     :                     ierr)
            else
               call wruser(idbuf//' '//equbuf//' = '//pixbuf(:ncar),
     :                     ierr)
            endif
            if(.not.xyinp) call wruser(' ',ierr)
*
* WRITE VALUES TO THE OUTPUT LIST
*
            if(xytoeq) then
               xo(nout+1)=ra
               yo(nout+1)=dec
            else
               xo(nout+1)=xy(1)
               yo(nout+1)=xy(2)
            endif
*
* IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
* IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
* RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
* SEQUENTIALLY NUMBERED '#N' FORM
*
            if(idbuf(1:1).eq.'#') then
               call ctoi(idbuf(2:),nb,ierr)
               if(ierr.eq.0) then
                  nblank=nb+1
                  write(idbuf,'(I20)')nb
                  idbuf(1:1)='#'
                  call lbgone(idbuf(2:))
               endif
            endif
*
* PUT ID INTO IDENTIFIER LIST
*
            do  i=1,20
               ido(i,nout+1)=ichar(idbuf(i:i))
            enddo
*
* SEE IF MAXIMUM OUTPUT LIST LENGTH HAS BEEN REACHED
*
            nout=nout+1
            if(nout.eq.lenout) more=.false.
*
* IF INPUT IS COMING FROM A LIST, SEE IF END HAS BEEN REACHED
* AND GO ROUND FOR NEXT VALUE
*
            if(xyinp.and.nout.eq.lstlen) more=.false.
         endif
      enddo
      call wruser(' ',ierr)
*
* PURGE THE LIST BEFORE LEAVING
*
      if(nout.gt.1) then
         call xyprgg(xo,yo,ido,nout,nsave,ierr)
         nout=nsave
      endif
*
* FINISH
*
      end
