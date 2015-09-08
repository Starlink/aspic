      subroutine ndfout
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Converts an INTERIM IRAS image (either ASPIC or EDRS) or
*       CRDD file, into an NDF structure, usable within IRAS90.
*
*SOURCE
*       NDFOUT.FOR in NDFOUT.TLB
*
*METHOD
*       Each row in the output NDF contains data obtained from the
*       equivalent row in the input BDF. The output NDF has the
*       following components defined:
*
*       DATA - Contains the scaled data values (i.e. NOT integer data 
*              numbers) from the BDF in _REAL format, in the units 
*              described by the UNITS component. Storage form is 
*              either PRIMITIVE or SIMPLE (selected by the user). Any
*              invalid or blank values in the BDF are set to the 
*              Starlink "BAD" value (VAL__BADR) in the output. The 
*              "bad pixel" flag is set true if any bad values are found,
*              and false otherwise.
*
*       TITLE - Contains the value of the BDF descriptor TITLE if the 
*               input is an EDRS/X image. Otherwise contains the value 
*               of the OBJECT descriptor. If neither of these can be 
*               found, the title reverts to the name of the input BDF 
*               file. If even this fails, the title is set to "Output 
*               from EDRSX:NDFOUT".
*
*       LABEL - Set to "Survey CRDD" for CRDD files, and "Surface 
*               brightness" for images.
*
*       UNITS - Set to pW/(M**2) for CRDD files, and MJy/sr for images.
*
*       All other NDF components are undefined. The output contains an
*       extension called "IRAS" with an HDS type of "IRAS". This 
*       extension can contain the following components:
*
*       ASTROMETRY - This is an IRAS90 astrometry structure created by
*               the IRA package. It is only created if the input is an
*               image containing the following descriptors; CRVAL1,
*               CRVAL2, CRPIX1, CRPIX2, CDELTA1, CDELT2, CROTA1, 
*               INSTRUME.
*
*       CRDD_INFO - This is a structure which holds information used by 
*               the IRAS90 CRDD handling package, IRC. This component 
*               is only created if the input BDF is a CRDD file. At the 
*               moment, there are some details of this structure which 
*               havn't been finalised, so this structure may have to 
*               be altered slightly some time in the future. The current
*               definition of this structure is contained in  IRAS90 
*               document 1 (ID/1).
*
*       IMAGE_INFO - A structure which is analagous to CRDD_INFO for use
*               with IRAS images. Defined in ID/12. It contains the 
*               components, INSTRUMENT, BAND, FIELDLON, FIELDLAT and
*               FIELDSCS.
*       
*       IMAGE_TYPE - A _CHAR*14 component which gives the type of IRAS 
*               image. It can take one of the values; CPC, GALACTIC 
*               PLANE, DEEP SKY, COMBINED, SKYFLUX, SPLINE, MEMCRDD, 
*               UNKNOWN.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtfmts,wruser,gtstrn,gt2dir,bdfnam,ustrln,movlev
*       THIS PACKAGE (NDFOUT.TLB):
*              scale,crdin,crdout,imgout,hist
*       EDRS:
*              wrerr,getpar,gtdscr,lbgone
*       INTERIM:
*              rdkeyc,frdata
*       HDS:
*              hds_start,hds_new,cmp_put0i,cmp_put0r,cmp_put0c,
*              hds_close,dat_find,dat_new0i,dat_new0r,dat_new0c
*       NDF:
*              ndf_place,ndf_new,ndf_map,ndf_cput,ndf_annul,
*              ndf_sbad, ndf_newp
*       ERR:
*              err_flush,err_annul
*       PRIMDAT:
*              vec_btod,vec_wtod,vec_itod,vec_rtod,vec_dtod,vec_ubtod,
*              vec_uwtod,val_itod,val_rtod
*
*STARLINK PARAMETERS
*       BAND/read/      The IRAS band number of the input image.
*       DTYPE/read/     The input data type to be used for conversion
*       HDSTYPE/read/   The HDS data type for the output NDF
*       ILEVEL/read/    The user information display level
*       INPUT/read/     The input BDF image
*       NDFFORM/read/   The form of NDF required, PRIMITIVE or SIMPLE
*       OUTPUT/write/   The name of the output NDF
*       NOINCARN/error/ Accessed if the input file contains no incarnations
*       BADTYPE/error/  Accessed if input data format is not supported.
*       NONEVAL/error/  Accessed if the output would contain no valid data
*       NOOUTPUT/error/ Accessed if an error occurs creating the output NDF
*       TOOBAD/error/   Accessed if too many bad parameter values are given
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       enddo
*       real*8
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/9/90
*-------------------------------------------------------------------
      implicit none

      include 'SAE_PAR'

      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include 'UTILITIES(DS_COM)'


*
* DECLARE LOCAL VARIABLES
*
      character alts*80         ! List of format specifiers available to user
      integer   band            ! IRAS band no. of input data.
      character bdfnam*40       ! Name of input BDF file.
      real      blank           ! Value of blank pixels in input
      real      bscale          ! Scale factor for input data values
      real      bzero           ! Zero level for input data values
      logical   crdd            ! True if i/p is a CRDD file.
      character cval*30         ! Dummy character argument
      real*8    dblank		! Double precision version of BLANK
      logical	deflt		! If true then a default is given user.
      integer	end		! Offset from start to end of file name
      integer	err		! Position of 1st error detected by PRIMDAT
      integer   errlen          ! Length of ADAM error message.
      character errmsg*80       ! ADAM error message.
      character fmtnam(7)*17    ! Descriptions of the recognized formats
      character fmtspc(7)*2     ! Strings for specifying formats to user
      character form*9          ! Storage form of NDF arrays.
      logical   formts(7)       ! Each flag is set true if corresponding
                                ! incarnation exists in the input image
      integer   frmid           ! Frame identifier for CRDD descriptors
      integer   i               ! Loop count
      integer   ialt            ! Pointer to start of next specifier in alts
      integer   ierr            ! Status return value
      integer   ifmts(7)        ! The recognized formats codes
      integer   ilevel          ! Amount of information to display on screen
      character*(DAT__SZLOC) iloc ! HDS locator to output container file
      integer	indf		! NDF locator for output NDF
      integer   iorig           ! Selected input data type in range 1 to 7
      character iname*2         ! Format specifier selected by user
      integer   ipin            ! Pointer to start of image data area
      integer   ipout           ! Pointer to start of output data area
      integer   itype           ! The position of the selected format specifier
                                ! within list of alternatives
      integer   ival            ! Dummy integer argument
      character*(DAT__SZLOC) jloc ! HDS locator to output NDF
      integer	lbnd(2)		! Lower pixel bounds of output NDF
      logical   magic           ! True if magic pixels are present in the data
      integer   nchar           ! No. of characters in value returned by GTSTRN
      character ndffil*40       ! Name of output SDF file.
      character ndfnam*40       ! Name of output NDF structure
      integer	ndform          ! 1 for a SIMPLE ndf, 2 for a PRIMITIVE NDF
      integer	nel		! No. of elements in data array.
      integer	nerr		! No. of errors detected by PRIMDAT
      integer   nfmt            ! No. of incarnations in the input image
      integer	nbad		! No. of bad pixels in output
      integer   nlin            ! No. of lines in input image
      integer   npix            ! No. of pixels in input image
      character*30 oname	! Name of HDS data type of output NDF
      integer	otype		! Index to HDS data type of output NDF
      integer	place		! NDF place holder for output NDF.
      character prbuf*80        ! Buffer for output to screen
      real      rval            ! Dummy real argument
      integer	start		! Index of start of file name.
      integer	status		! Inherited status value
      character title*30        ! Title for output image
      integer	ubnd(2)		! Upper pixel bounds of output NDF
      integer	ustrln		! Function giving used length of a string
      real*8    val_itod	! PRIMDAT type convertion function
      real*8    val_rtod	! PRIMDAT type convertion function

*
* INITIALIZE FORMAT DATA (SEE SUN4)
*
      data      ifmts/
     :                    101,
     :                    102,
     :                    104,
     :                    204,
     :                    208,
     :                    301,
     :                    302/,
     :          fmtnam/
     :                    'Signed byte',
     :                    'Signed word',
     :                    'Signed longword',
     :                    'Real',
     :                    'Double precision',
     :                    'Unsigned byte',
     :                    'Unsigned word'/,
     :          fmtspc/
     :                    'SB',
     :                    'SW',
     :                    'SL',
     :                    'R',
     :                    'DP',
     :                    'UB',
     :                    'UW'/

*
* CALL GTFMTS TO FIND OUT THE DATA TYPES CORRESPONDING TO THE
* INCARNATIONS OF THE INPUT DATA FRAME
*
      call gtfmts('INPUT',formts,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif

*
* GET INFORMATION DISPLAY LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)

*
* FIND OUT HOW MANY INCARNATIONS THERE ARE IN THE INPUT DATA FRAME
*
      nfmt=0
      do i=1,7
         if(formts(i)) nfmt=nfmt+1
      enddo

*
* IF THERE ARE MORE THAN 1 INCARNATION IN THE INPUT, THEN ASK THE
* USER WHICH INCARNATION IS TO BE USED FOR THE CONVERSION
*
      if(nfmt.gt.1) then

*
* UNLESS ILEVEL WAS GIVEN AS 1 DISPLAY OPTIONS ON SCREEN
*
         if(ilevel.gt.1) then
            call wruser(' ',ierr)
            call wruser('Incarnations of the input exist with the '//
     :                     'following data types:',ierr)
            call wruser(' ',ierr)
         endif
         alts=' '
         ialt=1
         do i=1,7
            if(formts(i)) then
               if(ilevel.gt.1) then
                  write(prbuf,10) fmtnam(i),fmtspc(i)
   10             format('   ',A17,' ( specify as "',A2,'" )')
                  call wruser(prbuf,ierr)
               endif
               alts(ialt:ialt+2)=fmtspc(i)//','
               ialt=ialt+3
            endif
         enddo
         alts(ialt-1:ialt-1)='.'

*
* ASK USER FOR REQUIRED FORMAT SPECIFIER
*
         if(ilevel.gt.1) call wruser(' ',ierr)
         itype=1
         call gtstrn('DTYPE',.true.,alts,1,itype,iname,nchar,ierr)
         if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif
         iorig=1
         do while(fmtspc(iorig).ne.iname.and.iorig.le.7)
            iorig=iorig+1
         enddo

*
* IF THERE WAS ONLY 1 INCARNATION IN THE INPUT, FIND THE INDEX OF ITS
* DATA TYPE
*
      else if(nfmt.eq.1) then
         iorig=0
         do i=1,7
            if(formts(i)) iorig=i
         enddo

*
* IF THERE WERE NO INCARNATIONS THEN GIVE ERROR MESSAGE AND STOP
*
      else
         if(iorig.eq.0) then
            call wrerr('NOINCARN')
            goto 999
         endif
      endif

*
* IF THE INPUT DATA TYPE IS BYTE, UNSIGNED BYTE OR UNSIGNED WORD, ABORT.
*
      if(iorig.eq.1.or.iorig.eq.6.or.iorig.eq.7) then
         call wrerr('BADTYPE')
         goto 999
      endif


*
* MAP THE INPUT FILE, USING THE REQUESTED DATA TYPE.
*
      call gt2dir('INPUT',ifmts(iorig),.false.,npix,nlin,ipin,ierr)
      if(ierr.ne.0) goto 999

*
* ATTEMPT TO READ CRDD DESCRIPTORS INTO COMMON
*
      call crdin('INPUT',band,frmid,ierr)

*
* SET A FLAG IF THE INPUT IS NOT A CRDD FILE, AND GET THE IRAS BAND 
* NUMBER FROM THE USER. 
*
      if(ierr.eq.0) then
         crdd = .true.

      else if(ierr.eq.11) then
         crdd = .false.
         call gtstrn('BAND',.false.,'1,2,3,4,12,25,60,100.',1,band,
     :               cval,nchar,ierr)
         band=mod(band-1,4)+1

      else
         goto 999
      endif
      
*
* GET THE HDS DATA TYPE FOR THE OUTPUT DATA_ARRAY. A RESTRICTED SET IS 
* AVAILABLE.
*
      otype=2
      call gtstrn('HDSTYPE',.true.,'DOUBLE,REAL,INTEGER,WORD.',1,
     :             otype,oname,nchar,ierr)
      oname='_'//oname

*
* GET THE NAME OF THE OUTPUT NDF STRUCTURE, USING A RUN TIME DEFAULT 
* EQUAL TO THE NAME OF THE INPUT BDF.
*
      call gtinam('INPUT',bdfnam,ierr)
      bdfnam(index(bdfnam,';'):) = ' '
      call chr_ucase(bdfnam)

      if(ierr.eq.0) then
         start=index(bdfnam,']')+1
         end=index(bdfnam(start:),'.')-1
         if(end.lt.0) end=ustrln(bdfnam(start:))
         ndffil=bdfnam(start:start+end-1)
         deflt=.true.

      else
         bdfnam=' '
         deflt=.false.
      endif

      call rdkeyc('OUTPUT',deflt,1,ndffil,ival,ierr)

*
* NDF STRUCTURES MUST BE CONTAINED WITHIN FILES WITH EXTENSION .SDF.
* REMOVE ANY USER SPECIFIED TYPE FOR THE OUTPUT.
*
      start=index(ndffil,']')+1
      end=index(ndffil(start:),'.')-1
      if(end.lt.0) end=ustrln(ndffil(start:))
      ndfnam=ndffil(start:start+end-1)
      ndffil=ndffil(:start+end-1)//'.SDF'
      call lbgone(ndffil)
      call lbgone(ndfnam)

*
* CREATE AN HDS CONTAINER FILE TO HOLD THE OUTPUT SDF FILE
*
      status=SAI__OK
      call hds_start(status)
      call hds_new(ndffil,ndfnam(:min(DAT__sznam,ustrln(ndfnam))),
     :             'NDF',0,0,iloc,status)

*
* SEE IF A SIMPLE OR PRIMITIVE NDF IS TO BE CREATED. DEFAULT IS SIMPLE 
* FOR CRDD FILES, PRIMATIVE FOR IMAGES (KAPPA CURRENTLY WILL ONLY
* ACCEPT PRIMATIVE NDFS).
*
      if( crdd ) then
         ndform=1
      else
         ndform=2
      endif

      call gtstrn('NDFFORM',.true.,'SIMPLE,PRIMITIVE.',1,ndform,form,
     :             nchar,ierr)

*
* GET AN NDF PLACE HOLDER FOR THE NDF AND THEN CREATE IT
*
      call ndf_place(iloc,'NDF',place,status)
      lbnd(1)=1
      lbnd(2)=1
      ubnd(1)=npix
      ubnd(2)=nlin
      if(ndform.eq.1) then
         call ndf_new(oname,2,lbnd,ubnd,place,indf,status)
      else
         call ndf_newp(oname,2,ubnd,place,indf,status)
      endif

*
* MAP THE NDF DATA_ARRAY AS A DOUBLE PRECISION ARRAY
*
      call ndf_map(indf,'DATA','_DOUBLE','WRITE',ipout,nel,status)

*
* CHECK THE NDF HAS BEEN CREATED OK
*
      if(status.ne.SAI__OK) then
         call err_flush(status)
         call wrerr('NOOUTPUT')
         goto 998
      endif

*
* PERFORM THE CONVERSION. NOTE, BAD PIXELS ARE CONVERTED SINCE THEY
* PROBABLY WILL NOT CONFORM TO THE ADAM VALUES. 
*
      if(iorig.eq.2) then
         call convw(%val(ipin),npix,nlin,1,npix,ubnd(1),
     :              %val(ipout),status)

      else if(iorig.eq.3) then
         call convi(%val(ipin),npix,nlin,1,npix,ubnd(1),
     :              %val(ipout),status)

      else if(iorig.eq.4) then
         call convr(%val(ipin),npix,nlin,1,npix,ubnd(1),
     :              %val(ipout),status)

      else if(iorig.eq.5) then
         call convd(%val(ipin),npix,nlin,1,npix,ubnd(1),
     :              %val(ipout),status)
      endif

*
* IF DEALING WITH AN IMAGE, SET UP DEFAULT FITS DESCRIPTORS AND THEN 
* GET VALUES FROM INPUT FRAME. MODIFY BSCALE AND BZERO TO PRODUCE OUTPUT
* UNITS OF MJy/Sr. IF THE REQUESTED DATA TYPE WAS SIGNED WORD, LOOK FOR
* THE DESCRIPTOR "INVAL". IF IT IS NOT FOUND, OR IF ANOTHER DATA TYPE 
* IS REQUESTED, LOOK FOR THE DESCRIPTOR "BLANK" INSTEAD.
*
      if(.not.crdd) then
         bscale=1.0
         bzero=0.0
         call gtdscr('INPUT','BSCALE','REAL',ival,bscale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,bzero,cval,ierr)
         bscale=bscale*1.0E-6
         bzero=bzero*1.0E-6

         magic=.false.
         if(iorig.eq.2) then
            call gtdscr('INPUT','INVAL','REAL',ival,blank,cval,ierr)
            if(ierr.eq.0) magic=.true.
         endif

         if(.not.magic) then
            call gtdscr('INPUT','BLANK','REAL',ival,blank,cval,ierr)
            if(ierr.eq.0) magic=.true.
         endif

*
* CONVERT THE BLANK VALUE TO DOUBLE PRECISION
*
         dblank=val_rtod(.false.,blank,status)

* 
* IF DEALING WITH CRDD, COPY BSCALE, BZERO AND BLANK FROM COMMON. MODIFY
* BSCALE AND BZERO TO TAKE ACCOUNT OF THE OCT 1984 CALIBRATION CHANGE, 
* AND TO PRODUCE OUTPUT VALUES IN UNITS OF 1.0E-12 OF A WATT PER SQUARE
* METRE.
*
      else
         magic = .true.
         dblank=val_itod(.false.,DS_bpx(frmid),status)
         bscale=DS_bsc(frmid)*DT_o84(band)*1.0E12
         bzero=DS_bze(frmid)*DT_o84(band)*1.0E12
      endif

*
* CALL SCALE TO APPLY THE SCALING TO THE INPUT DATA AND TO REPLACE
* BLANK VALUES WITH THE ADAM "MAGIC" VALUES.
*
      call scale(%val(ipout),nel,bscale,bzero,dblank,magic,nbad)

      call wruser(' ',ierr)

      if(nbad.eq.nel) then
         call wrerr('NONEVAL')
         goto 998

      else if(nbad.eq.1) then
         if(ilevel.ge.2) then
            call wruser('   Output NDF contains 1 bad data value',ierr)
         endif

      else
         if(ilevel.ge.2) then
            write(prbuf,20) nbad
  20        format('   Output NDF contains ',I9,' bad data values')
            call lbgone(prbuf(24:))
            call wruser(prbuf,ierr)
         endif

      endif

      call wruser(' ',ierr)

*
* IF A SIMPLE NDF IS BEING CREATED, SET THE BAD PIXELS FLAG IN THE 
* OUTPUT NDF
*
      if(ndform.eq.1) then
         if(nbad.eq.0) then
            call ndf_sbad(.false.,indf,'DATA',status)
         else
            call ndf_sbad(.true.,indf,'DATA',status)
         endif
      endif

*
* GET "TITLE" OR "OBJECT" DEPENDING ON DATA TYPE.
*
      if(.not.crdd) then
         title=' '
         if(iorig.eq.2) then
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title,
     :                   ierr)
         endif

         if(ustrln(title).eq.0) then
            call gtdscr('INPUT','OBJECT','CHARACTER',ival,rval,title,
     :                   ierr)
         endif

      else
         title=DS_obj(frmid)
      end if

*
* IF NO TITLE WAS FOUND USE THE NAME OF THE INPUT BDF IMAGE.
*
      if(ustrln(title).eq.0) title=bdfnam
      if(ustrln(title).eq.0) title='Output from EDRSX:NDFOUT'

*
* STORE THE TITLE IN THE OUTPUT NDF
*
      call ndf_cput(title(:ustrln(title)),indf,'TITLE',status)

*
* CREATE THE "UNITS" NDF COMPONENT AND THEN CALL A ROUTINE TO CREATE
* THE EXTENSION AND STORE INFORMATION FROM THE INPUT. 
*
      if(crdd) then
         call ndf_cput('pW/(M**2)',indf,'UNITS',status)
         call crdout(indf,band,frmid,status)

      else
         call ndf_cput('MJy/sr',indf,'UNITS',status)
         call imgout('INPUT',indf,band,status)
      endif

*
* PUT SOME HISTORY IN THE OUTPUT NDF.
*
      call hist(indf,'EDRSX:NDFOUT',1,ndffil(:start+end+3)//
     :          ' created from '//bdfnam,status)

*
* ANNUL THE NDF LOCATOR.
*
  998 call ndf_annul(indf,status)

*
* CHECK THAT THE NDF STORAGE FORM IS STILL THAT WHICH WAS REQUESTED BY 
* THE USER (THE NDF SYSTEM SOMETIMES DOES IMPLICIT FORM CONVERSION).
*
      call ndf_find(iloc,'NDF',indf,status)
      call ndf_form(indf,'DATA',form,status)
      call ndf_annul(indf,status)

      if(form.eq.'SIMPLE'.and.ndform.ne.1.or.
     :   form.ne.'SIMPLE'.and.ndform.eq.1) then
         call wruser('WARNING - The output NDF storage form is '//form,
     :                ierr)
      endif

*
* COPY THE NDF TO THE TOP LEVEL OF THE HDS STRUCTURE
*
      call dat_find(iloc,'NDF',jloc,status)
      call movlev(jloc,iloc,status)

*
* CLOSE DOWN HDS
*
      call hds_close(iloc,status)
      call hds_stop(status)

*
* FLUSH ANY ERROR.
*
      if(status.ne.sai__OK) call err_flush(status)

*
* FINISH
*
  999 call frdata(' ',ierr)

      end
