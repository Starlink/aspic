      subroutine imgout(inpar,indf,band,status)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Create an IRAS extension to describe an IRAS image held in 
*       the NDFs DATA array, and assign a value to the NDF component
*       "LABEL".
*
*SOURCE
*       IMGOUT.FOR in NDFOUT.TLB
*
*METHOD
*       An object called "IMAGE_INFO" is created within the IRAS 
*       extension. This object holds the components; 
*       o  INSTRUMENT (_CHAR)
*       o  BAND       (_INTEGER)
*       o  TYPE       (_CHAR)
*       o  FIELDLON   (_DOUBLE)
*       o  FIELDLAT   (_DOUBLE)
*       o  FIELDSCS   (_CHAR)
*
*       The IMAGE_INFO component TYPE is given the value UNKNOWN.
*       The type of IRAS image is determined by looking at the BDF
*       descriptor INSTRUME. This follows the method of IPMAF
*       routine STORE, in which strings are searched for within 
*       INSTRUME as follows:
*
*       MEMCRDD          -  Output from MEMCRDD.
*       CPC              -  Chopped Photometric Channel map.
*       SUR or DEEP      -  Deep Sky image (=Pointed Observation?)
*       COMBINED         -  Combined image.
*       IRAS and SPL     -  Spline map.
*       IRAS but not SPL -  Skyflux map.
*       GALPL            -  Galactic plane map.
*
*       The image type is then stored in a component of the IRAS 
*       extension called IMAGE_TYPE. It can take any of the values; CPC,
*       DEEP SKY, COMBINED, SPLINE, SKYFLUX, GALACTIC PLANE, MEMCRDD, 
*       UNKNOWN.
*
*       The projection, and sky coordinate system produced by the 
*       projection are also determined by the INSTRUME descriptor.
*       All maps have Gnomonic projections and Equatorial(1950) 
*       coordinates, except as follows:
*
*       o  DEEP SKY maps have orthographic projections.
*       o  GALACTIC PLANE maps have Lambert equivalent cylindrical 
*          projections, and Galactic coordinates.
*       o  COMBINED images, for which INSTRUME contains the string 
*          "GAL" have Galactic coordinates.
*
*       An IRAS90 astrometry structure is then produced using the IRA 
*       package. The NDF component "LABEL" is assigned a value of
*       "Surface brightness".
*       
*ARGUMENTS       
*   INPUT:
*       inpar   character       Name of INTERIM parameter associated 
*                               with the input image.
*       indf    integer         NDF identifier for the output NDF.
*	status	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       status  integer         Exit status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              rdimds,wrerr
*       IRAS90 (IRA):
*              ira_init,ira_creat,ira_close
*       NDF:
*              ndf_xnew
*       HDS:
*              dat_new0c,dat_annul,cmp_put0c
*
*STARLINK PARAMETERS
*       'inpar'(Read)  The parameter associated with the input BDF.
*        NOCOMB(Error) Accessed if the input image claims to be a 
*                      COMBINED image, but CROTA is not zero.
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/2/91
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE STARLINK ADAM ENVIRONMENT CONSTANTS.
*
      include 'SAE_PAR'

*
* INCLUDE IRAS90 AND IRA PACKAGE CONSTANTS.
*
      include 'I90_PAR'
      include 'IRA_PAR'

*
* DECLARE ARGUMENTS
*
      integer	band,indf,status
      character inpar*(*)

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
      integer   ida     ! IRA identifier.
      character iiloc*(dat__szloc)! HDS locator to the IMAGE_INFO object.
      character instrm*15! INSTRUME descriptor value.

      integer	ierr	! Temporary status value
      character itype*14! Input image type.
      double precision p(8)! Projection parameter values.
      character proj*(ira__szprj)! Projection name.
      character scs*(ira__szscs) ! Sky coordinate system.
      character xloc*(dat__szloc) ! HDS locator for IRAS extension.

*
* CHECK INHERITED STATUS
*
      if(status.ne.SAI__OK) goto 999

*
* GET THE NECESSARY DESCRIPTORS FROM THE INPUT IRAS IMAGE
*
      call rdimds(inpar,.false.,crval1,crval2,crpix1,crpix2,cdelt1,
     :             cdelt2,crota1,instrm,ierr)
      if(ierr.eq.-1) call wrerr('NOCOMB')
      if(ierr.ne.0) then
         status=SAI__ERROR
         goto 999
      endif

*
* FIND WHAT SORT OF IMAGE IT IS BY LOOKING AT THE INSTRUME DESCRIPTOR.
*
      if(index(instrm,'CPC').ne.0) then
         itype='CPC'

      else if(index(instrm,'SUR').ne.0 .or.
     :        index(instrm,'DEEP').ne.0 ) then
         itype='DEEP SKY'

      else if(index(instrm,'COMBINED').ne.0 ) then
         itype='COMBINED'

      else if(index(instrm,'GALPL').ne.0 ) then
         itype='GALACTIC PLANE'

      else if(index(instrm,'IRAS').ne.0 ) then
         if( index(instrm,'SPL').ne.0 ) then
            itype='SPLINE '
         else
            itype='SKYFLUX'
         endif

      else if(index(instrm,'MEMCRDD').ne.0 ) then
         itype='MEMCRDD'

      else      
         itype = 'UNKNOWN'
      endif

*
* SELECT THE PROJECTION AND SKY COORDINATE SYSTEM ON THE BASIS OF THE 
* INSTRUME DESCRIPTOR (DEFAULT SCS IS "EQUATORIAL(1950)" AND DEFAULT 
* PROJECTION IS "GNOMONIC"). THESE DESIGNATIONS ARE DERIVED FROM THE
* IPMAF ROUTINES "STORE" AND "IJTOEQ".
*
      scs = 'EQUATORIAL(1950)'
      proj='GNOMONIC'

*
* COMBINED IMAGES MAY BE IN EITHER EQUATORIAL OR GALACTIC COORDS, CHECK 
* FOR COORD LABEL 'GAL' WHICH INDICATES AN IMAGE IN GALACTIC COORDS, 
* OTHERWISE EQUATORIAL COORDS ARE ASSUMED
*
      if(itype.eq.'COMBINED') then
         if(index(instrm,'GAL').ne.0) scs='GALACTIC'

*
* POINTED OBSERVATIONS HAVE AN ORTHOGRAPHIC PROJECTION.
*
      else if(itype.eq.'DEEP SKY') then
         proj='ORTHOGRAPHIC'

*
* GALACTIC PLANE IMAGES HAVE LAMBERT EQUIVALENT CYLINDRICAL 
* PROJECTIONS, AND GALACTIC SKY COORDINATES.
*           
      else if(itype.eq.'GALACTIC PLANE') then
         proj='LAMBERT'
         scs='GALACTIC'
      endif

*
* INITIALISE THE IRA ASTROMETRY PACKAGE.
*
      call ira_init(status)      

*
* CREATE THE IRAS EXTENSION.
*
      call ndf_xnew(indf,'IRAS','IRAS',0,0,xloc,status)

* 
* SET UP THE ARRAY OF PROJECTION PARAMETER VALUES, AND CREATE THE 
* ASTROMETRY STRUCTURE IN THE OUTPUT NDF. (ASSUME THE REFERENCE POINT
* IS AT THE *CENTRE* OF THE REFERENCE PIXEL).
*
      p(1)=crval1*ira__dtor
      p(2)=crval2*ira__dtor
      p(3)=real(crpix1)-0.5
      p(4)=real(crpix2)-0.5
      p(5)=cdelt1*ira__dtor
      p(6)=cdelt2*ira__dtor
      p(7)=(180.0-crota1)*ira__dtor
      p(8)=0.0

      call ira_creat( proj, 8, p, scs, ira__irjep, indf, ida, status )

*
* CLOSE DOWN THE IRA PACKAGE.
*
      call ira_close(status)

*
* CREATE THE IMAGE_INFO OBJECT.
*
      call dat_new(xloc,'IMAGE_INFO','IMAGE_INFO',0,0,status)
      call dat_find(xloc,'IMAGE_INFO',iiloc,status)

*
* CREATE A COMPONENT CALLED "INSTRUMENT" TO INDICATE THAT THIS IS
* DATA FROM THE SURVEY ARRAY OR THE CPC.
*
      call dat_new0c(iiloc,'INSTRUMENT',6,status)
      if( itype .eq. 'CPC' ) THEN
         call cmp_put0c(iiloc,'INSTRUMENT','CPC',status)
      else
         call cmp_put0c(iiloc,'INSTRUMENT','SURVEY',status)
      end if

*
* WRITE THE IRAS BAND NUMBER TO A COMPONENT OF IMAGE_INFO CALLED "BAND".
*
      call dat_new0i(iiloc,'BAND',status)
      call cmp_put0i(iiloc,'BAND',band,status)

*
* STORE THE VALUE "UNKNOWN" AS THE IRAS90 IMAGE TYPE.
*
      call dat_new0c(iiloc,'TYPE',7,status)
      call cmp_put0c(iiloc,'TYPE','UNKNOWN',status)

* 
*  STORE THE REFERENCE POINT FOR THE FIELD.
*
      call dat_new0d(iiloc,'FIELDLON',status)
      call cmp_put0d(iiloc,'FIELDLON',p(1),status)

      call dat_new0d(iiloc,'FIELDLAT',status)
      call cmp_put0d(iiloc,'FIELDLAT',p(2),status)

      call dat_new0c(iiloc,'FIELDSCS',ira__szscs,status)
      call cmp_put0c(iiloc,'FIELDSCS',scs,status)

*
* WRITE THE IMAGE TYPE TO A COMPONENT OF THE IRAS EXTENSION CALLED 
* "IMAGE_TYPE"
*
      call dat_new0c(xloc,'IMAGE_TYPE',14,status)
      call cmp_put0c(xloc,'IMAGE_TYPE',itype,status)

*
* ANNUL THE LOCATORS.
*
      call dat_annul(iiloc,status)
      call dat_annul(xloc,status)

*
* SET THE NDF "LABEL" COMPONENT.
*
      call ndf_cput('Surface brightness',indf,'LABEL',status)

*
* FINISH
*
  999 continue

      end
