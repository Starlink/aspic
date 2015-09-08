      subroutine crdout(indf,band,frmid,status)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Create an IRAS extension to describe an IRAS CRDD file, and 
*       assign a value to the NDF component "LABEL".
*
*SOURCE
*       CRDOUT.FOR in NDFOUT.TLB
*
*METHOD
*       The NDF component "LABEL" is given the value "Survey CRDD".
*
*       The IRAS extension is created holding a single component called
*       CRDD_INFO. This object holds the following components (see 
*       IRAS90 document ID/1 for more information about these):
*       o  DET_NUMBERS  - The size of this array is the same as the 
*             size of the 2nd dimension of the DATA array. Note, this 
*             is not in general the same as the upper limit of the DATA
*             array, but in this application it is, because the lower 
*             limit of the DATA array is always 1.
*       o  DET_ORIGIN - Set to zero.
*       o  BAND 
*       o  REF_RA - This is taken from the RA-REQ INTERIM descriptor. 
*       o  REF_DEC - This is taken from the DEC-REQ INTERIM descriptor.
*       o  NOM_SPEED - Always set to the radians per second equivalent
*             of 3.85 arc-minutes per second. 
*       o  SOP
*       o  OBS (NB, this is set to zero since BDF CRDD files don't
*          contain any record of the Observation number).
*       o  DETAILS - This is a structure of HDS type SURVEY_BSIGHT 
*             containing details of the boresight pointing data as 
*             follows:
*             o  BASE_UTCS [_DOUBLE] - The UTCS of the "base" data 
*                   sample. This will be sample number 1. It may be 
*                   advantageous to add another component (BASE_SAMP)
*                   to specify the base sample in order to allow 
*                   for situations where sample number 1 does not exist.
*                   In the mean time the IRC package assume a base 
*                   sample number of 1. This could easily be changed if
*                   necessary.
*             o  BASE_MJD [_DOUBLE] - The  Modified Julian Date of the 
*                   base sample.
*             o  BORE_POSNS [_INTEGER] - The number of boresight 
*                   positions stored in the following arrays.
*             o  UTCS_OFFSET(BORE_POSNS) [_REAL] - The difference in 
*                   UTCS between each boresight position and the base 
*                   sample.
*             o  PSI(BORE_POSNS) [_REAL] - The clock angle (see Exp. 
*                   Supp. fig III.B.7) at each boresight position, in
*                   radians. Note, this is PSI, not PHI.
*             o  THETA(BORE_POSNS) [_REAL] - The cone angle (see Exp. 
*                   Supp. fig III.B.7) at each boresight position, in
*                   radians.
*             o  LAMBDA_SUN(BORE_POSNS) [_REAL] - The ecliptic longitude
*                   (1950) of the sun, at each boresight position in 
*                   radians.
*             o  LAMBDA(BORE_POSNS) [_REAL] - The ecliptic longitude
*                   (1950) of the boresight, at each boresight position
*                   in radians.
*             o  BETA(BORE_POSNS) [_REAL] - The ecliptic latitude
*                   (1950) of the boresight, at each boresight position
*                   in radians.
*
*ARGUMENTS       
*   INPUT:
*       indf    integer         NDF identifier for the output NDF.
*       band    integer         IRAS band number, 1-4.
*       frmid   integer         Frame which holds the CRDD descriptors.
*	status	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       status  integer         Exit status: 0 - success
*
*SUBROUTINES CALLED
*       NDF:
*              ndf_xnew,ndf_cput
*       HDS:
*              dat_new,dat_find,dat_new1i,dat_new1r,dat_new0i,dat_new0r,
*              dat_new0d,dat_annul,cmp_put1i,cmp_put0i,cmp_put0r,
*              cmp_put0d,cmp_put1r
*       IRA:
*              ira_ctor
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/3/91
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
* INCLUDE DESCRIPTOR VALUES COMMON BLOCK, AND DETECTOR DATA.
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include 'UTILITIES(DS_COM)'

*
* DECLARE ARGUMENTS
*
      integer	band,indf,status,frmid

*
* DECLARE LOCAL VARIABLES
*
      character ciloc*(DAT__SZLOC)! Locator to the CRDD_INFO object.
      real*8    decref            ! DEC of the reference point, in rads.
      character dloc*(DAT__SZLOC) ! Locator to the DETAILS object.
      integer   i                 ! Loop count.
      integer	ierr	          ! Temporary status value
      double precision mjd        ! Modified Julian Date at first data 
                                  ! sample in the output NDF.
      real      radval(DS_mxb)    ! Radian equivalent to an angle in 
                                  ! degrees.
      real*8    raref             ! RA of the reference point, in rads.
      character xloc*(dat__szloc) ! HDS locator for IRAS extension.

*
* CHECK INHERITED STATUS
*
      if(status.ne.SAI__OK) goto 999

*
* CREATE THE IRAS EXTENSION.
*
      call ndf_xnew(indf,'IRAS','IRAS',0,0,xloc,status)

*
* CREATE AND FIND THE CRDD_INFO OBJECT.
*
      call dat_new(xloc,'CRDD_INFO','CRDD_INFO',0,0,status)
      call dat_find(xloc,'CRDD_INFO',ciloc,status)

*
* REMOVE APOSTROPHIES FROM STRINGS HOLDING RA AND DEC OF REFERENCE POINT
*
      do i=1,12
         if(DS_rar(frmid)(i:i).eq.'''') DS_rar(frmid)(i:i)=' '
         if(DS_dcr(frmid)(i:i).eq.'''') DS_dcr(frmid)(i:i)=' '
      enddo

*
* CONVERT THE RA AND DEC OF THE REFERENCE POINT FROM FORMATTED
* STRINGS TO ANGLES IN RADIANS.
*
      call ira_ctod(DS_rar(frmid),DS_dcr(frmid),'EQUATORIAL(B1950)',
     :              raref,decref, status )

*
* CREATE THE REF_RA COMPONENT.
*
      call dat_new0d(ciloc,'REF_RA',status)
      call cmp_put0d(ciloc,'REF_RA',raref,status)

*
* CREATE THE REF_DEC COMPONENT.
*
      call dat_new0d(ciloc,'REF_DEC',status)
      call cmp_put0d(ciloc,'REF_DEC',decref,status)

*
* CREATE THE DET_NUMBERS COMPONENT.
*
      call dat_new1i(ciloc,'DET_NUMBERS',DS_nde(frmid),status)
      call cmp_put1i(ciloc,'DET_NUMBERS',DS_nde(frmid),DT_bal(1,band),
     :               status)

*
* CREATE THE DET_ORIGIN COMPONENT.
*
      call dat_new0i(ciloc,'DET_ORIGIN',status)
      call cmp_put0i(ciloc,'DET_ORIGIN',0,status)

*
* CREATE THE BAND COMPONENT.
*
      call dat_new0i(ciloc,'BAND',status)
      call cmp_put0i(ciloc,'BAND',band,status)

*
* CREATE THE NOM_SPEED COMPONENT (RADS/SEC EQUIVALENT OF 3.85 
* ARC-MINS/SEC )
*
      call dat_new0r(ciloc,'NOM_SPEED',status)
      call cmp_put0r(ciloc,'NOM_SPEED',1.1199E-3,status)

*
* CREATE THE SOP COMPONENT.
*
      call dat_new0i(ciloc,'SOP',status)
      call cmp_put0i(ciloc,'SOP',DS_sop(frmid),status)

*
* CREATE THE OBS COMPONENT.
*
      call dat_new0i(ciloc,'OBS',status)
      call cmp_put0i(ciloc,'OBS',0,status)

*
* CREATE THE DETAILS COMPONENT WITH HDS TYPE "SURVEY_BSIGHT", THEN FIND
* IT.
*
      call dat_new(ciloc,'DETAILS','SURVEY_BSIGHT',0,0,status)
      call dat_find(ciloc,'DETAILS',dloc,status)

*
* CREATE COMPONENT "BASE_UTCS" (THE UTCS AT OUTPUT DATA SAMPLE NUMBER 
* 1)
*
      call dat_new0d(dloc,'BASE_UTCS',status)
      call cmp_put0d(dloc,'BASE_UTCS',DS_ust(frmid),status)

*
* CREATE COMPONENT "BASE_MJD" (THE MODIFIED JULIAN DATE AT SAMPLE 
* NUMBER 1). NOTE, THE EXPRESSION FOR CONVERTING UTC TO MJD IS BASED
* ON ZERO UTCS = 00:00 1-JAN-1981. THE VERSION USED IN PREVIOUS INTERIM
* SOFTWARE WAS BASED ON ZERO UTCS = 12:00 31-DEC-1980.
*
      mjd = 4.4605D4+DS_ust(frmid)/8.64D4
      call dat_new0d(dloc,'BASE_MJD',status)
      call cmp_put0d(dloc,'BASE_MJD',mjd,status)

*
* CREATE COMPONENT BORE_POSNS (NO. OF BORESIGHT POSITIONS)
*
      call dat_new0i(dloc,'BORE_POSNS',status)
      call cmp_put0i(dloc,'BORE_POSNS',DS_bsa(frmid),status)

*
* CREATE COMPONENT "UTCS_OFFSET" (THE TIME SINCE THE BASE UTCS VALUE).
*
      call dat_new1r(dloc,'UTCS_OFFSET',DS_bsa(frmid),status)
      call cmp_put1r(dloc,'UTCS_OFFSET',DS_bsa(frmid),DS_but(1,frmid),
     :               status)

*
* CREATE COMPONENT "PSI" (THE CLOCK ANGLE) IN RADIANS.
*
      call dat_new1r(dloc,'PSI',DS_bsa(frmid),status)

      do i=1,DS_bsa(frmid)
         radval(i)=DS_psi(i,frmid)*IRA__DTOR
      enddo

      call cmp_put1r(dloc,'PSI',DS_bsa(frmid),radval,status)

*
* CREATE COMPONENT "THETA" (THE CONE ANGLE) IN RADIANS.
*
      call dat_new1r(dloc,'THETA',DS_bsa(frmid),status)

      do i=1,DS_bsa(frmid)
         radval(i)=DS_the(i,frmid)*IRA__DTOR
      enddo

      call cmp_put1r(dloc,'THETA',DS_bsa(frmid),radval,status)

*
* CREATE COMPONENT "LAMBDA_SUN" (THE ECLIPTIC LONGITUDE (1950) OF THE
* SUN) IN RADIANS.
*
      call dat_new1r(dloc,'LAMBDA_SUN',DS_bsa(frmid),status)

      do i=1,DS_bsa(frmid)
         radval(i)=DS_sol(i,frmid)*IRA__DTOR
      enddo

      call cmp_put1r(dloc,'LAMBDA_SUN',DS_bsa(frmid),radval,status)

*
* CREATE COMPONENT "LAMBDA" (THE ECLIPTIC LONGITUDE (1950) OF THE
* BORESIGHT) IN RADIANS.
*
      call dat_new1r(dloc,'LAMBDA',DS_bsa(frmid),status)

      do i=1,DS_bsa(frmid)
         radval(i)=DS_lam(i,frmid)*IRA__DTOR
      enddo

      call cmp_put1r(dloc,'LAMBDA',DS_bsa(frmid),radval,status)

*
* CREATE COMPONENT "BETA" (THE ECLIPTIC LATITUDE (1950) OF THE
* BORESIGHT) IN RADIANS.
*
      call dat_new1r(dloc,'BETA',DS_bsa(frmid),status)

      do i=1,DS_bsa(frmid)
         radval(i)=DS_bet(i,frmid)*IRA__DTOR
      enddo

      call cmp_put1r(dloc,'BETA',DS_bsa(frmid),radval,status)

*
* ANNUL THE LOCATORS
*
      call dat_annul(dloc,status)
      call dat_annul(ciloc,status)
      call dat_annul(xloc,status)

*
* SET THE STANDARD NDF COMPONENT "LABEL" TO BE "Survey CRDD"
*
      call ndf_cput('Survey CRDD',indf,'LABEL',status)

*
* FINISH
*
  999 if(ierr.ne.0) status =sai__error

      end
