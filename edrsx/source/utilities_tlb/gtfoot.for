      subroutine gtfoot(name,deflt,band,frmid,iptr,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Aquire an AO footprints file in DSBs INTERIM format.
*
*SOURCE
*       GTFOOT.FOR in UTILITIES.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUTS:
*   OUTPUTS:
*       ierr              integer              Error status
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/6/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCCK HOLDING DESCRIPTOR VALUES ASSOCIATED WITH EACH
* AO FOOTPRINT FILE (VARIABLES CALLED AO_xxx)'
*
      include 'UTILITIES(AO_COM)'

*
* DECLARE ARGUMENTS
*
      integer	frmid,band,iptr,ierr
      logical	deflt
      character	name*(*)
      
*
* DECLARE LOCAL VARIABLES
*
      character	cval*1	 ! Dummy character argument
      integer	iptemp(3)! Temporary storage for pointers to each plane
			 ! of AO data stack
      integer	ival	 ! Dummy integer argument
      integer	nim      ! No. of planes in AO data stack (should be 3)
      real	rval	 ! Dummy real argument

*
* SET DESCRIPTOR LEVEL TO ONE HIGHER THAN PREVIOUS TOP LEVEL
*
      frmid=AO_top+1
      ierr=0
      if(frmid.gt.AO_mxf) then
         call wrerr('DESCFULL')
         ierr=18
         goto 999
      endif

*
* MAP THE 3D AO FOOTPRINT DATA FILE
*
      call gt3dir(name,104,deflt,AO_nys(frmid),AO_nde(frmid),nim,
     :            iptemp,ierr)
      if(ierr.ne.0) then
         if(ierr.ne.1.or..not.deflt) then
            call wrerr('TOOBAD')
            ierr=16
         endif
         goto 999
      else
         iptr=iptemp(1)
      endif

*
* CHECK THAT THE DATA STACK HAS THE CORRECT NO. OF PLANES (3)
*
      if(nim.ne.3) then
         call wrerr('NOTAO')
         ierr=11
         goto 999
      endif      

*
* GET BAND NUMBER AND CHECK IT IS VALID
*
      call gtdscr(name,'BAND','INTEGER',band,rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BAND')
         ierr=20
         goto 999
      endif
*
      if(band.lt.1.or.band.gt.4) then
         call wrerr('INVBAND')
         ierr=21
         goto 999
      endif

*
* READ IN ALL OTHER ASSOCIATED DESCRIPTORS
*
      call gtdscr(name,'BLANK','INTEGER',AO_bpx(frmid),rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BLANK')
         ierr=22
         goto 999
      endif

      call gtdscr(name,'BSCALE','REAL',ival,AO_bsc(frmid),cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BSCALE')
         ierr=23
         goto 999
      endif

      call gtdscr(name,'BZERO','REAL',ival,AO_bze(frmid),cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BZERO')
         ierr=24
         goto 999
      endif

      call gtdscr(name,'LEG','INTEGER',AO_leg(frmid),rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('LEG')
         ierr=25
         goto 999
      endif

      call gtdscr(name,'SOP','INTEGER',AO_sop(frmid),rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('SOP')
         ierr=26
         goto 999
      endif

      call gtdscr(name,'OBS','INTEGER',AO_obs(frmid),rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('OBS')
         ierr=27
         goto 999
      endif

      call gtdscr(name,'OBJECT','CHARACTER',ival,rval,AO_obj(frmid),
     :            ierr)
      if(ierr.ne.0) then
         call ptderr('OBJECT')
         ierr=28
         goto 999
      endif

      call gtdscr(name,'DIRN','CHARACTER',ival,rval,AO_dir(frmid),
     :            ierr)
      if(ierr.ne.0) then
         call ptderr('DIRN')
         ierr=29
         goto 999
      endif

      call gtdscr(name,'REF_RA','REAL',ival,AO_ra(frmid),cval,ierr)
      if(ierr.ne.0) then
         call ptderr('REF_RA')
         ierr=30
         goto 999
      endif

      call gtdscr(name,'REF_DEC','REAL',ival,AO_dec(frmid),cval,ierr)
      if(ierr.ne.0) then
         call ptderr('REF_DEC')
         ierr=31
         goto 999
      endif

      call gtdscr(name,'SCAN_ANG','REAL',ival,AO_ang(frmid),cval,ierr)
      if(ierr.ne.0) then
         call ptderr('SCAN_ANG')
         ierr=32
         goto 999
      endif

*
* INCREMENT POINTER TO NEXT FREE DESCRIPTOR SLOTS
*
      AO_top=AO_top+1

*
* FINISH
*
  999 continue

      end
