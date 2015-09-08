      subroutine boresp(frame,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets up common block /BORE_SP/ to hold cubic spline parameters
*       for the fit between time( independant variable) and the 
*	following boresight descriptors (dependant variable): 
*	1) solar longitude (SOLONG) 
*	2) clock angle (PSI)
*       3) sun angle (THETA) 
*
*SOURCE
*       BORESP.FOR in UTILITIES.TLB
*
*METHOD
*       The time (UTC) at which each boresight sample was taken
*       is read from the CRDD descriptors, and a NAG routine used to
*       find the cubic spline fit between the UTC values and
*       the corresponding SOLONG, PSI and THETA values. The parameters
*       describing the fits are stored in the common block /BORE_SP/.
*          The values are used to find the solar longitude, clock angle
*       and sun angle at any time during the scan in order to determine
*       the RA and DEC of the boresight. This routine is a succesor to 
*	routine BOREST which used least squares linear fits to SOLANG
*	and PSI (THETA was assumed constant), as also done in IPMAF
*       program I_CRDIMAGE.
*
*ARGUMENTS
*   INPUTS:
*       frame   integer         The depth at which the crdd descriptors
*                               are stored in the descriptor arrays in
*                               common.
*   OUTPUTS:
*       ierr    integer         Status: 0 - Sucess
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wrerr,gtwork
*	INTERIM:
*	       frdata
*       NAG:
*              e01baf
*
*STARLINK PARAMETERS
*       FEWBORE/error/  Accessed if the CRDD file has too few boresight
*                       samples for a straight line to be fitted
*	NONMON/error/   Accessed if the UTC values given to the NAG 
*			routine are not monotonic
*
*VAX SPECIFICS
*       implicit none
*	%val
*       REAL*8
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/11/89
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE COMMON BLOCK HOLDING CRDD DESCRIPTOR VALUES (VARIABLES
* CALLED DS_xxx)
*
      include 'UTILITIES(DS_COM)'

*
* DECLARE ARGUMENTS
*
      integer   frame,ierr

*
* DECLARE LOCAL VARIABLES
*
      real*8    BS_cac(DS_mxb+4)! Spline cooeficients of clock angle fit
      real*8    BS_cak(DS_mxb+4)! Spline knots for clock angle fit
      real*8    BS_slc(DS_mxb+4)! Spline cooeficients of solar long. fit
      real*8    BS_slk(DS_mxb+4)! Spline knots for solar long. fit
      real*8    BS_thc(DS_mxb+4)! Spline cooeficients of sun angle fit
      real*8    BS_thk(DS_mxb+4)! Spline knots for sun angle fit
      integer   i               ! Boresight sample counter
      integer	ifail		! NAG error status
      integer	iplw		! Pointer to NAG work space
      integer	lw		! Length of NAG work space
      integer	m		! No. of boresight samples to be fitted
      real*8    xval(DS_mxb)    ! Independant variables values
      real*8    yval(DS_mxb)    ! Dependant variables values


*
* COMMON BLOCK /BORE_SP/ HOLDS THE CALCULATED VALUES 
*
      common /BORE_SP/ BS_cac,BS_cak,BS_slc,BS_slk,BS_thc,BS_thk

*
* GET DOUBLE PRECISION WORKSPACE FOR USE IN NAG ROUTINES
*
      m=DS_bsa(frame)
      lw=6*m+16
      call gtwork('NAGW1','DOUBLE',lw,iplw,ierr)
      if(ierr.ne.0) goto 999

*
* CONVERT BORESIGHT SAMPLE TIMES AND SOLAR LONGITUDE SAMPLES TO
* DOUBLE PRECISION
*
      do i=1,m
         xval(i)=dble(DS_but(i,frame))
         yval(i)=dble(DS_sol(i,frame))
      enddo

*
* FIT CUBIC SPLINE TO SOLAR LONGITUDE SAMPLES. NB, THIS FIT WILL GIVE
* VALUES IN DEGREES (NOT RADIANS) SINCE THE BORESIGHT DATA CONTAINS
* VALUES IN DEGREES.
*
      ifail=1
      call e01baf(m,xval,yval,BS_slk,BS_slc,m+4,%val(iplw),lw,ifail)
      if(ifail.ne.0) goto 998

*
* CONVERT SATALLITE CLOCK ANGLE TO DOUBLE PRECISION
*
      do i=1,m

*
* CHECK FOR SCAN PASSING THROUGHT ZERO CLOCK ANGLE. IF SO, THEN SUBTRACT
* 360 DEGREES FROM ALL SUBSEQUENT VALUES
*
         if(i.gt.1) then
            if(DS_psi(i,frame)-DS_psi(i-1,frame).gt.180.0) then
               DS_psi(i,frame)=DS_psi(i,frame)-360.0
            endif
         endif

         yval(i)=dble(DS_psi(i,frame))

      enddo

*
* FIT CUBIC SPLINE TO CLOCK ANGLE SAMPLES
*
      call e01baf(m,xval,yval,BS_cak,BS_cac,m+4,%val(iplw),lw,ifail)
      if(ifail.ne.0) goto 998

*
* CONVERT SOLAR ASPECT ANGLE TO DOUBLE PRECISION
*
      do i=1,m
         yval(i)=dble(DS_the(i,frame))
      enddo

*
* FIT CUBIC SPLINE TO SOLAR ASPECT SAMPLES
*
      call e01baf(m,xval,yval,BS_thk,BS_thc,m+4,%val(iplw),lw,ifail)
      
*
* CHECK NAG ERROR STATUS
*
  998 continue

      ierr=ifail
      if(ifail.eq.1) then
         call wrerr('FEWBORE')
         
      else if(ifail.eq.2) then
         call wrerr('NONMON')

      endif

*
* FINISH
*
  999 call frdata('NAGW1',ifail)

      end
