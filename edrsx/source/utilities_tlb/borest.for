      subroutine borest(frame,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets up common block /BORE/ to hold the gradient and intercepts
*       of the least squares linear fit between time( independant
*       variable) and the following boresight descriptors (dependant
*       variable): solar longitude (SOLONG) , clock angle (PSI).
*       The sun angle (theta) (assumed constant over the scan length)
*       is also stored in the common block.
*
*SOURCE
*       BOREST.FOR in UTILITIES.TLB
*
*METHOD
*       The time (UTC) at which each boresight sample was taken
*       is read from the CRDD descriptors, and a NAG routine used to
*       find the least squares linear fit between the UTC values and
*       the corresponding SOLONG values and PSI values. The calculated
*       gradient and intercepts are stored in the common block /BORE/.
*          The values are used to find the solar longitude, clock angle
*       and sun angle at any time during the scan in order to determine
*       the RA and DEC of the boresight. The assumption of a linear form
*       for these quantities is used because it is also used in IPMAF
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
*              wrerr
*       NAG:
*              g02caf
*
*STARLINK PARAMETERS
*       FEWBORE/error/  Accessed if the CRDD file has too few boresight
*                       samples for a straight line to be fitted
*
*VAX SPECIFICS
*       implicit none
*       REAL*8
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/8/88
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
      real*8    BS_cac          ! Constant of linear fit to clock angle
      real*8    BS_cag          ! Gradient of linear fit to clock angle
      real*8    BS_slc          ! Constant of linear fit to solar long.
      real*8    BS_slg          ! Gradient of linear fit to solar long.
      real*8    BS_the          ! Constant sun angle value
      real*8    dtor            ! Factor to convert degress to radians
      integer   i               ! Boresight sample counter
      real*8    result(20)      ! Holds results from G02CAF
      real*8    xval(DS_mxb)    ! Independant variables values
      real*8    yval(DS_mxb)    ! Dependant variables values

      parameter (dtor=1.745329252D-2)

*
* COMMON BLOCK /BORE/ HOLDS THE CALCULATED VALUES WHICH ARE USED
*
      common /BORE/ BS_cac,BS_cag,BS_slc,BS_slg,BS_the

*
* CONVERT BORESIGHT SAMPLE TIMES AND SOLAR LONGITUDE SAMPLES TO
* DOUBLE PRECISION
*
      do i=1,DS_bsa(frame)
         xval(i)=dble(DS_but(i,frame))
         yval(i)=dble(DS_sol(i,frame))
      enddo

*
* FIT LEAST SQUARES STRAIGHT LINE TO SOLAR LONGITUDE SAMPLES
*
      ierr=1
      call g02caf(DS_bsa(frame),xval,yval,result,ierr)

*
* IF ALL SOLAR LONGITUDE VALUES ARE THE SAME THEN USE FIRST VALUE (IN
* RADIANS)
*
      if(ierr.ne.0) then
         if(ierr.eq.2) then
            BS_slg=0
            BS_slc=dble(DS_sol(1,frame))*dtor

*
* IF TOO FEW BORESIGHT SAMPLES, SEND MESSAGE AND RETURN
*
         else
            call wrerr('FEWBORE')
            goto 999
         endif

*
* IF NAG ROUTINE RAN SUCCESSFULLY, STORE GRADIENT AND CONSTANT
* IN RADIANS
*
      else
         BS_slg=result(6)*dtor
         BS_slc=result(7)*dtor
      endif

*
* CONVERT SATALLITE CLOCK ANGLE TO DOUBLE PRECISION
*
      do i=1,DS_bsa(frame)

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
* FIT LEAST SQUARES STRAIGHT LINE TO SOLAR LONGITUDE SAMPLES
*
      ierr=1
      call g02caf(DS_bsa(frame),xval,yval,result,ierr)

*
* IF TOO FEW BORESIGHT SAMPLES, SEND MESSAGE AND RETURN
*
      if(ierr.ne.0) then
         call wrerr('FEWBORE')
         goto 999

*
* IF NAG ROUTINE RAN SUCCESSFULLY, STORE GRADIENT AND CONSTANT
* IN RADIANS
*
      else
         BS_cag=result(6)*dtor
         BS_cac=result(7)*dtor
      endif

*
* ASSUME THAT THE SOLAR ASPECT ANGLE DOES NOT CHANGE DURING THE SCAN, SO
* USE THE FIRST VALUE CONVERTED TO RADIANS
*
      BS_the=dble(DS_the(1,frame))*dtor

*
* FINISH
*
  999 continue

      end
