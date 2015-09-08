      subroutine chkbor(samp,frame)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*
*SOURCE
*       CHKBOR.FOR in NDFOUT.TLB
*
*METHOD
*
*ARGUMENTS
*   INPUTS:
*       samp    integer The current CRDD sample no.
*       frame   integer Array depth at which CRDD descriptors are stored
*   OUTPUTS:
*
*SUBROUTINES CALLED
*       SLALIB:
*               SLA_epb
*       IPMAF (COADD.TLB):
*               ecltt
*       IPMAF (CRDIMAGE.TLB):
*               clktrad1
*
*VAX SPECIFICS
*       implicit none
*       REAL*8
*       end of line comments
*       subroutine names longer than 6 characters
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/11/89
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE COMMON BLOCK HOLDING IRAS MISSION PARAMETERS (VARIABLES
* CALLED IR_xxx).
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCK HOLDING DESCRIPTORS FROM CRDD FILE (VARIABLES
* CALLED DS_xxx).
*
      include 'UTILITIES(DS_COM)'

*
* INCLUDE COMMON BLOCK HOLDING INFO ABOUT DETECTORS (VARIABLES
* CALLED DT_xxx).
*
      include 'UTILITIES(DT_DAT)'


      include 'SAE_PAR'
      include 'I90_PAR'
      include 'IRA_PAR'

*
* DECLARE ARGUMENTS
*
      integer   samp,frame

*
* DECLARE LOCAL VARIABLES
*

      real      angle           ! Angle between sky y axis and scan
      real*8    BS_cac(DS_mxb+4)! Spline cooeficients of clock angle fit
      real*8    BS_cak(DS_mxb+4)! Spline knots for clock angle fit
      real*8    BS_slc(DS_mxb+4)! Spline cooeficients of solar long. fit
      real*8    BS_slk(DS_mxb+4)! Spline knots for solar long. fit
      real*8    BS_thc(DS_mxb+4)! Spline cooeficients of sun angle fit
      real*8    BS_thk(DS_mxb+4)! Spline knots for sun angle fit
      real*8    date            ! Besselian epoch of date
      real*8    ddec            ! DEC of boresight at given sample (in
                                ! radians)
      integer	ierr		! Local error status
      integer	m		! No. of boresight samples in fit
      real*8    psi             ! Clock angle for given sample
      real*8    dra             ! RA of boresight at given sample (in
                                ! radians)
      real*8    rtod            ! Factor to convert radians to degrees
      real*8    dscanr          ! Scan orientation (angle from north to
                                ! scan direction thru east in radians)
      real*8    SLA_epb         ! Function from SLALIB library
      real*8    solat           ! Solar latitude (=0 !)
      real*8    solong          ! Solar longitude for given sample
      real*8    time            ! Time offset from 1st to current sample
      real*8    theta           ! Sun angle for given sample
      real*8    twopi           ! Constant
      real*8    sla_epb2d,dlong,dlat,xtemp,ytemp

      parameter (twopi=6.283185307D0,rtod=57.29577951D0)

*
* COMMON BLOCK /BORE_SP/ HOLDS THE CUBIC SPLINE FIT PARAMETERS
*
      common /BORE_SP/ BS_cac,BS_cak,BS_slc,BS_slk,BS_thc,BS_thk

*
* GET THE TIME AT THE GIVEN BORESIGHT SAMPLE
*
      time=DS_but(samp,frame)

*
* CALCULATE THE VALUES OF SOLAR LONGITUDE, CLOCK ANGLE AND SUN ANGLE
* AT THE GIVEN TIME USING THE CUBIC SPLINE FITS
*
      m=DS_bsa(frame)

      ierr=1
      call e02bbf(m+4,BS_slk,BS_slc,time,solong,ierr)
      solong=solong/rtod

      call e02bbf(m+4,BS_cak,BS_cac,time,psi,ierr)
      psi=psi/rtod
      if(psi.lt.0) psi=psi+twopi

      call e02bbf(m+4,BS_thk,BS_thc,time,theta,ierr)
      theta=theta/rtod

*
* CONVERT "MODIFIED JULIAN DATE" TO BESSELIAN EPOCH
*
      date=SLA_epb(DS_mjd(frame))

*
* CONVERT SOLAR LONGITUDE FROM EPOCH 1950 TO LONGITUDE OF DATE USING
* IPMAF ROUTINE ECLTT FROM "COADD" LIBRARY.
*
      solat=0.0D0
      call ecltt(1950.0D0,solong,solat,date,solong,solat)

*
* CONVERT SATELLITE CO-ORDINATES TO RA AND DEC (1950) AND GET SCAN
* ORIENTATION USING IPMAF ROUTINE CLKTRAD1 FROM "CRDIMAGE" LIBRARY.
*
      call clktrad1(date,solong,theta,psi,dra,ddec,dscanr)

*
* CONVERT RA AND DEC TO ECLIPTIC (1950) COORDINATES
*
      call sla_fk45z( dra, ddec, date,xtemp, ytemp )
      call sla_eqecl( xtemp, ytemp, SLA_EPB2D(1950D0), dlong, dlat )
      write(*,*) 'Internal coords: ',dlong*IRA__RTOD,dlat*IRA__RTOD

*
* WRITE OUT THE LAMBDA AND BETA DESCREIPTORS
*
      write(*,*) 'Stored coords: ',DS_lam(samp,frame),DS_bet(samp,frame)

*
* FINISH
*
      end

