      subroutine scan(samp,frame,band,angle,ra,dec)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates the RA and DEC of the boresight when a given 
*	sample of CRDD was taken. Also returns the scan angle.
*
*SOURCE
*       SCAN.FOR in UTILITIES.TLB
*
*METHOD
*       Calculate the time offset of the given sample from the start
*       of the scan.
*       Calculate the values of solar longitude, clock angle and
*       sun angle for the given sample, using the linear relationships
*       stored in the common block /BORE/ (written by routine BOREST).
*       Convert these values to boresight RA and DEC (1950) and get
*       scan orientation (code copied from IPMAF CRDIMAGE program).
*
*ARGUMENTS
*   INPUTS:
*       samp    integer The current CRDD sample no.
*       frame   integer Array depth at which CRDD descriptors are stored
*       band    integer IRAS band no. of crdd
*   OUTPUTS:
*	ra	real	RA of boresight at given sample (degrees)
*	dec	real	DEC of boresight at given sample (degrees)
*	angle	real	Scan angle at given sample (angle from north to
*			scan direction thru east in degrees)
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
*       Trig functions in degrees
*       end of line comments
*       subroutine names longer than 6 characters
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/4/89
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

*
* DECLARE ARGUMENTS
*
      integer   samp,frame,band
      real      angle,ra,dec

*
* DECLARE LOCAL VARIABLES
*

      real*8    BS_cac          ! Constant of linear fit to clock angle
      real*8    BS_cag          ! Gradient of linear fit to clock angle
      real*8    BS_slc          ! Constant of linear fit to solar long.
      real*8    BS_slg          ! Gradient of linear fit to solar long.
      real*8    BS_the          ! Constant sun angle value
      real*8    date            ! Besselian epoch of date
      real*8    ddec            ! DEC of boresight at given sample (in
                                ! radians)
      real*8    psi             ! Clock angle for given sample
      real*8    dra             ! RA of boresight at given sample (in
                                ! radians)
      real*8    rtod            ! Factor to convert radians to degrees
      real*8    scanor          ! Scan orientation (angle from north to
                                ! scan direction thru east in radians)
      real*8    SLA_epb         ! Function from SLALIB library
      real*8    solat           ! Solar latitude (=0 !)
      real*8    solong          ! Solar longitude for given sample
      real      time            ! Time offset from 1st to current sample
      real*8    theta           ! Sun angle for given sample
      real*8    twopi           ! Constant

      parameter (twopi=6.283185307D0,rtod=57.29577951D0)

*
* COMMON BLOCK /BORE/ HOLDS THE CONSTANTS OF THE RELATIONSHIPS BETWEEN
* UTC AND SOLAR LONGITUDE, CLOCK ANGLE AND SUN ANGLE WRITTEN BY ROUTINE
* BOREST
*
      common /BORE/ BS_cac,BS_cag,BS_slc,BS_slg,BS_the

*
* CALCULATE THE TIME OFFSET FROM THE FIRST SAMPLE TO THE CURRENT SAMPLE
* ASSUMING SAMPLES OCCUR AT CONTSTANT FREQUENCY (GIVEN BY DT_SRT)
*
      time=real(samp-1)/real(DT_srt(band))

*
* CALCULATE THE VALUES OF SOLAR LONGITUDE, CLOCK ANGLE AND SUN ANGLE
* AT THE GIVEN TIME ASSUMING A LINEAR VARIATION OF EACH WITH TIME
*
      solong=BS_slg*time+BS_slc
      psi=BS_cag*time+BS_cac
      if(psi.lt.0) psi=psi+twopi
      theta=BS_the

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
      call clktrad1(date,solong,theta,psi,dra,ddec,scanor)

*
* CONVERT SCAN ANGLE, RA AND DEC FROM RADIANS TO DEGREES
*
      angle=SNGL(scanor*rtod)
      ra=SNGL(dra*rtod)
      dec=SNGL(ddec*rtod)

*
* FINISH
*
      end

