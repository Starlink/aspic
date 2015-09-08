      subroutine borpos(samp,frame,band,ra,dec,scanor,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates the RA and DEC (B1950) of the boresight at a
*       given sample number. Also returns the scan orientation. This
*	routine uses the cubic spline fits calculated by routine
*	BORESP (which must have been called first).
*
*SOURCE
*       BORPOS.FOR in UTILITIES.TLB
*
*METHOD
*       Calculate the time offset of the given sample from the start
*       of the scan.
*       Calculate the values of solar longitude, clock angle and
*       sun angle for the given sample, using the cubic spline fits
*       stored in the common block /BORE_SP/ (written by routine BORESP).
*       Convert these values to boresight RA and DEC (1950) and get
*       scan orientation (code copied from IPMAF CRDIMAGE program).
*
*ARGUMENTS
*   INPUTS:
*       samp    integer The current CRDD sample no.
*       frame   integer Array depth at which CRDD descriptors are stored
*       band    integer IRAS band no. of crdd
*   OUTPUTS:
*       ra      real    The RA of the boresight in degrees.
*       dec     real    The DEC of the boresight in degrees.
*       scanor  real    Scan orientation (angle from north to scan 
*                       direction thru east) in degrees.
*       ierr    integer Status. Zero for success.
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

*
* DECLARE ARGUMENTS
*
      integer   samp,frame,band,ierr
      real      ra,dec,scanor

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

      parameter (twopi=6.283185307D0,rtod=57.29577951D0)

*
* COMMON BLOCK /BORE_SP/ HOLDS THE CUBIC SPLINE FIT PARAMETERS
*
      common /BORE_SP/ BS_cac,BS_cak,BS_slc,BS_slk,BS_thc,BS_thk

*
* CALCULATE THE TIME OFFSET FROM THE FIRST SAMPLE TO THE CURRENT SAMPLE
* ASSUMING SAMPLES OCCUR AT CONTSTANT FREQUENCY (GIVEN BY DT_SRT)
*
      time=real(samp-1)/real(DT_srt(band))

*
* CHECK THAT THE TIME IS WITHIN THE RANGE WHICH CAN BE EVALUATED USING
* THE CUBIC SPLINE FITS.
*
      m=DS_bsa(frame)

      if(time.lt.BS_slk(4).or.time.gt.BS_slk(m+1)) then
         call wrerr('BADSPL')
         ierr=1
         goto 999
      endif

*
* CALCULATE THE VALUES OF SOLAR LONGITUDE, CLOCK ANGLE AND SUN ANGLE
* AT THE GIVEN TIME USING THE CUBIC SPLINE FITS
*


      ierr=0
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
* CONVERT THE VALUES IN RADIANS TO DEGREES.
*
      ra = real(dra*rtod)
      dec = real(ddec*rtod)
      scanor = real(dscanr*rtod)

*
* FINISH
*
 999  continue

      end

