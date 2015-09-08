      subroutine splntr(trfpsk,samp,frame,crdata,band,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates the 6 co-efficients of a linear transformation
*       from IRAS focal plane co-ordinates (z,y) in arcmins, to
*       trial sky image pixel co-ordinates for a given sample. This
*	routine uses the cubic spline fits calculated by routine
*	BORESP (which must have been called first).
*
*SOURCE
*       SPLNTR.FOR in UTILITIES.TLB
*
*METHOD
*       Calculate the time offset of the given sample from the start
*       of the scan.
*       Calculate the values of solar longitude, clock angle and
*       sun angle for the given sample, using the cubic spline fits
*       stored in the common block /BORE_SP/ (written by routine BORESP).
*       Convert these values to boresight RA and DEC (1950) and get
*       scan orientation (code copied from IPMAF CRDIMAGE program).
*       Calculate the trial sky pixel with this RA and DEC, and the
*       scan orientation on the trial sky.
*       Calculate the linear transformation co-efficients from sky pixel
*       to focal plane co-ords.
*
*ARGUMENTS
*   INPUTS:
*       samp    integer The current CRDD sample no.
*       frame   integer Array depth at which CRDD descriptors are stored
*	crdata(7) real  Array containint values describing RA and DEC
*			system of final image:
*       band    integer IRAS band no. of crdd
*   OUTPUTS:
*       trfpsk(6) real  Co-efficients of linear transformation from
*                       focal plane co-ords (z,y) in arcmins to sky
*                       image pixel co-ords (xx,yy)
*                         xx=trfpsk(1)+trfpsk(2)*z+trfpsk(3)*y
*                         yy=trfpsk(4)+trfpsk(5)*z+trfpsk(6)*y
*       ierr    integer Status: 0 for success.
*
* NB, The input array CRDATA should contain the following:
*    crdata(1) The RA of the sky image ref pixel in degrees (CRVAL1)
*    crdata(2) The DEC of the sky image ref pixel in degrees (CRVAL2)
*    crdata(3) The x axis pixel location of the sky image ref pixel
*							     (CRPIX1)
*    crdata(4) The y axis pixel location of the sky image ref pixel.
*							     (CRPIX2)
*    crdata(5) The cross scan size of a pixel in degrees of arc (CDELT1)
*    crdata(6) The in scan size of a pixel in degrees of arc (CDELT2)
*    crdata(7) Angle clockwise from north to -ve in scan axis in 
*                                                 degrees.    (CROTA1)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               rdtoxy
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
      real      trfpsk(6),crdata(7)

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
      real*8    dec             ! DEC of boresight at given sample (in
                                ! radians)
      integer	m		! No. of boresight samples in fit
      real*8    psi             ! Clock angle for given sample
      real*8    ra              ! RA of boresight at given sample (in
                                ! radians)
      real*8    rtod            ! Factor to convert radians to degrees
      real*8    scanor          ! Scan orientation (angle from north to
                                ! scan direction thru east in radians)
      real*8    SLA_epb         ! Function from SLALIB library
      real*8    solat           ! Solar latitude (=0 !)
      real*8    solong          ! Solar longitude for given sample
      real*8    time            ! Time offset from 1st to current sample
      real*8    theta           ! Sun angle for given sample
      real*8    twopi           ! Constant
      real      xx              ! X co-ord of boresight in trial sky
      real      yy              ! Y co-ord of boresight in trial sky

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
* CHECK THAT THE GIVEN TIME IS WITHIN THE RANGE WHICH CAN BE EVALUATED
* USING THE CUBIC SPLINE FITS.
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
      call clktrad1(date,solong,theta,psi,ra,dec,scanor)

*
* FIND TRIAL SKY IMAGE PIXEL WHICH HAS THE SAME RA AND DEC AS THE
* BORESIGHT
*
      call rdtoxy(xx,yy,crdata(1),crdata(2),int(crdata(3)),
     ;           int(crdata(4)),crdata(5),crdata(6),
     :            crdata(7),SNGL(ra*rtod),SNGL(dec*rtod))

*
* FIND ANGLE BETWEEN +VE Y AXIS AND SCAN DIRECTION (+VE Y TO +VE X IS
* DEFINED AS +VE ROTATION)
*
      angle=180-crdata(7)-SNGL(scanor*rtod)

*
* CALCULATE THE CO-EFFICIENTS FOR THE REQUIRED TRANSFORMATION
*
      trfpsk(1)=xx
      trfpsk(2)=-cosd(angle)/(60*crdata(5))
      trfpsk(3)=-sind(angle)/(60*crdata(5))
      trfpsk(4)=yy
      trfpsk(5)=sind(angle)/(60*crdata(6))
      trfpsk(6)=-cosd(angle)/(60*crdata(6))

*
* FINISH
*
 999  continue

      end

