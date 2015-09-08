      subroutine calctc(yposn,ndet,nomdet,scanrt,smprat,tcorrs,
     :                  extend)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To calculate shifts to apply to each detector in the input
*       band in the in-scan direction (X) which will align the detector
*       data with the data from the detector which is nominally in the
*       centre of the required field. Shifts are represented as
*       fractional pixel shifts in the positive x direction.
*       An extra shift is then added to ensure that the nominally
*       central detector appears in the middle of the display.
*
*
*SOURCE
*       CALCTC.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       yposn   real(ndet)      Array containing the in-scan distance in
*                               arcmins of the centre of each detector
*                               from the boresight
*       ndet    integer         The number of detectors in the input
*       nomdet  integer         The z position (in pixels) of the nominally
*                               central detector
*       scanrt  real            IRAS scan rate in arcmins/s
*       smprat  integer         Frequency of detector samples in input band
*   OUTPUTS:
*       tcorrs  real(ndet)      The shifts (in pixels in +ve x direction)
*                               to apply to each detector to align data
*                               with nominal detector
*       extend  integer         The increase in the x dimension of the
*                               entire input data as a result of the
*                               above shifts
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer ndet,nomdet,smprat,extend
      real yposn(ndet),scanrt,tcorrs(ndet)
*
* DECLARE LOCAL VARIABLES
*
      integer   idet    ! Detector loop count
      real      nomcor  ! The shift which would align data at the
                        ! nominal detector with data at the boresight
      real      rsmpsz  ! The reciprocal of the sample size in arcmins
      real      smax    ! The maximum shift
      real      smin    ! The minimum shift
*
* CALCULATE RECIPROCAL OF SAMPLE SIZE IN ARCMINS
*
      rsmpsz=smprat/scanrt
*
* CALCULATE SHIFT WHICH WOULD ALIGN DATA FROM THE NOMINALLY CENTRAL
* DETECTOR WITH DATA FROM THE BORESIGHT
*
      nomcor=-rsmpsz*yposn(nomdet)
*
* INITIALISE MAX AND MIN SHIFTS TO SHIFTS TO ZERO
*
      smin=0
      smax=0
*
* CALCULATE SHIFT FOR EACH DETECTOR AND UPDATE MAX AND MIN SHIFTS
*
      do idet=1,ndet
         tcorrs(idet)=-rsmpsz*yposn(idet)-nomcor
         if(tcorrs(idet).lt.smin) smin=tcorrs(idet)
         if(tcorrs(idet).gt.smax) smax=tcorrs(idet)
      enddo
*
* CALCULATE NO. OF PIXELS BY WHICH THE DATA WILL BE EXTENDED DUE TO THESE
* SHIFTS
*
      extend=int(abs(smax-smin)+0.5)
*
* ADD THE SHIFT REQUIRED TO ENSURE THAT THE NOMINALLY CENTRAL DETECTOR IS
* IN THE MIDDLE OF THE DISPLAY
*
      do idet=1,ndet
         tcorrs(idet)=tcorrs(idet)-smin
      enddo
*
* FINISH
*
      end

