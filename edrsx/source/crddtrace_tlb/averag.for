      subroutine averag(data,npix,nlin,avrges,inval,minuns,maxuns)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To calculate an 'average' value (see METHOD) for the data
*       from each detector, for use in calculating the y axis offset
*       between data traces.
*
*SOURCE
*       AVERAG.FOR in CRDDTRACE.TLB
*
*METHOD
*       For each detector, the data values below which 10% and 80% of
*       all data values lie, are found. The 'average' value returned
*       for each detector is the mean of these two values.
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) real    The input CRDD data
*       npix            integer No. of data samples for each detector
*       nlin            integer No. of detector streams in data
*       inval           integer The value stored in invalid pixels
*       minuns          real    The minimum unscaled value in the input
*       maxuns          real    The maximum unscaled value in the input
*   OUTPUTS:
*       avrges(nlin)    real    'Average' data value for each detector
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               histpc,wrerr
*
*STARLINK PARAMETERS
*       NOVAL/error/    Accessed if at least one trace had no valid
*                       pixels
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/9/87
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval
      real      data(npix,nlin),avrges(nlin),maxuns,minuns

*
* DECLARE LOCAL VARIABLES
*
      logical   badlns  ! True if some detectors had no valid pixels
      integer   idet    ! Detector count
      integer   hist(300)       ! Histogram of data values
      integer   ierr    ! Error status from histpc
      real      pc(2)   ! Array holding the fractional positions in the
                        ! input image histogram, from which the 'average'
                        ! data value is calculated
      real      vpc(2)  ! Array holding the data values corresponding to
                        ! the hitogram positions in array pc

*
* LOOP TO CALCULATE THE 'AVERAGE' VALUE OF EACH DETECTOR STREAM
*
      badlns=.false.
      do idet=1,nlin

*
* FIND THE DATA LEVELS BELOW WHICH 10% AND 80% OF THE DATA LIES
*
         pc(1)=0.1
         pc(2)=0.8
         call histpc(data,npix,nlin,idet,inval,pc,vpc,2,hist,300,minuns,
     :               maxuns,ierr)
         if(ierr.eq.0) then

*
* USE THE MEAN OF THESE TWO VALUES AS THE 'AVERAGE' DATA VALUE
*
            avrges(idet)=0.5*(vpc(1)+vpc(2))

*
* IF THE MIN AND MAX DATA VALUES IN THE SELECTED DETECTOR STREAMS ARE
* EQUAL, SET THE AVERAGE VALUE TO THAT VALUE
*
         else if(ierr.eq.2) then
            avrges(idet)=minuns

*
* IF THE LINE HAS NO VALID PIXELS SET 'AVERAGE' VALUE TO INVALID VALUE
*
         else
            badlns=.true.
            avrges(idet)=inval
         endif
      enddo

*
* IF DETECTORS WERE FOUND WHICH HAD NO VALID PIXELS, GIVE A MESSAGE
*
      if(badlns) call wrerr('NOVAL')

*
* FINISH
*
      end
