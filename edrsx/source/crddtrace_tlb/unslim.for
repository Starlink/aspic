      subroutine unslim(data,npix,nlin,det,samplo,samphi,inval,unsmin,
     :                  unsmax)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To determine the maximum and minimum unscaled data values
*       which will be displayed for a particular detector
*
*SOURCE
*       UNSLIM.FOR in CRDDTRACE.TLB
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) real    The enire unscaled data
*       npix            integer Number of pixels per line in the data
*       nlin            integer The number of lines in the data
*       det             integer The detector to use
*       samplo          real    Lowest sample (pixel) to be displayed
*       samphi          real    Highest sample (pixel) to be displayed
*       inval           integer Data value for invalid pixels
*   OUTPUTS:
*       unsmin          real    The lowest data value to be displayed
*       unsmax          real    The highest data value to be displayed
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 9/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,det
      real      data(npix,nlin),samplo,samphi,unsmin,unsmax
*
* DECLARE LOCAL VARIABLES
*
      real      sample  ! Sample count
      integer   trace   ! Trace count
      real      unsval  ! Data value buffer
*
* INITIALIZE MAX AND MIN VALUES
*
      unsmin=1.0e30
      unsmax=-1.0e30
*
* LOOP THROUGH ALL SAMPLES TO BE DISPLAYED
*
      do sample=samplo,samphi
*
* ONLY USE THOSE SAMPLES WHICH ARE IN THE DATA AND ARE NOT INVALID
*
         if(sample.ge.0.5.and.sample.le.npix-0.5) then
            unsval=data(nint(sample),det)
            if(unsval.ne.inval) then
*
* UPDATE THE MAX AND MIN VALUES IF NECESSARY
*
               if(unsval.gt.unsmax) unsmax=unsval
               if(unsval.lt.unsmin) unsmin=unsval
            endif
         endif
      enddo
*
* IF NO VALID PIXELS WERE FOUND, SET MAX AND MIN TO INVAL
*
      if(unsmax.eq.-1.0e30) then
         unsmax=inval
         unsmin=inval
      endif
*
* FINISH
*
      end
