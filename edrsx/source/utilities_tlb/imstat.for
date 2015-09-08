      subroutine imstat(data,npix,nlin,inval,imax,imin,mean,dev,nval,
     :                  ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To find the max, min and mean data values, and the standard
*       deviation in a integer*2 image.
*
*SOURCE
*       IMSTAT.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       npix,nlin       integers        Size of input image
*       data(npix,nlin) integer*2       Input image
*       inval           integer         Invalid pixel value
*   OUTPUTS:
*       imax            integer         max value in input
*       imin            integer         Min value in input
*       mean            real            Mean value in input
*       dev             real            Standard deviation in input
*       nval            integer         No. of valid pixels in image
*       ierr            integer         Status : 0 - Success
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/4/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,ierr,imax,imin,nval
      integer*2 data(npix,nlin)
      real      mean,dev
*
* DECLARE LOCAL VARIABLES
*
      real*8    datsqr  ! Sum of squares of data values
      real*8    datsum  ! Sum of data values
      integer   datval  ! Temporary buffer for pixel values
      logical   first   ! True if max and min have not been assigned a value
      integer   line    ! Line count
      integer   pixel   ! Pixel count
*
* LOOP THROUGH ALL IMAGE PIXELS
*
      datsum=0
      datsqr=0
      nval=0
      first=.true.
      do line=1,nlin
         do pixel=1,npix
            datval=data(pixel,line)
*
* UPDATE MAX AND MIN VALUE IF NECESSARY
*
            if(datval.ne.inval) then
               datsum=datsum+datval
               datsqr=datsqr+datval*datval
               nval=nval+1
               if(first) then
                  imax=datval
                  imin=datval
                  first=.false.
               else
                  imax=max(imax,datval)
                  imin=min(imin,datval)
               endif
            endif
         enddo
      enddo
*
* IF IMAGE CONTAINED NO VALID PIXELS SET IERR=1 AND SET MAX AND MIN
* VALUES TO INVAL
*
      if(nval.eq.0) then
         ierr=1
         imin=inval
         imax=inval
         mean=inval
      else
         mean=real(datsum/nval)
         dev=sqrt((datsqr/nval)-(datsum/nval)*(datsum/nval))
         ierr=0
      endif
*
* FINISH
*
      end
