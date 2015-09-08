      subroutine fstlst(data,npix,nlin,inval,scale,zero,fstpix,lstpix,
     :                  line,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To find the location of the first and last valid,non-zero
*       pixels in a line of a I*2 2d image
*
*SOURCE
*       FSTLST.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) I*2     The data
*       npix,nlin       integer The size of the data
*       scale           real    The scale factor for the data
*       zero            real    The zero factor fro the data
*       inval           integer The flag for invalid pixels
*       line            integer The line no. of data for which first
*                               and last valid pixels are required
*   OUTPUTS:
*       fstpix          integer The pixel no. of the first valid pixel
*                               in the specified line of the input
*       lstpix          integer The pixel no. of the last valid pixel
*                               in the specified line of the input
*       ierr            integer Status return: 0 - Success
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
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 16/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,fstpix,lstpix,line,ierr
      integer*2 data(npix,nlin)
      real      scale,zero
*
* DECLARE LOCAL VARIABLES
*
      real      datval  ! The scaled data value at the current pixel
      logical   first   ! True when the first valid, non-zero pixel has
                        ! been found
      integer   pixel   ! Pixel count

*
* INITIALIZE FIRST PIXEL NOT FOUND, ETC
*
      first=.false.
      fstpix=-1
      lstpix=-1
*
* LOOP THROUGH ALL PIXELS IN IMAGE LINE
*
      do pixel=1,npix
*
* GET SCALED DATA VALUE ASSUMING PIXEL IS VALID
*
         datval=scale*data(pixel,line)+zero
*
* IF PIXEL IS VALID AND NOT ZERO....
*
         if(data(pixel,line).ne.inval.and.datval.ne.0) then
*
* IF THE FIRST VALID, NON-ZERO PIXEL HAS NOT BEEN FOUND THEN THE
* CURRENT PIXEL IS THE FIRST VALID NON-ZERO PIXEL
*
            if(.not.first) then
               fstpix=pixel
               first=.true.
            endif
*
* THE LAST PASS THROUGH THIS BLOCK WILL BE FOR THE LAST VALID, NON-ZERO
* PIXEL
*
            lstpix=pixel
         endif
*
* CHECK NEXT PIXEL
*
      enddo
*
* IF NO VALID NON-ZERO PIXELS WERE FOUND SET ERROR STATUS TO 1
*
      if(fstpix.eq.-1.and.lstpix.eq.-1) then
         ierr=1
      else
         ierr=0
      endif
*
* FINISH
*
      end
