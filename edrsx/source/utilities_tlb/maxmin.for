      subroutine maxmin(data,npix,nlin,inval,datmax,datmin,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To find the max and min data value in a integer*2 image.
*
*SOURCE
*       MAXMIN.FOR in UTILITIES.TLB
*
*METHOD
*
*
*
*ARGUMENTS
*   INPUTS:
*
*   OUTPUTS:
*
*
*USED BY
*
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*
*       THIS PACKAGE (.TLB):
*
*       EDRS:
*
*       INTERIM:
*
*
*STARLINK PARAMETERS
*       /read/
*       /write/
*       /error/
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       %val
*       end of line comments
*       do while
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) //87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,inval,ierr,datmax,datmin
      integer*2 data(npix,nlin)
*
* DECLARE LOCAL VARIABLES
*
      integer   datval  ! Temporary buffer for pixel values
      logical   first   ! True if max and min have not been assigned a value
      integer   line    ! Line count
      integer   pixel   ! Pixel count
*
* LOOP THROUGH ALL IMAGE PIXELS
*
      first=.true.
      do line=1,nlin
         do pixel=1,npix
            datval=data(pixel,line)
*
* UPDATE MAX AND MIN VALUE IF NECESSARY
*
            if(datval.ne.inval) then
               if(first) then
                  datmax=datval
                  datmin=datval
                  first=.false.
               else
                  datmax=max(datmax,datval)
                  datmin=min(datmin,datval)
               endif
            endif
         enddo
      enddo
*
* IF IMAGE CONTAINED NO VALID PIXELS SET IERR=1 AND SET MAX AND MIN
* VALUES TO INVAL
*
      if(first) then
         ierr=1
         datmin=inval
         datmax=inval
      else
         ierr=0
      endif
*
* FINISH
*
      end
