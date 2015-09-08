      subroutine convol(data,npixd,nlind,psf,npixp,nlinp,shiftx,shifty,
     :                  rinval,answer)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the value of the convolution of a psf image with a
*       data image assuming that the psf and data images are sampled
*       on equally spaced and oriented grids. It is also assumed that
*       the total sum of the psf image is 1.
*
*SOURCE
*       CONVOL.FOR in UTILITIES.TLB
*
*METHOD
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
      integer   npixd,nlind,npixp,nlinp,shiftx,shifty
      real      data(npixd,nlind),psf(npixp,nlinp),answer,rinval
*
* DECLARE LOCAL VARIABLES
*
      integer   datlin  ! No. of current line in data
      integer   datpix  ! No. of current pixel in data
      integer   ncont   ! No. of valid contributions to convolution
      integer   psflin  ! No. of current line in psf
      integer   psfpix  ! No. of current pixel in psf
*
* INITIALIZE THE SUM OF DATA*PSFAND THE NO. OF CONTRIBUTIONS TO THE SUM
*
      answer=0
      ncont=0
*
* LOOP THROUGH ALL LINES OF THE PSF IMAGE, AND CALCULATE
* THE CORRESPONDING LINE IN THE DATA IMAGE
*
      do psflin=1,nlinp
         datlin=psflin-shifty
         if(datlin.ge.1.and.datlin.le.nlind) then
*
* LOOP THROUGH ALL PIXELS IN THIS LINE OF THE PSF
*
            do psfpix=1,npixp
               datpix=psfpix-shiftx
               if(datpix.ge.1.and.datpix.le.npixd) then
*
* IF THIS PIXEL IS INVALID IN PSF OR DATA GO ON TO NEXT PIXEL
*
                  if(data(datpix,datlin).ne.rinval.and.
     :               psf(psfpix,psflin).ne.rinval) then
*
* INCREMENT THE ANSWER BY THE PRODUCT OF THE DATA AND THE PSF VALUES
* AT THIS PIXEL AND INCREMENT THE NO. OF CONTRIBUTIONS BY 1
*
                     answer=answer
     :                      +data(datpix,datlin)*psf(psfpix,psflin)
                     ncont=ncont+1
*
* GO ROUND FOR NEXT PSF PIXEL
*
                  endif
               endif
            enddo
         endif
      enddo
*
* NORMALIZE THE ANSWER TO IMAGE A VALUES
*
      if(ncont.gt.0) then
         answer=answer/ncont
      else
         answer=rinval
      endif
*
* FINISH
*
      end
