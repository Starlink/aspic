      subroutine quads(in,out,mo,no,mi,ni,flag)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Copy one image to another optionally swapping quadrants.
*
*SOURCE
*       QUADS.FOR in FOURIER.TLB
*
*METHOD
*	The images may have been padded out at high pixel and
*	line numbers (see subroutine PREPIM), therefore the 
*	centre of the image in not necessarily the centre of the
*	valid data. This means that the process of swapping quadrants
*	one way, and swapping them back again, are not identical. 
*	Argument "flag" specifies which process is required; a 
*	"forward" swap (flag=1) in which the swap is done around the 
*	centre of the unpadded data, or a "reverse" swap (flag=-1) in 
*	which the swap is done about a position equal to the "forward"
*	position reflected through the image centre.
*
*ARGUMENTS
*   INPUTS:
*	in(mo,no) real		The input image
*	mi	  integer	Pixels per line in the unpadded region
*	ni	  integer	No. of lines in the unpadded region
*       mo	  integer	No. of pixels per line in whole image
*	no	  integer	No. of lines in whole image
*       flag	  integer	If +1 then forward swap, if -1 then
*				reverse swap, if 0 then no swap.
*   OUTPUTS:
*	out(mo,no)real		The output image
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
*       D.S. Berry (MAVAD::DSB) 6/6/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   mo,no,mi,ni,flag
      real      in(mo,no),out(mo,no)

*
* DECLARE LOCAL VARIABLES
*
      integer	c1	! No. of pixels to right of forward swap centre
      integer	c2	! No. of lines above forward swap centre
      integer 	j	! Pixel count
      integer	k	! Line count
      integer	mih	! X (pixels) coords of forward swap centre
      integer	nih	! Y (lines) coord of forward swap centre

*
* SET UP CONSTANTS
*
      nih=ni/2
      mih=mi/2
      c1=mo-mih
      c2=no-nih

*
* IF REQUIRED FORWARD SWAP THE QUADRANTS
*
      if(flag.eq.1) then

         do k=1,nih

            do j=1,mih
               out(j+c1,k+c2)=in(j,k)
            enddo

            do j=mih+1,mo
               out(j-mih,k+c2)=in(j,k)
            enddo

         enddo

         do k=nih+1,no

            do j=1,mih
               out(j+c1,k-nih)=in(j,k)
            enddo

            do j=mih+1,mo
               out(j-mih,k-nih)=in(j,k)
            enddo

         enddo

*
* OR REVERSE SWAP THE QUADRANTS BACK AGAIN
*
      else if(flag.eq.-1) then
         do k=1,nih
            do j=1,mih
               out(j,k)=in(j+c1,k+c2)
            enddo
            do j=mih+1,mo
               out(j,k)=in(j-mih,k+c2)
            enddo
         enddo
         do k=nih+1,no
            do j=1,mih
               out(j,k)=in(j+c1,k-nih)
            enddo
            do j=mih+1,mo
               out(j,k)=in(j-mih,k-nih)
            enddo
         enddo

*
* OTHERWISE JUST COPY INPUT TO OUTPUT
*
      else
         do k=1,no
            do j=1,mo
               out(j,k)=in(j,k)
            enddo
         enddo
      endif

*
* FINISH
*
      end
