      subroutine scale(data,nel,bscale,bzero,blank,magic,nbad)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Apply the scale and zero factor to the given data vector, 
*       replace values of "blank" with the ADAM magic value, and 
*       count the number of bad pixels in the output.
*
*SOURCE
*       SCALE.FOR in NDFOUT.TLB
*
*ARGUMENTS       
*   INPUT:
*       data(nel) real*8	The data vector.
*	nel	  integer	The number of elements in the data vector
*       bscale	  real		The scale factor to apply.
*       bzero	  real		The zero offset to apply.
*       blank	  real*8        The magic value in the input.
*       magic	  logical	If false, then ignore magic values.
*   OUTPUTS:
*       data(nel) real*8        The scaled data
*       nbad	  integer	The number of bad pixels in the output.
*
*SUBROUTINES CALLED
*       none
*              
*VAX SPECIFICS
*       implicit none
*       REAL*8
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 20/9/90
*-------------------------------------------------------------------
      implicit none

      include 'prm_par'
*
* DECLARE ARGUMENTS
*
      integer	nel,nbad
      real*8	data(nel),blank
      real	bscale,bzero
      logical	magic

*
* DECLARE LOCAL VARIABLES
*
      integer	el	! Element counter

*
* INITIALIZE NUMBER OF BAD PIXELS IN THE OUTPUT
*
      nbad=0

*
* DO IT ASSUMING MAGIC VALUES EXIST
*
      if(magic) then

         do el=1,nel
            if(data(el).ne.blank) then
               data(el)=data(el)*bscale+bzero
            else
               data(el)=val__badd
               nbad=nbad+1
            endif
         enddo

*
* DO IT ASSUMING NO MAGIC VALUES EXIST
*
      else

         do el=1,nel
            data(el)=data(el)*bscale+bzero
         enddo

      endif

*
* FINISH
*
      end
