      subroutine memca1(file,npix,nlin,sum,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Gets a mask image from the user. The mask will be multiplied
*	by a sky image, and the total data sum in the product returned
*	to the user, along with the error on the total data sum.
*
*SOURCE
*       MEMCA1.FOR in MEMCRDD.TLB
*
*METHOD
*	The user is first asked for list of (X,Y) coordinates (refering
*	to the pixel no. in the sky image) in EDRS "XY list" format. An
*	image is then constructed which has the value 1.0 at all points
*	inside the polygon defined by the list of (X,Y) coordinates, and
*	the value 0.0 at all other points. If the user does not give an
*	XY list, he is prompted for an image (in EDRS format) to use
*	directly as the mask.
*       
*ARGUMENTS       
*   INPUT:
*	file		integer	The internal MEMSYS3 file no. to receive
*				the mask image.
*	npix	 	integer	The no. of pixels per line of the image
*	nlin		integer	The no. of lines in the image
*	ierr		integer	Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	sum		real	The total data sum in the mask
*       ierr    	integer Exit status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtwork,plyset,gt2dir,i2torl
*       EDRS:
*              gtxylr,extlst,gtstds
*       INTERIM:
*              frdata,cnpar
*
*STARLINK PARAMETERS
*	MASKPOLY	XY list giving polygon inside which mask is unity
*	MASK		An image giving the mask directly
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 1/11/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
*
* DECLARE ARGUMENTS
*
      integer	npix,nlin,ierr,file
      real	sum

*
* DECLARE LOCAL VARIABLES
*
      character	bdfnam*30! Name of a mask image BDF.
      integer	inval	! Flag for invalid pixels in the input image
      integer	ipin	! Pointer to input mask image
      integer	ipx	! Pointer to workspace holding X coords
      integer	ipxy	! Pointer to input XY list
      integer	ipy	! Pointer to worksddpace holding Y coords
      integer	istat	! Temporary status value
      integer	lstlen	! No. of records in XY list
      integer	nitem	! No. of items per record in XY list
      integer	nlinin	! No. of lines in input image
      integer	npixin	! No. of pixels per line in input image
      integer	offset	! Offset into final mask image
      real	scale	! Scale factor of input image
      character	title*30! Title from input image
      real	zero	! Zero offset of input image

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

* 
* ATTEMPT TO GET AN XY LIST DEFINING THE MASK
*
      call gtxylr('MASKPOLY',.true.,nitem,lstlen,ipxy,istat)

*
* IF SUCCESFUL, EXTRACT THE COORDINATES...
*
      if(istat.eq.0) then

         call gtwork('X','REAL',lstlen,ipx,ierr)
         if(ierr.eq.0) call gtwork('Y','REAL',lstlen,ipy,ierr)
         if(ierr.ne.0) goto 999

         call extlst(%val(ipxy),nitem,lstlen,%val(ipx),21,24)
         call extlst(%val(ipxy),nitem,lstlen,%val(ipy),25,28)

*
* SET MASK TO ALL ZEROS
*
         do offset=0,ME_mj-1
            ME_st(ME_kb(file)+offset)=0.0
         enddo

*
* SET THE MASK AREA INSIDE THE GIVEN POLYGON, TO UNITY
*
         call plyset(ME_st(ME_kb(file)),npix,nlin,%val(ipx),%val(ipy),
     :               lstlen,1.0,ierr)

*
* IF NO XY LIST GIVEN SEE IF USER WANTS TO SPECIFY AN IMAGE MASK
*
      else

         call memcf3('MASK',.true.,ME_st(ME_kb(file)),0.0,bdfnam,ierr)

      endif

*
* IF A MASK HAS BEEN OBTAINED SUCCESSFULLY, CALCULATE THE TOTAL DATA SUM
* WITHIN THE MASK
*
      sum=0.0
      if(ierr.eq.0) then
         do offset=0,ME_mj-1
            sum=sum+ME_st(ME_kb(file)+offset)
         enddo
      endif

*
* RELEASE THE BDFS,CANCEL THE PARAMETER ASSOCIATIONS AND  FINISH
*
  999 call frdata('MASKPOLY',istat)
      call frdata('MASK',istat)
      call cnpar('MASKPOLY',istat)
      call cnpar('MASK',istat)

      end
