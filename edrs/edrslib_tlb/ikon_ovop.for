      subroutine ikon_ovop(iplane,colour)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Open an overlay plane on the IKON for graphics.
*	Subsequent calls to GKS drawing primatives (polyline, polymarker
*	and text) will go to the specified plane and be in the specified
*	colour.
*
*METHOD
*       Ensure overlay plane no. is in range 0 to 7
*       Ensure IKON is displaying both frame buffers 0 and 1
*       Set current frame buffer to frame buffer 1 (the overlay buffer)
*       Set GKS pen no. to pixel value associated with required plane
*       Calculate RGB values for selected colour
*       Set all colour table entries for which the plane bit is set to
*          the required RGB values
*       Set IKON drawing mode to 'OR'
*       
*ARGUMENTS       
*   INPUTS:
*       iplane	integer		Overlay plane no. in range 0 to 7
*       colour	character*1	Colour  R for Red
*       				G for Green
*       				B for Blue
*       				C for Cyan
*       				Y for Yellow
*       				M for Magenta
*       				W for White
*       				anything else gives black
*
*SUBROUTINES CALLED
*	GKS:
*		gk8dwo,gqasf,gsasf,gsplci,gspmci,gstxci,gscr
*	SGS:
*		sgs_icurw
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer iplane
      character*1 colour
*
* DECLARE LOCAL VARIABLES
*
      real	blue	! Blue intensity of selected colour
      integer	code(99)! Ikon Pixel Engine Op. codes
      real	green	! Green intensity of selected colour
      integer	icol	! Integer pointing to chosen colour
      integer	ierr	! Error status
      integer	iwkid	! Workstation identifier for current device
      integer	jasf(13)! GKS Aspect Source Flags
      integer	nleft	! No. of bytes left in Ikon I/O buffer
      integer	np	! Overlay plane no. in range 0 to 7
      integer	pen	! GKS pen number(=actual value written to Ikon)
      integer	power(8)! List of powers of two
      real	red	! Red intensity of selected colour

      data power/1,2,4,8,16,32,64,128/
*
* ENSURE OVERLAY PLANE NO. IS IN RANGE 0 TO 7
*
      np=mod(iplane,8)
*
* ENSURE IKON IS DISPLAYING BOTH FRAME BUFFERS 0 AND 1 AND USING THE
* COLOUR TABLE ASSOCIATED WITH FRAME BUFFER 1
*
      code(1)=92*256 + 1	! Set frame grab control latch to
				! display both main and overlay buffers
*
* SET CURRENT FRAME BUFFER TO FRAME BUFFER 1 (THE OVERLAY BUFFER)
*
      code(2)=123*256 + 1 	! Display frame buffer 1
      code(3)=124*256 + 1 	! Read from frame buffer 1
      code(4)=125*256 + 1 	! Write to frame buffer 1
*
* SET IKON DRAWING MODE TO 'OR'
*
      code(5)=256*160 + 1	! Set line drawing mode to 'OR'
*
* SEND CODE BUFFER TO IKON
*
      call gk8dwo(4,5,code,nleft)
      call gk8dwo(5,1,code,nleft)
*
* SET GKS POLYLINE, POLYMARKER AND TEXT COLOUR INDECIES TO USE 
* INDIVIDUAL ATTRIBUTE VALUES
*
      call gqasf(ierr,jasf)
      jasf(3)=1
      jasf(6)=1
      jasf(10)=1
      call gsasf(jasf)
*
* SET GKS PEN NO.S TO PIXEL VALUE ASSOCIATED WITH REQUIRED PLANE
*
      pen=power(np+1)
      call gsplci(pen)
      call gspmci(pen)
      call gstxci(pen)
*
* CALCULATE RGB VALUES FOR SELECTED COLOUR
*
      if(colour.eq.'R') then
         red=1
         green=0
         blue=0
      else if(colour.eq.'G') then
         red=0
         green=1
         blue=0
      else if(colour.eq.'B') then
         red=0
         green=0
         blue=1
      else if(colour.eq.'C') then
         red=0
         green=1
         blue=1
      else if(colour.eq.'Y') then
         red=1
         green=1
         blue=0
      else if(colour.eq.'M') then
         red=1
         green=0
         blue=1
      else if(colour.eq.'W') then
         red=1
         green=1
         blue=1
      else
         red=0
         green=0
         blue=0
      endif
*
* SET ALL COLOUR TABLE ENTRIES FOR WHICH THE PLANE BIT IS SET TO
* THE REQUIRED RGB VALUES
*
      call sgs_icurw(iwkid)
      do icol=pen,255
         if(iand(icol,pen).ne.0) then
            call gscr(iwkid,icol,red,green,blue)
         endif
      enddo
*
* FINISH
*
      end
