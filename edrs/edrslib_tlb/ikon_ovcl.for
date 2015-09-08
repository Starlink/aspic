      subroutine ikon_ovcl(iplane,diss)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Closes the IKON overlay planes. If argument diss is true, all
*	overlays are disabled (not cleared). The argument iplane is not
*	used ,it is included to be consistent with ARGS_OVCL. ARGS_OVCL
*	also disables ALL overlays if diss is true, irrespective of the
*	value of iplane.
*
*METHOD
*       Reset IKON fill drawing mode register to 'replace screen value
*       with data value'.
*       If required disable the overlay planes.
*       These operations are performed by sending IKON command op codes
*       to the IKON using the GKS driver i/o routines.
*       
*ARGUMENTS       
*   INPUTS:
*       iplane	integer		Overlay plane no. in range 0 to 7
*       diss	logical		If true, then all overlays are disabled
*
*SUBROUTINES CALLED
*	GKS:
*		gk8dwo
*	SGS:
*		sgs_flush
*              
*VAX SPECIFICS
*       implicit none
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
      logical diss
*
* DECLARE LOCAL VARIABLES
*
      integer	code(2) ! Ikon Pixel Engine Op. codes
      integer	ierr	! Error status
      integer	nleft	! No. of bytes left in Ikon I/O buffer

*
* FLUSH THE SGS AND GKS BUFFERS
*
      call sgs_flush
*
* RESET FILL MODE REGISTER TO 'REPLACE SCREEN VALUE WITH DATA VALUE'
*
      code(1)=96*256+42	! Set 8 bit register
      code(2)=0		! to the value zero
      call gk8dwo(4,2,code,nleft)
*
* IF REQUIRED, DISABLE OVERLAYS (FRAME BUFFER 1)
*
      if(diss) then
         code(1)=92*256 + 3	! Set frame grab control latch to
				! main buffer only
         call gk8dwo(4,1,code,nleft)
      endif
*
* FLUSH IKON I/O BUFFER
*
      call gk8dwo(5,1,code,nleft)
*
* FINISH
*
      end
