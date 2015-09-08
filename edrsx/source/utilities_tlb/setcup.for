      subroutine setcup(x,y,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sets the cursor position on the current workstation. This
*       routine is a replacement for SGS_SETCU which doesn't work
*       on most devices. This routine uses ARGSLIB if the current
*       device is an ARGS.
*
*SOURCE
*       SETCUP.FOR in UTILITIES.TLB
*
*METHOD
*       If the current graphics device is not an ARGS, then the standard
*       SGS routine is called to set the cursor position. If it is an
*       ARGS then the normalisation and workstation transformations are
*       inquired from GKS and used to convert the given position into
*       ARGS units. The ARGSLIB cursor positioning routine in then
*       called to set the cursor position.
*
*ARGUMENTS
*   INPUTS:
*       x       real    The x co-ord of the cursor position in the
*                       current SGS world co-ord system
*       y       real    The y co-ord of the cursor position in the
*                       current SGS world co-ord system
*   OUTPUTS:
*       ierr    integer Error status: 0 - Success
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       SGS:
*               sgs_icurw,sgs_setcu
*       GKS:
*               gqwkc,gqcntn,gqnt,gqwkt
*       ARGSLIB:
*               srinit,args_curp
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ierr
      real      x,y
*
* DECLARE LOCAL VARIABLES
*
      integer   con     ! Connection identifier for current device
      integer   curntr  ! Identifier of current normalization transformation
      real     dspvwp(4)! Current display surface viewport
      real     dspwnd(4)! Current display surface window
      real     ndcvwp(4)! Current NDC plane viewport
      real     ndcwnd(4)! Current NDC plane window
      integer   pend    ! =1 if a transformation change is pending
      real     rwindo(4)! Requested display surface viewport
      real     rviewp(4)! Requested display surface window
      integer   type    ! Type number for current device (see SUN85 p38)
      integer   wkid    ! GKS workstation identifier
      integer   xargs   ! Requested x position in ARGS units (0-511)
      real      xndc    ! Requested x position in NDC units
      integer   yargs   ! Requested y position in ARGS units (0-511)
      real      yndc    ! Requested y position in NDC units
*
* DETERMINE CURRENT DEVICE TYPE NUMBER
*
      call sgs_icurw(wkid)
      call gqwkc(wkid,ierr,con,type)
      if(ierr.ne.0) goto 999
*
* IF THE DEVICE IS NOT AN ARGS, CALL THE STANDARD SGS ROUTINE
* (AT THE TIME OF WRITING, ARGS HAVE TYPE 160)
*
      if(type.ne.160) then
         call sgs_setcu(x,y)
*
* IF THE DEVICE IS AN ARGS GET THE WINDOW AND VIEWPORT FOR THE
* NORMALISED DEVICE CO-ORDINATE (NDC) PLANE AND THE DISPLAY SURFACE
* (SEE SUN 85 P35)
*
      else
         call gqcntn(ierr,curntr)
         if(ierr.eq.0) call gqnt(curntr,ierr,ndcwnd,ndcvwp)
         if(ierr.eq.0) call gqwkt(wkid,ierr,pend,rwindo,dspwnd,rviewp,
     :                            dspvwp)
         if(ierr.ne.0) goto 999
*
* CONVERT THE GIVEN POSITION FROM SGS CO-ORDS TO ARGS CO-ORDS
*
         xndc = (x - ndcwnd(1))/(ndcwnd(2) - ndcwnd(1)) *
     :          (ndcvwp(2) - ndcvwp(1)) + ndcvwp(1)
         yndc = (y - ndcwnd(3))/(ndcwnd(4) - ndcwnd(3)) *
     :          (ndcvwp(4) - ndcvwp(3)) + ndcvwp(3)
         xargs = nint((xndc - dspwnd(1))/(dspwnd(2) - dspwnd(1)) *
     :          (dspvwp(2) - dspvwp(1)) + dspvwp(1))
         yargs = nint((yndc - dspwnd(3))/(dspwnd(4) - dspwnd(3)) *
     :          (dspvwp(4) - dspvwp(3)) + dspvwp(3))
*
* CALL ARGSLIB ROUTINES TO ALLOCATE ARGS AND POSITION CURSOR
* (SRINIT DOES NOTHING ON SUBSEQUENT ENTRIES)
*
         call srinit(0,.false.,ierr)
         if(ierr.eq.0) call args_curp(0,xargs,yargs)
      endif
*
* FINISH
*
 999  continue

      end
