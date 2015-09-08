      subroutine argsop(info,clear,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Open the ARGS.
*       
*METHOD
*	Allocate and open the ARGS using ARGSLIB. Check the ARGS
*	database to ensure there is at least one image defined on 
*	the ARGS. Save the current zoom status for re-instatement
*	when the ARGS is finally closed. If required give information
*	about use of trackerball buttons.
*
*ARGUMENTS       
*   INPUTS:
*       info	logical		If true, then tell user how to use
*       			ARGS trackerball buttons
*	clear	logical		If true then clear all overlays.
*   OUTPUTS:
*       ierr    integer         Error status 0 - Success
*					     1 - ARGS not available
*					     2 - No images on ARGS
*
*SUBROUTINES CALLED
*	THIS PACKAGE:
*	       WRERR,WRUSER
*       ARGSLIB:
*              SRINIT
*       ASP_:
*              DZTOI
*       ARGS_:
*              ARGS_NUMIM,ARGS_RDPAR,ARGS_CLS
*              
*STARLINK PARAMETERS
*	NOARGS/ERROR/	Accessed if ARGS is not available
*	NOIMS/ERROR/	Accessed if there are no images displayed on
*			ARGS
*
*VAX SPECIFICS
*       implicit none
*	enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      logical 	info,clear
      integer	ierr
*
* DECLARE LOCAL VARIABLES
*
      integer	i	! Loop count
      integer	nval	! Dummy integer argument
*
* INCLUDE COMMON BLOCK HOLDING ARGS ZOOM STATUS
*
      include 'ARGSCOM.FOR'
*
* CALL SRINIT TO INITIALLISE ARGS
*
      ierr=0
      call srinit(0,.false.,ierr)
      if(ierr.ne.0) then
         ierr=1
         call wrerr('NOARGS')
         go to 999
      endif
*
* DETERMINE IF THERE IS AN IMAGE ON THE ARGS
*
      call args_numim(imlast)
      if(imlast.eq.0) then
         call wrerr('NOIMS')
         ierr=2
         go to 999
      endif
*
* OBTAIN CURRENT ZOOM STATUS FOR THE ARGS AND SAVE IN COMMON BLOCK
* 'ARGSCOM'. THIS ZOOM STATUS IS RE-INSTATED BY ROUTINE ARGSCL
*
      call args_rdpar('DISPZOOM',1,value,nval,ierr)
      call asp_dztoi('ZXC',value,ixc,ierr)
      call asp_dztoi('ZYC',value,iyc,ierr)
      call asp_dztoi('ZXF',value,ixf,ierr)
      call asp_dztoi('ZYF',value,iyf,ierr)
*
* IF NOT THERE, ASSUME NO ZOOM
*
      if(ixf.eq.0)then
         ixf=1
         iyf=1
         ixc=256
         iyc=256
      endif
      ierr=0
*
* IF REQUIRED, CLEAR ALL OVERLAY PLANES
*
      if(clear) then
         do i=8,15
            call args_cls(i)
         enddo
      endif
*
* IF USER IS TO BE TOLD HOW TO USE ARGS BUTTONS, PRINT INFORMATION
*
      if(info)then
         call wruser(' ',ierr)
         call wruser('   Use of ARGS buttons:',ierr)
         call wruser(' ',ierr)
         call wruser('      Button 1 (left) : enter coordinate',
     :               ierr)
         call wruser('        "    2        : reduce image zoom '//
     :               'factor',ierr)
         call wruser('        "    3        : increase image zoom '//
     :               'fact or',ierr)
         call wruser('        "    4 (right): terminate input',ierr)
         call wruser(' ',ierr)
      endif
*
* FINISH
*
 999  continue

      end
