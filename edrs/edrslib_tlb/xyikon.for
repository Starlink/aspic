      subroutine xyikon(ixd,iyd,x,y,exit)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To obtain an XY position from the IKON screen using the cursor
*       under mouse control.
*
*METHOD
*       This version does not allow pan and zoom. Instead a normal
*       GKS locator is used (a 'normal' cursor). Use of the mouse
*       buttons is as follows:
*
*       	Centre button: Exit
*               Left or right buttons: Select xy position
*       
*ARGUMENTS       
*   OUTPUTS:
*	x,y	real	Selected position in user units
*	ixd,iyd integer	Selected position in device co-ords
*       exit	logical	True if user wishes to quit
*
*SUBROUTINES CALLED
*       SGS:
*		sgs_reqcu
*	AGI:
*		ags_wtod
*              
*VAX SPECIFICS
*       implicit none
*	End of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real	x,y
      integer	ixd,iyd
      logical	exit
*
* DECLARE LOCAL VARIABLES
*
      real	dx	! X device co-ord
      real	dy	! Y device co-ord
      integer	ierr	! Error status
      integer	n	! No. of mouse button pressed by user
*
* OBTAIN A POSITION FROM THE USER USING THE CURSOR IN WORLD CO-ORDS
*
      call sgs_reqcu(x,y,n)
*
* TRANSLATE THE WORLD CO-ORDS TO DEVICE CO-ORDS
*
      call ags_wtod(1,x,y,dx,dy,ierr)
      ixd=nint(dx)
      iyd=nint(dy)
*
* ENSURE PIXEL INDICES ARE RETURNED
*
      x=nint(x+0.5)
      y=nint(y+0.5)
*
* IF BUTTON 2 WAS PRESSED, SET EXIT FLAG
*
      if(n.eq.0) then
         exit=.true.
      else
         exit=.false.
      endif
*
* FINISH
*
      end
