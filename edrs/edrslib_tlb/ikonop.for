      subroutine ikonop(info,clear,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Opens the IKON for use by program XYCUR. Optionally
*       displays the use of the IKON mouse buttons.
*
*METHOD
*	Opens the IKON overlay device using SGS, workstation
*	type 3201. Check the AGI database to ensure there is at 
*	least one image defined on the IKON and set up an SGS zone
*	identical to the one used to diaplay the image. Reset the 
*	pan and zoom status to no pan or zoom. If required give 
*	information about use of mouse buttons.
*       
*ARGUMENTS       
*   INPUTS:
*       info	logical		If true, then info is displayed
*       			about use of IKON mouse buttons.
*	clear	logical		If true, all overlay planes are cleared
*   OUTPUTS:
*       ierr    integer         Error status 0 - Success
*					     1 - IKON not available
*				 	     2 - No images on IKON
*					     3 - Unable to initialise
*						 SGS
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*              wrerr,wruser
*	SGS:
*	       sgs_init,sgs_clrfg,sgs_opnwk,sgs_close,sgs_selz,
*	       sgs_izone,sgs_setcu
*	GKS:
*	       gk8dwo
*	AGI:
*	       ags_gwnam,agi_rcl,ags_nzone,ags_szone
*              
*STARLINK PARAMETERS
*	NOIKON/ERROR/	Accessed if IKON cannot be opened
*	NOIMS/ERROR/	Accessed if there are no images displayed on
*			IKON
*	NOSGS/ERROR/	Accessed if SGS could not be initialised
*
*VAX SPECIFICS
*       implicit none
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
      logical info,clear
      integer ierr
*
* DECLARE LOCAL VARIABLES
*
      integer	code(4)	! IKON op codes to dezoom and home the display
      integer	ipic	! AGI picture no. of last image displayed on IKON
      integer	izone	! Current SGS zone no.
      integer	newzon	! SGS zone identifier for overlay zone
      integer	nleft	! No. of bytes left vacant in IKON i/o buffer
      character object*15! Contents of AGI OBJECT field for picture IPIC
      character style*15! Holds sytle of picture IPIC (should be IMAGE)
      character wkname*15! Name by which AGI recognises main IKON display
      real	xhi	! Upper limit of zone world co-ordinate system
      real	xlo	! Lower limit of zone world co-ordinate system
      real	xm	! Extent of zone in metres
      real	yhi	! Upper limit of zone world co-ordinate system
      real	ylo	! Lower limit of zone world co-ordinate system
      real	ym	! Extent of zone in metres

      data code/216,0,0,221/
*
* INITIALISE SGS SENDING ERROR MESSAGES TO THE SCREEN
*
      call sgs_init(6,ierr)
      if(ierr.ne.0) then
         call wrerr('NOSGS')
         ierr=3
         goto 999
      endif      
*
* PREVENT THE IKON BEING CLEARED WHEN IT IS OPENED UNLESS ARGUMENT
* "CLEAR" IS TRUE
*
      if(clear) then
         call sgs_clrfg(0)
      else
         call sgs_clrfg(1)
      endif
*
* OPEN IKON MAIN OVERLAY USING SGS AND CHECK THAT THERE ARE
* IMAGES ON MAIN IKON DISPLAY
*
      call sgs_opnwk('3201',izone,ierr)
      if(ierr.ne.0) THEN
         call wrerr('NOIKON')
         ierr=1
         goto 999
      endif
*
* GET AGI WORKSTATION NAME
*
      call ags_gwnam(wkname,ierr)
*
* CHECK THAT THERE IS AT LEAST ONE IMAGE ON THE IKON DEFINED IN
* THE AGI GRAPHICS DATABASE
*
      call agi_rcl(wkname,'IMAGE',ipic,ierr)
      if(ierr.ne.0.or.ipic.le.0) then
         call wrerr('NOIMS')
         ierr=2
         call sgs_close
         goto 999
      endif
*
* DEZOOM AND HOME THE DISPLAY
*
      call gk8dwo(4,4,code,nleft)
      call gk8dwo(5,1,code,nleft)
*
* CREATE AN SGS ZONE IN THE OVERLAY COVERING THE LAST IMAGE DISPLAYED
*
      call ags_nzone(wkname,ipic,style,object,newzon,ierr)
      if(ierr.ne.0) then
         call sgs_close
         goto 999
      endif
*
* SELECT THIS ZONE AS THE CURRENT SGS ZONE
*
      call sgs_selz(newzon,ierr)
*
* POSITION CURSOR IN CENTRE OF SGS ZONE
*
      call sgs_izone(xlo,xhi,ylo,yhi,xm,ym)
      call sgs_setcu(0.5*(xlo+xhi),0.5*(ylo+yhi))
*
* IF USER IS TO BE TOLD HOW TO USE MOUSE BUTTONS, PRINT INFORMATION
*
      if(info)then
         call wruser(' ',ierr)
         call wruser('   Use of IKON mouse buttons:',ierr)
         call wruser(' ',ierr)
         call wruser('      Button 1 (left) : enter coordinate',
     :               ierr)
         call wruser('        "    2        : enter coordinate',
     :               ierr)
         call wruser('        "    3        : terminate input',ierr)
         call wruser(' ',ierr)
         call wruser('   NB   No Pan or zoom available on IKON',ierr)
         call wruser(' ',ierr)
      endif
*
* FINISH
*
  999 continue

      end
