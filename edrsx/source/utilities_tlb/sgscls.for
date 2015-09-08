      subroutine sgscls( param )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Waitrs for a reply from the user (when using a VAXstation) 
*       then closes SGS.
*
*SOURCE
*       SGSCLS.FOR in UTILITIES.TLB
*
*METHOD
*       A check is made to see if the current workstation has a GNS
*       class of WINDOW. If it doesn't then SGS is closed and the 
*       routine exits. If it is, then the parameter supplied is used to
*       prompt the user for a logical value indicating if SGS is to be 
*       closed. If the user responds with a true value (YES, TRUE, Y, 
*       T etc) then SGS is closed and the routine exits. If the user 
*       responds with a false value, then he is re-prompted.
*
*ARGUMENTS       
*   INPUT:
*	param	character	INTERIM parameter to be used to prompt
*                               the user.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtbool
*       SGS:
*              sgs_icurw, sgs_close
*       GNS:
*              gns_iwcg
*       INTERIM:
*              cnpar
*              
*STARLINK PARAMETERS
*       'param'  
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 24/1/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      character param*(*)

*
* DECLARE LOCAL VARIABLES
*
      character class*6 ! GNS device CLASS.
      logical   close   ! True when device is to be closed.
      integer	istat	! Temporary status value
      integer	iwkid	! Current GKS workstation identifier.

*
* GET CURRENT WORKSTATION IDENTIFIER
*
      call sgs_icurw( iwkid )

*
* INQUIRE THE WORKSTATION CLASS.
*
      istat = 0
      call gns_iwcg( iwkid, 'CLASS', class, istat)

*
* IF THE DEVICE IS A WORKSTATION WINDOW...
*
      if( class .eq. 'WINDOW' .and. istat .eq. 0 ) then

* 
* WAIT UNTIL THE USER GIVES A TRUE VALUE FOR THE GIVEN PARAMETER.
*
 10      close = .true.
         call gtbool( param, .true., close, istat )
         if( .not. close .and. istat .eq. 0 ) then
            call cnpar( param, istat )
            goto 10
         end if

      end if

*
* CLOSE SGS
*
      call sgs_close

*
* FINISH
*
  999 continue

      end
