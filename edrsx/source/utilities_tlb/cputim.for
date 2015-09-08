      real function cputim(dummy)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the current process cpu time in seconds
*
*SOURCE
*       CPUTIM.FOR in UTILITIES.TLB
*
*METHOD
*       Call system service SYS$GETJPI
*
*
*ARGUMENTS
*   INPUTS:
*       dummy   real    A dummy argument to make the function call
*                       possible
*   OUTPUTS:
*       (function
*        value) real    cpu time in seconds used by process since
*                       creation
*
*USED BY
*       Programs under test
*
*VAX SPECIFICS
*       %val
*       end of line comments
*       system services
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 9/9/87
*----------------------------------------------------------------------
*
      include '($jpidef)'
      integer sys$getjpi,bufad,retad,buffer
      integer*2 code,blen
      common /itemlist/blen,code,bufad,retad,lstend
*
* SET UP ITEMLIST FOR $GETJPI
*
      blen=4             ! buffer length in bytes
      code=jpi$_cputim   ! code for required information
      bufad=%loc(buffer) ! address of buffer
      retad=0            ! address of buffer to contain actual length
                         !     of info returned by getjpi (not used)
      lstend=0           ! end of item list marker
*
* CALL SYSTEM SERVICE $GETJPI TO GET THE INFORMATION
*
      istat=sys$getjpi(,,,blen,,,)
      if(.not.istat) call lib$stop(%val(istat))
*
* CONVERT FROM 100ths SECONDS TO SECONDS
*
      cputim=buffer/100.0

      end
