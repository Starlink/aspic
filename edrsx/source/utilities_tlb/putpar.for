      subroutine putpar(name,type,ival,rval,cval,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To assign a value to an output starlink parameter
*
*SOURCE
*       PUTPAR.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       name    character       The name of the parameter
*       type    character       The type of data REAL, INTEGER, or
*                               CHARACTER
*       ival    integer         The integer value
*       rval    real            The real value
*       cval    character       The character value
*   OUTPUTS:
*       ierr    integer         Error status: 0 - Success
*                                             4 - Parameter not defined
*                                            10 - TYPE not recognized
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       INTERIM:
*               wrkeyi,wrkeyc,wrkeyr
*
*STARLINK PARAMETERS
*       'name'/write/   The argument 'name' contains the name of the
*                       parameter to be written
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 23/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*)     name,type,cval
      real              rval
      integer           ival,ierr
*
* DECLARE LOCAL VARIABLES
*
      real      rtemp(1)
      integer   itemp(1)
      character ctemp(1)*63
*
* IF TYPE IS REAL, CALL WRKEYR
*
      if(type.eq.'REAL') then
         rtemp(1)=rval
         call wrkeyr(name,rtemp,1,ierr)
*
* IF TYPE IS INTEGER CALL WRKEYI
*
      else if(type.eq.'INTEGER') then
         itemp(1)=ival
         call wrkeyi(name,itemp,1,ierr)
*
* IF TYPE IS CHARACTER, CALL WRKEYC
*
      else if(type.eq.'CHARACTER') then
         ctemp(1)=cval
         call wrkeyc(name,ctemp,1,ierr)
*
* IF TYPE WAS NOT RECOGNIZED, RETURN WITH IERR=10
*
      else
         ierr=10
      endif
*
* FINISH
*
      end
