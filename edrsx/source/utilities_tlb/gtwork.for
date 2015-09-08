      subroutine gtwork(name,format,size,iptr,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To aquire dynamic work space and return error numbers
*       appropriate for IRAS processing
*
*SOURCE
*       GTWORK.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS
*       name    character*(*)   The name to associate with the work
*                               space
*       format  character*(*)   String identifying type of space
*                               required. One of:
*                                       'REAL'    real*4
*                                       'DOUBLE'  real*8
*                                       'I*2'     integer*2
*                                       'INTEGER' integer*4
*                                       'BYTE'    byte
*
*       size    integer         No. of locations required in the work
*                               space
*   OUTPUTS
*       iptr    integer         Pointer to the start of the work space
*       ierr    integer         Status return
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr
*       INTERIM:
*               getdyn
*
*STARLINK PARAMETERS
*       NOSPACE/error/  Accessed if not able to get requested space
*       BADWORK/error/  Accessed if unrecognized format is requested
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           size,iptr,ierr
      character*(*)     name,format
*
* DECLARE LOCAL VARIABLES
*
      integer code      ! INTERIM format code
*
* GET INTERIM FORMAT CODE FROM FORMAT STRING
*
      if(format.eq.'REAL') then
         code=204
      else if(format.eq.'DOUBLE') then
         code=208
      else if(format.eq.'I*2') then
         code=102
      else if(format.eq.'INTEGER') then
         code=104
      else if(format.eq.'BYTE') then
         code=101
      else
         call wrerr('BADWORK')
         ierr=1
         goto 999
      endif
*
* CALL INTERIM ROUTINE GETDYN TO AQUIRE THE WORK SPACE
*
      call getdyn(name,code,size,iptr,ierr)
*
* MODIFY ERROR NUMBER TO FIT IN WITH IRAS MESSAGES
*
      if(ierr.ne.0) call wrerr('NOSPACE')
*
* FINISH
*
 999  continue

      end
