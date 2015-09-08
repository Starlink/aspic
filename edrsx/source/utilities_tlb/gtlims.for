      subroutine gtlims(name,lims,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get a pair of limits from user ensuring that
*       lims(2) is greater than lims(1)
*
*SOURCE
*       GTLIMS.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       name    character       Name of starlink parameter to use
*   OUTPUTS:
*       lims(2) real            Limits
*       ierr    integer         status return:  0 - Success
*                                               1 - Too many bad values
*                                               2 - Invalid parameter
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr
*       INTERIM:
*               cnpar,rdkeyr
*
*STARLINK PARAMETERS
*       'name'/read/    The content of string 'name' is a paremeter used
*                       to aquire the limits from the user
*       TOOBAD/error/   Accessed if too many bad values are given
*       EQULIMS/error/  Accessed if the limits given are equal
*       BADNAME/error/  Accessed if the parameter name given does not
*                       exist
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*) name
      real lims(2)
      integer ierr
*
* DECLARE LOCAL VARIABLES
*
      integer ival      ! Number of values returned by rdkeyr
      integer temp      ! Temporary storage used for swapping limits
      logical more      ! true if a valid pair of limits has not yet
                        ! been aquired
      integer nbad      ! Number of bad values given by user so far
*
* LOOP ROUND UNTIL MORE IS SET FALSE EITHER DUE TO A VALID PAIR
* OF LIMITS BEING AQUIRED OR DUE TO TOO MANY BAD VALUES HAVING
* BEEN GIVEN BY THE USER
*
      more=.true.
      nbad=0
      do while (more)
         call rdkeyr(name,.true.,2,lims,ival,ierr)
         if(ierr.le.1) then
            if(lims(2).eq.lims(1)) then
               call wrerr('EQULIMS')
               nbad=nbad+1
               if(nbad.ge.3) then
                  call wrerr('TOOBAD')
                  ierr=1
                  more=.false.
               else
                  call cnpar(name,ierr)
               endif
            else
               if(lims(2).lt.lims(1)) then
                  temp=lims(2)
                  lims(2)=lims(1)
                  lims(1)=temp
               endif
               more=.false.
               ierr=0
            endif
         else
            call wrerr('BADNAME')
            ierr=2
            more=.false.
         endif
      enddo

      end
