      subroutine minim( type, xytol, radtol, bcktol, rms, ierr )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       MINIM.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (MEMCRDD.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/9/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	ierr
      real      rms,xytol,radtol,bcktol
      character type*6

*
* INCLUDE LOCAL PARAMETER DEFINITION
*
      include '(SF_PAR)'

*
* INCLUDE COMMON BLOCK DEFINITION
*
      include '(SF_COM)'

*
* DECLARE LOCAL VARIABLES
*
      real      bl( maxvar )
      real      bu( maxvar )
      integer   i
      integer	ifail
      integer   iw( liw )
      integer	j
      integer   n
      character prbuf*40
      real      w( lw )
      real      x( maxvar )

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* STORE THE MINIMISATION TYPE IN COMMON.
*
      SF_typ = type 

*
* COPY REQUIRED VARIABLES TO THE NAG ARGUMENT LIST, AND SET UP UPPER 
* AND LOWER BOUNDS.
*
      if( type(:5) .eq. 'POWER' ) then
         n = SF_npk
         do i = 1 , n
            x(i) = SF_pwr(i)
            bl(i) = 0.1
            bu(i) = 10.0
         end do

      else if( type .eq. 'RADIUS' ) then
         n = SF_npk
         do i = 1 , n
            x(i) = SF_rad(i)
            bl(i) = max( x(i) - radtol, 1.0E-2 )
            bu(i) = x(i) + radtol
         end do

      else if( type(:4) .eq. 'PEAK' ) then
         n = SF_npk
         do i = 1 , n
            x(i) = SF_pk(i)
            bl(i) = 0.3333*x(i)
            bu(i) = 3.0*x(i)
         end do

      else if( type(:2) .eq. 'XY' ) then
         n = 2*SF_npk
         do i = 1, SF_npk
            j = 2*i - 1
            x(j) = SF_xc(i)
            bl(j) = x(j) - xytol
            bu(j) = x(j) + xytol
            x(j+1) = SF_yc(i)
            bl(j+1) = x(j+1) - xytol
            bu(j+1) = x(j+1) + xytol
         end do

      else if( type(:4) .eq. 'BACK' ) then
         n = SF_npk
         do i = 1 , n
            x(i) = SF_bck(i)
            bl(i) = x(i) - bcktol
            bu(i) = x(i) + bcktol
         end do
         
      else
         ierr = 1
         call wrerr('BADTYP')
         go to 999

      end if

*
* CALL THE NAG ROUTINE TO DO THE MINIMISATION.
*
      ifail = 1
      call e04jae( n, 0, bl, bu, x, rms, iw, liw, w, lw, ifail )

*
* COPY FINAL VARIABLE VALUES TO COMMON.
*
      if( type(:5) .eq. 'POWER' ) then
         do i = 1, n
            SF_pwr(i) = x(i)
         end do

      else if( type .eq. 'RADIUS' ) then
         do i = 1, n
            SF_rad(i) = x(i)
         end do

      else if( type(:4) .eq. 'PEAK' ) then
         do i = 1, n
            SF_pk(i) = x(i)
         end do

      else if( type(:2) .eq. 'XY' ) then
         do i = 1, SF_npk
            j = 2*i - 1
            SF_xc(i) = x(j)
            SF_yc(i) = x(j+1)
         end do

      else if( type(:4) .eq. 'BACK' ) then
         do i = 1, n
            SF_bck(i) = x(i)
         end do
         
      else
         ierr = 1
         call wrerr('BADTYP')
         go to 999

      end if

*
* FINISH
*
  999 continue

      end
