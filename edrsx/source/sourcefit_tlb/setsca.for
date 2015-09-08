      subroutine setsca( m, n, f, fjac, x, bl, bu )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SETSCA.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*
*   OUTPUTS:
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
* INCLUDE LOCAL PARAMETERS.
*
      include '(SF_PAR)'

*
* INCLUDE COMMON BLOCK.
*
      include '(SF_COM)'

*
* DECLARE ARGUMENTS
*
      integer	m, n
      real*8	f( m ),	fjac( m, n ), x( n ), bl(n), bu(n)

*
* DECLARE LOCAL VARIABLES
*
      integer	i
      integer	j
      real*8	sum
      real*8	sumsq

*
* FIND THE MEAN AND STANDARD DEVIATION OF THE F VECTOR.
*
      sum = 0.0
      sumsq = 0.0
      do i = 1, m
         sum = sum + f(i)
         sumsq = sumsq + f(i)**2
      end do

*
* SET UP THE OBJECTIVE FUNCTION SCALING CONSTANTS.
*
      SF_a = 0.0
      SF_b = sqrt( sumsq/m )

*
* SET UP THE VARIABLE SCALING CONSTANTS.
*
      do j = 1, n

         sum = 0.0
         do i = 1, m
            sum = sum + fjac( i, j )*( f( i ) - SF_a )/SF_b
         end do

         SF_k( j ) = max( abs( sum/SF_b ), 10.0/(bu(j)-bl(j)) )
         SF_c( j ) = -SF_k( j )*x( j )

      end do

*
* THE MULTIPLICATIVE SCALING FACTOR FOR THE CROSS PRODUCT TERM IN THE 
* EXPONENT OF THE FITTING FUNCTION SHOULD BE EQUAL TO THE GEOMETRIC
* MEAN OF THE FACTORS FOR THE X*X AND Y*Y TERMS.
*
      do j =1, n-7, 7
         SF_k( j + 5 ) = sqrt( SF_k( j + 1 )*SF_k( j + 3 ) )
         SF_c( j + 5 ) = -SF_k( j + 5 )*x( j + 5 )
      end do

*
* FINISH
*
      end
