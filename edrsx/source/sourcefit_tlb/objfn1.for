      subroutine objfn1( npix, nlin, image, mode, m, n, ldfj, x, f, 
     :                   fjac )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       OBJFN1.FOR in SOURCEFIT.TLB
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
      integer npix, nlin, m, n, ldfj, mode
      integer*2 image( npix, nlin )
      real*8  x(n), f(m), fjac(ldfj,n)

*
* DECLARE LOCAL VARIABLES
*
      real*8	back
      integer	basvar
      real*8	c1
      real*8	c2
      real*8	c3
      real*8	c4
      real*8	c5
      real*8	c6
      real*8	c7
      real*8	c8
      real*8	c9
      real*8	c10
      real*8	c11
      real*8	dfac1
      real*8	dfac2
      real*8	dfac3
      real*8	dfac4
      real*8	dfac5
      real*8	dfac6
      real*8	dfac7
      real*8	dfacbk
      integer	i
      integer	ival
      integer	lin
      integer	pix
      real*8	x1
      real*8	x2
      real*8	x3
      real*8	x4
      real*8	x5
      real*8	x6
      real*8	x7

      real*8    dh(maxvar),sum,sumsq,mean,sigma
      integer   j

*
* STORE THE UNSCALED BACKGROUND VALUE IN A SCALAR VARIABLE.
*
      back = ( x( n ) - SF_c( n ) )/SF_k( n )

*
* INITIALISE THE RESIDUAL ARRAY TO BE THE FIT TO THE BACKGROUND VALUE
* MINUS THE DATA VALUE, IN THE REGION BEING USED TO FIT THE DATA. 
* INVALID PIXELS ARE IGNORED.
*
      i = 0 

      do lin = SF_ylo, SF_yhi
         do pix = SF_xlo, SF_xhi

            ival = image( pix, lin )
            if( ival .ne. SF_inv ) then
               i = i + 1
               f( i ) = back - ival
            end if

         end do
      end do            


*
* LOOP ROUND EACH PEAK. EACH PEAK HAS SIX ASSOCIATED VARIABLES IN THE 
* ARRAY X, STARTING AT INDEX "BASVAR".
*
      do basvar = 1, n - 7, 7

*
* STORE THE SEVEN VARIABLES ASSOCIATED WITH THIS PEAK.
*
         x1 = ( x( basvar ) - SF_c( basvar ) )
     :         /SF_k( basvar )
         x2 = ( x( basvar + 1 ) - SF_c( basvar + 1 ) )
     :         /SF_k( basvar + 1 )
         x3 = ( x( basvar + 2 ) - SF_c( basvar + 2 ) )
     :         /SF_k( basvar + 2 )
         x4 = ( x( basvar + 3 ) - SF_c( basvar + 3 ) )
     :         /SF_k( basvar + 3 )
         x5 = ( x( basvar + 4 ) - SF_c( basvar + 4 ) )
     :         /SF_k( basvar + 4 )
         x6 = ( x( basvar + 5 ) - SF_c( basvar + 5 ) )
     :         /SF_k( basvar + 5 )
         x7 = ( x( basvar + 6 ) - SF_c( basvar + 6 ) )
     :         /SF_k( basvar + 6 )

*
* SET UP DERIVATIVE SCALING FACTORS.
*
         dfac1 = 1.0/( SF_b*SF_k( basvar ) )
         dfac2 = 1.0/( SF_b*SF_k( basvar + 1 ) )
         dfac3 = 1.0/( SF_b*SF_k( basvar + 2 ) )
         dfac4 = 1.0/( SF_b*SF_k( basvar + 3 ) )
         dfac5 = 1.0/( SF_b*SF_k( basvar + 4 ) )
         dfac6 = 1.0/( SF_b*SF_k( basvar + 5 ) )
         dfac7 = 1.0/( SF_b*SF_k( basvar + 6 ) )

*
* LOOP ROUND EACH PIXEL IN THE AREA OF THE INPUT IMAGE BEING USED IN
* THE FIT.
*
         i = 0

         do lin = SF_ylo, SF_yhi
            do pix = SF_xlo, SF_xhi

*
* PASS ON IF THE PIXEL IS INVALID.
*
               if( image( pix, lin ) .ne. SF_inv ) then

*
* INCREMENT THE COUNTER INTO THE OUTPUT ARRAYS.
*
                  i = i + 1

*
* EVALUATE VARIOUS INTERMEDIATE VALUES.
*
                  c1 = pix - x3
                  c2 = lin - x5
                  c3 = c1*c1
                  c4 = c2*c2
                  c5 = c1*c2
                  c6 = max( 1.0D-10, x2*c3 + x4*c4 + x6*c5 )
                  c7 = c6**( x7 - 1.0 )
                  c8 = c7*c6
                  c9 = exp( -c8 )
                  c10 = x1*c9
                  c11 = c10*c7*x7
   
*
* ADD THE CURRENT PEAKS VALUE TO THE RESIDUAL.
*
                  f( i ) = f( i ) +  c10

*
* EVALUATE EACH OF THE SEVEN PARTIAL DERIVATIVES, INCLUDING SCALING 
* FACTORS.
*
                  fjac( i, basvar     ) = dfac1*c9
                  fjac( i, basvar + 1 ) = -dfac2*c11*c3
                  fjac( i, basvar + 2 ) = 2.0*x2*dfac3*c11*c1
                  fjac( i, basvar + 3 ) = -dfac4*c11*c4
                  fjac( i, basvar + 4 ) = 2.0*x4*dfac5*c11*c2
                  fjac( i, basvar + 5 ) = -dfac6*c11*c5
                  fjac( i, basvar + 6 ) = -dfac7*c10*c8*log( c6 )

               end if

            end do
         end do

      end do

*
* SCALE THE RESIDUALS AND SET UP THE DERIVATIVES OF EACH RESIDUAL WRT 
* THE BACKGROUND VALUE.
*
      dfacbk = 1.0/( SF_b*SF_k( n ) )

      sum = 0.0
      sumsq = 0.0

      do i = 1, m

         f( i ) = ( f(i) - SF_a )/SF_b
         fjac( i, n ) = dfacbk

         sum = sum + f( i )
         sumsq = sumsq + f( i )**2

      end do            

      mean = sum/m
      sigma = sqrt( sumsq/m - mean**2 )
C      write(*,*) ' '
C      write(*,*) ' '
C      write(*,*) ' '
C      write(*,41) 0.5*sumsq, mean, sigma
 41   format('  H=',G13.6,'       mean(h)=',G13.6,' sigma(h): ',G13.6 )

C      write(*,*) ' '
      do basvar = 1, n-7, 7
C         write(*,42) ( x(basvar+j), j = 0,6 )
 42      format(7(G11.4,' ') )
      end do
C      write(*,43) x(n)
 43   format( G11.4 )

      do j = 1, n
         dh(j) = 0.0
      
         do i = 1, m
            dh(j) = dh(j) + f(i)*fjac(i,j)
         end do

      end do

C      write(*,*) ' '
      do basvar = 1, n-7, 7
C         write(*,42) ( dh(basvar+j), j = 0,6 )
      end do
C      write(*,43) dh(n)

*
* FINISH
*
 999  continue

      end
