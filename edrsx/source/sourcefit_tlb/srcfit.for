      subroutine srcfit( ipin, npix, nlin, inval, bscale, bzero, xlims,
     :                   ylims, fwhm, unit, ilevel,
     :                   power, fwhmtol, npeak, x0, y0, xytol, back, 
     :                   maxpk, bcktol, n, x, fits, ierr )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SRCFIT.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
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
      integer	ipin, npix, nlin, inval, npeak, n, unit, maxpk, ilevel,
     :          ierr
      real	bscale, bzero, xlims(2), ylims(2), fwhm, power, 
     :          fwhmtol, x0(npeak), y0(npeak), xytol, back, bcktol,
     :          fits( npix, nlin, npeak )
      real*8	x(n)

*
* DECLARE LOCAL VARIABLES
*
      real*8	a
      real*8	a2
      real*8    alpha
      real*8	bl( maxvar )
      real*8	bu( maxvar )
      real*8	c
      real*8	cjac
      real*8	clamda( maxvar )
      real*8	covs2
      real*8	fwhm1
      real*8	fwhm2
      real*8	fwhma
      real*8	fwhmb
      integer	i
      integer	ifail
      integer	ipf
      integer	ipfjac
      integer	ipiw
      integer	ipw
      integer	istate( maxvar )
      integer	iuser
      integer	ival
      integer	j
      integer	liwork
      integer	lwork
      integer	m
      integer	niter
      real*8	objf
      real*8    pa
      character prbuf*80
      real*8	r( maxvar, maxvar )
      real*8	rms
      real*8    snr
      real	sum
      real 	temp
      real*8	user

*
* EXTERNAL ROUTINES.
*
      external dummy
      external objfun

*
* INITILAISE THE ERROR STATUS TO INDICATE SUCCESS.
*
      ierr = 0

*
* COPY THINGS TO COMMON.
*
      SF_inv = inval
      SF_ipi = ipin
      SF_nln = nlin
      SF_npx = npix
      SF_xlo = nint( xlims(1) )
      SF_xhi = nint( xlims(2) )
      SF_ylo = nint( ylims(1) )
      SF_yhi = nint( ylims(2) )

*
* SET UP THE INITIAL VALUES FOR THE UNSCALED NAG ROUTINE VARIABLES, 
* TOGETHER WITH THE CORRESPONDING LIMITS.
*

      alpha = (0.48045)**(1.0/power)

      do i = 1, npeak
         j = 7*( i - 1 )

         call interp( %val( ipin ), npix, nlin, inval, x0(i), y0(i),
     :               x( j + 1 ) )

         if( x( j + 1 ) .eq. inval ) then
            call wrerr( 'INVPEAK' )
            ierr = 1
            go to 999

         else if( x( j + 1 ) .le. back ) then
            call wrerr( 'HIGHBACK')
            ierr = 2
            go to 999

         else
            x( j + 1 ) = x( j + 1 ) - back
         end if
         bl( j + 1 ) = 0.0
         bu( j + 1 ) = 10.0*maxpk

         x( j + 2 ) = 4.0*alpha/(fwhm**2)
         bl( j + 2 ) = 4.0*alpha/( (fwhm+fwhmtol)**2 )
         bu( j + 2 ) = 4.0*alpha/( ( max( 1.0E-6, fwhm-fwhmtol) )**2 )

         x( j + 3 ) =  x0( i ) - 1.0D-3
         bl( j + 3 ) = max( xlims(1), x0( i ) - xytol )
         bu( j + 3 ) = min( xlims(2), x0( i ) + xytol )

         x( j + 4 ) =  x( j + 2 )
         bl( j + 4 ) = bl( j + 2 )
         bu( j + 4 ) = bu( j + 2 )

         x( j + 5 ) =  y0( i ) - 1.0D-3
         bl( j + 5 ) = max( ylims(1), y0( i ) - xytol )
         bu( j + 5 ) = min( ylims(2), y0( i ) + xytol )

         x( j + 6 ) =  0.0
         bu( j + 6 ) = alpha*
     :                 (1.0/( bl( j + 4 )**2 ) - 1.0/( bu( j + 4 )**2 ))
         bl( j + 6 ) = -bu( j + 6 )

         x( j + 7 ) =  0.5*power
         bl( j + 7 ) = 0.01
         bu( j + 7 ) = 5

      end do

      x( n ) =  back
      bl( n ) = back - bcktol
      bu( n ) = back + bcktol

*
* COUNT THE NUMBER OF VALID PIXELS IN THE REGION TO BE USED FOR THE FIT.
*
      call count( npix, nlin, %val( ipin ), inval, SF_xlo, SF_xhi, 
     :            SF_ylo, SF_yhi, m ) 
      if( m .eq. 0 ) then
         call wrerr( 'NOVALID')
         ierr = 1
         go to 999
      end if

*
* GET WORKSPACE.
*
      call gtwork( 'F', 'DOUBLE', m, ipf, ierr )
      if( ierr .ne. 0 ) go to 999

      call gtwork( 'FJAC', 'DOUBLE', m*n, ipfjac, ierr )
      if( ierr .ne. 0 ) go to 999

      liwork = 3*n 
      call gtwork( 'IWORK', 'INTEGER', liwork, ipiw, ierr )

      lwork = 20*n + m*( n + 3 )
      call gtwork( 'WORK', 'DOUBLE', lwork, ipw, ierr )

*
* ESTABLISH THE VARIABLE AND RESIDUAL SCALINGS.
*
      SF_a = 0.0
      SF_b = 1.0
      do i = 1, n
         SF_c( i ) = 0.0
         SF_k( i ) = 1.0
      end do

      call objfun( 2, m, n, m, x, %val(ipf), %val(ipfjac), 1, iuser, 
     :             user )

      call getrms( m, %val(ipf), SF_a, SF_b, rms )
      rms = bscale*rms

      if( ilevel .ge. 1.0 ) then
         write( prbuf, 18 ) rms
 18      format( '  Initial RMS deviation per pixel in source area is ',
     :           G13.6 )
         call lbgone( prbuf( 53: ) )
         call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write(unit,*) prbuf 
      end if

      do i = 1, npeak
         snr = x(7*i-6)*bscale/rms

         if( ilevel .ge. 2 ) then
            write(prbuf,19) i,snr
            call lbgone( prbuf( 29: ) )
            call wruser( prbuf, ierr )
            if( unit .gt. 0 ) write(unit,*) prbuf 
 19         format('  Initial SNR for peak ',I1,' is ',G13.6 )
         end if

         if( snr .lt. 2.0 ) x(7*i-6) = 2.0*rms/bscale

      end do

      call setsca( m, n, %val( ipf ), %val( ipfjac ), x, bl, bu )

*
* SCALE THE INITIAL VARIABLE VALUES AND VARIABLE BOUNDS.
*
      do i = 1, n
         x( i ) = x( i )*SF_k( i ) + SF_c( i )
         bl( i ) = bl( i )*SF_k( i ) + SF_c( i )
         bu( i ) = bu( i )*SF_k( i ) + SF_c( i )
      end do         

*
* CALL THE NAG MINIMISATION ROUTINE.
*
      call e04urf( '  Nolist               ')
      call e04urf( '  Major Print Level   0')

      ifail = 1
      call e04upf( m, n, 0, 0, 1, 1, m, maxvar, a, bl, bu, 
     :             dummy, objfun, niter, istate, c, cjac, %val(ipf), 
     :             %val(ipfjac), clamda, objf, r, x, %val(ipiw), liwork,
     :             %val(ipw), lwork, iuser, user, ifail )

*
* IF NAG COULDN'T FIND A GOOD FIT, WARN THE USER.
*
      if( ifail. ne. 0 ) call wrerr( 'NAGERR')

*
* REMOVE THE SCALING FROM THE FINAL VARIABLE VALUES.
*
      do i = 1, n
         x( i ) = ( x( i ) - SF_c( i ) )/SF_k( i )
      end do         


*
* EVALUATE THE FINAL RMS ERROR BETWEEN THE FIT AND THE DATA.
*
      call getrms( m, %val(ipf), SF_a, SF_b, rms )
      rms = bscale*rms

*
* DISPLAY THE RFINAL RMS RESIDUAL.
*
      if( ilevel .gt. 0 ) call wruser( ' ', ierr )      

      write( prbuf, 20 ) rms
 20   format( '  Final RMS deviation per pixel in source area is ',
     :        G13.6 )
      call lbgone( prbuf( 51: ) )

      if( ilevel .gt. 0 ) then
         call wruser( prbuf, ierr )
         call wruser( ' ', ierr )      
      end if

      if( unit .gt. 0 ) then
         write( unit, * ) ' '
         write( unit, * ) prbuf
         write( unit, * ) ' '
      end if

*
* LOOP THROUGH EVERY PEAK.
*
      do i = 1, npeak
         j = 7*( i - 1 )

*
* PRODUCE AN IMAGE OF EACH FITTED SOURCE, AND FIND THE TOTAL DATA SUM 
* IN THE SOURCE.
*
         call srcgen( fits(1,1,i), npix, nlin, x(j+1), sum )

*
* EVALUATE THE POSITION ANGLE OF AN ELLIPSE AXIS (ACW FROM 
* THE DIRECTION OF INCREASING PIXEL NUMBER) IN THE RANGE -90 DEG TO
* +90 DEG
*
         if( x( j + 2 ) .ne. x( j + 4 ) ) then
            pa = 0.5*atan( x( j + 6 )/ ( x( j + 2 ) - x( j + 4 ) ) )
         else
            if( x( j + 6 ) .gt. 0.0 ) then
               pa = 0.7853981634
            else
               pa = -0.7853981634
            end if
         end if

*
* EVALUATE THE FWHM ALONG THE EACH ELLIPSE AXIS.
*
         a2 = 2.0*( 0.6931472**(1.0/x( j + 7 )))
         covs2 = x( j + 6 )/sin( 2.0*pa )
         
         fwhma = 2.0*sqrt( abs( 
     :                     a2/( x( j + 2 ) + x( j + 4 ) + covs2 ) ) )
         fwhmb = 2.0*sqrt( abs(  
     :                     a2/( x( j + 2 ) + x( j + 4 ) - covs2 ) ) )

*
* IF THE CROSS PRODUCT COEFFICIENT IS POSITIVE THE MAJOR AXIS WILL BE
* AT A POSITION ANGLE BETWEEN 0 AND -90 DEGREES
*
         if( x( j + 6 ) .gt. 0.0 ) then
            if( pa .gt. 0.0 .and. pa .le. 1.570796327 ) then
               fwhm1 = min( fwhma, fwhmb )
               fwhm2 = max( fwhma, fwhmb )
            else
               fwhm2 = min( fwhma, fwhmb )
               fwhm1 = max( fwhma, fwhmb )
            end if

*
* IF THE CROSS PRODUCT COEFFICIENT IS NEGATIVE THE MAJOR AXIS WILL BE
* AT A POSITION ANGLE BETWEEN 0 AND +90 DEGREES
*
         else
            if( pa .gt. 0.0 .and. pa .le. 1.570796327 ) then
               fwhm1 = max( fwhma, fwhmb )
               fwhm2 = min( fwhma, fwhmb )
            else
               fwhm2 = max( fwhma, fwhmb )
               fwhm1 = min( fwhma, fwhmb )
            end if
         end if

*
* MAKE "AXIS 1" THE MAJOR AXIS.
*
         if( fwhm1 .lt. fwhm2 ) then
            a2 = fwhm1
            fwhm1 = fwhm2
            fwhm2 = a2
            pa = pa + 1.570796327 
         end if

*
* CONVERT THE POSITION ANGLE FROM BEING RELATIVE TO THE DIRECTION OF
* INCREASING PIXEL NUMBER, TO BEING RELATIVE TO THE DIRECTION OF
* INCREASING LINE NUMBER.
*
         pa = pa - 1.570796327 
      
*
* DISPLAY THE RESULTS FOR THIS PEAK
*
         write( prbuf, 30 ) i
  30     format( '  Source ',I2,' ...' )         
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 40 ) x( j + 3 ), x( j + 5 )
  40     format( '     Centre coordinates      : ( ',F7.1,', ',F7.1,
     :           ' )' )
         call lbgone( prbuf( 41: ) )
         call lbgone( prbuf( 33: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 50 ) fwhm1
  50     format( '     FWHM on axis 1          : ',F7.1 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 60 ) fwhm2
  60     format( '     FWHM on axis 2          : ',F7.1 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf
         
         write( prbuf, 70 ) pa*57.29577951
  70     format( '     Position angle of axis 1: ',F7.1 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 80 ) bscale*x( j + 1 )
  80     format( '     Source amplitude        : ',G13.6 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 90 ) 2.0*x( j + 7 )
  90     format( '     Power index             : ',F8.2 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         write( prbuf, 100 ) bscale*sum
 100     format( '     Total data sum in source: ',G13.6 )
         call lbgone( prbuf( 32: ) )
         if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
         if( unit .gt. 0 ) write( unit, * ) prbuf

         if( ilevel .gt. 0 ) call wruser( ' ', ierr )
         if( unit .gt. 0 ) write( unit, * ) ' '

      end do

      write( prbuf, 110 ) bscale*x( n ) + bzero
 110  format( '     Background level        : ',G13.6 )
      call lbgone( prbuf( 32: ) )
      if( ilevel .gt. 0 ) call wruser( prbuf, ierr )
      if( unit .gt. 0 ) write( unit, * ) prbuf

      if( ilevel .gt. 0 ) call wruser( ' ', ierr )
      if( unit .gt. 0 ) write( unit, * ) ' '

*
* FINISH
*
  999 continue

      end
