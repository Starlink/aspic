      subroutine SOURCE
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       SOURCE.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*       None
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
*       D.S. Berry (MAVAD::DSB) 7/9/91
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE LOCAL PARAMETERS.
*
      include '(SF_PAR)'

*
* DECLARE LOCAL VARIABLES
*
      real	back
      real	bcktol
      real	bscale
      real      bzero
      real	cutoff
      character cval*1
      real	dev
      character file*50
      real      fwhm
      real	fwhmtol
      integer	i
      integer	ierr
      integer	ilevel
      integer	imax
      integer	imin
      integer	inval
      integer	ipim
      integer	ipout
      integer	ipxyl
      integer	ipx
      integer	ipy
      integer   ipwork
      integer	ival
      integer	lstlen
      real	mean
      integer	n
      integer	nitem
      integer	nlin
      integer	npix
      integer	nval
      character pname*7
      real	power
      real	rscale
      real	rzero
      real	rval
      integer	unit
      real*8   	x( maxvar )
      real      xlims(2)
      real	xytol
      real      ylims(2)

      data unit/0/

*
* GET THE INPUT IMAGE CONTAINING THE FEATURES TO BE FITTED.
*     
      call gt2dir( 'IMAGE', 102, .false., npix, nlin, ipim, ierr )
      if( ierr .ne. 0 ) go to 999

*
* INPUT IMAGE OBTAINED SUCCESSFULLY... EXTRACT REQUIRED DESCRIPTOR
* ITEMS
*
      inval = -100000
      bscale = 1.0
      bzero = 0.0
      call gtdscr( 'IMAGE', 'INVAL', 'INTEGER', inval, rval, cval, ierr)
      call gtdscr( 'IMAGE', 'BSCALE', 'REAL', ival, bscale, cval, ierr )
      call gtdscr( 'IMAGE', 'BZERO', 'REAL', ival, bzero, cval, ierr )
 
*
* OBTAIN THE XY LIST CONTAINING THE INITIAL GUESSES AT THE SOURCE 
* POSITIONS.
*
      call gtxylr( 'XYLIST', .false., nitem, lstlen, ipxyl, ierr )
      if( ierr .ne. 0 ) go to 999

*
* ABORT IF TOO MANY SOURCES ARE TO BE FITTED.
*
      if( lstlen .gt. maxsrc ) then
         call wrerr( 'TOOMANY' )
         go to 999
      end if

*
* OBTAIN WORKSPACE FOR INPUT XY LIST
*
      call gtwork( 'X', 'INTEGER', lstlen, ipx, ierr )
      if( ierr. eq. 0 ) call gtwork( 'Y', 'INTEGER', lstlen, ipy, ierr )
      if( ierr .ne. 0 ) go to 999
 
*
* COPY INPUT X AND Y TO WORKSPACE
*
      call extlst( %val(ipxyl), nitem, lstlen, %val(ipx), 21, 24 )
      call extlst( %val(ipxyl), nitem, lstlen, %val(ipy), 25, 28 )

*
* GET INFORMATION LEVEL TO DISPLAY.
*
      ilevel = 1.0
      call getpar( 'ILEVEL', 'INTEGER', 1, 0.0, 3.0, .true., ilevel, 
     :              rval, ierr )

*
* GET A GUESS AT THE MEAN POWER INDEX.
*
      power = 2.0
      call getpar( 'POWER', 'REAL', 1, 0.2, 5.0, .true., ival, power, 
     :              ierr )

*
* GET A GUESS AT THE MEAN SOURCE WIDTH (FWHM).
*
      fwhm = 3.0
      call getpar( 'FWHM', 'REAL', 1, 1.0E-6, real( min(npix,nlin) ),
     :             .true., ival, fwhm, ierr )

*
* SET UP DEFAULTS FOR THE REGION OF THE IMAGE TO BE INCLUDED IN THE FIT.
*
      call region( lstlen, %val(ipx), %val(ipy), npix, nlin, fwhm, 
     :             xlims, ylims )

*
* GET THE ACTUAL REGION OF THE IMAGE TO BE INCLUDED IN THE FIT.
*
      call gtlims( 'XLIMS', xlims, ierr )
      if( ierr .eq. 0 ) call gtlims( 'YLIMS', ylims, ierr )
      if( ierr .ne. 0 ) go to 999

      xlims(1) = max( 1.0, xlims(1) )
      xlims(2) = min( real(npix), xlims(2) )

      ylims(1) = max( 1.0, ylims(1) )
      ylims(2) = min( real(nlin), ylims(2) )

*
* GET A GUESS AT THE TOTAL MEAN BACKGROUND LEVEL.
*
      call imstat( %val(ipim), npix, nlin, inval, imax, imin, mean, dev,
     :             nval, ierr )
      if( ierr .ne. 0 ) go to 999

      back = max( real(imin), mean - 2.0*dev )*bscale + bzero
      call getpar( 'BACKGRND', 'REAL', 1, -1.0E20, 1.0E20, .true., ival,
     :              back, ierr )

*
* CONVERT THE VALUE INTO THE BACKGROUND PER SOURCE.
*
      back = back/lstlen
      back = ( back - bzero ) / bscale

*
* GET THE MAXIMUM SHIFT ALLOWED FROM THE INITIAL X AND Y VALUES.
*
      xytol = 20.0
      call getpar( 'XYTOL', 'REAL', 1, 0.0, 1.0E20, .true., ival,
     :              xytol, ierr )

*
* GET THE MAXIMUM SHIFT ALLOWED FROM THE INITIAL SOURCE WIDTH VALUE.
*
      fwhmtol = 10.0
      call getpar( 'FWHMTOL', 'REAL', 1, 0.0, 1.0E20, .true., ival,
     :              fwhmtol, ierr )

*
* GET THE MAXIMUM SHIFT ALLOWED FROM THE INITIAL BACKGROUND VALUE.
*
      bcktol = 2.0*dev*bscale
      call getpar( 'BACKTOL', 'REAL', 1, 0.0, 1.0E20, .true., ival,
     :              bcktol, ierr )
      bcktol = bcktol/bscale

*
* OPEN A LOG FILE.
*
      unit = 10
      file = 'SOURCEFIT.LOG'
      call gtfile( 'LOGFILE', unit, .true., 'NEW', file, ierr )
      if( ierr .ne. 0 ) unit = 0

*
* GET WORK SPACE TO HOLD THE FINAL FITTED SOURCE IMAGES.
*
      call gtwork( 'FITS','REAL',npix*nlin*lstlen,ipwork,ierr)
      if( ierr .ne. 0 ) go to 999

*
* CALCULATE THE TOTAL NUMBER OF PARAMETERS IN THE FIT
*
      n = 7*lstlen+1

*
* FIT EACH OF THE IDENTIFIED SOURCES IN THE IMAGE.
*
      call srcfit( ipim, npix, nlin, inval, bscale, bzero, xlims, 
     :             ylims, fwhm, unit, ilevel,
     :             power, fwhmtol, lstlen, %val(ipx), %val(ipy), xytol,
     :             back, imax - imin,
     :             bcktol, n, x, %val(ipwork), ierr )

      if( ierr .ne. 0 ) go to 999      

*
* WRITE OUT THE BACKGROUND VALUE TO A PROGRAM PARAMETER.
*
      call wrkeyr( 'BACKFIT', bscale*real( x(n) )+bzero, 1, 
     :              ierr )

*
* GET THE CUTOFF VALUE FOR USE WHEN CREATING RATIO IMAGES.
*
      cutoff = 0.05
      call getpar( 'CUTOFF', 'REAL', 1, 0.0, 1.0E20, .true., ival,
     :              cutoff, ierr )

*
* PRODUCE IMAGE HOLDING SUM OF THE FITTED SOURCES WITH FITTED 
* BACKGROUND ADDED.
*
      call gt2diw( 'IMGSUM', 102, .true., npix, nlin, ipout, ierr )
      if( ierr. eq. 0 ) then
         call sumout( %val(ipwork), npix, nlin, lstlen, %val( ipout ), 
     :                nint( x(n) ), inval )
         call cydscr( 'IMAGE', 'IMGSUM', ierr )
         call frdata( 'IMGSUM', ierr )
      end if

*
* LOOP ROUND EACH SOURCE.
*
      do i = 1, lstlen

*
* PRODUCE AN IMAGE HOLDING THE FITTED SOURCE WITH NO BACKGROUND ADDED
*
         write( pname, '(A,I1)' ) 'IMGSRC',i
         call gt2diw( pname, 102, .true., npix, nlin, ipout, ierr )

         if( ierr .eq. 0 ) then
            call srcout( %val(ipwork), npix, nlin, lstlen, i, 
     :                   %val( ipout ), nint(-bzero/bscale), inval )
            call cydscr( 'IMAGE', pname, ierr )
            call frdata( pname, ierr )
         end if

*
* PRODUCE AN IMAGE HOLDING THE RATIO OF THIS SOURCE TO THE SUM OF 
* SOURCES (WITH NO BACKGROUNDS ADDED).
*
         pname = ' '
         write( pname, '(A,I1)' ) 'RATIO',i
         call gt2diw( pname, 102, .true., npix, nlin, ipout, ierr )

         if( ierr .eq. 0 ) then
            call ratout( %val(ipwork), npix, nlin, lstlen, i, 
     :                   %val( ipout ), inval, real( x(i*7-6)*cutoff ), 
     :                   rscale, rzero )
            call cydscr( 'IMAGE', pname, ierr )
            call ptdscr( pname, 'BSCALE', 'REAL', ival, rscale, cval, 
     :                   ierr )
            call ptdscr( pname, 'BZERO', 'REAL', ival, rzero, cval, 
     :                   ierr )
            call frdata( pname, ierr )
         end if

      end do

  999 continue

*
* CLOSE THE LOG FILE.
*
      if( unit .gt. 0 ) then
         close( unit )
         call wruser( ' ', ierr )
         call wruser( '   Results logged to '//file, ierr )
         call wruser( ' ', ierr )
      end if

*
* RELEASE THE INPUT IMAGE AND XY LIST.
*
      call frdata( 'IMAGE', ierr )
      call frdata( 'XYLIST', ierr )

      end
