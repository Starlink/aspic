      subroutine args_poscl(pmin,pmax,delay)
*+
*   ARGS_POSC
*
*   Controls the data out enable of the ARGS such that the pixel
*   planes are enabled to the display one by one in a fixed
*   pattern.
*   With this routine, the pattern is such that the display
*   oscillates between planes PMIN and PMAX, the speed determined
*   by the parameter DELAY.
*
*   Two modes are available; change mode using the left hand
*   button.
*   Scan mode:
*   The speed may be changed using the centre two trackerball
*   buttons; exit via the right hand button.
*   Step mode:
*   The lh centre button (green) steps through the sequence;
*   the rh centre button (red) changes sequence direction.
*   Exit via the right hand button.
*
*   Given:  (arguments)
*	PMIN	I	low end bit plane (0-14)
*	PMAX	I	high end bit plane (1-15); must be > PMIN
*	DELAY	I	delay parameter (0-31)
*
*   Called:
*	LOAD_BPOSCL, WRITE_BPOSCL, RUN_BPOSCL: local
*
*   J.A.Cooke/UOE/22Mar82
*-

      integer pmin,pmax,delay
      integer nmin,nmax,ndel,ntemp
      integer*2 idat(3)

*   mask planes to range 0-15.....
      nmin=iand(pmin,'000F'X)
      nmax=iand(pmax,'000F'X)

*   mask delay to range 0-31.....
      delay=iand(delay,'001F'X)

*   check plane values.....
      if (nmin.gt.nmax) then
         ntemp=nmin
         nmin=nmax
         nmax=ntemp
      endif
      if (nmin.eq.nmax) then
         nmin=0
         nmax=15
      endif

*   input data to ARGS program.....
      idat(1)=nmin
      idat(2)=nmax
      idat(3)=delay

*   load and run ARGS program.....
      call load_bposcl
      call write_bposcl (idat)
      call run_bposcl

      end
