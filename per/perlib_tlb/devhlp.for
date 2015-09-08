      subroutine per_devhlp
 
*
*   This suibroutine simply types out some useful
*   information about the names of devices available
*   when using this software.
*
*   Written by K.F.Hartley at RGO on 2-3-84
*
      integer status
      call wruser('The following devices are available :-',status)
      call wruser(' ',status)
      call wruser('Those with cursors are:-',status)
      call wruser('      ARGS      GOC       CIFER     HP2648',status)
      call wruser('      TEK       OVERLAY',status)
      call wruser(' ',status)
      call wruser('Those without cursors are :-',status)
      call wruser('      VERS      PRIN      CC81      CALCOMP',status)
      call wruser('     LOCAL (Same as CALCOMP!)',status)
      call wruser(' ',status)
 
      end
 
 
 
