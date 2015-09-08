      subroutine args_def1(config,istat)
*+
*   ARGS_DEF1
*
*   perform ARGS DEF1 order;
*   obtains current ARGS configuration
*
*   implemented using the ARGS order code DEF1
*
*   Given:  (common)
*     CHAN    I    in CARGS1.INC from ARGSLIB
*
*   Returned:  (argument)
*     CONFIG  I    ARGS configuration word
*     ISTAT   I    status return; 0 for success, 1 for fail
*
*   Called:
*     ARGS_IDCALLRD: ARGSLIB
*
*   J.A.Cooke/UOE/19Jan82
*-
      integer*2 bufin(5),bufout(5)
      integer config,istat,jstat,kstat
      integer*4 sys$qiow

      include 'LIBDIR:ARGS(CARGS1)'

      external io$_readlblk

*   set input buffer instruction.....
      bufin(5)='5501'X

*   do the qio.....

*      did not work .. new version of idcall .. don't have source
*      jstat=args_idcallrd(1,CHAN,bufin,6,bufout,516)

      nbin=5
      nbout=5
      jstat=sys$qiow(,%val(chan),%val(%loc(io$_readlblk)),
     -  kstat,,,bufin,%val(nbin),bufout,%val(nbout),,)

      if (jstat.eq.1) then

         istat=0

      else

         istat=1

      endif

      end
