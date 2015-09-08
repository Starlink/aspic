      subroutine args_trlog (in,out,status)

*+  Translate logical name 'in' into 'out' until
*   translation fails (If fails, 'out' contains copy of 'in')
*
*   arguments
*       in      character      input logname
*       out     character      output logname
*       status  integer        error status (0/1=success/fail)

      external ss$_normal,ss$_notran
      integer status,start,length,sys$trnlog,nchar
      character in*(*),out*(*),buff*63

      status=%loc(ss$_normal)
      buff=in

      do while (status.and.status.ne.%loc(ss$_notran))
          call gen_flimcs(buff,start,length)
          status=sys$trnlog(buff(start:start+length-1),nchar,out,,,)
          buff=out(:nchar)
      enddo

      if (nchar.lt.len(out)) then
          out(nchar+1:)=' '
      endif

      if (status.and.out.ne.in) then
          status=0
      else
          status=1
      endif

      end
