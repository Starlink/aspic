	subroutine crb_ccd17
C+
C   This is a short-term solution to a problem created by
C   the La Palma CCD - it writes unsigned 16 bit integers
C   which FITS believes to be signed ones.
C
C   This simply reads the data and converts to REAL*4 format
C   (Version 2, WFL, 5th August 1985)
C   (Version 3, WFL, 5th August 1985, replace zeroes with bias estimate of
C   1330)
C
      include 'interim(errpar)'
      include 'interim(fmtpar)'
      integer ax(2),status
      call rdimag('IMAGE',fmt_sw,2,ax,i,ipin,status)
      if (status.eq.err_normal) then
         call wrimag('OUTPUT',fmt_r,ax,2,ipo,status)
         if (status.eq.err_normal) then
            call convert(%val(ipin),ax(1),ax(2),%val(ipo))
         else
            call wrerr('ERROUT',status)
         end if
      else
         call wrerr('ERRIN',status)
      end if
      call frdata(' ',status)
      call exit
      end


      subroutine convert(in,n,m,out)
      integer n,m,status
      integer*2 in(n,m)
      real*4    out(n,m)
      character string*80
      r64k=64.0*1024.0
      do j=1,m
         do i=1,n
            if (in(i,j).eq.0) then
               write (string,'(a,i,a,i,a)') 'Zero at (',i,',',j,')'
               call wruser (string,status)
               out(i,j)=1330.0
            else if (in(i,j).gt.0) then
               out(i,j)=real(in(i,j))
            else
               out(i,j)=real(in(i,j)) + r64k
            end if
         end do
      end do
      end
