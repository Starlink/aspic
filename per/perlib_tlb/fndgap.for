      subroutine per_fndgap(a1,n1,a2,n2,t,dt,np)
 
*
*   This sorts out the inter-relations between two sets of data.
*
*   In particular it finds the first and last epoch of each set, and
*   how many samples in each and how many are missing, and
*   what is the increment between them.
*
*   Written by K.F.hartley at RGO on 3-2-84
*
      integer n1(2),n2(2),status
      double precision a1(n1(1),n1(2)),a2(n2(1),n2(2)),t(4)
      double precision dt
      integer np(4)
      character*72 text
 
*
*   First the pointers
*
      np(1)=1
      np(2)=n1(2)
 
*
*   Then the increments
*
      dt1 = a1(1,2) - a1(1,1)
      dt2 = a2(1,2) - a2(1,1)
      dt3 = (a1(1,n1(2))-a1(1,1))/real(n1(2)-1)
      dt4 = (a2(1,n2(2))-a2(1,1))/real(n2(2)-1)
      ave=(dt1+dt2+dt3+dt4)/4.0
      rms=sqrt( ( (dt1-ave)*(dt1-ave) + (dt2-ave)*(dt2-ave) +(dt3-ave)
     : *(dt3-ave) + (dt4-ave)*(dt4-ave) )/4.0)
 
      if (rms.lt.0.01*ave) then
         dt = ave
 
      else
         call wruser('Time increments do not agree',istat)
         write (text,900) dt1,dt2
900      format(1h ,'Increments between first two points are ',2f12.8)
         call wruser(text,status)
         write (text,910) dt3,dt4
910      format(1h ,'Average gaps are ',2f12.8)
         call wruser(text,status)
         call wruser(' ',status)
         write (text,920) ave
920      format(1h ,'Average of all four values is ',f12.8)
         call wruser(text,status)
         write (text,930) rms
930      format (1h ,'RMS deviation from average value is ',f12.8)
         call wruser(text,status)
         call exit
      end if
 
 
*
*   Now the times
*
      t(1) = a1(1,1)
      t(2) = a1(1,n1(2))
      t(3) = a2(1,1)
      t(4) = a2(1,n2(2))
 
*
*   now back to the gap
*
*
*   Now try to find out if the gap contains an integral
*   number of samples
*
      gap=(t(3)-t(2))/dt
      ngap=int(gap+0.5)
      phase=abs(gap-real(ngap))
 
      if (phase.ge.0.1) then
         call wruser('Gap is not a whole number of samples wide',
     :   status)
         write (text,940) phase
940      format (1h ,'Error is ',f8.5,' of a sample')
         call wruser(text,status)
         call exit
      end if
 
      ngap=ngap-1
      np(3) = np(2) + ngap + 1
      np(4) = np(3) + n2(2) -1
 
      end
 
 
 
