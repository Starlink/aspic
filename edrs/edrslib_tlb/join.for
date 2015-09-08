      subroutine join(id,x,y,apref,idb,xb,yb,bpref,lena,lenb,lenout)
 
*
*
      real x(lena+lenb),y(lena+lenb),xb(lenb),yb(lenb)
      byte id(20,lena+lenb),idb(20,lenb)
      character apref*(*),bpref*(*),tid*20
 
*
*
 
      if(apref.ne.' ') then
 
         do 20 i=1,lena
            tid=' '
 
            do 10 l=1,20
               nn=id(l,i)
               tid(l:l)=char(nn)
10          continue
 
            call lbgone(tid)
            tid=apref(:lens(apref))//tid
            call lbgone(tid)
 
            do 11 l=1,20
               id(l,i)=ichar(tid(l:l))
11          continue
 
20       continue
 
      endif
 
 
*
*
 
      do 40 i=1,lenb
         x(lena+i)=xb(i)
         y(lena+i)=yb(i)
 
         do 30 l=1,20
            id(l,lena+i)=idb(l,i)
30       continue
 
 
         if(bpref.ne.' ')then
 
            do 80 l=1,20
               nn=id(l,i+lena)
               tid(l:l)=char(nn)
80          continue
 
            call lbgone(tid)
            tid=bpref(:lens(bpref))//tid
            call lbgone(tid)
 
            do 81 l=1,20
               id(l,i+lena)=ichar(tid(l:l))
81          continue
 
         endif
 
40    continue
 
 
*
* PURGE TO DELETE DUPLICATE ENTRIES
*
      call xyprgg(x,y,id,lena+lenb,lenout,ierr)
 
      end
 
 
 
