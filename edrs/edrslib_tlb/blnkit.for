      subroutine blnkit(ia,inval,npix,nlin,wind,len,oper,idir,iout,
     :ilevel)
 
*
      real wind(len)
      integer*2 ia(npix,nlin),iout(npix,nlin)
      character prbuf*80,op*22
 
*
* SCAN THROUGH IMAGE
*
 
      do 200 j=1,nlin
 
         do 100 i=1,npix
 
*
* IF THIS POINT LIES WITHIN THE WINDOW,SET IN=1
*
            in=0
            ic=i
 
            if(idir.eq.2) ic=j
            test=(ic-wind(1))*(wind(2)-ic)
 
            if(test.ge.0) in=1
 
*
* IF THIS POINT LIES IN THE WINDOW AND OPER=1,SET IT INVALID IN IOUT
* OTHER WISE COPY ITS VALUE TO IOUT
*
 
            if((in.eq.1).and.(oper.eq.1)) then
               iout(i,j)=inval
 
            else if((in.eq.0).and.(oper.eq.1)) then
               iout(i,j)=ia(i,j)
            endif
 
 
*
* IF THIS POINT LIES IN THE WINDOW AND OPER=2,COPY ITS VALUE TO IOUT.
* OTHERWISE SET IT INVALID IN IOUT
*
 
            if((in.eq.1).and.(oper.eq.2)) then
               iout(i,j)=ia(i,j)
 
            else if((in.eq.0).and.(oper.eq.2)) then
               iout(i,j)=inval
            endif
 
 
*
* DO NEXT PIXEL
*
100      continue
 
200   continue
 
 
*
* GIVE INFO IF REQUIRED
*
 
      if(ilevel.ge.2) then
         i1=wind(1)
         i2=wind(2)
         call wruser(' ',istat)
 
         if(oper.eq.1) then
            op=' HAVE BEEN BLANKED OUT'
 
         else
            op=' ONLY,HAVE BEEN COPIED'
         endif
 
 
         if(idir.eq.1) then
            write(prbuf,10) i1,i2,op
10          format(' COLUMNS ',i7,' TO ',i7,a22)
            call lbgone(prbuf(21:))
            call lbgone(prbuf(10:))
            call wruser(prbuf,istat)
 
         else
            write (prbuf,20) i1,i2,op
20          format(' LINES ',i7,' TO ',i7,a22)
            call lbgone(prbuf(19:))
            call lbgone(prbuf(8:))
            call wruser(prbuf,istat)
         endif
 
         call wruser(' ',istat)
      endif
 
 
*
* FINISH
*
      return
 
      end
 
 
 
