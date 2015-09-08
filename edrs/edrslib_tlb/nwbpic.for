      subroutine nwbpic(image,inval,scale,zero,range,thresh,roll)
      parameter (npix=60, nlines=23, nchar=40)
      integer*2 image(npix,nlines)
      character cdata*(nchar),char(0:nchar-1)*1,outbuf*(npix)
      logical roll
      equivalence (cdata,char(0))
      data cdata/' .1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ+*'/
      rrng=(nchar-1)/sign(max(1.0e-20,abs(range)),range)
 
      do 6 j=1,nlines
 
         do 4 i=1,npix
 
            if(image(i,j).eq.inval) then
               outbuf(i:i)=' '
 
            else
 
               if(roll) then
                  outbuf(i:i)=char(mod(max(0,nint((image(i,j)*scale
     :             +zero-thresh)*rrng)),nchar))
 
               else
                  outbuf(i:i)=char(min(max(0,nint((image(i,j)*scale
     :             +zero-thresh)*rrng)),nchar-1))
               endif
 
            endif
 
4        continue
 
         call wruser(' '//outbuf,istat)
6     continue
 
      return
 
      end
 
 
 
