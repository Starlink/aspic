      subroutine imgmag(ia,npixa,nlinea,invala,xmin,xmax,ymin,ymax,ib
     : ,npixb,nlineb,invalb,wk1,wk2,redfx,redfy,ierr)
 
*
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
      real wk1(npixb),wk2(npixb)
 
*
      ierr=0
 
      if(npixa.lt.1) then
         ierr=1
 
      else if(nlinea.lt.1) then
         ierr=2
 
      else if(npixb.lt.1) then
         ierr=3
 
      else if(nlineb.lt.1) then
         ierr=4
 
      else
         x1=max(min(xmin,xmax),0.5)
         x2=min(max(xmin,xmax),npixa+0.5)
         y1=max(min(ymin,ymax),0.5)
         y2=min(max(ymin,ymax),nlinea+0.5)
         dxin=x2-x1
         dxout=npixb
         dyin=y2-y1
         dyout=nlineb
         redfx=max(dxin,dyin,1.0e-20)/dxout
         redfy=max(dxin,dyin,1.0e-20)/dyout
         xoffs=0.5*(max(1.0,redfx)-min(1.0,redfx))
         yoffs=0.5*(max(1.0,redfy)-min(1.0,redfy))
         awtx=1.0+xoffs/min(max(redfx,1.0e-3),1.0)
         awty=1.0+yoffs/min(max(redfy,1.0e-3),1.0)
         bwtx=-1.0/min(max(redfx,1.0e-3),1.0)
         bwty=-1.0/min(max(redfy,1.0e-3),1.0)
         xshift=0.5*redfx
         yshift=0.5*redfy
 
         do 99 jout=1,nlineb
            ystart=y1+redfy*(jout-1)
            ycen=ystart+yshift
            yend=min(ycen+yshift,y2)
            jstart=max(1,nint(ystart))
            jend=min(nlinea,nint(yend))
 
            if(ystart.ge.y2) jend=jstart-1
 
            do 41 l=1,npixb
               wk1(l)=0.0
               wk2(l)=0.0
41          continue
 
 
            do 98 jin=jstart,jend
               dy=abs(jin-ycen)
 
               if(dy.le.yoffs) then
                  wty=1.0
 
               else
                  wty=awty+bwty*dy
               endif
 
 
               do 97 iout=1,npixb
                  xstart=x1+redfx*(iout-1)
                  xcen=xstart+xshift
                  xend=min(xcen+xshift,x2)
                  istart=max(1,nint(xstart))
                  iend=min(npixa,nint(xend))
 
                  if(xstart.ge.x2) iend=istart-1
 
                  do 96 iin=istart,iend
 
                     if(ia(iin,jin).ne.invala) then
                        dx=abs(iin-xcen)
 
                        if(dx.le.xoffs) then
                           wtx=1.0
 
                        else
                           wtx=awtx+bwtx*dx
                        endif
 
                        wt=wtx*wty
                        wk1(iout)=wk1(iout)+ia(iin,jin)*wt
                        wk2(iout)=wk2(iout)+wt
                     endif
 
96                continue
 
97             continue
 
98          continue
 
 
            do 66 l=1,npixb
 
               if(wk2(l).ge.0.01) then
                  ib(l,jout)=nint(wk1(l)/wk2(l))
 
               else
                  ib(l,jout)=invalb
               endif
 
66          continue
 
99       continue
 
      endif
 
      return
 
      end
 
 
 
