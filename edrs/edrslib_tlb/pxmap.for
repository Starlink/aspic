      subroutine pxmap(ia,npa,nla,invala,ascale,azero,ix,npx,nlx,invalx
     : ,xscale,xzero,iy,npy,nly,invaly,yscale,yzero,iz,npz,nlz,invalz
     :  ,zscale,zzero,iw1,iw2)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO TRANSFORM THE PIXEL LOCATIONS OF AN IMAGE ACCORDING TO A
*       MAPPING GIVEN BY X AND Y COORDINATES STORED IN TWO OTHER IMAGES
*       AND TO PUT THE TRANSFORMED PIXELS INTO AN OUTPUT IMAGE,
*       AVERAGING THE VALUES WHERE 2 OR MORE MAPPED PIXELS COINCIDE.
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      integer*2 ia(npa,nla),ix(npx,nlx),iy(npy,nly),iz(npz,nlz)
      integer iw1(npz,nlz),iw2(npz,nlz)
 
*
* INITIALLISE THE WORKSPACE
*
 
      do 101 j=1,nlz
 
         do 100 i=1,npz
            iw1(i,j)=0
            iw2(i,j)=0
100      continue
 
101   continue
 
 
*
* SCAN THE AREA COMMON TO ALL INPUT IMAGES
*
 
      do 201 j=1,min(nla,nlx,nly)
 
         do 200 i=1,min(npa,npx,npy)
 
*
* CONSIDER ONLY THOSE PIXELS WHERE ALL INPUT IMAGES ARE VALID
*
 
            if(ia(i,j).ne.invala.and.ix(i,j).ne.invalx.and.iy(i,j).ne
     :       .invaly)then
 
*
* CALCULATE THE NEW PIXEL LOCATION
*
               x=ix(i,j)*xscale+xzero
               kx=nint(min(max(0.0,x),1.0e8))
               y=iy(i,j)*yscale+yzero
               ky=nint(min(max(0.0,y),1.0e8))
 
*
* IF IT LIES WITHIN THE OUTPUT IMAGE, STORE IT
*
 
               if(kx.ge.1.and.kx.le.npz.and.ky.ge.1.and.ky.le.nlz)then
                  iw1(kx,ky)=iw1(kx,ky)+ia(i,j)
                  iw2(kx,ky)=iw2(kx,ky)+1
               endif
 
            endif
 
200      continue
 
201   continue
 
 
*
* SET THE OUTPUT SCALE, ZERO, ETC.
*
      invalz=invala
      zscale=ascale
      zzero=azero
 
*
* CALCULATE THE OUTPUT IMAGE VALUES FROM THE DATA BINNED IN THE
* WORKSPACE
*
 
      do 301 j=1,nlz
 
         do 300 i=1,npz
 
            if(iw2(i,j).gt.0)then
               iz(i,j)=nint(real(iw1(i,j))/real(iw2(i,j)))
 
            else
               iz(i,j)=invalz
            endif
 
300      continue
 
301   continue
 
 
      end
 
 
 
