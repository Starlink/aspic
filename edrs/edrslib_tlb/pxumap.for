      subroutine pxumap(ia,npa,nla,invala,ascale,azero,ix,npx,nlx,
     :invalx,xscale,xzero,iy,npy,nly,invaly,yscale,yzero,iz,npz,nlz
     : ,invalz,zscale,zzero)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESAMPLE THE PIXELS OF AN IMAGE ACCORDING TO A
*       MAPPING GIVEN BY X AND Y COORDINATES STORED IN TWO OTHER IMAGES
*       AND TO PUT THE TRANSFORMED PIXELS INTO AN OUTPUT IMAGE.
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      integer*2 ia(npa,nla),ix(npx,nlx),iy(npy,nly),iz(npz,nlz)
 
*
* SET THE OUTPUT SCALE, ZERO, ETC.
*
      invalz=invala
      zscale=ascale
      zzero=azero
 
*
* INITIALLIZE THE OUTPUT IMAGE
*
 
      do j=1,nlz
 
         do i=1,npz
            iz(i,j)=invalz
         enddo
 
      enddo
 
 
*
* SCAN THE AREA COMMON TO ALL OUTPUT IMAGES
*
 
      do 201 j=1,min(nlz,nlx,nly)
 
         do 200 i=1,min(npz,npx,npy)
 
*
* CONSIDER ONLY THOSE PIXELS WHERE BOTH COORDINATE IMAGES ARE VALID
*
 
            if(ix(i,j).ne.invalx.and.iy(i,j).ne.invaly)then
 
*
* CALCULATE THE LOCATION TO RESAMPLE
*
               x=ix(i,j)*xscale+xzero
               kx=nint(min(max(0.0,x),1.0e8))
               y=iy(i,j)*yscale+yzero
               ky=nint(min(max(0.0,y),1.0e8))
 
*
* IF IT LIES WITHIN THE INPUT IMAGE, SAMPLE IT
*
 
               if(kx.ge.1.and.kx.le.npa.and.ky.ge.1.and.ky.le.nla)then
                  iz(i,j)=ia(kx,ky)
               endif
 
            endif
 
200      continue
 
201   continue
 
 
      end
 
 
 
