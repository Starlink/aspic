      subroutine imgtrm(ia,npixa,nlinea,invala,ix,iy,ib,npixb,nlineb
     : ,invalb)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY THAT REGION OF AN IMAGE STARTING AT PIXEL IX,IY AND
*       OF SIZE NPIXB,NLINEB FROM THE INPUT IMAGE IA TO THE OUTPUT
*       IMAGE IB. IF THE AREA EXTENDS OUTSIDE IA, THE CORRESPONDING
*       OUTPUT PIXELS ARE SET INVALID.
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb),ival
 
*
* SCAN THE OUTPUT IMAGE LINES
*
 
      do 101 j=1,nlineb
 
*
* IF OUTSIDE THE INPUT IMAGE, THE RESULT IS INVALID
*
 
         if(j+iy-1.lt.1.or.j+iy-1.gt.nlinea)then
 
            do 66 i=1,npixb
               ib(i,j)=invalb
66          continue
 
 
         else
 
*
* OTHERWISE COPY THE LINE
*
 
            do 67 i=1,npixb
 
*
* IF OUTSIDE INPUT IMAGE, OUTPUT IS INVALID
*
 
               if(i+ix-1.lt.1.or.i+ix-1.gt.npixa)then
                  ib(i,j)=invalb
 
               else
                  ival=ia(i+ix-1,j+iy-1)
 
                  if(ival.eq.invala)then
                     ib(i,j)=invalb
 
                  else
                     ib(i,j)=ival
                  endif
 
               endif
 
67          continue
 
         endif
 
101   continue
 
 
      end
 
 
 
