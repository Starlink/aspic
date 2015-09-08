      subroutine imgstk(ia,npixa,nlinea,invala,nim,ib,npixb,nlineb)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY A 2D IMAGE INTO A GIVEN PLANE OF A 3D IMAGE STACK,
*       PADDING THE SIZE OUT WITH INVALID PIXELS, OR CUTTING THE SIZE
*       DOWN AS NECESSARY.
*
*METHOD
*       STRAIGHTFORWARD COPYING OF THE INTEGER IMAGE VALUES
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               THE INPUT IMAGE TO BE INSERTED
*       NPIXA,NLINEA (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               THE INVALID PIXEL FLAG FOR IA (USED TO PAD IMAGE OUT IF
*               NECESSARY)
*       NIM (IN)
*       INTEGER
*               THE IMAGE PLANE INTO WHICH IA IS TO BE COPIED
*       IB (IN/OUT)
*       INTEGER*2(NPIXB,NLINEB,*)
*               THE 3D IMAGE STACK. OTHER PLANES ARE NOT ALTERED.
*       NPIXB,NLINEB (IN)
*       INTEGER
*               DIMENSIONS OF THE IMAGES IN IB
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb,*)
 
*
* SCAN THE OUTPUT IMAGE LINES
*
 
      do 99 j=1,nlineb
 
*
* IF WITHIN THE INPUT IMAGE, COPY INPUT TO OUTPUT
*
 
         if(j.le.nlinea) then
 
            do 89 i=1,npixb
 
               if(i.le.npixa) then
                  ib(i,j,nim)=ia(i,j)
 
*
* OTHERWISE THE OUTPUT PIXEL IS INVALID
*
 
               else
                  ib(i,j,nim)=invala
               endif
 
89          continue
 
 
         else
 
            do 88 i=1,npixb
               ib(i,j,nim)=invala
88          continue
 
         endif
 
99    continue
 
 
      end
 
 
 
