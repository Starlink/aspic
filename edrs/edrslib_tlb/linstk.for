      subroutine linstk(ia,npixa,nlinea,invala,linea,ib,npixb,nimage)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY A SPECIFIED LINE FROM AN IMAGE INTO A STACK OF IMAGE
*       LINES TAKEN FROM ELSEWHERE
*
*METHOD
*       COPY THE SPECIFIED LINE TO A SPECIFIED STACK LOCATION. PAD THE
*       LINE WITH INVALID PIXELS TO MAKE UP ANY EXTRA LENGTH REQUIRED.
*       REPLACE IT WITH INVALID PIXELS IF THE LINE SPECIFIED LIES
*       OUTSIDE THE INPUT IMAGE
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               THE INPUT IMAGE
*       NPIXA,NLINEA (IN)
*       INTEGER
*               DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       LINEA (IN)
*       INTEGER
*               SPECIFIES THE LINE TO BE COPIED FROM IA
*       IB (IN/OUT)
*       INTEGER*2(NPIXB,*)
*               THE STACK OF IMAGE LINES, EACH OF LENGTH NPIXB
*       NPIXB (IN)
*       INTEGER
*               THE LENGTH OF LINES IN IB
*       NIMAGE (IN)
*       INTEGER
*               THE LOCATION IN THE STACK IB INTO WHICH THE LINE IS
*               TO BE COPIED
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npixa,nlinea),ib(npixb,*)
 
*
* IF THE LINE REQUIRED LIES OUTSIDE THE INPUT IMAGE, FILL THE
* CORRESPONDING WORKSPACE WITH INVALID PIXEL VALUES
*
 
      if(linea.gt.nlinea) then
 
         do 1 i=1,npixb
            ib(i,nimage)=invala
1        continue
 
 
      else
 
*
* OTHERWISE COPY THE REQUIRED LINE TO THE WORKSPACE
*
 
         do 2 i=1,npixb
 
*
* IF THE WORKSPACE HAS LONGER LINES THAN THE IMAGE, PAD OUT WITH
* INVALID PIXEL VALUES
*
 
            if(i.le.npixa) then
               ib(i,nimage)=ia(i,linea)
 
            else
               ib(i,nimage)=invala
            endif
 
2        continue
 
      endif
 
      return
 
      end
 
 
 
