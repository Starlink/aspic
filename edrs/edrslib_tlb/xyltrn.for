      subroutine xyltrn(lista,nitem,lstlen,c,listb)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY A LINEAR TRANSFORMATION TO A LIST OF X,Y POSITIONS
*
*METHOD
*       APPLY THE TRANSFORMATION TO THE APPROPRIATE ELEMENTS OF AN
*       X,Y LIST
*
*ARGUMENTS
*       LISTA (IN)
*       INTEGER(NITEM,LSTLEN)
*               THE INPUT X,Y LIST
*       NITEM,LSTLEN (IN)
*       INTEGER
*               THE DIMENSIONS OF LISTA
*       C (IN)
*       REAL(6)
*               THE COEFFICIENTS DEFINING THE LINEAR TRANSFORMATION
*       LISTB (OUT)
*       INTEGER(NITEM,LSTLEN)
*               THE TRANSFORMED X,Y LIST
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer lista(nitem,lstlen),listb(nitem,lstlen),ix,iy
      real c(6),x,y
 
*
* USE EQUIVALENCE TO ACCESS X,Y POSITIONS AS REAL NUMBERS
*
      equivalence (ix,x),(iy,y)
 
*
* SCAN EACH LIST ENTRY
*
 
      do 2 j=1,lstlen
 
*
* COPY INPUT LIST TO OUTPUT LIST
*
 
         do 1 i=1,nitem
            listb(i,j)=lista(i,j)
1        continue
 
 
*
* EXTRACT THE X,Y POSITION FROM EACH ENTRY AND APPLY THE TRANSFORMATION
*
         ix=lista(6,j)
         iy=lista(7,j)
         xt=c(1)+c(2)*x+c(3)*y
         yt=c(4)+c(5)*x+c(6)*y
         x=xt
         y=yt
 
*
* PUT TRANSFORMED POSITIONS INTO OUTPUT LIST
*
         listb(6,j)=ix
         listb(7,j)=iy
2     continue
 
      return
 
      end
 
 
 
