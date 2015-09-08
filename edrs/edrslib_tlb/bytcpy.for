      subroutine bytcpy(a,b,n)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY ONE BYTE ARRAY TO ANOTHER OF THE SAME SIZE
*
*METHOD
*       COPY ONE ARRAY TO THE OTHER
*
*ARGUMENTS
*       A (IN)
*       BYTE(N)
*               ARRAY TO BE COPIED
*       B (OUT)
*       BYTE(N)
*               ARRAY TO BE COPIED INTO
*       N (IN)
*       INTEGER
*               DIMENSION OF A AND B
*
*CALLS
*       NONE
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      byte a(n),b(n)
 
*
* COPY A TO B
*
 
      do 1 i=1,n
         b(i)=a(i)
1     continue
 
      return
 
      end
 
 
 
