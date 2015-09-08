      integer function lens(ch)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO DETERMINE THE MINIMUM NUMBER OF CHARACTERS REQUIRED IN A
*       CHARACTER STRING AFTER TRAILING BLANKS HAVE BEEN REMOVED
*----------------------------------------------------------------------
*
*
      character ch*(*)
 
      do 1 i=len(ch),2,-1
 
         if(ch(i:i).ne.' ')go to 2
1     continue
 
2     lens=i
 
      end
 
 
 
