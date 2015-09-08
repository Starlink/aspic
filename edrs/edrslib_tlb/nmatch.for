      integer function nmatch(str1,str2)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COUNT THE NUMBER OF CHARACTER MATCHES BETWEEN TWO CHARACTER
*       STRINGS
*
*METHOD
*       COMPARE CORRESPONDING CHARACTERS, COUNTING THE MATCHES
*
*ARGUMENTS
*       STR1,STR2 (IN)
*       CHARACTER*(*)
*               THE CHARACTER STRINGS
*       NMATCH (FUNCTION NAME)
*       INTEGER
*               THE NUMBER OF CHARACTER MATCHES
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character str1*(*),str2*(*)
      nmatch=0
 
*
* FIND LENGTH OF SHORTER STRING
*
      nchar=min(len(str1),len(str2))
 
*
* COMPARE CORRESPONDING CHARACTERS, COUNTING MATCHES
*
 
      do 1 i=1,nchar
 
         if(str1(i:i).eq.str2(i:i)) then
            nmatch=nmatch+1
         endif
 
1     continue
 
      return
 
      end
 
 
 
