      subroutine lbgone(str)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REMOVE LEADING BLANKS FROM A CHARACTER STRING
*
*METHOD
*       FIND THE FIRST NON-BLANK CHARACTER, THEN SHIFT THE REMAINING
*       CHARACTERS LEFT AND PAD THE END WITH BLANKS
*
*ARGUMENTS
*       STR (IN/OUT)
*       CHARACTER*(*)
*               THE CHARACTER STRING
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character str*(*)
 
*
* SCAN STRING, LOOKING FOR FIRST NON-BLANK CHARACTER
*
 
      do 1 i=1,len(str)
 
*
* IF FIRST NON-BLANK IS NOT THE FIRST CHARACTER, SHIFT REMAINING
* CHARACTERS TO LEFT
*
 
         if(str(i:i).ne.' ') then
 
            if(i.gt.1) then
 
               do 2 j=1,len(str)+1-i
                  newchr=j+i-1
                  str(j:j)=str(newchr:newchr)
2              continue
 
 
*
* PAD END OF STRING WITH BLANKS
*
               str(j:)=' '
            endif
 
            go to 3
 
         endif
 
1     continue
 
3     return
 
      end
 
 
 
