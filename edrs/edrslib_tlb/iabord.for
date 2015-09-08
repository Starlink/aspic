      integer function iabord(str1,str2,nbyt)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO DETERMINE THE ALPHABETICAL ORDER OF 2 ASCII BYTE STRINGS
*
*METHOD
*       COMPARE EACH BYTE IN TURN, THE FIRST DIFFERENCE GIVES THE
*       ORDER
*
*ARGUMENTS
*       STR1,STR2 (IN)
*       BYTE(NBYT)
*               THE INPUT STRINGS
*       NBYT (IN)
*       INTEGER
*               LENGTH OF THE STRINGS
*       IABORD (FUNCTION NAME-OUT)
*       INTEGER
*               RETURNS 1 FOR AB ORDER, 0 FOR SAME AND -1 FOR REVERSE
*               ORDER
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
      byte str1(nbyt),str2(nbyt)
      iabord=0
 
*
* SCAN THROUGH THE BYTES. THE FIRST NON-EQUAL PAIR DECIDES THE
* ALPHABETICAL ORDER. IF ALL EQUAL, IABORD REMAINS AT ZERO.
*
 
      do 1 i=1,nbyt
 
         if(str1(i).lt.str2(i)) then
            iabord=1
            go to 2
 
 
         else if(str1(i).gt.str2(i)) then
            iabord=-1
            go to 2
 
         endif
 
1     continue
 
2     return
 
      end
 
 
 
