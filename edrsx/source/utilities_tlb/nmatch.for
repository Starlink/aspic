      integer function nmatch(str1,str2)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To count the number of character matches between two strings
*
*SOURCE
*       NMATCH.FOR in UTILITIES.TLB
*
*ARGUMENTS
*       STR1    character       The 1st character string
*       STR2    character       The 2ns character string
*
*USED BY
*       All programs that use routine GTSTRN or EDRS routine GETCMD
*
*SUBROUTINES CALLED
*       None
*
*AUTHOR
*       R.F. Warren-Smith
*       (modified by D.S. Berry (MAVAD::DSB) 26/8/87 to handle
*        lower case strings)
*-------------------------------------------------------------------
*
      character str1*(*),str2*(*)
      nmatch=0

*
* FIND LENGTH OF SHORTER STRING
*
      nchar=min(len(str1),len(str2))

*
* COMPARE CORRESPONDING CHARACTERS, CONVERTING TO UPPER CASE AND
* COUNTING MATCHES
*

      do 1 i=1,nchar

         ichar1=ichar(str1(i:i))
         if(ichar1.ge.ichar('a').and.ichar1.le.ichar('z')) then
            ichar1=ichar1-32
         endif

         ichar2=ichar(str2(i:i))
         if(ichar2.ge.ichar('a').and.ichar2.le.ichar('z')) then
            ichar2=ichar2-32
         endif

         if(ichar1.eq.ichar2) then
            nmatch=nmatch+1
         endif

1     continue

      return

      end



