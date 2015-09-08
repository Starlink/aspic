      integer function nindex(str1,str2,n)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE N'TH OCCURRENCE OF A SUBSTRING IN A CHARACTER STRING
*
*METHOD
*       SEARCH REPEATEDLY, STARTING EACH SEARCH IMMEDIATELY AFTER THE
*       PREVIOUS OCCURRENCE
*
*ARGUMENTS
*       STR1 (IN)
*       CHARACTER*(*)
*               THE CHARACTER STRING
*       STR2 (IN)
*       CHARACTER*(*)
*               THE SUBSTRING
*       NINDEX (FUNCTION NAME)
*       INTEGER
*               RETURNS LOCATION OF SUBSTRING
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
      integer start,shift
 
*
* INITIALLISE RESULT AND FIND LENGTH OF FIRST STRING
*
      nindex=0
      length=len(str1)
 
*
* SEARCH FOR EACH OCCURRENCE OF STRING 2
*
 
      do 1 i=1,n
 
*
* SET START OF SEARCH AT CHARACTER FOLLOWING LAST OCCURRENCE
*
         start=nindex+1
 
         if(start.le.length) then
            shift=index(str1(start:),str2)
 
            if(shift.eq.0) then
 
*
* IF NOT FOUND, RESULT =LENGTH+1
*
               nindex=length+1
               go to 2
 
 
            else
 
*
* IF FOUND, SET RESULT TO POSITION FOUND
*
               nindex=nindex+shift
            endif
 
 
         else
 
*
* IF END OF STRING REACHED, OCCURRENCE DOES NOT EXIST..
*
            nindex=length+1
            go to 2
 
         endif
 
1     continue
 
2     return
 
      end
 
 
 
