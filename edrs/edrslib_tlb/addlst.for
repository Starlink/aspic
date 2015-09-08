      subroutine addlst(list,nitem,lstlen,item,ibyt1,ibyt2)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ADD COLUMNS OF ENTRIES TO A LIST
*
*METHOD
*       COPY REQUIRED BYTE COLUMNS INTO THE LIST
*
*ARGUMENTS
*       LIST (IN/OUT)
*       BYTE(4*NITEM,LSTLEN)
*               LIST TO BE UPDATED
*       NITEM (IN)
*       INTEGER
*               NO. OF 4-BYTE ITEMS IN LIST
*       LSTLEN (IN)
*       INTEGER
*               NO. OF ENTRIES (ROWS) IN LIST
*       ITEM (IN)
*       INTEGER
*               ARRAY CONTAINING COLUMNS TO BE INSERTED
*       IBYT1,IBYT2 (IN)
*       INTEGER
*               START AND FINISH BYTE COLUMNS TO BE REPLACED
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
      byte list(4*nitem,lstlen),item(ibyt2-ibyt1+1,lstlen)
 
*
* COPY ITEM INTO THE REQUIRED REGION OF LIST
*
 
      do 2 j=1,lstlen
 
         do 1 i=ibyt1,ibyt2
            list(i,j)=item(i-ibyt1+1,j)
1        continue
 
2     continue
 
      return
 
      end
 
 
 
