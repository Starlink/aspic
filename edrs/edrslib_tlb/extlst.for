      subroutine extlst(list,nitem,lstlen,item,ibyt1,ibyt2)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO EXTRACT COLUMNS OF ENTRIES FROM A TABLE
*
*METHOD
*       COPY THE REQUIRED COLUMNS TO AN OUTPUT LIST
*
*ARGUMENTS
*       LIST (IN)
*       BYTE(4*NITEM,LSTLEN)
*               THE INPUT LIST
*       NITEM (IN)
*       INTEGER
*               THE NUMBER OF 4-BYTE ITEMS IN THE LIST
*       LSTLEN (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN THE INPUT LIST
*       ITEM (OUT)
*       BYTE(IBYT2-IBYT1+1,LSTLEN)
*               AN ARRAY TO HOLD THE LIST OF COLUMNS EXTRACTED FROM
*               THE INPUT LIST
*       IBYT1,IBYT2 (IN)
*       INTEGER
*               THE STARTING AND ENDING BYTE COLUMNS TO BE EXTRACTED
*               FROM THE LIST
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
* COPY THE REQUITED REGION OF LIST INTO ITEM
*
 
      do 2 j=1,lstlen
 
         do 1 i=ibyt1,ibyt2
            item(i-ibyt1+1,j)=list(i,j)
1        continue
 
2     continue
 
      return
 
      end
 
 
 
