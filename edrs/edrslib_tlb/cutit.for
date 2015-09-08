      subroutine cutit(ia,inval,npix,nlin,drange,len,ilevel,iout,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRODUCE A COPY OF THE INPUT IMAGE IN WHICH ALL PIXELS WITH
*       INTENSITIES OUTSIDE THE RANGE SPECIFIED IN THE ARGUMENT DRANGE
*       ARE FLAGGED AS INVALID
*
*METHOD
*       SCAN THROUGH THE OUTPUT GIVING EACH PIXEL THE INTENSITY OF THE
*       CORRESPONDING INPUT PIXEL SO LONG AS THAT INTENSITY LIES
*       BETWEEN DRANGE(1) AND DRANGE(2).IF IT DOES NOT,THEN SET IT
*       EQUAL TO THE INVALID FLAG
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLIN)
*              THE INPUT IMAGE
*       INVAL (IN)
*       INTGER
*              THE FLAG VALUE FOR INVALID PIXELS
*       NPIX,NLIN (IN)
*       INTEGER
*              THE DIMENSIONS OF IA
*       DRANGE (IN)
*       INTEGER(LEN)
*              THE UPPER AND LOWER LIMITS OF THE CUT,GIVEN AS
*              CORRESPONDING INTEGER VALUES AND NOT AS REAL DATA VALUES
*       LEN (IN)
*       INTEGER
*              THE NUMBER OF ELEMENTS IN THE DRANGE ARRAY.MUST BE 2.
*       ILEVEL (IN)
*       INTEGER
*              THE INTERACTION LEVEL
*       IOUT (OUT)
*       INTEGER*2(NPIX,NLIN)
*              THE OUTPUT IMAGE
*       IERR (OUT)
*       INTEGER
*              THE ERROR RETURN FLAG,ZERO FOR SUCCESS
*
*CALLS
*       STARLINK:
*              WRUSER
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
      integer drange(len)
      integer*2 ia(npix,nlin),iout(npix,nlin)
      character prbuf*80

      parameter (maxint=32767,minint=-32767)

*
* SET THE OUTPUT INVALID PIXEL VALUE
*
      if(inval.lt.minint.or.inval.gt.maxint) then
         invalo=minint
      else
         invalo=inval
      endif

*
* SET THE ERROR FLAG OFF AND INITIALISE PIXEL COUNTERS
*
      ierr=0
      ngood=0
      nrej=0
 
*
* SCAN THROUGH THE OUTPUT IMAGE
*
 
      do 20 j=1,nlin
 
         do 10 i=1,npix
            val=ia(i,j)
 
*
* IF INPUT PIXEL IS INVALID SO IS OUTPUT PIXEL
*
 
            if(val.eq.inval) then
               iout(i,j)=invalo
 
*
* IF THE INPUT PIXEL IS VALID CHECK TO SEE IF IT LIES WITHIN THE
* LIMITS SPECIFIED BY DRANGE
*
 
            else
               test=(val-drange(1))*(val-drange(2))
 
               if(test.le.0) then
 
*
* IF IT DOES,THEN COPY IT TO THE OUTPUT AND INCREMENT THE GOOD PIXEL
* COUNT BY ONE
*
                  iout(i,j)=val
                  ngood=ngood+1
 
*
* IF IT DOES NOT,THEN SET THE OUTPUT PIXEL INVALID AND INCREMENT THE
* REJECTED PIXEL COUNT BY ONE
*
 
               else
                  iout(i,j)=invalo
                  nrej=nrej+1
               endif
 
            endif
 
 
*
* DO NEXT PIXEL
*
10       continue
 
20    continue
 
 
*
* IF INPUT CONTAINED NO PIXELS WITHIN THE RANGE SPECIFIED BY DRANGE
* THEN SET IERR=1 AND EXIT
*
 
      if(ngood.eq.0) then
         ierr=1
         goto 99
 
      endif
 
 
*
* PRINT OUT INFO IF REQUIRED
*
 
      if(ilevel.ge.2) then
         write(prbuf,30) nrej
30       format(5x,i8,' PIXELS HAVE BEEN REJECTED')
         call wruser(prbuf,istat)
      endif
 
 
*
* FINISH
*
99    return
 
      end
 
 
 
