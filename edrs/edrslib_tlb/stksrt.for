      subroutine stksrt(c,nc,ip,ndiff,iw)
 
*
      character c(nc)*(*),t*80
      integer ip(nc),iw(nc)
      logical repeat
 
*
* FIND LENGTH OF INPUT STRINGS
*
      nn=min(80,len(c(1)))
 
*
* INITIALLISE POINTERS IN WORKSPACE
*
 
      do 1 i=1,nc
         iw(i)=i
1     continue
 
 
*
* PERFORM A BUBBLE SORT TO PUT CHARACTER STRINGS INTO ALPHABETICAL
* ORDER
*
      repeat=.true.
 
33    if(repeat) then
         repeat=.false.
 
         do 2 i=1,nc-1
 
            if(c(i+1).lt.c(i)) then
               repeat=.true.
 
*
* SWAP PAIRS WHICH ARE IN THE WRONG ORDER
*
               t(:nn)=c(i+1)
               c(i+1)=c(i)
               c(i)=t(:nn)
 
*
* PERMUTE THE POINTERS IN THE SAME WAY
*
               it=iw(i+1)
               iw(i+1)=iw(i)
               iw(i)=it
            endif
 
2        continue
 
         go to 33
 
      endif
 
 
*
* POINTERS NOW POINT TO ORIGINAL POSITIONS. SCAN LIST TO REMOVE
* REPEATED ENTRIES AND REVERSE THE POINTING DIRECTION
*
      ndiff=1
      ip(iw(1))=1
 
      do 3 i=2,nc
 
*
* IF A DIFFERENT CHARACTER STRING IS FOUND, COUNT IT AND PUT IT
* IN THE CORRECT PLACE IN THE LIST
*
 
         if(c(i).ne.c(ndiff)) then
            ndiff=ndiff+1
            c(ndiff)=c(i)
         endif
 
 
*
* SET THE APPROPRIATE OUTPUT POINTER TO ITS NEW LOCATION
*
         ip(iw(i))=ndiff
3     continue
 
 
      end
 
 
 
