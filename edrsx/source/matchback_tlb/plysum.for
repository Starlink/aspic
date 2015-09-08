      subroutine plysum(ia,npixa,nlinea,scale,zero,invala,x,y,nxy,sum,
     :                  ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To calculate the sum of all pixel values within a given polygon
*
*SOURCE
*	PLYSUM.FOR in MATCHBACK.TLB
*
*METHOD
*       Find the smallest rectangle which encompasses the polygon. For
*       each image line in the rectangle, find all the intersections
*       with polygon edges. Sort the intersections into increasing x
*       position order. Sum alternate regions between these intersect-
*       ions, allowing for invalid pixels.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               THE INPUT IMAGE 
*       NPIXA,NLINEA (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*	SCALE,ZERO (IN)
*	REAL
*		THE SCALE ANB ZERO VALUES FOR THE INPUT IMAGE
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       X,Y (IN)
*       REAL(NXY)
*               THE POSITIONS OF THE POLYGON VERTICES
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF POLYGON VERTICES
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       D.S.Berry (Based on EDRS routine PLYSMP by R.F. WARREN-SMITH)
*-----------------------------------------------------------------------
*
*
      integer ia(npixa,nlinea),limit(4)
      logical exit
      real x(nxy),y(nxy)
 
*
* SET MAXIMUM ALLOWABLE NUMBER OF LINE CROSSINGS
*
      parameter (mxcrs=100)
      real xcross(mxcrs)
 
*
* CHECK ARGUMENT VALIDITY
*
      if(nxy.lt.3) then
         ierr=1
      else
         ierr=0
 
*
* FIND THE MAX AND MIN X AND Y RANGE OF THE X,Y POSITIONS
*
         xmin=1.0e20
         xmax=-1.0e20
         ymin=1.0e20
         ymax=-1.0e20
         do 1 n=1,nxy
            xmin=min(x(n),xmin)
            xmax=max(x(n),xmax)
            ymin=min(y(n),ymin)
            ymax=max(y(n),ymax)
1        continue

*
* CONVERT RANGES TO INTEGER LIMITS RESTRICTED TO THE IMAGE SIZE
*
         minx=int(min(max(-1.0e8,xmin),1.0e8))
         if(real(minx).lt.xmin) minx=minx+1
         limit(1)=max(minx,1)
         limit(2)=min(int(min(max(-1.0e8,xmax),1.0e8)),npixa)

         miny=int(min(max(-1.0e8,ymin),1.0e8))
         if(real(miny).lt.ymin) miny=miny+1
         limit(3)=max(miny,1)
         limit(4)=min(int(min(max(-1.0e8,ymax),1.0e8)),nlinea)
 
*
* IF THE POLYGON HAS ZERO AREA, QUIT WITH IERR=3
*
         if(limit(1).ge.limit(2).or.limit(3).ge.limit(4)) then
            ierr=3
            goto 999
         endif

*
* SCAN THE RANGE OF LINES AFFECTED
*
         sum=0
         do 200 j=limit(3),limit(4)
            yl=j
 
*
* PROBLEMS OCCUR IN COUNTING THE NUMBER OF INTERSECTIONS IF ANY
* IMAGE LINE PASSES EXACTLY THROUGH A POLYGON VERTEX. THEREFORE
* THE LINE POSITIONS ARE SHIFTED BY A NEGLIGIBLE AMOUNT PERT
* TO ENSURE THIS DOES NOT HAPPEN
*
            pert=0.0001
54          ncross=0
 
*
* SCAN THROUGH THE X,Y POSITIONS, TESTING IF EACH POLYGON SIDE
* INTERSECTS THE IMAGE LINE
*
             do 99 n1=1,nxy
               n2=n1+1
 
*
* POLYGON VERTICES CYCLE BACK TO THE START
*
               if(n2.gt.nxy) n2=1
               test=((y(n1)-yl)-pert)*((yl-y(n2))+pert)
 
*
* IF TEST IS ZERO, THE LINE PASSES THROUGH A VERTEX... CHANGE PERT
* AND START AGAIN
*
               if(test.eq.0.0) then
                  pert=pert+0.0001
                  go to 54
 
*
* IF TEST IS POSITIVE, ADJACENT VERTICES LIE ON OPPOSITE SIDES
* OF THE IMAGE LINE... CALCULATE THE POINT OF INTERSECTION AND
* STORE IT
*
               else if(test.gt.0) then
                  ncross=ncross+1
 
                  if(ncross.le.mxcrs) then
                     dy=y(n2)-y(n1)
 
                     if(abs(dy).lt.1.0e-20) dy=sign(1.0e-20,dy)
                     xcross(ncross)=x(n1)+((yl-y(n1))*(x(n2)-x(n1))/dy)
 
                  else
 
*
* IF STORAGE FOR INTERSECTIONS IS EXCEEDED, RETURN WITH IERR=2
*
                     ierr=2
                     go to 999
 
                  endif
 
               endif
 
99          continue
 
*
* IF LINE INTERSECTS THE POLYGON, SORT INTERSECTIONS INTO X ORDER
*
            if(ncross.gt.1) then
               exit=.false.
               ntop=ncross
 
56             if(.not.exit) then
                  exit=.true.
                  ntop=ntop-1
 
                  do 100 n=1,ntop
 
*
* SWAP ADJACENT VALUES IF IN WRONG ORDER
*
                     if(xcross(n).gt.xcross(n+1)) then
                        xt=xcross(n+1)
                        xcross(n+1)=xcross(n)
                        xcross(n)=xt
                        exit=.false.
                     endif
 
100               continue
 
*
* LOOP WHILE INTERCHANGE WAS NECESSARY
*
                  go to 56
 
               endif
 
*
* SCAN THROUGH THE ORDERED INTERSECTIONS IN PAIRS
*
               do 61 n=2,ncross,2
                  xmin=xcross(n-1)
                  minx=int(min(max(-1.0e8,xmin),1.0e8))
 
                  if(real(minx).lt.xmin) minx=minx+1
                  minx=max(limit(1),minx)
                  maxx=min(limit(2),int(min(max(-1.0e8,xcross(n)),1
     :             .0e8)))
 
*
* SUM PIXELS LYING BETWEEN EACH PAIR OF INTERSECTIONS FROM IA
* ALLOWING FOR INVALID VALUES
*
                  do 60 i=minx,maxx
 
                     if(i.le.npixa.and.j.le.nlinea) then
 
                        if(ia(i,j).ne.invala) then
                           sum=sum+ia(i,j)
                        endif
                     endif
 
60                continue
 
61             continue
 
            endif
 
200      continue
 
      endif

*
* CONVERT THE INTEGER DATA SUM INTO A SCALED DATA SUM
*
      sum=scale*sum+zero 


999   continue
 
      end
