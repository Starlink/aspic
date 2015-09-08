      subroutine plysmp(ia,npixa,nlinea,invala,ascale,azero,x,y,nxy
     : ,ib,npixb,nlineb,invalb,bscale,bzero,limit,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY A POLYGONAL REGION OF ONE IMAGE INTO THE CORRESPONDING
*       PIXELS OF ANOTHER
*
*METHOD
*       FIND THE SMALLEST RECTANGLE WHICH ENCOMPASSES THE POLYGON. FOR
*       EACH IMAGE LINE IN THE RECTANGLE, FIND ALL THE INTERSECTIONS
*       WITH POLYGON EDGES. SORT THE INTERSECTIONS INTO INCREASING X
*       POSITION ORDER. COPY ALTERNATE REGIONS BETWEEN THESE INTERSECT-
*       IONS, ALLOWING FOR INVALID PIXELS AND DIFFERING SCALE FACTORS
*       IN THE 2 IMAGES
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               THE IMAGE TO BE COPIED FROM
*       NPIXA,NLINEA (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       ASCALE,AZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR IA
*       X,Y (IN)
*       REAL(NXY)
*               THE POSITIONS OF THE POLYGON VERTICES
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF POLYGON VERTICES
*       IB (IN/OUT)
*       INTEGER*2(NPIXB,NLINEB)
*               THE IMAGE TO BE COPIED INTO
*       NPIXB,NLINEB (IN)
*       INTEGER
*               THE DIMENSIONS OF IB
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       BSCALE,BZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR IB
*       LIMIT (OUT)
*       INTEGER(4)
*               THE LIMITS XMIN,XMAX,YMIN,YMAX OF THE SMALLEST RECTANGLE
*               ENCLOSING THE POLYGON
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
      integer limit(4)
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
* CONVERT RANGES TO INTEGER LIMITS RESTRICTED TO THE OUTPUT
* IMAGE SIZE
*
         minx=int(min(max(-1.0e8,xmin),1.0e8))
 
         if(real(minx).lt.xmin) minx=minx+1
         limit(1)=max(minx,1)
         limit(2)=min(int(min(max(-1.0e8,xmax),1.0e8)),npixb)
         miny=int(min(max(-1.0e8,ymin),1.0e8))
 
         if(real(miny).lt.ymin) miny=miny+1
         limit(3)=max(miny,1)
         limit(4)=min(int(min(max(-1.0e8,ymax),1.0e8)),nlineb)
 
*
* CALCULATE SCALE FACTORS TO APPLY TO IA TO CONVERT TO IB
*
         bs=bscale
 
         if(abs(bs).lt.1.0e-20) bs=sign(1.0e-20,bs)
         scalef=ascale/bs
         zerol=(azero-bzero)/bs
 
*
* SCAN THE RANGE OF OUTPUT LINES AFFECTED
*
 
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
* COPY PIXELS LYING BETWEEN EACH PAIR OF INTERSECTIONS FROM IA
* INTO IB, ALLOWING FOR INVALID VALUES, THE SIZE OF IMAGE A AND
* THE DIFFERING SCALE FACTORS
*
 
                  do 60 i=minx,maxx
 
                     if(i.le.npixa.and.j.le.nlinea) then
 
                        if(ia(i,j).eq.invala) then
                           ib(i,j)=invalb
 
                        else
                           bval=ia(i,j)*scalef+zerol
 
                           if(bval.gt.32767.0.or.bval.lt.-32767.0) then
                              ib(i,j)=invalb
 
                           else
                              ib(i,j)=nint(bval)
                           endif
 
                        endif
 
 
                     else
                        ib(i,j)=invalb
                     endif
 
60                continue
 
61             continue
 
            endif
 
200      continue
 
      endif
 
999   return
 
      end
 
 
 
