      subroutine comprs(ia,npix,nlin,inval,x,y,nxy,idir,nop,iout,minpix
     : ,fval,ilevel,icom,npixout,bscale,bzero,out,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COLLAPSE A POLYGONAL AREA OF AN IMAGE INTO A '1D' IMAGE.
*
*METHOD
*       FIND THE SMALLEST RECTANGLE WHICH ENCOMPASSES THE POLYGON. FOR
*       EACH IMAGE LINE OR COLUMN IN THE RECTANGLE, FIND ALL THE
*       INTERSECTIONS WITH POLYGON EDGES. SORT THE INTERSECTIONS INTO
*       INCREASING X OR Y POSITION ORDER. BIN ALTERNATE REGIONS BETWEEN
*       INTERSECTIONS, ALLOWING FOR INVALID PIXELS.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLIN)
*               THE IMAGE TO BE COLLAPSED
*       NPIX,NLIN (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       X,Y (IN)
*       REAL(NXY)
*               THE POSITIONS OF THE POLYGON VERTICES
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF POLYGON VERTICES
*       IDIR (IN)
*       INTEGER
*               THE AXIS TO COLLAPSE ALONG.IDIR=1 IS THE X AXIS AND
*               IDIR=2 IS THE Y AXIS.
*       NOP (IN)
*       INTEGER
*               IF SET TO 1 THEN NO POLY GON WAS GIVEN AND THE DEFAULT
*               (THE WHOLE IMAGE) IS TO BE USED.
*       IOUT (OUT)
*       INTEGER*2(NPIXOUT,1)
*               THE OUTPUT '1D' IMAGE
*       MINPIX (IN)
*       INTEGER
*               THE MINIMUM NUMBER OF VALID PIXELS IN EACH BIN REQUIRED
*               TO FORM A VALID OUTPUT PIXEL
*       FVAL (IN)
*       REAL
*               THE MINIMUM FRACTION OF VALID PIXELS IN EACH BIN
*               REQUIRED TO FORM A VALID OUTPUT PIXEL
*       ILEVEL (IN)
*       INTEGER
*               THE INTERACTION LEVEL
*       ICOM (IN)
*       INTEGER
*               IF SET TO 1,THE OUTPUT PIXELS ARE THE AVERAGE OF THE
*               INPUT PIXELS,IF SET TO 2,THEY ARE THE SUM OF THE INPUT
*               PIXELS
*       INTEGER
*       NPIXOUT (IN)
*       INTEGER
*               THE NUMBER OF PIXELS IN THE OUTPUT IMAGE
*       OUT (IN)
*       REAL(NPIXOUT)
*               AN ARRAY TO HOLD THE REAL VERSION OF THE OUTPUT PRIOR
*               TO CONVERSION TO INTEGER
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       EDRS:
*               LBGONE
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       D.S. BERRY
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlin),iout(npixout,1)
      character prbuf*80,oper*1
      integer limit(4)
      logical exit
      real x(nxy),y(nxy),out(npixout)
      parameter (minint=-32767,maxint=32767)
 
*
* SET MAXIMUM ALLOWABLE NUMBER OF LINE CROSSINGS
*
      parameter (mxcrs=100)
      real xcross(mxcrs)
 
*
* IF NO POLYGON WAS PROVIDED SET UP DEFAULT POLYGON
*
 
      if(nop.eq.1) then
         x(1)=1
         y(1)=1
         x(2)=npix
         y(2)=1
         x(3)=npix
         y(3)=nlin
         x(4)=1
         y(4)=nlin
      endif
 
 
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
         limit(2)=min(int(min(max(-1.0e8,xmax),1.0e8)),npix)
         miny=int(min(max(-1.0e8,ymin),1.0e8))
 
         if(real(miny).lt.ymin) miny=miny+1
         limit(3)=max(miny,1)
         limit(4)=min(int(min(max(-1.0e8,ymax),1.0e8)),nlin)
 
*
* SWAP LIMITS IF COLLAPSE ALONG Y REQUIRED
*
 
         if(idir.eq.1) then
            lim1=limit(1)
            lim2=limit(2)
            lim3=limit(3)
            lim4=limit(4)
 
         else
            lim1=limit(3)
            lim2=limit(4)
            lim3=limit(1)
            lim4=limit(2)
         endif
 
 
*
* INITIALIZE TOTAL GOOD AND BAD PIXEL COUNTERS
*
         nbatot=0
         ngotot=0
         nbaout=0
         ngoout=0
         dmax=-1.0e20
         dmin=1.0e20
 
*
* SCAN THROUGH LINES (OR COLUMNS) AFFECTED
*
 
         do 200 j=lim3,lim4
            yl=j
 
*
* PROBLEMS OCCUR IN COUNTING THE NUMBER OF INTERSECTIONS IF ANY
* IMAGE LINE OR COLUMN PASSES EXACTLY THROUGH A POLYGON VERTEX.
* THEREFORE THE LINE OR COLUMN POSITIONS ARE SHIFTED BY A NEGLIGIBLE
* AMOUNT PERT TO ENSURE THIS DOES NOT HAPPEN
*
            pert=0.0001
54          ncross=0
 
*
* SCAN THROUGH THE X,Y POSITIONS, TESTING IF EACH POLYGON SIDE
* INTERSECTS THE IMAGE LINE OR COLUMN
*
 
            do 99 n1=1,nxy
               n2=n1+1
 
*
* POLYGON VERTICES CYCLE BACK TO THE START
*
 
               if(n2.gt.nxy) n2=1
 
*
* SET UP NECESSARY IMAGE CO-ORDINATE VALUES
*
 
               if(idir.eq.1) then
                  cv1=y(n1)
                  cv2=y(n2)
                  dv1=x(n1)
                  dv2=x(n2)
 
               else
                  cv1=x(n1)
                  cv2=x(n2)
                  dv1=y(n1)
                  dv2=y(n2)
               endif
 
               test=((cv1-yl)-pert)*((yl-cv2)+pert)
 
*
* IF TEST IS ZERO, THE LINE OR COLUMN PASSES THROUGH A VERTEX... CHANGE
* PERT AND START AGAIN
*
 
               if(test.eq.0.0) then
                  pert=pert+0.0001
                  go to 54
 
 
*
* IF TEST IS POSITIVE, ADJACENT VERTICES LIE ON OPPOSITE SIDES
* OF THE IMAGE LINE OR COLUMN... CALCULATE THE POINT OF INTERSECTION AND
* STORE IT
*
 
               else if(test.gt.0) then
                  ncross=ncross+1
 
                  if(ncross.le.mxcrs) then
                     dy=cv2-cv1
 
                     if(abs(dy).lt.1.0e-20) dy=sign(1.0e-20,dy)
                     xcross(ncross)=dv1+((yl-cv1)*(dv2-dv1)/dy)
 
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
* IF LINE OR COLUMN INTERSECTS THE POLYGON, SORT INTERSECTIONS INTO X
* ORDER
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
 
               ngood=0
               nbad=0
               sum=0
 
*
* SCAN THROUGH THE ORDERED INTERSECTIONS IN PAIRS
*
 
               do 61 n=2,ncross,2
                  xmin=xcross(n-1)
                  minx=int(min(max(-1.0e8,xmin),1.0e8))
 
                  if(real(minx).lt.xmin) minx=minx+1
                  minx=max(lim1,minx)
                  maxx=min(lim2,int(min(max(-1.0e8,xcross(n)),1.0e8)))
 
*
* BIN PIXELS LYING BETWEEN EACH PAIR OF INTERSECTIONS,COUNTING THE
* NUMBER OF GOOD AND BAD PIXELS INCLUDED IN THE BIN.
*
 
                  do 60 i=minx,maxx
 
                     if(idir.eq.1) then
                        in=ia(i,j)
 
                     else
                        in=ia(j,i)
                     endif
 
 
                     if(in.ne.inval) then
                        sum=sum+(bscale*in+bzero)
                        ngood=ngood+1
 
                     else
                        nbad=nbad+1
                     endif
 
60                continue
 
 
*
* DO NEXT PAIR OF INTERSECTIONS ON THIS LINE OR COLUMN
*
61             continue
 
 
*
* IF THIS BIN SATISFIES THE MINPIX AND FRACVAL CONDITIONS,INCLUDE IT
* IN THE OUTPUT IMAGE,INCREMENTING THE GOOD OR BAD PIXEL COUNT
* ACCORDINGLY
*
 
               if((ngood.ge.minpix).and.(ngood.ge.fval*(ngood+nbad)))
     :           then
 
                  if(icom.eq.1) then
                     out(j+1-lim3)=sum/ngood
 
                  else
                     out(j+1-lim3)=sum
                     dmax=amax1(dmax,sum)
                     dmin=amin1(dmin,sum)
                  endif
 
                  ngoout=ngoout+1
 
               else
                  out(j+1-lim3)=inval
                  nbaout=nbaout+1
               endif
 
 
*
* INCREMENT THE TOTAL PIXEL COUNTERS
*
               ngotot=ngotot+ngood
               nbatot=nbatot+nbad
            endif
 
 
*
* DO NEXT LINE OR COLUMN
*
200      continue
 
 
*
* IF THERE ARE NO VALID PIXELS IN INPUT OR OUTPUT,SEND MESSAGE AND END
*
 
         if(ngotot.eq.0) then
            ierr=3
            goto 999
 
 
         else if(ngoout.eq.0) then
            ierr=4
            goto 999
 
         endif
 
 
*
* IF OPERATION IS SUMATION CALCULATE NEW SCALE AND ZERO FACTORS
*
         rbscl=1.0/bscale
 
         if(icom.eq.2) then
            bscale=max(1.0e-20,dmax-dmin)/(0.75*(maxint-minint))
            bzero=((dmax+dmin)-(maxint+minint))*0.5
            rbscl=1.0/bscale
         endif
 
 
*
* TRANSFORM REAL ARRAY 'OUT' TO INTEGER ARRAY 'IOUT'
*
 
         do 70 j=1,npixout
 
            if(out(j).ne.inval) then
               iout(j,1)=nint((out(j)-bzero)*rbscl)
 
            else
               iout(j,1)=inval
            endif
 
70       continue
 
 
*
* PRINT OUT INFO
*
 
         if(ilevel.ge.2) then
            ntot=nbaout+ngoout
            write(prbuf,77) ntot,nbaout
77          format('   OUTPUT CONTAINS ',i8,' PIXELS,OF WHICH ',i8,
     :      ' ARE INVALID')
            call lbgone(prbuf(45:))
            call lbgone(prbuf(20:))
            call wruser(' ',istat)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
 
            if(ilevel.ge.3) then
               ntot=ngotot+nbatot
               write(prbuf,78) ntot,nbatot
78             format('   INPUT AREA USED CONTAINS ',i8,' PIXELS,',
     :         'OF WHICH ',i8,' ARE INVALID.')
               call lbgone(prbuf(54:))
               call lbgone(prbuf(29:))
               call wruser(prbuf,istat)
            endif
 
         endif
 
      endif
 
999   return
 
      end
 
 
 
