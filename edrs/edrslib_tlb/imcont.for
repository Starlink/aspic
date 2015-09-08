      subroutine imcont(ia,npix,nlines,inval,scale,zero,cont,ncont,
     :                  done,colour,iplane,device)
 
*
*
      character device*(*),colour*(*)
      parameter (maxpts=5000)
 
*
      integer*2 ia(npix,nlines)
      real cont(ncont),b(5),cx(4),cy(4),dx(4),dy(4),x(maxpts),y(maxpts)
      integer newsid(4),imove(4),jmove(4)
      logical confus,linend,offimg,badpix,anote,above,overly
      logical done(npix,nlines)
 
*
* SET UP FUNCTIONS
*
      offimg(i,j)=(i.lt.1).or.(j.lt.1).or.(i.ge.npix).or.(j.ge.nlines)
      badpix(i,j)=(ia(i,j).eq.inval).or.(ia(i+1,j).eq.inval).or.(ia(i
     : +1,j+1).eq.inval).or.(ia(i,j+1).eq.inval)
      dist(i,j)=(x(i)-x(j))**2+(y(i)-y(j))**2
 
*
* SET UP COORDINATES OF CELL CORNERS
*
      data cx/0.0,1.0,1.0,0.0/
      data cy/0.0,0.0,1.0,1.0/
 
*
* SET UP DIFFERENTIAL COORDINATES OF CELL CORNERS
*
      data dx/1.0,0.0,-1.0,0.0/
      data dy/0.0,1.0,0.0,-1.0/
 
*
* SET UP TABLES GIVING DIRECTIONS TO MOVE FROM THE CELL SIDE
* WHERE A CONTOUR LEAVES
*
      data imove/0,1,0,-1/
      data jmove/-1,0,1,0/
 
*
* SET UP TABLE GIVING THE SIDE OF ENTRY IN THE NEW CELL FROM
* THE SIDE OF EXIT FROM THE OLD CELL
*
      data newsid/3,4,1,2/
 
*
* SEE IF GRAPHICS ARE TO GO TO THE ARGS OR IKON OVERLAYS
*
      call sgs_widen(device,itype,iconid,ierr)
      if(itype.eq.161.or.itype.eq.3201) then
         overly=.true.
      else
         overly=.false.
      endif

*
* OPEN GRAPHICAL OUTPUT AND DRAW AXES
*
 
      if(.not.overly)then
 
*
* OPEN SGS
*
         istat=0
         call sgs_open(device,idzone,istat)
         if(istat.ne.0) then
            call wrerr('NODEVICE')
            goto 99
         endif
 
*
* GET A ZONE TO PLOT IN
*
         call sgs_zshap(1.,'CL',izone1,istat)
         call sgs_selz(izone1,istat)
 
*
         call sgs_zone(0.05,0.95,0.05,0.95,izone2,istat)
         call sgs_selz(izone2,istat)
 
*
         ar=real(npix)/real(nlines)
         call sgs_zshap(ar,'CC',izone3,istat)
         call sgs_selz(izone3,istat)
 
*
* SET COORDINATE SYSTEM
*
         call sgs_sw(0.5,npix+0.5,0.5,nlines+0.5,istat)
         call sgs_box(0.5,npix+0.5,0.5,nlines+0.5)

*
* IF OUTPUT IS GOING TO ARGS OR IKON OVERLAYS, THE COORDINATE
* SYSTEM OF THE LAST DISPLAYED IMAGE WILL BE USED FOR DRAWING THE 
* CONTOURS. OPEN THE DEVICE AND SET UP THE REQUIRED OVERLAY PLANE
* WITH THE REQUIRED COLOUR
*
      else
         if(itype.eq.161) then
            call argsop(.false.,.false.,ierr)
            device='ARGS'
         else
            call ikonop(.false.,.false.,ierr)
            device='IKON'
         endif
         if(ierr.ne.0) goto 99
         call ovopen(device,iplane,colour)
      endif
 
*
* SCAN THROUGH EACH CONTOUR LEVEL
*
 
      do 100 k=1,ncont
         cval=cont(k)
 
*
* INITIALLISE STORE OF CELLS DONE
*
 
         do 42 j=1,nlines-1
 
            do 41 i=1,npix-1
               done(i,j)=.false.
41          continue
 
42       continue
 
 
*
* INITIALLISE COUNTER FOR NUMBER OF X,Y COORDINATES TO PLOT
*
         npts=0
 
*
* SCAN IMAGE, LOOKING FOR A CELL CONTAINING THE CURRENT CONTOUR
* LEVEL
*
 
         do 120 j=1,nlines-1
 
            do 110 i=1,npix-1
 
*
* IF CELL HAS ALREADY BEEN CONTOURED, OMIT IT
*
 
               if(done(i,j))go to 110
 
*
* NOTE THIS CELL HAS BEEN LOOKED AT
*
               done(i,j)=.true.
 
*
* DON'T USE THIS CELL IF THERE IS A BAD PIXEL ADJACENT
*
 
               if(badpix(i,j))go to 110
 
*
* EXTRACT DATA VALUES AND TEST IF THEY CONTAIN THE CONTOUR
*
               b(1)=ia(i,j)*scale+zero
               b(2)=ia(i+1,j)*scale+zero
               b(3)=ia(i+1,j+1)*scale+zero
               b(4)=ia(i,j+1)*scale+zero
               bmax=max(b(1),b(2),b(3),b(4))
 
               if(cval.ge.bmax)go to 110
               bmin=min(b(1),b(2),b(3),b(4))
 
               if(cval.le.bmin)go to 110
               b(5)=b(1)
 
*
* INITIALLISE POINTERS TO CELLS ON THIS CONTOUR
*
               ii=i
               jj=j
 
*
* INITIALLISE CELL SIDE WHERE CONTOUR ENTERS
*
               lin=0
30             nexit=0
 
*
* SCAN CELL SIDES, SEARCHING FOR INTERSECTIONS WITH CONTOUR
*
               anote=b(1).ge.cval
 
               do 90 l=1,4
                  above=b(l+1).ge.cval
 
*
* DON'T COUNT CONTOUR EXITS FROM THE SAME SIDE AS ENTRY
*
 
                  if((above.neqv.anote).and.(l.ne.lin))then
                     lside=l
                     nexit=nexit+1
                     npts=npts+1
 
*
* CALCULATE THE COORDINATES OF CONTOUR EXIT AND STORE IN X,Y
*
                     fract=(cval-b(l))/(b(l+1)-b(l))
                     x(npts)=ii+cx(l)+dx(l)*fract
                     y(npts)=jj+cy(l)+dy(l)*fract
                  endif
 
                  anote=above
90             continue
 
 
*
* CELL IS CONFUSED IF THE NUMBER OF CONTOUR EXITS DOES NOT MATCH
* THE NUMBER OF ENTRIES
*
 
               if(lin.eq.0)then
                  confus=nexit.ne.2
 
               else
                  confus=nexit.ne.1
               endif
 
 
*
* FIND COORDINATES OF NEXT CELL WHICH CONTOUR ENTERS
*
               ii=ii+imove(lside)
               jj=jj+jmove(lside)
 
*
* FIND SIDE OF NEW CELL WHICH IT ENTERS
*
               lin=newsid(lside)
 
*
* END OF CURRENT CONTOUR LINE IF CONTOUR GOES OFF EDGE OF IMAGE,
* HITS AN INVALID PIXEL, ENTERS A CELL ALREADY CONTOURED, LEAVES
* A CONFUSED CELL OR EXCEEDS THE STORAGE SPACE FOR X AND Y
*
 
               if(offimg(ii,jj))then
                  linend=.true.
 
               else
                  linend=badpix(ii,jj).or.done(ii,jj).or.confus.or.
     :            (npts.ge.maxpts)
               endif
 
 
*
* IF CONTINUING ON THIS CONTOUR, EXTRACT DATA FOR NEXT CELL
* AND MARK THE CELL DONE
*
 
               if(.not.linend)then
                  b(1)=ia(ii,jj)*scale+zero
                  b(2)=ia(ii+1,jj)*scale+zero
                  b(3)=ia(ii+1,jj+1)*scale+zero
                  b(4)=ia(ii,jj+1)*scale+zero
                  b(5)=b(1)
                  done(ii,jj)=.true.
 
*
* RETURN TO ANALYSE THE NEW CELL
*
                  go to 30
 
               endif
 
 
*
* IF THE LAST CELL ON A CONTOUR WAS CONFUSED, DETERMINE WHICH
* PAIRING OF EXIT POINTS GIVES THE SHORTEST CONTOUR
*
 
               if(confus)then
 
                  if(dist(npts,npts-1)+dist(npts-2,npts-3).gt.dist(npts
     :             -1,npts-2)+dist(npts-3,npts))then
 
*
* SWAP THE PAIRING IF NECESSARY
*
                     xtemp=x(npts-1)
                     ytemp=y(npts-1)
                     x(npts-1)=x(npts-2)
                     y(npts-1)=y(npts-2)
                     x(npts-2)=xtemp
                     y(npts-2)=ytemp
                  endif
 
                  npts=npts-2
               endif
 
 
*
* PLOT THE STORED CONTOUR AND RESET THE NUMBER OF COORDINATES
*
 
               if(.not.overly)then
                  call gpl(npts,x,y)
 
                  if(confus)call gpl(2,x(npts+1),y(npts+1))
 
               else 
                  call ovpoly(x,y,npts,device)
 
                  if(confus)call ovpoly(x(npts+1),y(npts+1),2,device)
               endif
 
               npts=0
110         continue
 
120      continue
 
100   continue
 
 
*
* CLOSE GRAPHICS OUTPUT
*
 
      if(.not.overly)then
         call sgs_close
 
      else 
         call ovclos(device,iplane)
         if(device.eq.'ARGS') then
            call argscl
         else
            call ikoncl
         endif
      endif
 
 99   continue
 
      end
 
 
 
