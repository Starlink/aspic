      subroutine viewit(ia,npix,nlin,inval,bscale,bzero,ix,iy,isize
     : ,title,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO WRITE OUT THE VALUES OF THE INPUT IMAGE PIXELS CONTAINED
*       WITHIN THE INTERSECTION OF THE IMAGE AND A SQUARE AREA DEFINED
*       BY CENTRE (IX,IY) AND SIDE ISIZE,TO A FILE "VIEW.LIS" READY FOR
*       PRINTING.
*
*METHOD
*      WRITE OUT IMAGE SCALE FACTOR,ZERO LEVEL AND TITLE AS HEADER.
*      CALCULATE THE CORNERS OF THE RECTANGULAR INTERSECTION OF THE
*      SQUARE AREA AND THE IMAGE AND WRITE OUT CO-ORDINATES OF THE TOP
*      LEFT HAND CORNER.WRITE OUT THE INTEGER VALUE OF THE IMAGE AT
*      EACH POINT IN THE RECTANGLE.WRITE OUT THE CO-ORDINATES OF THE
*      BOTTOM RIGHT HAND CORNER.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLIN)
*              THE INPUT IMAGE
*       NPIX,NLIN (IN)
*       INTEGER
*              THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*              THE INVALID FLAG VALUE
*       BSCALE (IN)
*       REAL
*              THE IMAGE SCALE FACTOR
*       BZERO (IN)
*       REAL
*              THE IMAGE ZERO LEVEL
*       IX,IY (IN)
*       INTEGER
*              THE CO-ORDINATES OF THE CENTRE OF THE SQUARE AREA
*       ISIZE (IN)
*       INTEGER
*              THE LENGTH OF A SIDE OF THE SQUARE AREA
*       TITLE (IN)
*       CHARACTER
*              THE TITLE OF THE INPUT IMAGE
*       IERR (OUT)
*       INTEGER
*              THE ERROR FLAG.ZERO FOR SUCCESS
*
*CALLS
*       EDRS:
*              LBGONE
*       STARLINK:
*              ITOC
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlin)
      character prbuf*129,title*(*)
 
*
* ATTACH FILE "VIEW.LIS" TO FORTRAN UNIT 10
*
      open(10,file='VIEW.LIS',status='NEW')
 
*
* WRITE OUT TITLE
*
      write(10,'(1X,A)') title
 
*
* WRITE OUT SCALE FACTOR AND ZERO LEVEL
*
      write(10,10) bscale,bzero
10    format(' SCALE=',g13.6,' ZERO=',g13.6)
      write(10,15)
15    format(1x)
 
*
* SET UP CORNERS OF AREA TO BE PRINTED OUT
*
      ihalf=real(isize)/2.0
      ixlo=max0(1,ix-ihalf)
      ixhi=min0(npix,ix+ihalf)
      iylo=max0(1,iy-ihalf)
      iyhi=min0(nlin,iy+ihalf)
 
*
* IF SQUARE DOES NOT INTERSECT IMAGE,GIVE MESSAGE AND END
*
      ierr=0
 
      if((ixlo.gt.npix).or.(iylo.gt.nlin)) then
         ierr=1
         goto 99
 
 
      else if((ixhi.lt.1).or.(iyhi.lt.1)) then
         ierr=1
         goto 99
 
      endif
 
 
*
* WRITE OUT CO-ORDINATES OF TOP LEFT CORNER
*
      write(prbuf,20) ixlo,iyhi
20    format(' (',i7,',',i7,')')
      call lbgone(prbuf(11:))
      call lbgone(prbuf(3:))
      write(10,'(A)') prbuf
 
*
* SCAN THROUGH LINES IN THE AREA TO BE PRINTED
*
 
      do 40 j=iyhi,iylo,-1
 
*
* SET UP COUNTER 'N' FOR NO. OF PIXEL VALUES CONTAINED IN PRBUF
* AND 'IVS' THE POSITION OF THE FIRST PIXEL IN EACH LINE OF
* OUTPUT.
*
         n=0
         ivs=0
 
*
* SCAN THROUGH PIXELS IN THIS LINE
*
 
         do 30 i=ixlo,ixhi
            in=ia(i,j)
 
*
* IF PRBUF CONTAINS 18 VALUES,WRITE IT OUT AND INITIALIZE IT
*
 
            if(n.eq.18) then
               write(10,'(A)') prbuf
               prbuf=' '
 
*
* INCREMENT IVS BY 18 AND RESET N
*
               ivs=ivs+18
               n=0
            endif
 
 
*
* CACULATE 'IV',THE CO-ORDINATE IN THE INTERNAL FILE,PRBUF,TO WRITE TO
*
            iv=i+1-ixlo-ivs
 
*
* IF INPUT PIXEL IS VALID CALL ITOC TO COVERT ITS INTEGER VALUE TO THE
* CORRESPONDING CHARACTER VALUE
*
 
            if(in.ne.inval) then
               call itoc(in,prbuf(iv*7-6:iv*7),istat)
               n=n+1
 
*
* IF INPUT PIXEL IS INVALID WRITE OUT "  ***  "
*
 
            else
               prbuf(iv*7-6:iv*7)='  ***  '
               n=n+1
            endif
 
 
*
* DO NEXT PIXEL
*
30       continue
 
 
*
* WRITE OUT ANY PIXEL VALUES LEFT FROM THIS LINE OF INPUT,AND FINISH
* IT WITH A COMMA
*
         prbuf(iv*7+1:iv*7+1)=','
         write(10,'(A)') prbuf
 
*
* DO NEXT LINE OF INPUT
*
40    continue
 
 
*
* WRITE OUT CO-ORDINATES OF BOTTOM RIGHT CORNER
*
      prbuf=' '
      nchar=min0(ixhi-ixlo+1,18)*7-18
      write(prbuf(nchar:),20) ixhi,iylo
      call lbgone(prbuf(nchar+11:))
      call lbgone(prbuf(nchar+3:))
      write(10,'(A)') prbuf
 
*
* CLOSE OUTPUT FILE
*
      close(10)
 
*
* FINISH
*
99    return
 
      end
 
 
 
