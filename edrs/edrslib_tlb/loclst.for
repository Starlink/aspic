      subroutine loclst(xa,ya,ida,nin,image,npix,nlines,inval,isize
     : ,isign,shftmx,maxit,toll,ilevel,xb,yb,idb,nout,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE CENTROIDS OF A SET OF IMAGE FEATURES AND PRINT
*       THE RESULTS
*
*METHOD
*       PRINT TITLE FOR TABLE OF RESULTS, CALL LOCATE FOR EACH FEATURE
*       TO FIND THE CENTROIDS. COPY SUCCESSFUL LOCATIONS TO OUTPUT
*       POSITION AND IDENTIFIER LIST. PRINT RESULTS.
*
*ARGUMENTS
*       XA,YA (IN)
*       REAL(NIN)
*               APPROXIMATE FEATURE POSITIONS
*       IDA (IN)
*       BYTE(20,NIN)
*               LIST OF ASCII IDENTIFIERS FOR THE INPUT POSITIONS
*       NIN (IN)
*       INTEGER
*               NUMBER OF INPUT POSITIONS
*       IMAGE (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IMAGE
*       ISIZE (IN)
*       INTEGER
*               SIZE OF SEARCH AREA SIDE
*       ISIGN (IN)
*       INTEGER
*               POSITIVE IF IMAGE FEATURES ARE POSITIVE, OTHERWISE
*               NEGATIVE
*       SHFTMX (IN)
*       REAL
*               MAX SHIFT ALLOWED FROM STARTING POSITIONS
*       MAXIT (IN)
*       INTEGER
*               MAX NUMBER OF CENTROIDING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY REQUIRED IN THE CENTROID POSITION
*       ILEVEL (IN)
*       INTEGER
*               FLAG TO CONTROL PRINTING OF RESULTS
*       XB,YB (OUT)
*       REAL(NIN)
*               OUTPUT CENTROID ESTIMATES. THOSE NOT FOUND ARE OMITTED
*       IDB (OUT)
*       BYTE(20,NIN)
*               ASCII IDENTIFIERS FOR THE POSITIONS IN THE OUTPUT
*               POSITION LISTS X,Y
*       NOUT (OUT)
*       INTEGER
*               THE NUMBER OF SUCCESSFULLY FOUND LOCATIONS IN X,Y,IDB
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               LOCATE
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES INTEGER*2 AND BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 image(npix,nlines)
      real xa(nin),ya(nin),xb(nin),yb(nin)
      byte ida(20,nin),idb(20,nin)
      character prbuf*80,errmsg(2)*24,id*20
      data errmsg/'  MAXIMUM SHIFT EXCEEDED','  NO IMAGE FEATURE FOUND'
     :/
 
*
* IF ILEVEL IS ABOVE 1, PRINT TITLES
*
 
      if(ilevel.gt.1) then
         call wruser(' ',istat)
         write(prbuf,14)
14       format(t27,'POSITION SEARCHED',t57,'CENTROID FOUND')
         call wruser(prbuf,istat)
         write(prbuf,17)
17       format(t27,'-----------------',t57,'--------------')
         call wruser(prbuf,istat)
         write(prbuf,15)
15       format('     IDENTIFIER',t24,2(' X COORD.',5x,' Y COORD.',5x))
         call wruser(prbuf,istat)
         write(prbuf,16)
16       format('     ----------',t24,2(' --------',5x,' --------',5x))
         call wruser(prbuf,istat)
      endif
 
 
*
* SCAN THROUGH LIST, CALLING LOCATE TO FIND EACH CENTROID IN TURN
*
      nout=0
 
      do 7 i=1,nin
         call locate(xa(i),ya(i),image,npix,nlines,inval,isize,isign
     :    ,shftmx,maxit,toll,xb(nout+1),yb(nout+1),ierr)
 
*
* IF A FATAL ERROR IS FOUND, RETURN
*
 
         if(ierr.ge.3) go to 99
 
*
* IF OK, TRANSFER ID TO OUTPUT LIST
*
 
         if(ierr.eq.0) then
            nout=nout+1
 
            do 77 l=1,20
               idb(l,nout)=ida(l,i)
77          continue
 
 
*
* IF ILEVEL IS ABOVE 1, PRINT THE SUCCESSFUL LOCATION
*
 
            if(ilevel.gt.1) then
 
               do 78 l=1,20
                  nchar=ida(l,i)
                  id(l:l)=char(nchar)
78             continue
 
               write(prbuf,10)id,xa(i),ya(i),xb(nout),yb(nout)
10             format(' ',a20,t23,4(ss,g13.6,1x))
               call wruser(prbuf,istat)
            endif
 
 
         else
 
*
* IF THE CENTROID WAS NOT FOUND, THEN IF ILEVEL IS ABOVE 2, PRINT
* DETAILS
*
 
            if(ilevel.gt.2) then
 
               do 79 l=1,20
                  nchar=ida(l,i)
                  id(l:l)=char(nchar)
79             continue
 
               write(prbuf,11)id,xa(i),ya(i),errmsg(ierr)
11             format(' ',a20,t23,2(ss,g13.6,1x),a24)
               call wruser(prbuf,istat)
            endif
 
         endif
 
7     continue
 
 
*
* IF A LIST HAS BEEN PRINTED, ADD SPACING AT THE END
*
 
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         call wruser(' ',istat)
      endif
 
99    return
 
      end
 
 
 
