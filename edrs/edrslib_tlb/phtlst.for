      subroutine phtlst(xa,ya,ida,nin,image,npix,nlines,inval,scale
     : ,zero,isize,sigma,e,theta,g,range,zermag,ilevel,xb,yb,idb,rmag
     :  ,nout)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN INTEGRATED MAGNITUDES FOR A SET OF STARS, GIVEN
*       APPROXIMATE POSITIONS, AND TO PRINT THE RESULTS.
*
*METHOD
*       PRINT HEADINGS. CALL PHOTRY FOR EACH STAR TO CALCULATE THE
*       INTEGRATED MAGNITUDE. PRINT A TABLE OF THE RESULTS.
*
*ARGUMENTS
*       XA,YA (IN)
*       REAL(NIN)
*               INPUT LISTS OF APPROXIMATE POSITIONS
*       IDA (IN)
*       BYTE(20,NIN)
*               ASCII IDENTIFIERS FOR THE INPUT POSITIONS
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
*       SCALE,ZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR IMAGE
*       ISIZE (IN)
*       INTEGER
*               SIDE OF SEARCH SQUARE IN WHICH DATA IS USED
*       SIGMA (IN)
*       REAL
*               THE 'SIGMA' FOR THE STAR PROFILE
*       E (IN)
*       REAL
*               THE AXIS RATIO OF THE ELLIPTICAL STAR IMAGES
*       THETA (IN)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS TO THE X DIRECTION
*               IN RADIANS (X THROUGH Y POSITIVE)
*       G (IN)
*       REAL
*               THE EXPONENT IN THE STAR RADIAL PROFILE FUNCTION
*       RANGE (IN)
*       REAL
*               THE RADIUS IN UNITS OF SIGMA OUT TO WHICH THE RADIAL
*               PROFILE IS FITTED
*       ZERMAG (IN)
*       REAL
*               CONSTANT MAGNITUDE ADDED TO ALL RESULTS
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       XB,YB (OUT)
*       REAL(NIN)
*               OUTPUT X,Y STAR POSITIONS
*       IDB (OUT)
*       BYTE(20,NIN)
*               ASCII IDENTIFIERS FOR OUTPUT POSITIONS
*       RMAG (OUT)
*       REAL(NIN)
*               OUTPUT MAGNITUDES
*       NOUT (OUT)
*       INTEGER
*               NUMBER OF OUTPUT POSITIONS IN XB,YB,IDB,RMAG
*
*CALLS
*       THIS PACKAGE:
*               PHOTRY,BYTCPY
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
      real xa(nin),ya(nin),xb(nin),yb(nin),rmag(nin)
      byte ida(20,nin),idb(20,nin)
      character prbuf*80,errmsg(5)*30,id*20
 
*
* SET UP ERROR MESSAGES
*
      data errmsg(1)/' STAR CENTRE NOT FOUND        '/,errmsg(2)/
     :' TOO LITTLE DATA TO FIND STAR '/,errmsg(3)/
     :' TOO LITTLE DATA TO FIND STAR '/,errmsg(4)/
     :' TOO LITTLE DATA TO FIT STAR  '/,errmsg(5)/
     :' FIT GIVES NEGATIVE BRIGHTNESS'/
 
*
* IF ILEVEL.GE.2 PRINT HEADINGS FOR TABLE OF RESULTS
*
 
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         write(prbuf,16)
16       format(48x,'PEAK BRIGHTNESS')
         call wruser(prbuf,istat)
         write(prbuf,14)
14       format(5x,'IDENTIFIER',t24,' X COORD.',5x,' Y COORD.',4x,
     :   '/ BACKGROUND',4x,'MAGNITUDE')
         call wruser(prbuf,istat)
         write(prbuf,15)
15       format(5x,'----------',t24,' --------',5x,' --------',4x,
     :   '------------',4x,'---------')
         call wruser(prbuf,istat)
      endif
 
 
*
* FOR EACH INPUT POSITION, CALL PHOTRY TO EVALUATE THE STELLAR
* MAGNITUDE
*
      nout=0
 
      do 7 i=1,nin
         call photry(image,npix,nlines,inval,scale,zero,xa(i),ya(i)
     :    ,isize,sigma,e,theta,g,range,xb(nout+1),yb(nout+1),cenden
     :     ,backgd,toti,totmag,ierr)
 
*
* IF THE MAGNITUDE WAS OBTAINED SUCCESSFULLY, INSERT THE IDENTIFIER
* AND MAGNITUDE IN THE OUTPUT LIST
*
 
         if(ierr.eq.0) then
            nout=nout+1
            call bytcpy(ida(1,i),idb(1,nout),20)
            rmag(nout)=totmag+zermag
 
*
* IF ILEVEL.GE.2 PRINT THE SUCCESSFUL RESULT
*
 
            if(ilevel.ge.2) then
 
               do 78 l=1,20
                  nchar=ida(l,i)
                  id(l:l)=char(nchar)
78             continue
 
               write(prbuf,10)id,xb(nout),yb(nout),cenden,rmag(nout)
10             format(1x,a20,t23,2(ss,g13.6,1x),1x,ss,g12.5,4x,ss,g12
     :          .5)
               call wruser(prbuf,istat)
               write(prbuf,9)backgd
9              format(51x,ss,g12.5)
               call wruser(prbuf,istat)
            endif
 
 
         else
 
*
* IF THE MAGNITUDE WAS NOT FOUND, THEN IF ILEVEL.GE.3 PRINT
* THE COORDINATES AND AN ERROR MESSAGE
*
 
            if(ilevel.ge.3) then
 
               do 79 l=1,20
                  nchar=ida(l,i)
                  id(l:l)=char(nchar)
79             continue
 
               write(prbuf,11)id,xa(i),ya(i),errmsg(ierr)
11             format(1x,a20,t23,2(ss,g13.6,1x),a30)
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
 
      return
 
      end
 
 
 
