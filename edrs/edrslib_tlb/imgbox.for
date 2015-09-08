      subroutine imgbox(ia,npix,nlines,inval,ix,iy,nmin,istor,nstor
     : ,iline,nline)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO SMOOTH AN IMAGE USING A RECTANGULAR 'LOCAL MEAN' FILTER
*
*METHOD
*       SMOOTH EACH LINE BY RUNNING A LOCAL MEAN FILTER THROUGH IT.
*       STORE THE RESULT IN WORKSPACE, THEN REPEAT THE PROCESS DOWN
*       THE IMAGE COLUMNS. TAKE ACCOUNT OF INVALID PIXELS.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       IX,IY (IN)
*       INTEGER
*               SIZE OF SMOOTHING RECTANGLE IN X,Y
*       NMIN (IN)
*       INTEGER
*               MIN NUMBER OF VALID PIXELS NEEDED IN SMOOTHING AREA
*       ISTOR (WORKSPACE)
*       INTEGER(NPIX,NLINES)
*               INTERMEDIATE STORAGE
*       NSTOR (WORKSPACE)
*       INTEGER*2(NPIX,NLINES)
*               INTERMEDIATE STORAGE
*       ILINE (WORKSPACE)
*       INTEGER(NPIX)
*               INTERMEDIATE STORAGE
*       NLINE (WORKSPACE)
*       INTEGER*2(NPIX)
*               INTERMEDIATE STORAGE
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
      integer*2 ia(npix,nlines),nstor(npix,nlines),nline(npix)
      integer istor(npix,nlines),iline(npix),oldx,newx,oldy,newy,thresh
      npix2=2*npix
      nline2=2*nlines
 
*
* MAKE BOX SIZES NEXT LARGEST ODD NUMBER AND POSITIVE
*
      idx=max(0,ix/2)
      iside=2*idx+1
      idy=max(0,iy/2)
      jside=2*idy+1
 
*
* SET THRESHOLD FOR NO. OF GOOD PIXELS IN BOX BETWEEN 1 AND MAX
* POSSIBLE NUMBER
*
      thresh=min(max(1,nmin),iside*jside)
 
*
* FIRST SMOOTH ALONG EACH LINE
* ----------------------------
*
 
      do 101 j=1,nlines
 
*
* INITIALLISE RUNNING SUM OF DATA (ISUM) AND NO. OF GOOD PIXELS (NSUM)
*
         isum=0
         nsum=0
 
*
* START WITH BOX SIZE ISIDE*1 CENTRED ON PIXEL (0,J)
*
 
         do 102 ii=-idx,idx
            i=ii
 
*
* BOX WILL PROJECT OFF ENDS OF LINES, SO REFLECT TO KEEP IT INSIDE
*
 
            if(i.lt.1) i=2-i
 
            if(i.gt.npix) i=npix2-i
 
*
* PROTECT AGAINST EXTREME CASES WHERE BOX IS SO BIG IT GOES OFF
* OPPOSITE END AFTER REFLECTION
*
            i=min(max(1,i),npix)
 
*
* IF PIXEL FOUND IS VALID, ADD IT TO SUM OF PIXELS WITHIN BOX
*
 
            if(ia(i,j).ne.inval) then
               isum=isum+ia(i,j)
               nsum=nsum+1
            endif
 
102      continue
 
 
*
* NOW STEP THE BOX ALONG THE LINE
*
 
         do 106 i=1,npix
 
*
* FIND POSITION OF OLD PIXEL TO BE REMOVED FROM LEFT AND NEW PIXEL
* TO BE ADDED AT RIGHT
*
            oldx=i-idx-1
            newx=i+idx
 
*
* REFLECT AT ENDS OF LINE
*
 
            if(oldx.lt.1) oldx=2-oldx
 
            if(oldx.gt.npix) oldx=npix2-oldx
            oldx=min(max(1,oldx),npix)
 
            if(newx.lt.1) newx=2-newx
 
            if(newx.gt.npix) newx=npix2-newx
            newx=min(max(1,newx),npix)
 
*
* IF OLD PIXEL IS VALID, SUBTRACT FROM SUMS
*
 
            if(ia(oldx,j).ne.inval) then
               isum=isum-ia(oldx,j)
               nsum=nsum-1
            endif
 
 
*
* IF NEW PIXEL IS VALID, ADD TO SUMS
*
 
            if(ia(newx,j).ne.inval) then
               isum=isum+ia(newx,j)
               nsum=nsum+1
            endif
 
 
*
* STORE SUMS ALONG LINE IN WORKSPACE
*
            istor(i,j)=isum
            nstor(i,j)=nsum
106      continue
 
101   continue
 
 
*
* NOW SMOOTH DOWN COLUMNS
* -----------------------
*
* INITIALLISE SUMS.. THIS TIME PROCESSING A WHOLE LINE AT ONCE
*
 
      do 16 i=1,npix
         iline(i)=0
         nline(i)=0
16    continue
 
 
*
* SUM OVER A BOX OF SIZE 1*JSIDE CENTRED ON PIXEL (I,0), WHERE I
* SCANS ALONG WHOLE LINE
*
 
      do 602 jj=-idy,idy
         j=jj
 
*
* REFLECT AT TOP AND BOTTOM OF IMAGE
*
 
         if(j.lt.1) j=2-j
 
         if(j.gt.nlines) j=nline2-j
         j=min(max(1,j),nlines)
 
*
* FORM A SUM FOR EACH PIXEL IN THE LINE FROM THE DATA NOW STORED IN
* THE WORKSPACE
*
 
         do 161 i=1,npix
            iline(i)=iline(i)+istor(i,j)
            nline(i)=nline(i)+nstor(i,j)
161      continue
 
602   continue
 
 
*
* NOW STEP DOWN THE IMAGE
*
 
      do 606 j=1,nlines
 
*
* FIND LOCATION OF OLD LINE TO SUBTRACT AT TOP AND NEW LINE
* TO ADD AT BOTTOM
*
         oldy=j-idy-1
         newy=j+idy
 
*
* REFLECT AT TOP AND BOTTOM OF IMAGE
*
 
         if(oldy.lt.1) oldy=2-oldy
 
         if(oldy.gt.nlines) oldy=nline2-oldy
         oldy=min(max(1,oldy),nlines)
 
         if(newy.lt.1) newy=2-newy
 
         if(newy.gt.nlines) newy=nline2-newy
         newy=min(max(1,newy),nlines)
 
*
* TAKE OFF OLD LINE
*
 
         do 162 i=1,npix
            iline(i)=iline(i)-istor(i,oldy)
            nline(i)=nline(i)-nstor(i,oldy)
 
*
* ADD NEW LINE
*
            iline(i)=iline(i)+istor(i,newy)
            nline(i)=nline(i)+nstor(i,newy)
 
*
* IF SUFFICIENT PIXELS PRESENT, FORM OUTPUT, OTHERWISE OUTPUT IS
* NOT VALID
*
 
            if(nline(i).ge.thresh) then
               ia(i,j)=nint(real(iline(i))/nline(i))
 
            else
               ia(i,j)=inval
            endif
 
162      continue
 
606   continue
 
      return
 
      end
 
 
 
