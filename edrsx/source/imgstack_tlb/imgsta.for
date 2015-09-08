      subroutine imgsta
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COMBINE MUTUALLY ALIGNED AND NORMALISED IMAGES INTO A
*       SINGLE OUTPUT IMAGE. A copy of EDRS NSTACK (see method).
*
*SOURCE
*       IMGSTA.FOR in IMGSTACK.TLB
*
*METHOD
*       OBTAIN EACH INPUT IMAGE AND EXTRACT THE REQUIRED DESCRIPTOR
*       ITEMS. OBTAIN AN OUTPUT IMAGE. TREAT EACH LINE IN TURN...
*       COPY ONE LINE FROM EACH IMAGE INTO WORKSPACE USING LINSTK, THEN
*       COMBINE THEM INTO A SINGLE OUTPUT LINE USING COMBIN. WHEN ALL
*       LINES HAVE BEEN TREATED, UPDATE THE OUTPUT DESCRIPTOR ITEMS
*          IMGSTACK is a copy of the EDRS program NSTACK, modified so
*       that the brightest regions cannot saturate when the first image
*       given does not have a wide enough dynamic range.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       IMAGE1,IMAGE2...IMAGE20
*               THE INPUT IMAGES
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       OUTPUT
*               THE OUTPUT IMAGE
*       METHOD
*               THE METHOD OF COMBINING THE IMAGES
*       ERRORS
*               ESTIMATES OF THE IMAGE ERRORS..USED IN WEIGHTING THE
*               IMAGES
*       MINIMAGE
*               MINIMUM NUMBER OF GOOD IMAGES REQUIRED AT ANY POINT
*       BADVALUE/ERROR/
*               ACCESSED IF AN INVALID VALUE IS GIVEN FOR 'ERRORS'
*       TITLE
*               TITLE TO REPLACE THE TITLE OF 'IMAGE1' IN THE OUTPUT
*               IMAGE
*
*CALLS
*       THIS PACKAGE (UTILITIES.TLB):
*               SCAZER
*       EDRS:
*               LBGONE,GT2DIR,GTDSCR,GT2DIW,GETCMD,GETPAR,RNGERR,LINSTK,
*               COMBIN,PTDSCR
*       INTERIM:
*               GETDYN,WRERR,RDKEYR,CNPAR,RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH (modified by D.S. Berry)
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,method*8,imgchr*8

*
* SET MAXIMUM NUMBER OF IMAGES WHICH CAN BE STACKED
*
      parameter (maxim=20)

*
* DIMENSION ARRAYS TO HOLD DETAILS OF EACH IMAGE
*
      real weight(maxim),scale(maxim),zero(maxim),work(maxim,2)
      integer inval(maxim),npix(maxim),nlines(maxim),ip(maxim)

*
* INITIALLISE SIZE OF OUTPUT IMAGE
*
      npout=1
      nlout=1

*
* COUNT THROUGH EACH POSSIBLE INPUT IMAGE
*

      do 10 im=1,maxim

*
* FORM THE APPROPRIATE PARAMETER NAME
*
         write(imgchr,11)im
11       format('IMAGE',i3)
         call lbgone(imgchr(6:))

*
* OBTAIN THE INPUT IMAGE
*
         call gt2dir(imgchr,102,(im.gt.2),npix(im),nlines(im),ip(im)
     :    ,ierrim)

*
* IF NO IMAGE WAS OBTAINED, ABORT IF IT WAS ONE OF THE FIRST TWO
* IMAGES. OTHERWISE END THE INPUT LOOP.
*

         if(ierrim.ne.0) then

            if(im.le.2) then
               go to 99


            else
               go to 101

            endif


         else

*
* IF IMAGE WAS OBTAINED SUCCESSFULLY, SET OUTPUT IMAGE SIZE TO THE
* LARGEST IMAGE SO FAR
*
            npout=max(npout,npix(im))
            nlout=max(nlout,nlines(im))

*
* STORE THE NECESSARY DESCRIPTOR INFORMATION FROM THE IMAGE
*
            inval(im)=-100000
            scale(im)=1.0
            zero(im)=0.0
            call gtdscr(imgchr,'INVAL','INTEGER',inval(im),rval,cval
     :       ,ierr)
            call gtdscr(imgchr,'BSCALE','REAL',ival,scale(im),cval,
     :      ierr)
            call gtdscr(imgchr,'BZERO','REAL',ival,zero(im),cval,ierr)
         endif


*
* END OF INPUT LOOP:
*
10    continue


*
* SET NUMBER OF IMAGES BEING USED AND OBTAIN WORKSPACE
*
101   im=im-1
      call getdyn('LSTACK',102,npout*im,iplst,istlst)

*
* IF WORKSPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*

      if(istlst.ne.0) then
         call wrerr('NOSPACE')
         go to 99

      endif


*
* OBTAIN OUTPUT IMAGE FRAME
*
      call gt2diw('OUTPUT',102,.false.,npout,nlout,ipout,ierrou)

      if(ierrou.eq.0) then

*
* OUTPUT IMAGE OBTAINED SUCCESSFULLY...OBTAIN REQUIRED METHOD
* OF COMBINING IMAGES
*
         imeth=1
         call getcmd('METHOD','MEAN,MEDIAN,MODE,MINIMUM,MAXIMUM.',1
     :    ,imeth,method,lmeth,ierr)

*
* OBTAIN MINIMUM NUMBER OF VALID VALUES REQUIRED AT EACH PIXEL
*
         nmin=1
         call getpar('MINIMAGE','INTEGER',1,1.0,real(im),.true.,nmin
     :    ,rval,ierr)

*
* SET DEFAULT WEIGHTS FOR IMAGES
*

         do 61 i=1,im
            weight(i)=1.0
61       continue


*
* IF THE METHOD REQUIRES WEIGHTS TO BE ASSIGNED TO EACH IMAGE,
* OBTAIN ERROR ESTIMATES FOR EACH IMAGE
*

         if((method.eq.'MEAN').or.(method.eq.'MODE')) then
64          call rdkeyr('ERRORS',.true.,im,weight,nval,istat)

*
* CONVERT ERRORS TO WEIGHTS
*

            do 62 i=1,im

               if(weight(i).lt.1.0e-10.or.weight(i).gt.1.0e10) then

*
* IF ANY ERROR ESTIMATE WAS INVALID, GIVE MESSAGE AND RETURN
* FOR NEW ENTRY
*
                  call wrerr('BADVALUE')
                  call rngerr('***ALL VALUES','REAL',1.0e-10,1.0e10)
                  call cnpar('ERRORS',istat)
                  go to 64


               else
                  weight(i)=(1.0/weight(i))**2
               endif

62          continue

         endif


*
* OBTAIN TITLE
*
         title(1)=' '
         call gtdscr('IMAGE1','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call rdkeyc('TITLE',.true.,1,title,nval,istat)

*
* OBTAIN DESCRIPTOR ITEMS FOR OUTPUT IMAGE
*

         if(abs(inval(1)).le.32767) then
            invalb=inval(1)

         else
            invalb=-32767
         endif

*
* CALCULATE A SCALE AND ZERO FACTOR WHICH WILL COVER THE ENTIRE INPUT
* DATA RANGE
*
         call scazer(scale,zero,im,bscale,bzero)
         scalef=1.0/sign(max(1.0e-20,abs(bscale)),bscale)
         zerol=-bzero*scalef

*
* COUNT THROUGH THE OUTPUT IMAGE LINES
*

         do 500 j=1,nlout

*
* COPY THE CORRESPONDING INPUT LINE OF EACH IMAGE INTO THE WORKSPACE
* USING LINSTK
*

            do 498 image=1,im
               call linstk(%val(ip(image)),npix(image),nlines(image)
     :          ,inval(image),j,%val(iplst),npout,image)
498         continue


*
* CALL COMBIN TO COMBINE THE INPUT LINES INTO A SINGLE OUTPUT LINE
* USING THE METHOD SPECIFIED IN IMETH
*
            call combin(%val(iplst),npout,im,weight,imeth,scale,zero
     :       ,inval,nmin,scalef,zerol,invalb,j,%val(ipout),work)
500      continue


*
* COPY DESCRIPTOR FROM FIRST INPUT IMAGE TO OUTPUT AND ADD NEW ITEMS
*
         call cydscr('IMAGE1','OUTPUT',istat)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',npout,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',nlout,rval,cval,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
      endif


*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return

      end



