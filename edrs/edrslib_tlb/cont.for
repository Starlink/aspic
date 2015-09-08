      subroutine cont
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLOT A CONTOUR MAP, OMITTING INVALID PIXELS
*
*WRITTEN BY
*       R.F. WARREN-SMITH (modified by D.S. Berry to support GKS 7.2
*	devices 7/7/88)
*----------------------------------------------------------------------
*
*
      parameter (ncont=50)
      parameter (minint=-32768,maxint=32767)
      character prbuf*40,cval*1,ctype*10,device*30,opt*200,plfile(1)*30,
     :          colour*7
      real contl(ncont),area(ncont)
      integer icontl(ncont),ihist(minint:maxint)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierr)
 
*
* IF OBTAINED OK, EXTRACT REQUIRED DESCRIPTOR ITEMS
*
 
      if(ierr.eq.0) then
         inval=-100000
         scale=1.0
         zero=0.0
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
 
*
* OBTAIN REQUIRED GRAPHICS OUTPUT DEVICE
*
         call defdev(device)
         call getdev('DEVICE',device,.false.,ierr)
         if(ierr.ne.0) goto 99
         call sgs_widen(device,itype,iconid,ierr)
         if(ierr.ne.0) goto 99
 
*
* IF PLOTTING IS TO BE DONE IN AN ARGS OR IKON OVERLAY, GET THE COLOUR 
* FOR THE CONTOURS AND THE OVERLAY PLANE TO USE 
*
         if(itype.eq.161.or.itype.eq.3201) then
            icol=1
            call getcmd('COLOUR','RED,GREEN,BLUE,YELLOW,CYAN,MAGENTA,'//
     :                  'BLACK,WHITE.',1,icol,colour,lcol,ierr)
            if(colour.eq.'BLACK') colour='.BLACK'
            iplane=icol
            call getpar('OVERLAY','INTEGER',1,1.0,8.0,.true.,iplane,
     :                  rval,ierr)
            iplane=iplane+7
         endif

*
* OBTAIN THE TYPE OF CONTOUR SPACING REQUIRED
*
         ntype=1
         call getcmd('SPACING','AUTOMATIC,FREE,LINEAR,MAGNITUDES.',1
     :    ,ntype,ctype,ltype,ierr)
 
*
* IF LINEAR OR MAGNITUDE INTERVALS ARE REQUIRED, OBTAIN THE
* STARTING LEVEL AND INTERVAL
*
 
         if(ctype.eq.'LINEAR'.or.ctype.eq.'MAGNITUDES') then
 
*
* OBTAIN THE NUMBER OF CONTOUR LEVELS
*
            ncon=5
            call getpar('NCONT','INTEGER',1,1.0,real(ncont),.true.,ncon
     :       ,rval,ierr)
            c1=0.0
            call getpar('START','REAL',1,-1.0e20,1.0e20,.true.,ival
     :       ,c1,ierr)
            dcon=1.0
            call getpar('INTERVAL','REAL',1,1.0e-20,-1.0e-20,.true.
     :       ,ival,dcon,ierr)
 
*
* CALCULATE THE OTHER CONTOUR LEVELS
*
 
            do 14 i=1,ncon
 
               if(ctype.eq.'LINEAR')then
                  contl(i)=c1+(i-1)*dcon
 
               else
                  contl(i)=c1*10.0**(-(i-1)*dcon/2.5)
               endif
 
14          continue
 
 
*
* IF AUTOMATICALLY PLACED CONTOURS ARE REQUIRED, CALL PCHIST TO
* DETERMINE THE LEVELS
*
 
         else if(ctype.eq.'AUTOMATIC')then
            ncon=5
            call getpar('NCONT','INTEGER',1,1.0,real(ncont),.true.,ncon
     :       ,rval,ierr)
 
*
* ASSIGN CONTOURS SUCH THAT THEY ENCLOSE AN IMAGE AREA FOR
* WHICH THE EQUIVALENT RADIUS INCREASES BY EQUAL INCREMENTS
*
            top=sqrt(0.1/ncon)
            bot=sqrt(0.95-0.45/max(ncon-5,1))
 
            do 23 i=1,ncon
               area(i)=((i-1)*(bot-top)/max(1,ncon-1)+top)**2
 
               if(scale.ge.0.0)area(i)=1.0-area(i)
23          continue
 
            call pchist(%val(ipin),npix,nlines,inval,area,icontl,ncon
     :       ,ihist,minint,maxint,ierr)
 
*
* CONVERT TO REAL DATA VALUES
*
 
            do 24 i=1,ncon
               contl(i)=icontl(i)*scale+zero
24          continue
 
 
*
* IF FREELY SPECIFIED CONTOURS ARE REQUIRED, OBTAIN THE LEVELS
* DIRECTLY FROM THE ENVIRONMENT
*
 
         else if(ctype.eq.'FREE')then
            nbad=0
            maxcon=min(ncont,20)
44          call rdkeyr('LEVELS',.false.,maxcon,contl,ncon,istat)
 
*
* IF NO CONTOUR LEVELS WERE GIVEN, GIVE ERROR AND TRY AGAIN
*
 
            if(istat.ne.0)then
               call wrerr('NOCONTR')
               call wruser(' ',istat)
               call cnpar('LEVELS',istat)
               nbad=nbad+1
 
*
* AFTER 3 ERRORS, QUIT
*
 
               if(nbad.ge.3)go to 99
               go to 44
 
            endif
 
         endif
 
 
*
* DISPLAY THE CONTOUR LEVELS
*
 
         if(ilevel.ge.2)then
            call wruser(' ',istat)
            call wruser('   CONTOUR LEVELS...',istat)
 
            do 77 i=1,ncon
               write(prbuf,21)contl(i)
21             format(20x,g13.6)
               call wruser(prbuf,istat)
77          continue
 
            call wruser(' ',istat)
         endif
 
 
*
* OBTAIN WORKSPACE FOR CONTOURING ALGORITHM
*
         call getdyn('DONE',104,npix*nlines,ipdone,istata)
 
         if(istata.ne.0)then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
 
*
* PLOT THE CONTOURS
*
         call imcont(%val(ipin),npix,nlines,inval,scale,zero,contl,ncon
     :    ,%val(ipdone),colour,iplane,device)
      endif
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
 
      end
 
 
 
