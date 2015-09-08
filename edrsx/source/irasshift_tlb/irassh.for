      subroutine irassh
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESAMPLE AN IMAGE AT POINTS DEFINED BY TRANSFORMING THE
*       PIXEL COORDINATES OF THE OUTPUT IMAGE. After the resampling,
*       the IRAS specific descriptors of the shifted image are updated
*       to account for the shift.
*
*SOURCE
*       IRASSH.FOR in IRASSHIFT.TLB
*
*METHOD
*       OBTAIN THE INPUT IMAGE AND TRANSFORM COEFFICIENTS. OBTAIN THE
*       OUTPUT IMAGE. CALL REBIN TO RESAMPLE THE IMAGE BY THE REQUIRED
*       METHOD. UPDATE OUTPUT DESCRIPTOR ITEMS.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT IMAGE
*       TRCOEFFS
*               6 REAL COEFFICIENTS DEFINING THE TRANSFORMATION
*       METHOD
*               SPECIFIES THE RESAMPLING INTERPOLATION METHOD
*       CONSERVE
*               (LOGICAL) IF SET TRUE, THE IMAGE IS RESCALED TO
*               CONSERVE TOTAL DATA SUM
*       NPIXOUT
*               NUMBER OF PIXELS PER LINE IN THE OUTPUT IMAGE
*       NLINEOUT
*               NUMBER OF LINES IN THE OUTPUT IMAGE
*       OUTPUT
*               OUTPUT IMAGE
*       TITLE
*               TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*       NOCOMB/ERROR/
*               ACCESSED IF AN IMAGE PRODUCED BY COMBINE HAS A NON-ZERO
*               CROTA1
*
*CALLS
*       EDRSX UTILITIES:
*               INVERT,HINT
*       EDRS:
*               GT2DIR,GETCMD,GETPAR,GT2DIW,GTDSCR,REBIN,PTDSCR
*       INTERIM:
*               RDKEYR,RDKEYL,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH.
*       Modified by D.S. Berry to update IRAS descriptors (29/10/87).
*-----------------------------------------------------------------------
*
*
      character cval*1,cmethd*10,title(1)*30,instrb*30
      real d(6),c(6),midxb,midyb
      logical consrv(1)
      integer crpx1a,crpx2a,crpx1b,crpx2b
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierr1)

      if(ierr1.eq.0) then

*
* INPUT IMAGE SUCESSFULLY OBTAINED...
* OBTAIN POSITION TRANSFORMATION COEFFICIENTS
*
         c(1)=0.0
         c(2)=1.0
         c(3)=0.0
         c(4)=0.0
         c(5)=0.0
         c(6)=1.0
         call rdkeyr('TRCOEFFS',.false.,6,c,nval,istat)

*
* OBTAIN TYPE OF INTERPOLATION REQUIRED
*
         method=2
         call getcmd('METHOD','NEAREST,LINEAR,UNIFORM.',1,method,cmethd
     :    ,lmethd,ierr)

*
* DETERMINE IF TOTAL DATA COUNT IS TO BE CONSERVED
*
         consrv(1)=.false.
         call rdkeyl('CONSERVE',.true.,1,consrv,nval,istat)

*
* CALCULATE DEFAULT FOR OUTPUT IMAGE SIZE SUCH THAT AS MUCH AS POSSIBLE
* OF THE IMAGE IS CONTAINED WITHIN THE OUTPUT
*
         call invert(c,d,istat)
         if(istat.eq.0) then
            icx1=d(1)+d(2)+d(3)+1
            icy1=d(4)+d(5)+d(6)+1
            icx2=d(1)+d(2)+d(3)*nlines+1
            icy2=d(4)+d(5)+d(6)*nlines+1
            icx3=d(1)+d(2)*npix+d(3)*nlines+1
            icy3=d(4)+d(5)*npix+d(6)*nlines+1
            icx4=d(1)+d(2)*npix+d(3)+1
            icy4=d(4)+d(5)*npix+d(6)+1
            npout=max(1,icx1,icx2,icx3,icx4)
            nlout=max(1,icy1,icy2,icy3,icy4)
         else
            npout=npix
            nlout=nlines
         endif

*
*
* OBTAIN ACTUAL SIZE OF OUTPUT IMAGE
*
         call getpar('NPIXOUT','INTEGER',1,1.0,10000.0,.true.,npout
     :    ,rval,ierr)
         call getpar('NLINEOUT','INTEGER',1,1.0,10000.0,.true.,nlout
     :    ,rval,ierr)

*
* OBTAIN OUTPUT IMAGE
*
         call gt2diw('OUTPUT',102,.false.,npout,nlout,ipout,ierr2)

         if(ierr2.eq.0) then

*
* SUCCESSFULLY OBTAINED...
* EXTRACT TITLE, INVALID FLAG, SCALE AND ZERO FROM INPUT DESCRIPTOR
*
            title(1)=' '
            inval=-100000
            scale=1.0
            zero=0.0
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
            call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
            call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)

*
* SET OUTPUT INVALID PIXEL FLAG
*

            if(abs(inval).le.32767) then
               invalb=inval

            else
               invalb=-32767
            endif


*
* CALL REBIN TO RESAMPLE THE INPUT IMAGE OVER THE ENTIRE AREA
* COVERED BY THE OUTPUT IMAGE
*
            call rebin(%val(ipin),npix,nlines,inval,invalb,1,npout,1
     :       ,nlout,c,s,method,%val(ipout),npout,nlout,ierr)

*
* COPY DESCRIPTOR FROM INPUT TO OUTPUT AND UPDATE IMAGE SIZE
*
            call cydscr('INPUT','OUTPUT',istat)
            call ptdscr('OUTPUT','NAXIS1','INTEGER',npout,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','NAXIS2','INTEGER',nlout,rval,cval
     :       ,ierr)

*
* MODIFY IMAGE SCALE TO CONSERVE TOTAL COUNTS AND UPDATE SCALE,ZERO
* AND INVALID FLAG IN OUTPUT DESCRIPTOR
*

            if(consrv(1)) then
               scale=scale*s
               zero=zero*s
            endif

            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)

*
* ADD TITLE
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
*
* UPDATE IRAS SPECIFIC DESCRIPTORS
*--------------------------------------------------------------------
* GET THE IMAGE IRAS DESCRIPTORS FROM UNSHIFTED IMAGE
*
            call rdimds('INPUT',.false.,crvl1b,crvl2b,crpx1b,crpx2b,
     :             cdlt1b,cdlt2b,crotab,instrb,ierr)
            if(ierr.eq.-1) call wrerr('NOCOMB')
            if(ierr.ne.0) then
               call wrerr('NOCHANGE')
               goto 999
            endif
*
* THE SHIFTED IMAGE REFERENCE PIXEL IS THE CENTRE PIXEL
*
            crpx1a=int(0.5*(npout+1))
            crpx2a=int(0.5*(nlout+1))
*
* SEE WHERE THIS PIXEL WAS IN THE UNSHIFTED IMAGE
*
            midxb=c(1)+c(2)*crpx1a+c(3)*crpx2a
            midyb=c(4)+c(5)*crpx1a+c(6)*crpx2a
*
* FIND THE RA AND DEC OF THIS PIXEL POSITION USING THE DESCRIPTORS IN
* THE UNSHIFTED IMAGE. THESE ARE THE VALUES OF CRVAL1 AND CRVAL2 IN THE
* SHIFTED IMAGE.
*
            call xytord(midxb,midyb,crvl1b,crvl2b,crpx1b,crpx2b,cdlt1b,
     :                  cdlt2b,crotab,crvl1a,crvl2a)
*
* CALCULATE THE X SIZE OF A PIXEL OF THE SHIFTED IMAGE IN DEGREES OF ARC
*
            cdlt1a=sqrt(cdlt1b*cdlt1b*c(2)*c(2)+cdlt2b*cdlt2b*c(5)*c(5))
            cdlt2a=sqrt(cdlt1b*cdlt1b*c(3)*c(3)+cdlt2b*cdlt2b*c(6)*c(6))
*
* CALCULATE THE ANTI-CLOCKWISE ANGLE BETWEEN NORTH AND THE NEGATIVE
* Y AXIS FOR THE SHIFTED IMAGE
*
            crotaa=crotab+atan2d(c(3),c(2))
            if(crotaa.lt.0) crotaa=crotaa+360.0
            if(crotaa.ge.360) crotaa=crotaa-360.0
*
* THE VALUE OF CROTA1 STORED IN IMAGEA WILL USE THE DEFINITION USED BY
* CRDIMAGE (CLOCKWISE ANGLE FROM NORTH TO -VE Y AXIS). THEREFORE ENSURE
* THAT THE FILE IS RECOGNIZED AS A CRDIMAGE OUTPUT.
*
            call ptdscr('OUTPUT','INSTRUME','CHARACTER',ival,rval,
     :                  'SURVEY',ierr)
*
* WRITE OUT THE NEW VALUES FOR THE SHIFTED IMAGE DESCRIPTORS
*
            call wrimds('OUTPUT',crvl1a,crvl2a,crpx1a,crpx2a,cdlt1a,
     :                  cdlt2a,crotaa,ierr)
*-----------------------------------------------------------------------

         endif

      endif


*
* RELEASE DATA AREAS AND FINISH
*
 999  call frdata(' ',istat)

      end



