      subroutine resamp
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESAMPLE AN IMAGE AT POINTS DEFINED BY TRANSFORMING THE
*       PIXEL COORDINATES OF THE OUTPUT IMAGE
*
*METHOD
*       OBTAIN THE INPUT IMAGE AND TRANSFORM COEFFICIENTS. OBTAIN THE
*       OUTPUT IMAGE. CALL REBIN TO RESAMPLE THE IMAGE BY THE REQUIRED
*       METHOD. UPDATE OUTPUT DESCRIPTOR ITEMS
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
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GETCMD,GETPAR,GT2DIW,GTDSCR,REBIN,PTDSCR
*       STARLINK:
*               RDKEYR,RDKEYL,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,cmethd*10,title(1)*30
      real c(6)
      logical consrv(1)
 
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
* OBTAIN SIZE OF OUTPUT IMAGE, USING INPUT SIZE AS DEFAULT
*
         npout=npix
         nlout=nlines
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
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
