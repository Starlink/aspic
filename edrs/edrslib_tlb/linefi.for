      subroutine linefi
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO GENERATE AN IMAGE, EACH LINE OF WHICH IS A CONSTANT OR
*       STRAIGHT LINE FIT TO THE CORRESPONDING INPUT IMAGE LINE
*
*METHOD
*       OBTAIN INPUT IMAGE AND EXTRACT DESCRIPTOR ITEMS. DETERMINE
*       THE METHOD OF FITTING THE INPUT LINES, THEN OBTAIN THE
*       OUTPUT IMAGE. CALL LINFIL TO FIT THE LINES. PRINT MESSAGE
*       IF ANY LINES WERE NOT FITTED. UPDATE OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF MESSAGES
*       INPUT
*               INPUT IMAGE
*       METHOD
*               METHOD OF FITTING THE INPUT LINES
*       MINPIX
*               MIMINUM NUMBER OF VALID PIXELS REQUIRED IN EACH LINE
*       OUTPUT
*               OUTPUT IMAGE
*       TITLE
*               TITLE TO REPLACE INPUT TITLE IN OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,GETCMD,GT2DIW,LINFIL,LBGONE,PTDSCR
*       STARLINK:
*               WRUSER,RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30,cval*1,method*10,prbuf*80
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE FRAME
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS
*
         title(1)=' '
         invala=-100000
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',invala,rval,cval,ierr)
 
*
* SET OUTPUT INVALID PIXEL FLAG
*
 
         if(abs(invala).le.32767) then
            invalb=invala
 
         else
            invalb=-32767
         endif
 
 
*
* OBTAIN METHOD OF FITTING INPUT LINES AND THE MINIMUM NUMBER OF
* VALID PIXELS REQUIRED IN EACH LINE
*
         imeth=1
         call getcmd('METHOD','CONSTANT,LINEAR.',1,imeth,method,lmeth
     :    ,ierr)
         nmin=imeth
         call getpar('MINPIX','INTEGER',1,real(imeth),real(npix),.true
     :    .,nmin,rval,ierr)
 
*
* OBTAIN OUTPUT IMAGE
*
         call gt2diw('OUTPUT',102,.false.,npix,nlines,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT IMAGE OBTAINED SUCCESSFULLY...CALL LINFIL TO PERFORM THE
* FITTING
*
            call linfil(%val(ipin),npix,nlines,invala,imeth,nmin,invalb
     :       ,nbad,%val(ipout))
 
*
* IF SOME LINES WERE NOT FITTED, GIVE MESSAGE
*
 
            if(nbad.gt.0.and.ilevel.ge.2) then
               write(prbuf,66) nbad
66             format('   ',i10,
     :         ' IMAGE LINE(S) CONTAINED INSUFFICIENT DATA TO FIT')
               call lbgone(prbuf(4:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
            endif
 
 
*
* OBTAIN OUTPUT TITLE AND UPDATE OUTPUT DESCRIPTOR
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call cydscr('INPUT','OUTPUT',istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
      call frdata(' ',istat)
 
      end
 
 
 
