      subroutine pixfil
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REPLACE INVALID PIXELS IN AN IMAGE WITH NEW VALUES WHICH
*       MATCH THE SURROUNDING DATA
*
*METHOD
*       OBTAIN INPUT IMAGE. OBTAIN WORKSPACE. CALL FILLIN TO
*       REPLACE THE INVALID PIXELS BY SOLVING LAPLACE'S EQUATION IN
*       THE INVALID REGIONS. PRINT THE NUMBER OF PIXELS REPLACED AND
*       INFORMATION ABOUT THE ITERATIONS IN FILLIN.
*       UPDATE THE OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUT
*               THE INPUT IMAGE
*               IMAGE
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       OUTPUT
*               THE OUTPUT IMAGE
*       NITER
*               THE NUMBER OF ITERATIONS REQUIRED
*       SIZE
*               INITIAL SMOOTHING SIZE
*       NONEVAL/ERROR/
*               ACCESSED IF THE INPUT IMAGE HAS NO VALID PIXELS
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,IMGSUM,GT2DIW,FILLIN,LBGONE,PTDSCR
*       STARLINK:
*               WRERR,GETDYN,WRUSER,RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,prbuf*80
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS
*
         title(1)=' '
         inval=-100000
         scale=1.0
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
 
*
* OBTAIN WORKSPACE FOR FILLIN
*
         call getdyn('DSUM',204,npix*nlines,ipds,istds)
         call getdyn('WTSUM',204,npix*nlines,ipwts,istwts)
         call getdyn('DLAST',204,npix,ipdl,istdl)
         call getdyn('WTLAST',204,npix,ipwtl,istwtl)
 
*
* IF WORKSPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
         istmax=max(istds,istwts,istdl,istwtl)
 
         if(istmax.ne.0) then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npix,nlines,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT IMAGE OBTAINED SUCCESSFULLY...GET THE NUMBER OF ITERATIONS
* REQUIRED
*
            niter=2
            call getpar('NITER','INTEGER',1,2.0,100.0,.true.,niter,rval
     :       ,ierr)
 
*
* OBTAIN THE INITIAL SMOOTHING SIZE
*
            size=5.0
            call getpar('SIZE','REAL',1,0.1,1.0e6,.true.,ival,size,cval
     :       ,ierr)
 
*
* CALL FILLIN TO REPLACE THE INVALID PIXELS
*
            call fillin(%val(ipin),npix,nlines,inval,size,ilevel,cngmax
     :       ,cngrms,niter,scale,%val(ipout),nbad,%val(ipds),
     :       %val(ipwts),%val(ipdl),%val(ipwtl))
 
*
* IF THERE ARE NO VALID PIXELS, GIVE MESSAGE AND ABORT
*
 
            if(nbad.eq.npix*nlines) then
               call wrerr('NONEVAL')
               go to 99
 
            endif
 
 
*
* PRINT INFORMATIONAL MESSAGES
*
 
            if(ilevel.ge.2) then
               write(prbuf,15)niter
15             format('   ',i10,' ITERATION(S) COMPLETED')
               call lbgone(prbuf(4:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               write(prbuf,16)nbad
16             format('   ',i10,' INVALID PIXEL(S) REPLACED')
               call lbgone(prbuf(4:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               write(prbuf,17)cngmax
17             format('   MAXIMUM CHANGE IN LAST ITERATION WAS ',ss
     :          ,g13.6)
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
               write(prbuf,518)cngrms
518            format('   RMS CHANGE IN LAST ITERATION WAS ',ss,g13.6)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
               write(prbuf,18) size
18             format('   FINAL SMOOTHING SIZE=',ss,g13.6)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
            endif
 
 
*
* OBTAIN OUTPUT TITLE
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* UPDATE OUTPUT DESCRIPTOR ITEMS
*
            call cydscr('INPUT','OUTPUT',istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',-100000,rval,cval
     :       ,ierr)
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
