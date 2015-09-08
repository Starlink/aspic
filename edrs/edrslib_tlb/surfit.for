      subroutine surfit
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A POLYNOMIAL OR SPLINE SURFACE TO AN IMAGE
*
*METHOD
*       OBTAIN INPUT IMAGE. OBTAIN PARAMETERS CONTROLLING THE FITTING.
*       OBTAIN WORKSPACE FOR THE FITTING ROUTINE. CALL PLYIMG OR SPLIMG
*       TO PERFORM THE FITTING. UPDATE OUTPUT DESCRIPTOR ITEMS
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUT
*               THE INPUT IMAGE
*       OUTPUT
*               THE OUTPUT IMAGE I.E. THE FITTED SURFACE
*       FITTYPE
*               THE TYPE OF FIT REQUIRED: POLYNOMIAL OR DEFAULT
*       NXPAR,NYPAR
*               THE NUMBER OF FITTING PARAMETERS IN THE X AND Y
*               DIRECTIONS
*       IX,IY
*               SIZE OF THE DATA BINNING AREA IN THE X AND Y DIRECTIONS
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       NOTUNIQ/ERROR/
*               ACCESSED IF THE INPUT DATA DOES NOT UNIQUELY DEFINE
*               A LEAST SQUARES FIT
*       XSKNOTS/ERROR/
*               ACCESSED IF TOO MANY KNOTS WERE USED FOR THE NUMBER OF
*               DATA POINTS GIVEN
*       BINS2FEW/ERROR/
*               ACCESSED IF THERE ARE TOO FEW DATA BINS TO FIT
*       RMSERROR
*               OUTPUT PARAMETER: ESTIMATE OF THE RMS ERROR OF FIT
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GT2DIW,GTDSCR,SPLIMG,LBGONE,PTDSCR
*       STARLINK:
*               GETDYN,WRERR,WRUSER,WRKEYR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30,cval*1,prbuf*50,fittyp*10
      real rms(1)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE FRAME
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY... OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npix,nlines,ipb,ierrb)
 
         if(ierrb.eq.0) then
 
*
* OBTAIN FIT TYPE
*
            ifit=1
            call getcmd('FITTYPE','DEFAULT,POLYNOMIAL.',1,ifit,fittyp
     :       ,lfit,ierr)
 
*
* OBTAIN NUMBER OF FITTING PARAMETERS REQUIRED IN THE X AND Y
* DIRECTIONS
*
            nxpar=1
            nypar=1
            call getpar('NXPAR','INTEGER',1,1.0,15.0,.true.,nxpar,rval
     :       ,ierr)
            call getpar('NYPAR','INTEGER',1,1.0,15.0,.true.,nypar,rval
     :       ,ierr)
 
*
* OBTAIN DIMENSIONS OF RECTANGLES INTO WHICH THE DATA IS TO
* BE BINNED
*
            ix=10
            iy=10
            call getpar('IX','INTEGER',1,1.0,10000.0,.true.,ix,rval
     :       ,ierr)
            call getpar('IY','INTEGER',1,1.0,10000.0,.true.,iy,rval
     :       ,ierr)
 
*
* CALCULATE THE NUMBER OF BINS IN THE X AND Y DIRECTIONS
*
            xn=real(npix)/ix
            yn=real(nlines)/iy
            nx=xn
            ny=yn
 
            if(nx.lt.xn) nx=nx+1
 
            if(ny.lt.yn) ny=ny+1
 
*
* EXTRACT REQUIRED DESCRIPTOR ITEMS FROM INPUT IMAGE
*
            title(1)=' '
            invala=-100000
            scale=1.0
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call gtdscr('INPUT','INVAL','INTEGER',invala,rval,cval,
     :      ierr)
            call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
 
*
* SET INVALID FLAG FOR OUTPUT IMAGE
*
 
            if(abs(invala).gt.32767) then
               invalb=-32767
 
            else
               invalb=invala
            endif
 
 
*
* CALCULATE THE TOTAL NUMBER OF BINS AND THE STORAGE REQUIREMENT
* FOR THE POLYNOMIAL FITTING ROUTINE
*
 
            if(fittyp.eq.'POLYNOMIAL'.or.nxpar.lt.4.or.nypar.lt.4)then
               maxbin=nx*ny
               nw=4*maxbin
 
*
* OBTAIN WORKSPACE
*
               call getdyn('WORK',208,nw,ipwrk,istat)
 
               if(istat.ne.0)then
                  call wrerr('NOSPACE')
                  go to 99
 
               endif
 
 
*
* CALL PLYIMG TO FIT THE POLYNOMIAL SURFACE TO THE INPUT IMAGE
* AND PUT THE RESULT IN THE OUTPUT
*
               call plyimg(%val(ipa),npix,nlines,invala,ix,iy,nxpar
     :          ,nypar,maxbin,invalb,%val(ipb),rms(1),%val(ipwrk),ierr)
 
*
* REPORT FAILURES
*
 
               if(ierr.ne.0)then
                  call wrerr('NOPOLYFT')
                  go to 99
 
               endif
 
 
*
* CALCULATE THE TOTAL NUMBER OF BINS AND THE STORAGE REQUIREMENT
* FOR THE SPLINE FITTING ROUTINE
*
 
            else
               maxbin=nx*ny+2
               nxknot=nxpar-4
               nyknot=nypar-4
               npoint=max(maxbin,4)+(nxknot+1)*(nyknot+1)
 
*
* OBTAIN REQUIRED WORKSPACE
*
               call getdyn('WORK',208,5*maxbin,ipwrk,istwrk)
               call getdyn('POINT',104,npoint,ippnt,istpnt)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
               if((istwrk.ne.0).or.(istpnt.ne.0)) then
                  call wrerr('NOSPACE')
                  go to 99
 
               endif
 
 
*
* CALL SPLIMG TO FIT THE SPLINE SURFACE TO THE INPUT IMAGE
* AND PUT THE RESULTING IMAGE IN THE OUTPUT
*
               call splimg(%val(ipa),npix,nlines,invala,ix,iy,nxknot
     :          ,nyknot,maxbin,invalb,%val(ipb),rms(1),%val(ipwrk),
     :          %val(ippnt),ierr)
 
*
* IF IERR INDICATES THE SURFACE FIT WAS NOT UNIQUELY DEFINED, GIVE
* WARNING MESSAGE (A FIT IS STILL PRODUCED AND MAY BE SATISFACTORY
* ...SEE NAG DOCUMENTATION FOR E02DAF FOR DETAILS)
*
 
               if(ierr.eq.1) then
                  call wrerr('NOTUNIQ')
 
*
* FOR OTHER ERROR CONDITIONS, GIVE MESSAGE AND ABORT
*
 
               else if(ierr.eq.2) then
                  call wrerr('XSKNOTS')
                  go to 99
 
 
               else if(ierr.eq.3) then
                  call wrerr('BINS2FEW')
                  go to 99
 
               endif
 
            endif
 
 
*
* IF ILEVEL.GE.2, SHOW THE USER THE RMS ERROR OF FIT
*
 
            if(ilevel.ge.2) then
               write(prbuf,10)scale*rms(1)
10             format('   RMS ERROR OF FIT=',ss,g11.4,' PER PIXEL')
               call lbgone(prbuf(29:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
            endif
 
 
*
* WRITE THE RMS ERROR TO THE ENVIRONMENT, COPY THE INPUT DESCRIPTOR
* TO THE OUTPUT AND ADD NEW DESCRIPTOR ITEMS
*
            rms(1)=rms(1)*scale
            call wrkeyr('RMSERROR',rms,1,istat)
            call cydscr('INPUT','OUTPUT',istat)
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
