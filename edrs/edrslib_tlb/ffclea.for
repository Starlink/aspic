      subroutine ffclea
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REMOVE DEFECTS FROM A SUBSTANTIALLY FLAT IMAGE
*
*METHOD
*       OBTAIN INPUT IMAGE, WORKSPACE AND THE OUTPUT IMAGE. OBTAIN
*       PARAMETERS SPECIFYING THE FILTER BOX SIZE, NUMBER OF ITERATIONS
*       AND REJECTION THRESHOLD FOR THE REJECTION ROUTINE. EXTRACT
*       THE INPUT IMAGE DESCRIPTOR ITEMS REQUIRED, THEN CALL FFREJ TO
*       IMPLEMENT THE REJECTION ALGORITHM. UPDATE OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUT
*               THE INPUT IMAGE
*       NOSPACE/ERROR/
*               CALLED IF THE REQUIRED WORKSPACE CANNOT BE OBTAINED
*       OUTPUT
*               THE OUTPUT IMAGE
*       XSIZE,YSIZE
*               THE SIZE OF THE BOX AREA USED IN FILTERING THE IMAGE
*       NITER
*               THE NUMBER OF REJECTION ITERATIONS TO BE PERFORMED
*       GAMMA
*               THE NUMBER OF STANDARD DEVIATIONS AT WHICH POINTS ARE
*               REJECTED
*       ALLREJ/ERROR/
*               CALLED IF ALL THE INPUT PIXELS ARE REJECTED
*       NONEVAL/ERROR/
*               CALLED IF THERE ARE NO VALID PIXELS IN THE INPUT IMAGE
*       SIGMA
*               AN OUTPUT ESTIMATE OF THE RMS NOISE PER PIXEL IN THE
*               OUTPUT IMAGE
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GT2DIW,GTDSCR,FFREJ,PTDSCR
*       STARLINK:
*               GETDYN,WRERR,WRKEYR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30
      real sigma(1)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY...GET WORK SPACE
*
         call getdyn('ISTOR',104,npix*nlines,ipist,istist)
         call getdyn('NSTOR',102,npix*nlines,ipnst,istnst)
         call getdyn('ILINE',104,npix,ipil,istil)
         call getdyn('NLINE',102,npix,ipnl,istnl)
 
*
* IF SPACE NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
         if((istist.ne.0).or.(istnst.ne.0).or.(istil.ne.0).or.(istnl
     :    .ne.0)) then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npix,nlines,ipb,ierrb)
 
         if(ierrb.eq.0) then
 
*
* OUTPUT IMAGE OBTAINED SUCCESSFULLY...SET DEFAULT BOX SIZE, THEN
* OBTAIN SIZE FROM THE ENVIRONMENT
*
            ix=3
            call getpar('XSIZE','INTEGER',1,1.0,32767.0,.true.,ix,rval
     :       ,ierr)
 
*
* CONSTRAIN IY SO THAT THE NUMBER OF PIXELS IN THE BOX CANNOT EXCEED
* 32767... THIS COULD CAUSE OVERFLOW IN ROUTINE IMGBOX
*
            ix=2*max(0,ix/2)+1
            iymax=32767/ix
 
            if(mod(iymax,2).eq.0) iymax=iymax-1
            iy=min(iymax,3)
            botlim=1.0
 
            if(ix.eq.1) botlim=2.0
            call getpar('YSIZE','INTEGER',1,botlim,real(iymax),.true
     :       .,iy,rval,ierr)
            iy=2*max(0,iy/2)+1
 
*
* OBTAIN NUMBER OF ITERATIONS FOR REJECTION ALGORITHM
*
            niter=2
            call getpar('NITER','INTEGER',1,1.0,100.0,.true.,niter,rval
     :       ,ierr)
 
*
* OBTAIN NUMBER OF STANDARD DEVIATIONS FOR REJECTION THRESHOLD
*
            gamma=3.0
            call getpar('GAMMA','REAL',1,0.0,1.0e10,.true.,ival,gamma
     :       ,ierr)
 
*
* OBTAIN REQUIRED DESCRIPTOR ITEMS FROM INPUT IMAGE
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
* SET OUTPUT INVALID FLAG
*
 
            if(abs(invala).le.32767) then
               invalb=invala
 
            else
               invalb=-32767
            endif
 
 
*
* CALL FFREJ TO APPLY THE REJECTION ALGORITHM
*
            call ffrej(%val(ipa),npix,nlines,invala,niter,gamma,ix,iy
     :       ,ilevel,scale,sigma(1),%val(ipb),invalb,ngood,%val(ipist)
     :        ,%val(ipnst),%val(ipil),%val(ipnl))
 
*
* IF THERE ARE NO VALID PIXELS IN THE OUTPUT IMAGE, GIVE MESSAGE
* AND ABORT... EITHER THERE WERE NONE VALID ON ENTRY (NGOOD.LT.0)
* OR THEY HAVE ALL BEEN REJECTED (NGOOD=0)
*
 
            if(ngood.le.0) then
 
               if(ngood.eq.0) then
                  call wrerr('ALLREJ')
 
               else
                  call wrerr('NONEVAL')
               endif
 
               go to 99
 
            endif
 
 
*
* WRITE ESTIMATE OF NOISE LEVEL TO ENVIRONMENT
*
            call wrkeyr('SIGMA',sigma,1,istat)
 
*
* COPY DESCRIPTOR TO OUTPUT AND UPDATE VALUES
*
            call cydscr('INPUT','OUTPUT',istat)
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
