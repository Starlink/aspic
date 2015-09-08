      subroutine boxfil
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY A RECTANGULAR 'BOX' FILTER TO AN IMAGE
*
*METHOD
*       OBTAIN IMAGE AND ALLOCATE WORKSPACE. OBTAIN REQUIRED BOX
*       SIZE AND EXTRACT DESCRIPTOR ITEMS FROM IMAGE. CALL ROUTINE
*       IMGBOX TO APPLY FILTER WITH A THRESHOLD ON THE NUMBER OF
*       VALID PIXELS REQUIRED IN THE BOX AREA. UPDATE OUTPUT
*       DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               INPUT IMAGE
*       NOSPACE/ERROR/
*               CALLED IF WORKSPACE CANNOT BE OBTAINED
*       OUTPUT
*               OUTPUT IMAGE
*       XSIZE,YSIZE
*               BOX SIZE IN PIXELS IN THE X,Y DIRECTIONS
*       MINPIX
*               MINIMUM NUMBER OF VALID PIXELS REQUIRED IN BOX AREA
*       TITLE
*               TITLE TO REPLACE INPUT TITLE IN OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*                       GT2DIR,GT2DIW,GETPAR,GTDSCR,IMGCPY,IMGBOX,
*                       PTDSCR
*       STARLINK:
*                       GETDYN,WRERR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30,cval*1
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipa,ierra)
 
      if(ierra.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY...GET DYNAMIC MEMORY WORKSPACE
* FOR FILTERING ROUTINE
*
         call getdyn('ISTOR',104,npix*nlines,ipist,istist)
         call getdyn('NSTOR',102,npix*nlines,ipnst,istnst)
         call getdyn('ILINE',104,npix,ipil,istil)
         call getdyn('NLINE',102,npix,ipnl,istnl)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
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
* OUTPUT FRAME OBTAINED SUCCESSFULLY...OBTAIN RECTANGLE SIZE
*
            ix=3
            call getpar('XSIZE','INTEGER',1,1.0,32767.0,.true.,ix,rval
     :       ,ierr)
 
*
* MAKE RECTANGLE SIZE ODD AND SET LIMITS ON IY TO PREVENT THE
* NUMBER OF PIXELS IN THE RECTANGLE EXCEEDING 32767 (THIS COULD CAUSE
* OVERFLOW IN IMGBOX)
*
            ix=2*max(ix/2,0)+1
            iymax=32767/ix
 
            if(mod(iymax,2).eq.0) iymax=iymax-1
            iy=min(iymax,3)
            botlim=1.0
 
            if(ix.eq.1) botlim=2.0
            call getpar('YSIZE','INTEGER',1,botlim,real(iymax),.true
     :       .,iy,rval,ierr)
            iy=2*max(iy/2,0)+1
 
*
* CALCULATE NO. OF PIXELS PER RECTANGLE AND GET LIMIT ON MINIMUM
* NO. OF GOOD PIXELS REQUIRED IN RECTANGLE
*
            maxlim=ix*iy
            minpix=1
            call getpar('MINPIX','INTEGER',1,1.0,real(maxlim),.true
     :       .,minpix,rval,ierr)
 
*
* OBTAIN REQUIRED DESCRIPTOR ITEMS FROM INPUT IMAGE
*
            title(1)=' '
            inval=-100000
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
 
*
* COPY INPUT IMAGE TO OUTPUT
*
            call imgcpy(%val(ipa),npix,nlines,%val(ipb))
 
*
* CALL IMGBOX TO APPLY THE RECTANGULAR FILTER TO THE OUTPUT IMAGE
*
            call imgbox(%val(ipb),npix,nlines,inval,ix,iy,minpix,
     :      %val(ipist),%val(ipnst),%val(ipil),%val(ipnl))
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT AND ADD NEW TITLE
*
            call cydscr('INPUT','OUTPUT',istat)
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
