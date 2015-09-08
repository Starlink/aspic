      subroutine itfcor
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY AN ITF TABLE LINEARITY CORRECTION TO AN IMAGE
*
*METHOD
*       OBTAIN INPUT IMAGE AND ITF TABLE. EXTRACT ITF TABLE DESCRIPTOR
*       ITEMS AND CHECK VALIDITY. OBTAIN OUTPUT IMAGE. EXTRACT IMAGE
*       DESCRIPTOR ITEMS, THEN CALL LNCORN TO APPLY THE ITF CORRECTION.
*       UPDATE THE OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF ITF TITLE
*       INPUT
*               INPUT IMAGE
*       ITFTABLE
*               INPUT ITF TABLE
*       ITFINVAL/ERROR/
*               ACCESSED IF THE ITF TABLE IS NOT VALID
*       OUTPUT
*               OUTPUT IMAGE
*       TITLE
*               TITLE TO REPLACE INPUT TITLE IN OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,GT2DIW,LNCORN,PTDSCR
*       STARLINK:
*               CNPAR,WRERR,WRUSER,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30,itfttl*30,cval*1
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierr1)
 
      if(ierr1.eq.0) then
 
*
* SUCCESSFULLY OBTAINED... OBTAIN THE ITF TABLE TO BE APPLIED
*
67       call gt2dir('ITFTABLE',204,.false.,nentry,nlinet,iptab,ierr2)
 
         if(ierr2.eq.0) then
 
*
* ITF TABLE SUCCESSFULLY OBTAINED... EXTRACT TITLE AND TABLE LIMITS
* FROM DESCRIPTOR
*
            itfttl=' '
            call gtdscr('ITFTABLE','TITLE','CHARACTER',ival,rval,itfttl
     :       ,ierrt)
            botlim=0.0
            call gtdscr('ITFTABLE','LOLIM','REAL',ival,botlim,cval,
     :      ierrl)
            toplim=0.0
            call gtdscr('ITFTABLE','UPLIM','REAL',ival,toplim,cval,
     :      ierru)
 
*
* CHECK VALIDITY OF ITF TABLE... ALL DESCRIPTOR ITEMS PRESENT,
* ONLY 1 LINE IN THE INPUT IMAGE AND TOPLIM.GE.BOTLIM
*
 
            if((ierrl.ne.0).or.(ierru.ne.0).or.(nlinet.ne.1).or.(botlim
     :       .gt.toplim)) then
 
*
* IF INVALID, GIVE ERROR MESSAGE AND RETURN TO GET NEW ITF TABLE
*
               call cnpar('ITFTABLE',istat)
               call wrerr('ITFINVAL')
               go to 67
 
            endif
 
 
*
* SHOW THE USER THE ITF TABLE TITLE AND OBTAIN THE OUTPUT IMAGE
*
 
            if(ilevel.ge.2) then
               call wruser(' ',istat)
               call wruser('   ITF TITLE= '//itfttl,istat)
               call wruser(' ',istat)
            endif
 
            call gt2diw('OUTPUT',102,.false.,npix,nlines,ipout,ierr3)
 
            if(ierr3.eq.0) then
 
*
* OUTPUT IMAGE OBTAINED... OBTAIN INVALID FLAG, SCALE AND ZERO FROM
* INPUT IMAGE
*
               title(1)=' '
               inval=-100000
               ascale=1.0
               azero=0.0
               call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
               call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval
     :          ,ierr)
               call gtdscr('INPUT','BSCALE','REAL',ival,ascale,cval
     :          ,ierr)
               call gtdscr('INPUT','BZERO','REAL',ival,azero,cval,ierr)
 
*
* SET OUTPUT INVALID PIXEL FLAG
*
 
               if(abs(inval).le.32767) then
                  invalb=inval
 
               else
                  invalb=-32767
               endif
 
 
*
* CALL LNCORN TO APPLY THE LINEARITY CORRECTION IN THE ITF TABLE
*
               call lncorn(%val(ipin),npix,nlines,inval,ascale,azero
     :          ,botlim,toplim,%val(iptab),nentry,%val(ipout),invalb
     :           ,bscale,bzero,ierr)
 
*
* COPY THE INPUT DESCRIPTOR TO THE OUTPUT AND UPDATE THE INVALID FLAG
* SCALE FACTOR AND ZERO
*
               call cydscr('INPUT','OUTPUT',istat)
               call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :          ,ierr)
               call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval
     :          ,ierr)
               call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,
     :         ierr)
 
*
* ADD TITLE
*
               call rdkeyc('TITLE',.true.,1,title,nval,istat)
               call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
            endif
 
         endif
 
      endif
 
 
*
* RELEASE THE DATA AREAS AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
