      subroutine itfplo
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLOT THE LINEARITY CORRECTION IN AN ITF TABLE
*
*METHOD
*       OBTAIN THE INPUT TABLE AND CHECK ITS VALIDITY. PLOT THE
*       CONTENTS.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               INPUT ITF TABLE
*       ITFINVAL/ERROR/
*               ACCESSED IF THE ITF TABLE IS INVALID
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE IS NOT AVAILABLE
*       DEVICE
*               SPECIFIES GRAPHICS DEVICE TO BE USED
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GTDSCR,TABPLT,GETDEV,DEFDEV
*       STARLINK:
*               CNPAR,WRERR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title*30,cval*1,device*30
 
*
* OBTAIN INPUT ITF TABLE
*
67    call gt2dir('INPUT',204,.false.,nentry,nlinet,iptab,ierrtb)
 
      if(ierrtb.eq.0) then
 
*
* SUCCESSFULLY OBTAINED... EXTRACT TITLE AND LOWER AND UPPER TABLE
* LIMITS FROM DESCRIPTOR
*
         title=' '
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title,ierrt)
         botlim=0.0
         call gtdscr('INPUT','LOLIM','REAL',ival,botlim,cval,ierrl)
         toplim=0.0
         call gtdscr('INPUT','UPLIM','REAL',ival,toplim,cval,ierru)
 
*
* CHECK VALIDITY OF INPUT... ALL DESCRIPTOR ITEMS PRESENT,
* ONLY 1 LINE IN THE IMAGE AND TOPLIM.GE.BOTLIM
*
 
         if((ierrl.ne.0).or.(ierru.ne.0).or.(nlinet.ne.1).or.(botlim
     :    .gt.toplim)) then
 
*
* IF INVALID, GIVE ERROR MESSAGE AND RETURN TO GET NEW INPUT
*
            call cnpar('INPUT',istat)
            call wrerr('ITFINVAL')
            go to 67
 
         endif
 
 
*
* OBTAIN WORKSPACE FOR ROUTINE TABPLT
*
         call getdyn('WORK',204,nentry,ipwrk,istwrk)
 
*
* IF WORKSPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
         if(istwrk.ne.0) then
            call wrerr('NOSPACE')
            go to 99
 
         endif
 
 
*
* DETERMINE GRAPHICS DEVICE TO BE USED
*
         call defdev(device)
         call getdev('DEVICE',device,.false.,ierr)
         if(ierr.ne.0) goto 99
 
*
* CALL TABPLT TO PLOT TABLE
*
         call tabplt(%val(iptab),nentry,botlim,toplim,title,device,
     :   %val(ipwrk))
 
*
* FREE DATA AREA AND RETURN
*
      endif
 
99    call frdata(' ',istat)
      return
 
      end
 
 
 
