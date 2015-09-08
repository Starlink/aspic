      subroutine xycur
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN A LIST OF X,Y POSITIONS AND IDENTIFIERS FROM AN
*       IMAGING DEVICE SCREEN AND INSERT THEM IN A FILE. WHEN USING
*	CERTAIN DEVICES THE USER IS ALLOWED TO PAN AND ZOOM AROUND 
*	THE IMAGE. CURRENTLY ARGS AND IKON DISPLAYS ARE SUPPORTED.
*
*METHOD
*       OBTAIN AN INPUT FILE IF SUPPLIED. OBTAIN WORKSPACE AND COPY
*       INPUT FILE TO IT IF GIVEN. CALL XYINCR TO INTERACT WITH THE
*       SCREEN. OBTAIN AN OUTPUT FILE OF THE REQUIRED SIZE AND INSERT
*       THE LISTS INTO IT. UPDATE THE OUTPUT DESCRIPTOR ITEMS
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUT
*               OPTIONAL EXISTING FILE TO APPEND POSITIONS TO
*       MINENTRY
*               MIN NUMBER OF ENTRIES THE USER MUST GIVE
*       MAXENTRY
*               MAX NUMBER OF ENTRIES THE USER MAY GIVE
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       IDENTIFY
*               (LOGICAL) IF TRUE, PROGRAM PROMPTS FOR AN IDENTIFIER
*               FOR EACH X,Y POSITION
*       PLOT
*               SPECIFIES THE SYMBOL TO BE DRAWN ON THE SCREEN
*               AT EACH X,Y POSITION GIVEN.
*       COLOUR
*               COLOUR TO USE FOR PLOTTING
*       OVERLAY
*               OVERLAY PLANE TO USE FOR PLOTTING
*       XYTRAN
*               TRANSFORMATION COEFFICIENTS TO APPLY TO THE
*               COORDINATES
*       PREFIX
*               A CHARACTER PREFIX TO BE ADDED TO THE IDENTIFIERS
*	DEVICE
*		DEVICE ON WHICH IMAGE IS DISPLAYED
*       INFO
*               LOGICAL FLAG TO ENABLE PRINTING OF INFORMATIONAL
*               MESSAGE DESCRIBING USE OF ARGS BUTTONS
*       NOLIST/ERROR/
*               ACCESSED IF THERE ARE NO COORDINATES FOR THE OUTPUT
*               FILE
*	NOTSUPP/ERROR/
*		ACCESSED IF THE USER SPECIFIES AN UNSUPPORTED DEVICE
*       OUTPUT
*               OUTPUT FILE
*       TITLE
*               A TITLE FOR THE OUTPUT FILE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GTXYLR,EXTLST,GETCMD,XYINCR,GTXYLW,ADDLST,
*               LBGONE,GTDSCR,PTDSCR,IKONOP,ARGSOP,IKONCL,ARGSCL,
*		GETDEV,DEFDEV
*       STARLINK:
*               GETDYN,WRERR,RDKEYL,RDKEYR,WRUSER,RDKEYC,CYDSCR,FRDATA
*	SGS:
*		SGS_WIDEN
*
*NOTES
*       USES VAX %VAL FACILITY & SUBROUTINE NAMES WITH MORE THAN
*       6 CHARACTERS
*
*WRITTEN BY
*       R.F. WARREN-SMITH (modified by DS Berry to use IKON 29/6/88)
*----------------------------------------------------------------------
*
*
      character plot*10,cval*1,title(1)*30,prbuf*40,colour*7,value*80
     : ,prefix(1)*20,device*30
      logical ident(1),info(1)
      real tr(6)
      data tr/0.0,1.0,0.0,0.0,0.0,1.0/
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN AN OPTIONAL INPUT DATA FRAME
*
      call gtxylr('INPUT',.true.,nitem,lstlen,ipin,ierri)
 
*
* IF INPUT NOT OBTAINED, SET DEFAULT VALUES FOR THE LIST DIMENSIONS
* NITEM=NO OF 4-BYTE ITEMS PER LIST RECORD
* LSTLEN=NO OF LIST RECORDS
*
 
      if(ierri.ne.0) then
         nitem=7
         lstlen=0
      endif
 
 
*
* OBTAIN MINIMUM PERMITTED NUMBER OF POSITIONS TO BE ENTERED
* THEN SET MINIMUM LENGTH OF OUTPUT LIST
*
      minent=0
      call getpar('MINENTRY','INTEGER',1,0.0,1.0e6,.true.,minent,rval
     : ,ierr)
      minlen=lstlen+minent
 
*
* SET DEFAULT FOR MAX NUMBER OF ENTRIES, THEN OBTAIN VALUE FROM
* ENVIRONMENT
*
      maxent=max(1000,minent+1000)
      call getpar('MAXENTRY','INTEGER',1,real(minent),1.0e6,.true.,
     :maxent,rval,ierr)
      maxlen=lstlen+maxent
 
*
* OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND X,Y
* VALUES
*
      call getdyn('ID',104,5*maxlen,ipid,istati)
      call getdyn('X',104,maxlen,ipx,istatx)
      call getdyn('Y',104,maxlen,ipy,istaty)
 
*
* IF SPACE NOT AVAILABLE... GIVE ERROR MESSAGE AND ABORT
*
 
      if((istati.ne.0).or.(istatx.ne.0).or.(istaty.ne.0)) then
         call wrerr('NOSPACE')
         go to 99
 
      endif
 
 
*
* IF INPUT WAS SUPPLIED, COPY INPUT LIST DATA TO WORKSPACE
*
 
      if(ierri.eq.0) then
         call extlst(%val(ipin),nitem,lstlen,%val(ipid),1,20)
         call extlst(%val(ipin),nitem,lstlen,%val(ipx),21,24)
         call extlst(%val(ipin),nitem,lstlen,%val(ipy),25,28)
      endif
 
 
*
* DETERMINE IF IDENTIFIERS ARE TO BE PROMPTED FOR
*
      ident(1)=.false.
      call rdkeyl('IDENTIFY',.true.,1,ident,nval,istat)
 
*
* DETERMINE TYPE OF PLOTTING REQUIRED ON SCREEN
*
      iplot=2
      call getcmd('PLOT','NONE,CROSS,POLYGON,FRAME.',1,iplot,plot,lplot
     : ,ierr)
 
*
* OBTAIN PLOTTING COLOUR
*
      icol=2
      call getcmd('COLOUR','GREEN,RED,BLUE,YELLOW,CYAN,MAGENTA,BLACK,W'/
     : / 'HITE.',1,icol,colour,lcol,ierr)
 
*
* OBTAIN NUMBER OF OVERLAY PLANE FOR PLOTTING
*
      iplane=icol
      call getpar('OVERLAY','INTEGER',1,1.0,8.0,.true.,iplane,rval,
     :ierr)
      iplane=iplane+7
 
*
* OBTAIN TRANSFORMATION COEFFICIENTS TO APPLY TO COORDINATES
*
      call rdkeyr('XYTRAN',.true.,6,tr,nval,istat)
 
*
* OBTAIN IDENTIFIER PREFIX
*
      prefix(1)=' '
      call rdkeyc('PREFIX',.true.,1,prefix,nval,istat)
 
*
* DETERMINE IF USER IS TO BE TOLD HOW TO USE DEVICE BUTTONS
*
      info(1)=.true.
      call rdkeyl('INFO',.true.,1,info,nval,istat)
*
* OBTAIN DEVICE ON WHICH IMAGE IS TO BE DISPLAYED
*
      call defdev(device)
      call getdev('DEVICE',device,.false.,ierr)
      if(ierr.ne.0) goto 99
      call sgs_widen(device,itype,iconid,ierr)
      if(ierr.ne.0) goto 99

*
* IF AN ARGS IS TO BE USED, CALL ARGSOP TO SET UP THE ARGS
*
      if(itype.eq.160) then
         device='ARGS'
         call argsop(info,.false.,ierr)

*
* IF AN IKON IS TO BE USED, CALL IKONOP TO SET UP THE IKON
*
      else if(itype.eq.3200) then
         device='IKON'
         call ikonop(info,.false.,ierr)

*
* IF ANY OTHER DEVICE IS SPECIFIED, QUIT
*
      else
         call wrerr('NOTSUPP')
         goto 99
      endif

*
* IF AN ERROR OCCURED WHILE SETTING UP THE DEVICE, QUIT
*
      if(ierr.ne.0) goto 99
 
*
* CALL XYINCR TO ADD NEW VALUES TO THE WORKSPACE
*
      call xyincr(%val(ipid),%val(ipx),%val(ipy),lstlen,minlen,maxlen
     : ,ident(1),plot,colour,iplane,ilevel,tr,prefix(1),device,ierr)
 
*
* CLOSE DOWN THE SELECTED DEVICE
*
      if(device.eq.'ARGS') then
         call argscl
      else
         call ikoncl
      endif
*
* IF NO LIST OBTAINED, GIVE ERROR MESSAGE AND ABORT... OTHERWISE
* OBTAIN OUTPUT DATA FRAME
*
 
      if(lstlen.le.0) then
         call wrerr('NOLIST')
         go to 99
 
 
      else
         call gtxylw('OUTPUT',.true.,7,lstlen,ipout,ierr2)
 
         if(ierr2.eq.0) then
 
*
* OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS FROM WORKSPACE TO
* OUTPUT DATA FRAME
*
            call addlst(%val(ipout),7,lstlen,%val(ipid),1,20)
            call addlst(%val(ipout),7,lstlen,%val(ipx),21,24)
            call addlst(%val(ipout),7,lstlen,%val(ipy),25,28)
 
*
* TELL USER HOW MANY ENTRIES IN OUTPUT LIST
*
 
            if(ilevel.ge.2) then
               write(prbuf,104)lstlen
 
               if(lstlen.eq.1) prbuf(28:)='ENTRY'
104            format('   OUTPUT LIST HAS ',i7,' ENTRIES')
               call lbgone(prbuf(20:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
            endif
 
 
*
* SET DEFAULT OUTPUT TITLE THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
*
            title(1)='Output from XYCUR'
 
            if(ierri.eq.0) then
               call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
            endif
 
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT (IF AVAILABLE) THEN UPDATE
* DESCRIPTOR ITEMS
*
 
            if(ierri.eq.0) call cydscr('INPUT','OUTPUT',istat)
            call ptdscr('OUTPUT','NITEM','INTEGER',7,rval,cval,ierr)
            call ptdscr('OUTPUT','LSTLEN','INTEGER',lstlen,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
         endif
 
      endif
 
 
*
* FREE ALL DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
