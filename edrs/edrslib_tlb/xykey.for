      subroutine xykey
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN A LIST OF X,Y POSITIONS AND IDENTIFIERS FROM THE
*       KEYBOARD AND INSERT THEM IN A FILE
*
*METHOD
*       OBTAIN AN INPUT FILE IF SUPPLIED. OBTAIN WORKSPACE AND COPY
*       INPUT FILE TO IT IF GIVEN. CALL XYINKY TO INTERACT WITH THE
*       KEYBOARD. OBTAIN AN OUTPUT FILE OF THE REQUIRED SIZE AND INSERT
*       THE LISTS INTO IT. UPDATE THE OUTPUT DESCRIPTOR ITEMS
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF INFORMATIONAL
*               MESSAGES
*       INPUT
*               OPTIONAL EXISTING FILE TO ADD POSITIONS TO
*       MAXENTRY
*               MAX NUMBER OF LIST ENTRIES (USED TO ASSIGN WORKSPACE)
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       NOLIST/ERROR/
*               ACCESSED IF NO ENTRIES WERE GIVEN AT THE KEYBOARD
*       OUTPUT
*               OUTPUT FILE
*       TITLE
*               A TITLE FOR THE OUTPUT FILE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GTXYLR,EXTLST,XYINKY,GTXYLW,ADDLST,LBGONE,GTDSCR,
*               PTDSCR
*       STARLINK:
*               GETDYN,WRERR,WRUSER,RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,prbuf*40
 
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
* SET DEFAULT FOR MAX LENGTH OF OUTPUT LIST, THEN OBTAIN VALUE FROM
* ENVIRONMENT
*
      lenout=lstlen+100
      call getpar('MAXENTRY','INTEGER',1,real(lstlen+1),1.0e6,.true
     : .,lenout,rval,ierr)
 
*
* OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND X,Y
* VALUES
*
      call getdyn('ID',104,5*lenout,ipid,istati)
      call getdyn('X',104,lenout,ipx,istatx)
      call getdyn('Y',104,lenout,ipy,istaty)
 
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
* CALL XYINKY TO ADD NEW KEYBOARD VALUES TO THE WORKSPACE
*
      call xyinky(%val(ipid),%val(ipx),%val(ipy),lenout,lstlen,ierr)
 
*
* IF NO LIST OBTAINED, GIVE ERROR MESSAGE AND ABORT... OTHERWISE
* OBTAIN OUTPUT DATA FRAME
*
 
      if(lstlen.le.0) then
         call wrerr('NOLIST')
         go to 99
 
 
      else
         call gtxylw('OUTPUT',.false.,7,lstlen,ipout,ierr2)
 
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
* DEFAULT OUTPUT TITLE IS EITHER THE INPUT TITLE, OR BLANK.
* THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
*
            title(1)=' '
 
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
 
 
 
