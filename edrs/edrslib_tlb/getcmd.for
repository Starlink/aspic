      subroutine getcmd(name,cmdlst,ireply,ncomm,idcomm,lencom,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN CHARACTER PARAMETERS FROM THE ENVIRONMENT, CHECK
*       THEIR VALIDITY AND HANDLE ANY ERRORS WHICH OCCUR
*
*METHOD
*       EXTRACT THE DEFAULT VALUE FROM THE INPUT LIST OF ALTERNATIVES.
*       OBTAIN A VALUE FROM THE ENVIRONMENT. IF OBTAINED OK, CALL
*       CMDWRD TO CHECK ITS VALIDITY AGAINST THE LIST OF ALTERNATIVES.
*       IF VALID, USE IT, OTHERWISE PROMPT FOR A NEW VALUE.
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               THE NAME OF THE PARAMETER TO BE OBTAINED
*       CMDLST (IN)
*       CHARACTER*(*)
*               A LIST OF ALTERNATIVE VALID PARAMETER VALUES, SEPARATED
*               BY COMMAS AND TERMINATED WITH A FULL STOP.
*       IREPLY (IN)
*       INTEGER
*               IF SET TO 1 OR MORE, THE ROUTINE ISSUES HELPFUL MESSAGES
*               TO THE USER IF ERRONEOUS VALUES ARE ENTERED
*       NCOMM (IN/OUT)
*       INTEGER
*               ON ENTRY, SPECIFIES THE POSITION IN THE LIST 'CMDLST'
*               OF THE DEFAULT VALUE FOR THE PARAMETER. ON EXIT, GIVES
*               THE POSITION OF THE VALUE OBTAINED FROM THE ENVIRONMENT
*       IDCOMM (OUT)
*       CHARACTER*(*)
*               THE FULL FORM OF THE VALUE OBTAINED FROM THE ENVIRONMENT
*       LENCOM (OUT)
*       INTEGER
*               THE NUMBER OF NON-BLANK CHARACTERS IN IDCOMM
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       'NAME'
*               PARAMETER NAME TO BE OBTAINED IS GIVEN IN THE ARGUMENT
*               'NAME'
*       BADCMD/ERROR/
*               ACCESSED IF AN INVALID VALUE IS GIVEN FOR THE PARAMETER
*       AMBIGCMD/ERROR/
*               ACCESSED IF AN AMBIGUOUS ABBREVIATION IS USED FOR
*               THE PARAMETER
*       TOOBAD/ERROR/
*               ACCESSED IF THE MAXIMUM PERMISSIBLE NUMBER OF INVALID
*               VALUES HAS BEEN GIVEN AND THE ROUTINE IS TO USE THE
*               DEFAULT VALUE
*
*CALLS
*       THIS PACKAGE:
*               NINDEX,CMDWRD
*       STARLINK:
*               RDKEYC,WRERR,WRUSER,CNPAR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*),cmdlst*(*),idcomm*(*),cmd(1)*80
      logical exit
      integer start,end,endlst,cerr
 
*
* SET MAX. NUMBER OF ERRONEOUS ENTRIES ALLOWED
*
      parameter (maxwng=4)
      ierr=0
      nwrong=0
 
*
* FIND THE BEGINNING AND END OF THE DEFAULT PARAMETER VALUE IN THE
* INPUT LIST
*
      start=nindex(cmdlst,',',ncomm-1)+1
      endlst=nindex(cmdlst,'.',1)-1
      end=min(nindex(cmdlst,',',ncomm)-1,endlst)
      length=end-start+1
 
*
* IF IT HAS ZERO LENGTH, ABORT WITH IERR=1
*
 
      if(length.le.0) then
         ierr=1
 
      else
 
*
* SET DEFAULT VALUE, THEN OBTAIN NEW VALUE FROM THE ENVIRONMENT
*
67       cmd(1)=cmdlst(start:end)
         call rdkeyc(name,.true.,1,cmd,nvals,keyerr)
 
*
* IF PARAMETER NAME IS INVALID, EXIT WITH IERR=2
*
 
         if(keyerr.eq.4) then
            ierr=2
            nwrong=0
            exit=.true.
 
*
* IF NULL WAS ENTERED, USE THE DEFAULT VALUE
*
 
         else if(keyerr.eq.1) then
            idcomm=cmdlst(start:end)
            lencom=length
            nwrong=0
            exit=.true.
 
         else
 
*
* IF VALUE WAS ENTERED SUCCESSFULLY, CALL CMDWRD TO IDENTIFY IT
*
            call cmdwrd(cmdlst(:endlst),cmd(1),ninput,nid,idcomm,lencom
     :       ,cerr)
 
*
* IF THE INPUT WAS BLANK, USE THE DEFAULT
*
 
            if(cerr.eq.4) then
               idcomm=cmdlst(start:end)
               lencom=length
               nwrong=0
               exit=.true.
 
*
* IF NOT A VALID VALUE, GIVE MESSAGE AND RETURN FOR A NEW VALUE
*
 
            else if(cerr.eq.3) then
               call wrerr('BADCMD')
               nwrong=nwrong+1
               exit=.false.
 
*
* IF AN AMBIGUOUS ABBREVIATION, GIVE MESSAGE AND RETURN FOR A NEW
* VALUE
*
 
            else if(cerr.eq.2) then
               call wrerr('AMBIGCMD')
               nwrong=nwrong+1
               exit=.false.
 
            else
 
*
* OTHERWISE OK...
*
               nwrong=0
               exit=.true.
               ncomm=nid
 
*
* IF MIS-TYPED, GIVE MESSAGE, BUT CONTINUE
*
 
               if(cerr.eq.1) then
 
                  if(ireply.ge.1) then
                     call wruser(' ***'''//cmd(1)(:ninput)//''' IS NOT'/
     :                / ' VALID',istat)
                     call wruser(' ***ASSUMING YOU MEAN '''//
     :               idcomm(:lencom)//'''',istat)
                  endif
 
               endif
 
            endif
 
         endif
 
 
*
* IF PARAMETER WAS BAD, BUT MAX NUMBER OF BAD ENTRIES HAS NOT BEEN
* REACHED, GIVE MESSAGE SHOWING THE VALID VALUES
*
 
         if(nwrong.lt.maxwng) then
 
            if(nwrong.gt.0) then
 
               if(ireply.ge.1) then
                  call wruser(' ***VALID VALUES ARE: '//cmdlst(:endlst)
     :             ,istat)
               endif
 
            endif
 
 
         else
 
*
* IF MAX. NUMBER OF BAD ENTRIES HAS BEEN REACHED, GIVE MESSAGE AND USE
* THE DEFAULT
*
            idcomm=cmdlst(start:end)
            lencom=length
            call wrerr('TOOBAD')
 
            if(ireply.ge.1) then
               call wruser(' ***DEFAULTING TO '''//idcomm(:lencom)//
     :         '''',istat)
            endif
 
            go to 99
 
         endif
 
 
*
* IF A NEW VALUE IS NEEDED, CANCEL OLD VALUE AND RETURN FOR A NEW ONE
*
 
         if(.not.exit) then
            call cnpar(name,istat)
            go to 67
 
         endif
 
      endif
 
99    return
 
      end
 
 
 
