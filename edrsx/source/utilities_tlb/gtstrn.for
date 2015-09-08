      subroutine gtstrn(name,null,cmdlst,ireply,ncomm,idcomm,
     :                  lencom,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To obtain character parameters from the environment, check
*       their validity and handle any errors which occur
*
*SOURCE
*       GTSTRN.FOR in UTILITIES.TLB
*
*METHOD
*       If a null value is allowed extract the default value from the
*       input list of alternatives. obtain a value from the environment.
*       If obtained ok, call cmdwrd to check its validity against the
*       list of alternatives. If valid, use it, otherwise prompt for a new
*       value.
*
*ARGUMENTS
*   INPUTS:
*       name    character*(*)   The name of the parameter to be obtained
*       null    logical         If true, a null entry is ok, otherwise it is
*                               an error condition.
*       cmdlst  character*(*)   a list of alternative valid parameter values,
*                               separated by commas and terminated with a full
*                               stop.
*       ireply  integer         If set to 1 or more, the routine issues helpful
*                               messages to the user if erroneous values are
*                               entered
*       ncomm   integer         On entry, specifies the position in the list
*                               'cmdlst' of the default value for the
*                               parameter.
*   OUTPUTS:
*       ncomm   integer         On exit, gives the position of the value
*                               obtained from the environment
*       idcomm  character*(*)   The full form of the value obtained from the
*                               environment
*       lencom  integer         The number of non-blank characters in idcomm
*       ierr    integer         Error flag:
*                                       0 - Success
*                                       4 - Invalid parameter name
*                                       16- Too many bad entries
*                                       51- Zero length default
*
*
*USED BY
*       EDRSIN
*
*SUBROUTINES CALLED
*       EDRS:
*               NINDEX,CMDWRD,WRERR
*       STARLINK:
*               RDKEYC,WRUSER,CNPAR
*
*STARLINK PARAMETERS
*       'NAME'/READ/
*               Parameter name to be obtained is given in the argument
*               'name'
*       NONULL/ERROR/
*               Accessed if a null entry is given and NULL is false.
*       BADCMD/ERROR/
*               Accessed if an invalid value is given for the parameter
*       AMBIGCMD/ERROR/
*               Accessed if an ambiguous abbreviation is used for
*               the parameter
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/8/87
*       based on EDRS routine GETCMD by R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*),cmdlst*(*),idcomm*(*),cmd(1)*80
      logical exit,null,deflt
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
      endlst=nindex(cmdlst,'.',1)-1
 67   if(ncomm.ne.0) then
         deflt=.true.
         start=nindex(cmdlst,',',ncomm-1)+1
         end=min(nindex(cmdlst,',',ncomm)-1,endlst)
         length=end-start+1
*
* IF IT HAS ZERO LENGTH, ABORT WITH IERR=51
*
         if(length.le.0) then
            ierr=51
            goto 99
         endif
         cmd(1)=cmdlst(start:end)
      else
         deflt=.false.
         cmd(1)=' '
      endif
*
* SET DEFAULT VALUE, THEN OBTAIN NEW VALUE FROM THE ENVIRONMENT
*
      call rdkeyc(name,deflt,1,cmd,nvals,keyerr)
*
* IF PARAMETER NAME IS INVALID, EXIT WITH IERR=4
*
      if(keyerr.eq.4) then
         ierr=4
         nwrong=0
         exit=.true.
*
* IF NULL WAS ENTERED AND NULL IS ALLOWED, USE THE DEFAULT VALUE
*
      else if(keyerr.eq.1) then
         if(null) then
            if(deflt) then
               idcomm=cmdlst(start:end)
               lencom=length
            else
               idcomm=' '
               lencom=0
            endif
            nwrong=0
            exit=.true.
            ierr=0
*
* IF NULL ENTERED AN NULL IS NOT ALLOWED, GIVE MESSAGE AND RETURN FOR NEW
* ENTRY
*
         else
            call wrerr('NONULL')
            exit=.false.
         endif
      else
*
* IF VALUE WAS ENTERED SUCCESSFULLY, CALL CMDWRD TO IDENTIFY IT
*
         call cmdwrd(cmdlst(:endlst),cmd(1),ninput,nid,idcomm,lencom
     :               ,cerr)
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
     :                        / ' VALID',istat)
                  call wruser(' ***ASSUMING YOU MEAN '''//
     :                        idcomm(:lencom)//'''',istat)
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
* IF MAX. NUMBER OF BAD ENTRIES HAS BEEN REACHED, ABORT WITH IERR=16
*
         ierr=16
         go to 99
      endif
*
* IF A NEW VALUE IS NEEDED, CANCEL OLD VALUE AND RETURN FOR A NEW ONE
*
      if(.not.exit) then
         call cnpar(name,istat)
         go to 67
      endif
99    continue

      end



