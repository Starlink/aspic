      subroutine stl_getppi(entry,dfault,maxval,carray,actval,status)
 
*++
*     GETPPI - Get Program Parameter Information.
*
*     This routine serves as an interim interface to the command
*     process. It is used to obtain program parameter information
*     from the user environment.
*
*     An entry is assumed to have been reserved in the Parameter
*     Control Tables (PCT) for the specified parameter; the input
*     argument 'ENTRY' points to this.
*
*     If the entry has null values, (STATE=NULL), then the status
*     is set accordingly and no further operations are performed.
*
*     If the entry is already active, (STATE=ACTIVE), then the values
*     previously stored are returned to the program.
*
*     If the entry has been cancelled, (STATE=CANCEL), then new values
*     are obtained directly from the user.
*
*     If the entry is in its 'ground' state, (STATE=GROUND), then there
*     are three methods by which the parameter values can be obtained:
*
*       (1) The user command line.
*       (2) Connection File defaults.
*       (3) User response to prompts.
*
*     The user may have specified the parameter on the command
*     line thus:  ....COORDS=1,1,512,512.... Any parameters
*     supplied in this fashion, will have been written to a buffer
*     by the command process in the form:-
*
*       /name=[value,value...]/name=[value,value...]/.....
*
*     The specified parameter name is searched for in this buffer and,
*     if found, any associated values are assumed to be those required
*     by the applications program.
*
*     The Connection File is then searched, to confirm the entry is
*     valid and to determine the parameter type.
*
*     If the user has not supplied the parameter value(s) on the
*     command line, then the corresponding Connection File entry
*     will be checked for any default value(s) which, if they exist,
*     will be returned to the applications program.
*
*     If no defaults exist, then the user will be prompted for the
*     parameters value(s), which he enters in the form:
*
*        [value][,value][,value].....[,value]
*
*     The user can just hit <return> after the prompt, if he does not
*     wish to supply any value(s) at all. The user can specify a 'null'
*     default value in the connection file by two consecutive slashes.
*
*
*     CALL STL_GETPPI(ENTRY,DFAULT,MAXVAL,CARRAY,ACTVAL,STATUS)
*
*     Input parameters:
*     ----------------
*     ENTRY:   INTEGER expression:
*     DFAULT:  LOGICAL expression:
*     MAXVAL:  INTEGER expression:
*
*     Output parameters:
*     -----------------
*     CARRAY:  CHARACTER array:
*     ACTVAL:  INTEGER variable:
*     STATUS:  INTEGER variable:      Status return.
*
*
*     D.PEARCE  14/SEP/80  VERSION #2
*     ALTERED BY R.F. WARREN-SMITH TO ISSUE PROMPTS DERIVED FROM
*             AN ASSOCIATED HELP FILE BY CALLING GTHELP
*--
*
      implicit      integer(a-z)
 
*
      character*(*) carray(*),buff*80
      integer       entry,maxval,actval,status
      logical       dfault
 
*
      character*80  buffer,parm(3),values(32)
      logical       usrsup
 
*
      include 'interim(pctcom)'
      include 'interim(errpar)'
 
*
*
*     .....check for NULL state
 
      if (pct_state(entry).eq.pct_null) then
         status=err_parnul
         goto 90
 
      endif
 
 
*
*     .....check for ACTIVE state
 
      if (pct_state(entry).eq.pct_active) then
         status=err_normal
         call stl_rdpv(pct_pvaddr(1,entry),maxval,carray,actval)
         goto 90
 
      endif
 
 
*
*     .....check for CANCEL state
 
      if (pct_state(entry).eq.pct_cancel) then
 
*
*        .....get values from direct user-input
*
88       call stl_usrinp(carray,maxval,dfault,pct_name(entry),values
     :    ,actval)
 
*
* IF ENTRY IS A ? OR ??, GIVE HELP
*
         buff=values(1)
         call lbgone(buff)
 
         if(buff.eq.'?')then
            call gthelp(pct_name(entry),'INFO',istat)
            go to 88
 
         endif
 
 
         if(buff.eq.'??')then
            call gthelp(pct_name(entry),'HELP',istat)
            go to 88
 
         endif
 
 
*
*        .....process raw values
         call stl_proval(entry,values,maxval,actval,carray,status)
         goto 90
 
 
*
      endif
 
 
*
*
*     .....check for GROUND state
 
      if (pct_state(entry).eq.pct_ground) then
 
*
*        .....Search User Command Buffer for specified name
89       call stl_serucb(pct_name(entry),buffer,status)
 
*
*        .....set user-supplied flag accordingly
         usrsup=(status.eq.err_normal)
 
*
*        .....extract and translate values on command line
 
         if (usrsup) then
            call gen_extrss(buffer,'=',parm,2,n)
            call stl_trnval(parm(2))
            call gen_extrss(parm(2),',',values,32,actval)
         endif
 
 
*
*        .....Search Program Connection File for specified name
         call stl_serpcf(pct_name(entry),buffer,status)
 
*
*        .....check status
 
         if (status.ne.err_normal) goto 90
 
*
*        .....extract parameter name, type and values list
         call gen_extrss(buffer,'/',parm,3,n)
 
*
*        .....set parameter type in Parameter Control Tables
         pct_type(entry)=parm(2)
 
*
*        .....if no user-supplied parameters on command line ....
 
         if (.not.usrsup) then
 
*
*           .....translate and extract any connection-file defaults
            call stl_trnval(parm(3))
            call gen_extrss(parm(3),',',values,32,actval)
 
*
*           .....if there aren't any, prompt user (unless null default)
 
            if (actval.eq.0.and.index(buffer,'//').eq.0) then
 
*
* USE HELP MESSAGE AS A PROMPT IF REQUIRED
*
               istat=lib$sys_trnlog('DSCL_HELP_PROMPT',,buff,,,)
               call lbgone(buff)
 
               if(buff(1:4).eq.'TRUE')then
                  call gthelp(pct_name(entry),'PROMPT',istat)
               endif
 
               call stl_usrinp(carray,maxval,dfault,pct_name(entry)
     :          ,values,actval)
            endif
 
 
*
         endif
 
 
*
* GIVE HELP IF NEEDED
*
         buff=values(1)
         call lbgone(buff)
 
         if(buff.eq.'?')then
            call gthelp(pct_name(entry),'INFO',istat)
            go to 88
 
         endif
 
 
         if(buff.eq.'??')then
            call gthelp(pct_name(entry),'HELP',istat)
            go to 88
 
         endif
 
 
*
*        .....process raw values
         call stl_proval(entry,values,maxval,actval,carray,status)
         goto 90
 
 
*
      endif
 
 
*
*
90    return
 
      end
 
 
 
