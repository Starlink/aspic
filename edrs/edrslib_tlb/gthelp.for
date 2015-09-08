      subroutine gthelp(pname,text,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OUTPUT INFORMATIONAL, HELP AND ERROR MESSAGES TO THE USER
*       USING TEXT IN A HELP LIBRARY. THE HELP LIBRARY MUST BE
*       IN A STANDARD FORMAT TO ENABLE THE ROUTINE TO FIND THE
*       RELEVANT INFORMATION. THE INFORMATION IS OUTPUT 'IN-LINE'
*       WITH NORMAL PROGRAM OPERATION
*
*METHOD
*       ACCORDING TO THE TYPE OF INFORMATION TO BE OUTPUT, THE ROUTINE
*       CALLS THE LIBRARY MANAGEMENT ROUTINE LBR$OUTPUT_HELP WITH
*       A VARIETY OF HELP KEYWORDS AND OUTPUT ROUTINES SO AS TO PRODUCE
*       THE REQUIRED EFFECT ON THE SCREEN. FAILURE TO FIND THE
*       INFORMATION IS SIGNALLED WITH AN APPROPRIATE MESSAGE
*
*ARGUMENTS
*       PNAME (IN)
*       CHARACTER*(*)
*               THE STARLINK PARAMETER NAME ON WHICH INFORMATION IS
*               REQUIRED. THIS MAY BE AN ERROR PARAMETER.
*       TEXT (IN)
*       CHARACTER*(*)
*               A KEYWORD SPECIFYING THE TYPE OF OUTPUT MESSAGE
*               REQUIRED. VALID VALUES ARE:
*               INFO:
*                       A CONCISE FORMAT HELP MESSAGE
*               HELP:
*                       THE USER IS PUT INTO THE HELP SYSTEM AT THE
*                       POINT CONTAINING THE INFORMATION AND REMAINS
*                       THERE UNTIL A NORMAL EXIT IS MADE FROM THE
*                       HELP LIBRARY
*               PROMPT:
*                       A PROMPT MESSAGE FOR USE PRIOR TO ACQUIRING
*                       A PARAMETER VALUE FROM THE USER
*               ERROR:
*                       ONLY APPLICABLE TO ERROR PARAMETERS, THIS
*                       USES THE AVAILABLE HELP TEXT AS AN ERROR
*                       MESSAGE.
*
*CALLS
*       THIS PACKAGE:
*               OPHELP
*       VAX LIBRARY MANAGEMENT ROUTINES:
*               LBR$OUTPUT_HELP
*       RUN TIME LIBRARY:
*               LIB$GET_INPUT,LIB$PUT_OUTPUT
*
*NOTES
*       USES THE COMMON BLOCK /HLPBL1/ TO COMMUNICATE WITH OPHELP
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK
*
      include 'HLPBL1COM.FOR'
 
*
* DECLARATIONS, ETC.
*
      character pname*(*),libnam*30,prog*80,text*(*)
      external lib$get_input,lib$put_output,ophelp
 
*
* DEFINE NAME OF HELP LIBRARY BEING USED
*
      data libnam/'EDRS:HELPLIB.HLB'/
 
*
* SEE IF THE HELP LIBRARY IS AVAILABLE
*
      open(99,file=libnam,readonly,status='OLD',iostat=iohelp)
      canhlp=iohelp.eq.0
 
      if(canhlp)close(99)
 
*
* EXTRACT THE CURRENTLY RUNNING PROGRAM NAME FROM THE TRANSLATION
* OF THE LOGICAL NAME 'DSCL_PROGCON' (THE NAME OF THE CONNECTION FILE)
*
      ist1=lib$sys_trnlog('DSCL_PROGCON',,prog,,,)
      n1=index(prog,':')
      prog=prog(n1+1:)
      n1=index(prog,']')
      prog=prog(n1+1:)
      n2=index(prog,'.')
      prog=prog(:n2-1)
 
*
* IF INFORMATION IS REQUIRED, EXTRACT IT FROM THE HELP LIBRARY
* AND OUTPUT IT IN A CONCISE FORMAT VIA THE OUTPUT ROUTINE OPHELP
*
 
      if(text.eq.'INFO')then
         nlout=-3
         trim=.true.
         call wruser(' ',istat)
 
         if(canhlp)istat=lbr$output_help(ophelp,,prog//' PARAMETERS '
     :   //pname,libnam,0,lib$get_input)
 
         if(.not.canhlp)then
            call wruser('Sorry, no information available.',istat)
            ierr=1
         endif
 
         call wruser(' ',istat)
 
*
* IF HELP IS REQUIRED, PUT THE USER INTO THE HELP LIBRARY
* AT THE CORRECT POINT
*
 
      else if(text.eq.'HELP')then
 
         if(canhlp)istat=lbr$output_help(lib$put_output,,prog//' PARAM'/
     :    / 'ETERS '//pname,libnam,,lib$get_input)
 
         if(.not.canhlp)then
            call wruser(' ',istat)
            call wruser('Sorry, no help information available',istat)
            ierr=1
            call wruser(' ',istat)
         endif
 
 
*
* IF A PROMPT IS REQUIRED, PRINT PARAMETER TITLE, THEN INFORMATION TEXT
*
 
      else if(text.eq.'PROMPT')then
         nlout=-3
         trim=.true.
         call wruser(' ',istat)
         call wruser('-->Give value for parameter '//
     :   pname(:lens(pname)),istat)
         call wruser (' ',istat)
 
         if(canhlp)istat=lbr$output_help(ophelp,,prog//' PARAMETERS '
     :   //pname,libnam,0,lib$get_input)
 
         if(.not.canhlp)then
            call wruser('Sorry, no prompt available.',istat)
            ierr=1
         endif
 
         call wruser(' ',istat)
 
*
* IF AN ERROR MESSAGE IS REQUIRED, OUTPUT THE TEXT ASSOCIATED WITH THE
* ERROR PARAMETER IN THE HELP LIBRARY (IN CONCISE FORMAT)
*
 
      else if(text.eq.'ERROR')then
         nlout=-3
         trim=.true.
         call wruser(' ',istat)
 
         if(canhlp)istat=lbr$output_help(ophelp,,prog//' ERRORS '//
     :   pname,libnam,0,lib$get_input)
 
         if(.not.canhlp)then
            ierr=1
 
         else
            call wruser(' ',istat)
         endif
 
      endif
 
 
*
* SET RETURN STATUS
*
      ierr=1
 
      if(canhlp)ierr=0
 
      end
 
 
 
