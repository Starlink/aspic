      subroutine gthelp(pname,text,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To output informational, help and error messages to the user
*       using text in a help library. The help library must be
*       in a standard format to enable the routine to find the
*       relevant information. The information is output 'in-line'
*       with normal program operation
*
*SOURCE
*       GTHELP.FOR in UTILITIES.TLB
*
*METHOD
*       The help library is search for in the same directory
*       as the program connection file. The first help library found
*       containing a module with the name of the current program is
*       used. If none are found, the logical name PAKHELP is translated
*       and the translation used as the library name. If this library
*       cannot be used a message is given and no help is output.
*              According to the type of information to be output, the
*       routine calls the library management routine lbr$output_help
*       with a variety of help keywords and output routines so as to
*       produce the required effect on the screen. Failure to find the
*       information is signalled with an appropriate message
*
*ARGUMENTS
*   INPUTS:
*       PNAME   character       The starlink parameter name on which
*                               information is required. this may be an error
*                               parameter.
*       TEXT    character       A keyword specifying the type of output message
*                               required. valid values are:
*
*                               INFO
*                                 A concise format help message
*                               HELP
*                                 The user is put into the help system at the
*                                 point containing the information and remains
*                                 there until a normal exit is made from the
*                                 help library
*                               PROMPT
*                                 A prompt message for use prior to acquiring
*                                 a parameter value from the user
*                               ERROR
*                                 Only applicable to error parameters, this
*                                 uses the available help text as an error
*                                 message.
*USED BY
*       All programs which access Starlink parameters (see GETPPI,WRERR)
*
*SUBROUTINE CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               OPHELP,OPNULL,FILENM,ustrln
*       VAX LIBRARY MANAGEMENT ROUTINES:
*               LBR$OUTPUT_HELP
*       RUN TIME LIBRARY:
*               LIB$GET_INPUT,LIB$PUT_OUTPUT
*
*NOTES
*       Uses the common block /HLPBL1/ to communicate with ophelp
*
*WRITTEN BY
*       R.F. Warren-Smith
*       (Modified by D.S. Berry (MAVAD::DSB) 3/2/88 to look for help
*       library in connection file directory.
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK AND SYMBOL DEFINITIONS
*
      include 'UTILITIES(HLPBL1COM)'
      include '($SSDEF)'
*
* DECLARATIONS, ETC.
*
      character pname*(*),prog*80,text*(*),dirn*80,filenm*80
      integer  ustrln
      external lib$get_input,ophelp,lib$put_output,opnull
*
* ONLY ON THE FIRST CALL TO THIS ROUTINE IS THE HELP LIBRARY LOOKED
* FOR. IF A LIBRARY IS FOUND ITS NAME IS STORED IN COMMON, IF NON
* IS FOUND THEN THE LIBRARY NAME IS SET BLANK AND NO MORE ATTEMPTS
* ARE MADE TO FIND IT
*
      if(libnam.eq.'FIRST TIME') then
         canhlp=.false.
*
* EXTRACT THE CURRENTLY RUNNING PROGRAM NAME FROM THE TRANSLATION
* OF THE LOGICAL NAME 'DSCL_PROGCON' (THE NAME OF THE CONNECTION FILE)
* AND ALSO GET THE DIRECTORY IN WHICH THE CONECTION FILE IS. IT IS
* ASSUMED THAT THE HELP IS TO BE OBTAINED FROM A HELP LIBRARY IN THAT
* DIRECTORY. IF NONE EXISTS, LOGICAL NAME PAKHELP IS TRANSLATED AND THE
* LIBRARY THUS OBTAINED IS USED
*
         ist1=lib$sys_trnlog('DSCL_PROGCON',,prog,,,)
         if(ist1.eq.SS$_NORMAL) then
            n1=index(prog,']')
            if(n1.gt.0) then
               dirn=prog(1:n1)
               prog=prog(n1+1:)
            else
               n1=index(prog,':')
               if(n1.gt.0) then
                  dirn=prog(1:n1)
               else
                  dirn=' '
               endif
               prog=prog(n1+1:)
            endif
            n2=index(prog,'.')
            if(n2.gt.1) prog=prog(:n2-1)
*
* SEE IF A HELP LIBRARY EXISTS IN THE SAME DIRECTORY AS THE PROGRAM
* CONNECTION FILE, WHICH CONTAINS AN ENTRY FOR THE CURRENT PROGRAM
*
            more=.true.
            do while(more)
               libnam=filenm(dirn(:ustrln(dirn))//'*.HLB')
               if(libnam.ne.' ') then
                  istat=lbr$output_help(opnull,,prog,libnam,0,
     :                                                   lib$get_input)
                  if(istat.eq.SS$_NORMAL) then
                     canhlp=.true.
                     more=.false.
                  endif
               else
                  more=.false.
               endif
            enddo
         endif
*
* IF NOT THEN TRANSLATE LOGICAL NAME PAKHELP AND SEE IF THAT GIVES A
* GOOD HELP LIBRARY
*
         if(.not.canhlp) then
            ist1=lib$sys_trnlog('PAKHELP',liblen,libnam,,,)
            if(ist1.eq.SS$_NORMAL) then
               istat=lbr$output_help(opnull,,prog,libnam,0,
     :                                             lib$get_input)
               if(istat.eq.SS$_NORMAL) canhlp=.true.
            endif
         endif
         if(.not.canhlp) libnam=' '
*
* IF THIS IS NOT THE FIRST TIME THROUGH THIS ROUTINE THEN IF NO
* LIBRARY NAME IS DEFINED FLAG NO HELP AVAILABLE. IF A LIBRARY NAME
* IS DEFINED THEN FLAG HELP AVAILABLE
*
      else
         if(libnam.eq.' ') then
            canhlp=.false.
         else
            canhlp=.true.
         endif
      endif
*
* INITIALIZE THE NUMBER OF LINES ACTUALLY DISPLAYED TO ZERO
* AND DEFINE THE SIZE OF A BLOCK (IN LINES) BEFORE A PAUSE IS
* GIVEN TO ALLOW USERER TO  READ THE TEXT
*
      nwrite=0
      block=30
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



