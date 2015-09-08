      subroutine gtfile(unit,prompt,flstat,filnam,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To open a text file for subsequent processing by Fortran
*       read and write operations. The file name is obtained from the
*       user.
*
*SOURCE
*       GTFILE.FOR in UTILITIES.TLB
*
*METHOD
*       If the argument filnam is not blank on entry, then an attempt is
*       made to open a file of that name. If this fails or if filnam is
*       blank, then the user is prompted for a file name.
*
*ARGUMENTS
*   INPUTS:
*       unit    integer         The fortran unit number to open the
*                               file on
*       prompt  character       The prompt to issue to the user
*       flstat  character       The file status to use in the OPEN
*                               statemnet eg 'NEW', 'OLD' or 'UNKNOWN'
*       filnam  character       A first choice file name.
*   OUTPUTS:
*       filnam  character       The name of the file opened.
*       ierr    integer         Status return 0  - Success
*                                             -1 - Null entered, no
*                                                  file opened.
*                                              1 - Too many bad values
*                                                  given (reported).
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               STRLEN,RDCHAR
*
*VAX SPECIFICS
*       implicit none
*       Carriagecontrol keyword in OPEN
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 2/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer           unit,ierr
      character*(*)     prompt,flstat,filnam
*
* DECLARE LOCAL VARIABLES
*
      integer   nbad    ! The current number of bad files given by user
      integer   strlen  ! A function giving length of a string minus any
                        ! trailing spaces or control characters
      character txfile*3! YES if given file is a text file
*
* INITIALIZE BAD FILE COUNTER
*
      nbad=0
*
* GET A FILE NAME FROM THE USER
*
  10  if(filnam.eq.' ') then
         call rdchar(filnam,prompt)
         write(*,*)
      endif
*
* IF NULL WAS ENTERED THEN EXIT WITH IERR=-1
*
      if(ichar(filnam(1:1)).eq.13) then
         ierr=-1
         goto 999
      endif
*
* ATTEMPT TO OPEN THE FILE
*
      open(unit,file=filnam(:strlen(filnam)),status=flstat,
     :     carriagecontrol='LIST',iostat=ierr)
*
* IF SUCCESSFUL THEN SEE IF FILE IS A TEXT FILE
*
      if(ierr.eq.0) then
         inquire(unit,formatted=txfile)
*
* IF IT ISN'T THEN SEND A MESSAGE TO USER, AND FLAG AN ERROR
*
         if(txfile.ne.'YES') then
            write(*,*) ' *** NOT A TEXT FILE'
            ierr=1
         endif
*
* IF FILE COULD NOT BE OPENED SEND MESSAGE
*
      else
         write(*,*) ' *** UNABLE TO OPEN SPECIFIED FILE'
      endif
*
* IF AN ERROR OCCURED WHILE OPENING THE FILE, OR IF THE FILE IS NOT A
* TEXT FILE, THEN GET ANOTHER FILE SO LONG AS MAXIMUM NUMBER OF BAD
* FILE NAMES HAS NOT BEEN EXCEEDED
*
      if(ierr.ne.0) then
         nbad=nbad+1
         if(nbad.lt.3) then
            filnam=' '
            goto 10
         else
            write(*,*) ' *** TOO MANY BAD VALUES GIVEN'
            ierr=1
         endif
      endif
*
* FINISH
*
 999  continue

      end
