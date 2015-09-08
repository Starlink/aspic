      character*(*) function filenm(filesp)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Finds the full VMS file specification from a specification
*       which may include wild cards. Succesive calls to filenm
*       return the next file in alphabetical order, until the non
*       remain. A blank string is then returned.
*
*SOURCE
*       FILENM.FOR in UTILITIES.TLB
*
*METHOD
*       Calls run time library routine LIB$FIND_FILE
*
*ARGUMENTS
*   INPUTS:
*       filesp  character*(*)   A file specification which may include
*                               wild cards
*   OUTPUTS:
*       filenm (function value) The full file specification for the next
*                               file satisfying the argument filesp
*
*USED BY
*       gthelp (i.e. all LIRAS progs)
*
*SUBROUTINES CALLED
*       RTL:
*               LIB$FIND_FILE
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       uses run time library routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/2/87
*-------------------------------------------------------------------
*
      implicit none
      include '($RMSDEF)'
*
* DECLARE ARGUMENTS
*
      character filesp*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer   contxt          ! Contains info used by LIB$FIND_FILE
                                ! for maintaining a pointer to files
      integer   istat           ! Status return from LIB$FIND_FILE
      integer   lib$find_file   ! RTL function
*
* THE CONTEXT IS STORED BETWEEN CALLS TO LIB$FIND_FILE
*
      data contxt /0/
      save contxt
*
* CALL RTL ROUTINE TO GET THE NEXT FILE NAME CONSISTENT WITH THE GIVEN
* FILE SPECIFICATION
*
      istat=lib$find_file(filesp,filenm,contxt)
*
* IF NONE WAS FOUND SET FILENM BLANK
*
      if(istat.ne.RMS$_NORMAL) filenm=' '
*
* FINISH
*
      end

