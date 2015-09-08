      function ustrln(string)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the length of a string excluding trailing spaces
*
*SOURCE
*       USTRLN.FOR in UTILITIES.TLB
*
*METHOD
*       This routine is identical to STRLEN (also in UTILITIES). It is 
*       present in order to get round the problem of the name clash 
*       between STRLEN and the STRLEN routine in the C run-time-library.
*
*ARGUMENTS
*   INPUT
*       string          character       The input string
*   OUTPUT
*       (function)      integer         The length of the input string
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 2/9/87
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      character string*(*)
      integer   ustrln

*
* DECLARE LOCAL VARIABLES
*
      integer  ic

*
* DO IT
*
      ustrln=len(string)

      ic=ichar(string(ustrln:ustrln))

      do while(ic.le.32.and.ustrln.ge.1)
         ustrln=ustrln-1
         ic=ichar(string(ustrln:ustrln))
      enddo
*
      end
