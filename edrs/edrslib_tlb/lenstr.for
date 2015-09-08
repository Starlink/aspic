      function lenstr(string)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the length of a string excluding trailing spaces
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
      integer   lenstr
*
* DO IT
*
      lenstr=len(string)
      do while(ichar(string(lenstr:lenstr)).le.34.
     :                             and.lenstr.ge.1)
         lenstr=lenstr-1
      enddo
*
      end
