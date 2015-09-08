      function strlen(string)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the length of a string excluding trailing spaces
*
*SOURCE
*       STRLEN.FOR in UTILITIES.TLB
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
      integer   strlen

*
* DECLARE LOCAL VARIABLES
*
      integer  ic

*
* DO IT
*
      write(*,*) ' '
      write(*,'(A)') '$STRLEN: ',string
      read(*,*) ic

      strlen=len(string)

      ic=ichar(string(strlen:strlen))

      do while(ic.le.32.and.strlen.ge.1)
         write(*,*) 'STRLEN,IC: ',strlen,ic
         strlen=strlen-1
         ic=ichar(string(strlen:strlen))
      enddo
*
      end
