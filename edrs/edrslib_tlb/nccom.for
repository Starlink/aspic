*      module NC_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Holds the user state variables for colours available
*       when using NCAR, and defines the colours available within
*       NCAR.
*
*METHOD
*       Common block /NCCOM/ holds following variables:
*
*       NC_ir             integer   Red part of each colour
*       NC_ig             integer   Green part of each colour
*       NC_ib             integer   Blue part of each colour
*       NC_in             integer   Total intensity for each colour
*       NC_ind            integer   Colour index for each colour
*       NC_cnm            character Name for each colour
*
*       Parameters declared in this module:
*
*       NC_ncl            integer   Number of colours defined
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/3/88
*-------------------------------------------------------------------
*
* DECLARE VARIABLES IN COMMON BLOCK /NCCOM/
*
      integer   i               ! Implicit loop count
      integer   NC_ncl          ! Number of colours defined
      parameter (NC_ncl=8)

      integer   NC_ir(NC_ncl)   ! Red part of each colour
      integer   NC_ig(NC_ncl)   ! Green part of each colour
      integer   NC_ib(NC_ncl)   ! Blue part of each colour
      integer   NC_in(NC_ncl)   ! Total intensity for each colour
      integer   NC_ind(NC_ncl)  ! Colour index for each colour
      character NC_cnm(NC_ncl)*10! Name for each colour

*
* DECLARE COMMON BLOCK /NCCOM/ TO HOLD INFO ABOUT AVAILABLE COLOURS
*
      common /NCCOM/ NC_cnm,NC_ir,NC_ig,NC_ib,NC_in,NC_ind
*
* SET UP THE COLOUR DATA
*
      data (NC_cnm(i),NC_ir(i),NC_ig(i),NC_ib(i),NC_in(i),i=1,NC_ncl)/

     :   'WHITE'      ,  1,  1,  1,  10000,
     :   'RED'        ,  1,  0,  0,  10000,
     :   'GREEN'      ,  0,  1,  0,  10000,
     :   'BLUE'       ,  0,  0,  1,  10000,
     :   'CYAN'       ,  0,  1,  1,  10000,
     :   'MAGENTA'    ,  1,  0,  1,  10000,
     :   'YELLOW'     ,  1,  1,  0,  10000,
     :   'BLACK'      ,  0,  0,  0,  10000/
