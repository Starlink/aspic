      PROGRAM E2DCOL
*+
*     Program:- E2DCOL.
*
*
*                     +-----------+
*                     |           |
*                     |  E2DCOL.  |
*                     |           |
*                     +-----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a Starlink image containing the default E2D colour table.
*
*
*  Usage:-
*   A default E2D colour table is generted and output as a Starlink image.
*   It can be loaded into an Args using the routine LUTREAD.
*     The E2D colour table is a stepped colour table with equal
*   increments. It comprises the folowing coour sequence
*   (ordered in increasing intensity);
*
*           Blue
*           red
*           orange
*           yellow
*           green
*           blue
*           violet
*           purple
*           brown
*           purple
*           turquoise
*           white
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  E2DCOLS.
*
*  A C Davenhall./ROE/                                    25/10/82.
*-
      CALL E2DCOLS
      END
