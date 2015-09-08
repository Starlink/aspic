      PROGRAM RINGCOL
*+
*     Program:- RINGCOL.
*
*
*                  +------------+
*                  |            |
*                  |  RINGCOL.  |
*                  |            |
*                  +------------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a continuous purple, blue, green, red, purple colour
*   table and save it as a Starlink image.
*
*
*  Usage:-
*   A continuous purple, blue, green, red, purple colour table
*   is generated and saved as Starlink image.
*     The advantage of this colour table is that it has the
*   same colour at either end and hence when it is used as
*   input to COLCYCLE the resultant colour table is everywhere
*   continuous, with no breaks.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   RINGCS.
*
*  A C Davenhall./ROE/                         27/10/82.
*-
      CALL RINGCS
      END
