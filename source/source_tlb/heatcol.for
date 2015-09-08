      PROGRAM HEATCOL
*+
*     Program:- HEATCOL.
*
*
*                     +------------+
*                     |            |
*                     |  HEATCOL.  |
*                     |            |
*                     +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a stepped pseudo "heat sequence" colour table and
*   output it as a Starlink image.
*
*
*  Description:-
*   A stepped pseudo "heat sequence" colour table is generated and
*   output as a Starlink image of user selected name. The pseudo
*   heat sequence differes from a true heat sequence in that
*   violet has been substituted for colours cooler than red. The
*   steps are all of equal size. The following sequence of colours
*   are generated (in order of increasing intensity);
*
*                Black
*                violet/black.
*                violet
*                red/violet
*                red
*                red/orange
*                orange
*                orange/yellow
*                yellow
*                yellow/white
*                white
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  HEATCOLS.
*
*  A C Davenhall./ROE/                                    26/10/82.
*-
      CALL HEATCOLS
      END
