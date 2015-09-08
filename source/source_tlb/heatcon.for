      PROGRAM HEATCON
*+
*     Program:- HEATCON.
*
*
*                     +------------+
*                     |            |
*                     |  HEATCON.  |
*                     |            |
*                     +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a continuous pseudo "heat sequence" colour table and
*   output it as a Starlink image.
*
*
*  Description:-
*   A continuous pseudo "heat sequence" colour table is generated and
*   output as a Starlink image of user selected name. The pseudo
*   heat sequence differs from a true heat sequence in that
*   violet has been substituted for colours cooler than red.
*   The following sequence of colours are generated (in order
*   of increasing intensity).
*
*                Black
*                violet
*                red
*                orange
*                yellow
*                yellow/white
*                white
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  HEATCONS.
*
*  A C Davenhall./ROE/                                    26/10/82.
*-
      CALL HEATCONS
      END
