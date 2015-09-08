      PROGRAM CURVAL
*+
*     Program:- CURVAL.
*
*
*                      +-----------+
*                      |           |
*                      |  CURVAL.  |
*                      |           |
*                      +-----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*   Display the coordinates and value of a cursor selected
*   pixel within an image.
*
*
*  Usage:-
*   The image to be studied should be displayed on an Args,
*   eg. using ADISP. CURVAL is then run. The cursor should
*   be positioned over the required pixel. The coordinates
*   and value of this pixel are then written to both the
*   user and the environment.
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   CURVALS.
*
*  A C Davenhall./ROE/                              28/10/82.
*-
      CALL CURVALS
      END
