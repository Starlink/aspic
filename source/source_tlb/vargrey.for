      PROGRAM VARGREY
*+
*     Program:-  VARGREY.
*
*
*                       +------------+
*                       |            |
*                       |  VARGREY.  |
*                       |            |
*                       +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*  Purpose:-
*   Generate a grey colour table with user defined end points and
*   save it as a Starlink image.
*
*
*  Usage:-
*   A grey colour table with user defined end points is generated
*   and save as a Starlink image. This image can be input to an
*   Args using LUTREAD.
*      The user is prompted for the name of the file to hold the
*   colour table generated and the intensity levels that are to
*   correspond to black and white. Intensities below the black
*   level are all set to black and those above the white level
*   are all set to white. In between the two extremities a
*   grey scale is continuously linearly interpolated.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   E2D:-   VRGREYS.
*
*  A C Davenhall./ROE/                                        25/10/82.
*-
      CALL VRGREYS
      END
