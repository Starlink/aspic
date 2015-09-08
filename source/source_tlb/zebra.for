      PROGRAM ZEBRA
*+
*     Program:- ZEBRA.
*
*
*                    +----------+
*                    |          |
*                    |  ZEBRA.  |
*                    |          |
*                    +----------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a "pseudo contour" colour table and output it as a
*   Starlink image.
*
*
*  Description:-
*   A colour table that is basically black with a set of white
*   stripes is generated, thus giving a set of pseudo contours.
*      The colour table is saved as a Starlink image and can
*   be input to an Args using LUTREAD.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  ZEBRAS.
*
*  A C Davenhall./ROE/                                   26/10/82.
*-
      CALL ZEBRAS
      END
