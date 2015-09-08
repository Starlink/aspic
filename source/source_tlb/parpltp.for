      PROGRAM PARPLTP
*+
*     Program:-  PARPLTP.
*
*
*                        +------------+
*                        |            |
*                        |  PARPLTP.  |
*                        |            |
*                        +------------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     From a file holding parameters representing ellipses fit
*   to contours extracted from a nebular image over a range of
*   intensity levels extract any two parameters for the ellipses
*   and plot one against the other.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     The file of parameters should have been previously prepared
*   using FITELPP. When PARPLTP is run the user is prompted for the
*   file required and then the graphics device which he wishes
*   the plots to be produced on. Finally he has to choose which
*   parameter to plot as the X axis and which as the Y axis. The
*   required plot is then produced and the program terminates.
*     The meaning of the names given to the various parameters
*   should be self explantory. However in case of difficulty
*   the following list should be consulted;
*
*     LOGI        -   Log (intensity above sky) of the contour fitted
*                     to.
*
*     XCENTRE     -   X coord. of the centre of the ellipse (pixels).
*
*     YCENTRE     -   Y   "  . "   "    "    "   "     "    (  "   ).
*
*     MAJAXIS     -   Semi major axis (pixels).
*
*     ELLIPT      -   Ellipticity.
*
*     ORIENT      -   Orientation (degrees).
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  PARPLTS.
*
*  A C Davenhall./ROE/                                       25/9/82.
*-
      CALL PARPLTS
      END
