      PROGRAM ELLPRTP
*+
*     Program:- ELLPRTP.
*
*
*                         +------------+
*                         |            |
*                         |  ELLPRTP.  |
*                         |            |
*                         +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     Produce a lineprinter listing of a file containing the
*   parameters for ellipses fitted to a set of isophotal levels
*   in a galaxy image by FITELLP.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     FITELLP should have been used to generate a file containing
*   the parameters of a set of ellipses fit to a range of isophotal
*   levels in an elliptical galaxy image. When ELLPRTP is run the
*   user is prompted for the name of this file and for the title
*   to appear on the output listing. The listing will then be
*   automatically produced on the lineprinter and the program
*   terminates.
*     The following parameters are listed for each ellipse fit:-
*
*        Log (Intensity above sky) of contour fitted to.
*
*        X Coord. of the centre of the ellipse (pixels).
*
*        Y   "  . "   "    "    "   "    "     (  "   ).
*
*        Semi-major axis (pixels).
*
*        Ellipticity.
*
*        Orientation (degrees).
*
*     The ellipticity, e, is defined as
*
*        e = 1.0 - (b/a)
*
*   where b is the semi-minor axis and a the semi-major axis. It
*   should not be confused with the eccentricity. The orientation
*   is measured anti-clockwise above the right pointing horizontal
*   in the original digitised image.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-    ELLPRTS.
*
*  A C Davenhall./ROE/                                      24/9/82.
*-
      CALL ELLPRTS
      END
