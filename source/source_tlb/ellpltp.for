      PROGRAM ELLPLTP
*+
*     Program:-  ELLPLTP.
*
*
*                       +------------+
*                       |            |
*                       |  ELLPLPT.  |
*                       |            |
*                       +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     To plot a set of contours extracted from a nebular image
*   at a range of isophotal levels with ellipses fit to
*   them superimposed.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     Files holding the extracted contours and ellipses fit to them
*   should have been generated using CNTEXTP and FITELP respectively.
*   When ELLPLTP is run the user will be prompted for these files.
*   He will then be prompted for the graphics device required. The
*   plots are then produced and the program terminates.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-    ELLPLTS.
*
*  A C Davenhall./ROE/                                     24/9/82.
*-
      CALL ELLPLTS
      END
