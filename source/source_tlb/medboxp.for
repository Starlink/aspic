      PROGRAM MEDBOXP
*+
*     Program:-  MEDBOXP.
*
*
*                         +------------+
*                         |            |
*                         |  MEDBOXP.  |
*                         |            |
*                         +------------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Compute the median of a patch of an array.
*
*
*  Usage:-
*   The user is prompted for the file name of the image from a
*   patch of which the median value is to be determined. If
*   BOXASP has been used previously to define a region of the
*   image on the Args the coords. it generated will be used,
*   otherwise the coords. will be prompted for.
*      The median is computed and wriiten out both to the environment
*   and the user.
*      If the total number of points in the patch is less than 25
*   the median will be computed exactly, otherwise it will be
*   estimated from a histogram of the patch.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   MEDIANS.
*
*  A C Davenhall./ROE/                                 28/10/82.
*-
      CALL MEDIANS
      END
