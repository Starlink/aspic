      PROGRAM MEANP
*+
*     Program:-  MEANP.
*
*
*                         +----------+
*                         |          |
*                         |  MEANP.  |
*                         |          |
*                         +----------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Compute the mean and its standard error from a patch of
*   an array.
*
*
*  Usage:-
*   The user is prompted for the file name of the image from a
*   patch of which the mean value is to be determined. If
*   BOXASP has been used previously to define a region of the
*   image on the Args the coords. it generated will be used,
*   otherwise the coords. will be prompted for.
*      The mean and its standard error are computed and written
*   out both to the environment and the user.
*      It should be noted that it is the standard error of the mean,
*   not the standard deviation which is computed, ie;
*
*                                  ______________
*                                 /  (x-mean)**2  \
*             standard error =   /  ------------
*                              \/      n.(n-1)
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   MEANS.
*
*  A C Davenhall./ROE/                                 28/10/82.
*-
      CALL MEANS
      END
