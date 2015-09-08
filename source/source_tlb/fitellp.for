      PROGRAM FITELLP
*+
*     Program:- FITELLP.
*
*
*                   +------------+
*                   |            |
*                   |  FITELLP.  |
*                   |            |
*                   +------------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     To take a file containing a set of extracted contours and
*   find the best fitting ellipse to each contour. The parmeters
*   of the fitted ellipses are saved as a Starlink image.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*     The file of extracted contours should first have been
*   prepared using CNTEXTP. FITELLP is then run. The user
*   will firstly be prompted for the files containing
*   the extracted contours and also the file containing the
*   Log I values corresponding to each contour. Next he will
*   be prompted for the file to hold the parameters found for
*   each contour.
*     The contours are processed seqentially. As each one is
*   fit a message will be displayed. This will indicate either
*   successful or unsuccessful completion of the fit. If the
*   fit is unsiccessful the initial guesses for the parameters
*   will be saved and the results for that contour should
*   be regarded as incorrect. When all the contours have been
*   processed the program terminates.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-  FITELLS.
*
*  A C Davenhall./ROE/                                    23/9/82.
*-
      CALL FITELLS
      END
