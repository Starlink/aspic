      PROGRAM CNTEXTP
*+
*     Program:- CNTEXTP.
*
*
*
*                    +------------+
*                    |            |
*                    |  CNTEXTP.  |
*                    |            |
*                    +-----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*      Generate a set of contours from a predifined region of an
*   image, suitable for subsequent ellipse fitting.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*      following the normal practice in galaxy photometry
*   the image to be contoured should be prepared in
*   intensity normalised to a sky level of 1.0, the sky not
*   being subtracted. If it is required to define the
*   region to be contoured using the cursor this should
*   be done by plotting it on the Args (eg. by ADISP) and
*   then running BOXASP.
*      CNTEXTP may then be run. The user will fIrst be prompted
*   for the name of the image to be analysed, the number
*   of contours to be extracted and the name of the files to
*   hold the extracted contours and their Log I values.
*   If the region to be contoured has not been predefined it
*   will now be prompted for.
*      Finally the user will be prompted for the Log I value
*   of the faintest contour and the increment, in Log I,
*   between contours. The contours will then be extracted and
*   the program terminates.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   CNTEXTS.
*
*  A C Davenhall./ROE/                               14/9/82.
*-
      CALL CNTEXTS
      END
