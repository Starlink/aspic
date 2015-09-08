      PROGRAM MAGCNTASP
*+
*     Program:-  MAGCNTASP.
*
*
*                       +--------------+
*                       |              |
*                       |  MAGCNTASP.  |
*                       |              |
*                       +--------------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*      To produce absolutely calibrated contours, in magnitudes
*   per square arcsecond, of an image or parts of an image,
*   on the versatec.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*      Following normal practice the image to be contoured should
*   be held as intensity normalised to a sky of 1.0, the sky
*   not being subtracted. If it is required to contour only
*   part of the image frame rather than all of it, the frame
*   should be displayed on an Args (eg. by using ADISP) and
*   ABOXASP used to mark and create a file containing the
*   coords. of the regions to be contoured.
*      MAGCNTASP can then be run. The user will first be prompted
*   for the filename of the frame to be contoured. He will
*   then be asked whether the entire array or parts of it
*   are to be contoured. If parts of the array are to be
*   contoured the previously prepared file holding the
*   coords. of the regions to be contoured will be prompted
*   for.
*      The user will next be prompted for the absolute sky brightness
*   for the frame, in mag./sq.arcsec. (previously determined,
*   perhaps using PECALBASP). He is then prompted for
*   the faintest isophote to be contoured and the increment
*   between isophotes, both in mag./sq.arcsec.
*      A file of plotting commands for the versatec will be
*   generated, which typically takes several minutes. Finally
*   the program terminates.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   E2D:-   MAGCNT.
*
*
*  A C Davenhall./ROE/                               16/9/82.
*-
      CALL MAGCNT
      END
