      PROGRAM MULCON
*+
*     Program:- MULCON.
*
*
*                       +----------+
*                       |          |
*                       |  MULCON. |
*                       |          |
*                       +----------+
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     To produce a contour map of an image frame, or a series of
*   contour maps of selected regions in an image frame. Output
*   files are generated suitable for sending to the Versatec.
*
*
*  Usage:-
*     MULCON may be used to either produce a single contour map of
*   an entire image frame or a series of maps of selected regions
*   in the frame. If the latter option is selected a file holding
*   the coordinates of the required regions must be prepared
*   prior to running MULCON. This file is prepared by displaying
*   the frame on an Args (eg. by using ADISP) and using ABOXASP
*   to mark the required regions and save their coordinates as
*   a file.
*     Once a file of coordinates has been prepared, if required,
*   MULCON can be run. First the user is prompted for the name
*   of the image frame to be contoured. Next he will be asked
*   if the entire image or a set of regions are to be contoured.
*   If a set of regions are selected he will be prompted for the
*   name of the file holding their coordinates. Finally the user
*   is prompted for the value of the base contour (threshold)
*   and the increment between contours. These values should be
*   given in the same units as the image array.
*     A file of plotting commands for the Versatec will now be
*   generated, which typically takes several minutes. The program
*   then terminates. The file of plotting commands may now be
*   sent to the versatec following the usual procedure at the
*   user's node (eg. VPLOT at ROE, the user should see his
*   site manager for local variants).
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   ROEASP:-    MULCONS.
*
*  A C Davenhall./ROE/                                       1/2/84.
*-
      CALL MULCONS
      END
