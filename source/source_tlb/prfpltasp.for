      PROGRAM PRFPLTASP
*+
*     Program:- PRFPLTASP.
*
*
*                   +--------------+
*                   |              |
*                   |  PRFPLTASP.  |
*                   |              |
*                   +--------------+
*
*
*  Purpose:-
*     Program to plot an extracted profile.
*
*
*  General Description:-
*     The purpose of PRFPLTASP is to plot a profile (major or
*   minor axis or equivalent profile) previously extracted
*   from a galaxy image and saved as a disk file. The
*   profile will be plotted precisely as it is held in the
*   file. A choice of output device and plotting format
*   are given.
*
*
*  Usage:-
*     The user is first prompted for the filename
*   of the profile to be plotted and then for the
*   title to appear on the plot.
*     An output device (T4010, Args or Versatec)
*   must be selected and then the output format
*   chosen. The profile may be plotted with the
*   data represented either as individual points,
*   joined by straight lines or as a "histogram".
*   If the data are plotted as points a choice of
*   plotting symbol is given.
*     The plot is then produced and the program
*   terminates. If the chosen output device is
*   the Versatec a file of plotting commands for
*   this device will be generated which must be
*   subsequently submitted to it.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   PRFPLT.
*
*  A C Davenhall./ROE/                       6/8/82.
*-
      CALL PRFPLT
      END
