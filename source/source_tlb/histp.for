      PROGRAM HISTP
*+
*
*     Program:-  HISTP.
*
*
*                      +----------+
*                      |          |
*                      |  HISTP.  |
*                      |          |
*                      +----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*      Produce a histogram from a section of a two-dimensional
*   image and determine some of the properties of the histogram.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Usage:-
*      This routine produces a histogram of a selected region of an
*   image and determines some of the properties of the histogram.
*   It includes an option for plotting the histogram and if this
*   option is to be used the program should be run on a T4010
*   terminal, with a cross hair cursor (or a terminal with a suitable
*   emulation mode).
*     The region of the image to be histogramed may be defined
*   either using the cursor or input from the keyboard. If the
*   cursor is to be used the image should be displayed on an Args
*   (eg. by using ADISP) and the required region defined using
*   BOXASP. HIST is then run. If the region is to be defined
*   from the keyboard HIST should be run without any preliminaries.
*     In either case the image to be histogramed is then prompted
*   for. If BOXASP has been used the coordinates of the region
*   will be picked up automatically, otherwise they will be prompted
*   for. In the latter case the default is the entire image.
*     A histogram of the selected region is then computed and
*   various properties of the histogram displayed. These include;
*   the maximum and minimum values in the histogram, the sum of
*   all the values and the mean, median and modal values.
*     The user is then placed at a small sub node where several
*   commands for plotting the histogram are available. These
*   commands are;
*
*    P      Plot the histogram.
*
*    C      Plot an expanded region of the histogram, the region
*           being defined using the cursor.
*
*    S      Plot an expanded region of the histogram, the region
*           being defined from the keyboard.
*
*    E      Leave the sub node and terminate HIST.
*
*
*   It should be noted that when an expanded region of the histogram
*   is chosen for display the existing histogram is not simply
*   replotted to a larger sxale, but rather a new histogram generated
*   stretching over the selected range. This has the advantage that
*   if resolution has been lost due to the finite number of bins
*   in the histogram it can be recovered by plotting smaller
*   sections of the histogram.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   HISTS.
*
*  B. D. Kelly./ROE/                                     1981.
*  A C Davenhall./ROE/                                   15/12/82.
*-
      CALL HISTS
      END
