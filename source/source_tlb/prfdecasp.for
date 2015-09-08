      PROGRAM PRFDECASP
*+
*     Program:- PRFDECASP.
*
*
*                       +--------------+
*                       |              |
*                       |  PRFDECASP.  |
*                       |              |
*                       +--------------+
*
*
*  Purpose:-
*     Determine the parameters of the bulge and disk components
*   in the observed profile of a disk galaxy.
*
*
*  General Description:-
*     PRFDECASP is intended to determine from a profile extracted
*   from a galaxy image the parameters (scale lengths and scale
*   brightnesses) characterising its disk and bulge components.
*   It is intended for use on "classical" late type systems
*   comprising only of a bulge and a disk component. The light
*   distribution in the bulge is represented by a de Vaucouleurs
*   r**1/4 law and the disk by a disk exponential law. The
*   parameters of the distribution are solved for by making
*   2 types of non-linear least squares fits to the profile.
*   The procedure closely follows that of Kormendy (1977) and
*   in his notation the two types of fit used are iterative
*   fitting and simultaneous least squares.
*     Though the operation of PRFDECASP is reasonably automatic
*   (see below) there are many possible pitfalls both in its
*   usage and interpreting its results. Anyone contemplating
*   seriously using the program should at least read Kormendy's
*   paper and probably also contact me. In particular care
*   must be taken when trying to use the program on any profile
*   exhibiting a "third component".
*
*
*  Usage:-
*      Prior to running PRFDECASP the user needs to determine the
*   regions of the profile over which the fit is to be made.
*   This is best done by inspecting a plot of the profile,
*   such as that produced by PRFPLTASP.
*      The iterative fitting technique requires the selection of
*   two regions of the profile, both being defined by their
*   minimum and maximum radius. The first is a region of the
*   profile, close to the nucleus, where the bulge light
*   dominates, but outside the seeing disk and
*   the second is a region further out where disk light is
*   dominant. The transition region where light from both
*   components is of similar importance should be avoided.
*   The simultaneous least squares fitting requires a single
*   region spanning the entire portion of the profile to
*   be fitted (again the seeing degraded nucleus should be
*   excluded).
*     Once these regions have been selected the program can be
*   run. The user will first be prompted for the filename of
*   the profile to be analysed. This profile should be held
*   as Log I above the sky. A series of parameters will then
*   be prompted for. The first is a parameter for the non-linear
*   least squares routine. Under all normal circumstances the
*   default supplied should be adopted. The sky brightness
*   (mag./sq.arcsec.) appropriate to the profile will then be
*   requested. Following this the bulge and disk fitting ranges
*   are entered. Finally a title to appear on the printout is
*   input.
*     The fit to the profile is then made and messages will be
*   displayed indicating whether the iterative fitting and
*   simultaneous least squares methods were successful or not.
*   The results of the decomposition are written to a file
*   called DECOMP.LIS which is suitable for printing. These
*   results consist of the values for the bulge and disk
*   parameters as they iterate towards convergenve and the
*   final fits generated with their residuals from the observed
*   profile.
*     Plot files suitable for submission to the Versatec are
*   produced for the final fits produced by both techniques. On
*   these plots the observed profile is shown as either a set of
*   points or a "histogram", depending on the number of points it
*   contains. The individual bulge and disk components are shown
*   as dotted lines and the composite, fitted profile is shown as
*   a dashed line.
*
*
*  Reference:-
*   Kormendy J., 1977, ApJ, Vol.217, pp406-419.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   PRFDEC.
*
*  A C Davenhall./ROE/                                    5/8/82.
*  A C Davenhall./ROE/ {Modified}                        29/7/83.
*-
      CALL PRFDEC
      END
