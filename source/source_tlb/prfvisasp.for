      PROGRAM PRFVISASP
*+
*     Program:- PRFVISASP.
*
*
*                            +--------------+
*                            |              |
*                            |  PRFVISASP.  |
*                            |              |
*                            +--------------+
*
*
*  Purpose:-
*     Visually compare a profile extracted from a galaxy image with
*   a calculated profile of arbitary parameters.
*
*
*  General Description:-
*     PRFVISASP is used to visually compare a profile that has been
*   extracted from a galaxy image (either a major or minor axis
*   profile or an equivalent profile) with a "model" computed
*   from parameters input by the user. The light distribution in the
*   "model" is calculated as the sum of a de Vaucouleurs r**1/4
*   law, representing light from the bulge and a disk exponential
*   law, representing light from the disk. Values for the
*   parameters in these laws are obtained from the user. The scale
*   brightnesses are requested in mag./sq.arcsec and converted
*   into intensity above the sky by using the sky brightness.
*     The profiles may be compared by plotting them both
*   superposed or by plotting their residuals. The residuals
*   are defined in the sense log I(observed) - log I(calculated).
*
*
*  Usage:-
*     The user is prompted for the filename of the profile to be
*   inspected and the title to appear on the graphs plotted.
*   A menu of commands then appears. The following commands are
*   available;
*
*  DEVICE (D) Select a graphics device for output. The options
*             are T4010, Args or Versatec.
*
*  COMPARE (C) Compare the observed profile with a "model". The
*             user will be prompted for the sky brightness
*             and the parameters for the disk and bulge
*             components of the model. The two profiles will
*             then be plotted superposed, so that the adequacy
*             of the calculated profile to reproduce the
*             observed one may be adjudged.
*
*  RESID (R)  The residuals between the observed and calculated
*             profiles are plotted.
*
*  HELP (H)   List the commands available.
*
*  EXIT (E)   Terminate the program.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   PROFVIS.
*
*  A C Davenhall./ROE/                                    3/8/82.
*-
      CALL PROFVIS
      END
