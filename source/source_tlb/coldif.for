      PROGRAM COLDIF
*+
*     Program:-  COLDIF.     {Colour difference}.
*
*
*                      +----------+
*                      |          |
*                      |  COLDIF. |
*                      |          |
*                      +----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*   Generate a colour difference profile from two profiles extracted
*   along the same axis of a nebular image in different colours.
*
*
*  General Description:-
*     COLDIF takes the files representing the brightness distribution
*   in two different colours along a selected axis in a nebular
*   image and from this generates a colour difference profile.
*   The profile generated is computed in the sense;
*
*               Colour 1 - Colour 2.
*
*   ie. if a (B-V) profile were to be computed from B and V profiles
*   the B profile should be entered first. COLDIF works on profiles
*   of the type produced by PRIAXEASP or EQPROFASP, the intensities
*   may be expressed as either sky normalised intensity or
*   log10(intensity above sky). The program will prompt the user
*   for the way in which the intensities are held and both files
*   must be of the same type. The user will also be prompted
*   for the absolute sky brightness, in mag./sq.arcsec. in both
*   colours.
*     The colour difference profile generated is held in mag./sq.arcsec.
*   The two input profiles may be of unequal length and spacing.
*   The output profile will be as long as the shorter of the two
*   input profiles.
*     The file generated may be plotted using PRFPLTASP and printed
*   using PRFPRTASP (note; when the latter program is used to print
*   a file the user should reply 'No' to the question "Is the profile
*   to be output in mag./sq.arcsec." because it is already held
*   in these units.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutines called;
*   E2D:-   COLDF1.
*
*  A C Davenhall./ROE/                                  3/4/83.
*-
      CALL COLDF1
      END
