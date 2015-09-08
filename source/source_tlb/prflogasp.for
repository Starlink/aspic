      PROGRAM PRFLOGASP
*+
*     Program:- PRFLOGASP.
*
*
*                        +--------------+
*                        |              |
*                        |  PRFLOGASP.  |
*                        |              |
*                        +--------------+
*
*
*  Purpose:-
*     To convert a profile held as intensity normalised to a sky level
*   of 1.0, with the sky not subtracted, into log intensity above
*   the sky.
*
*
*  General Description:-
*     The ASPIC galaxy photometry routines produce a galaxy image
*   array held as intensity normalised to a sky of 1.0, with
*   the sky not subtracted. Further, the axis extraction routines,
*   eg. PRIAXEASP (but not the equivalent profile routines) give
*   profiles in the same format as the image from which they
*   were extracted. However the profile analysis
*   routines (PRFVISASP, PRFDECASP) require the profile to be
*   input as Log I above the sky. PRFLOGASP takes a profile
*   held as intensity relative to the sky and generates a new
*   one held as log intensity above the sky. Thus the following
*   quantity is computed for each point in the input profile;
*
*                      Log(intensity - 1.0E0)
*
*   For points close to or below the sky this quantity is not
*   defined. The values for such points are replaced by the mean
*   value of the nearest interior and exterior points that are
*   above the sky.
*     It should be noted that this procedure will bias any least
*   squares type fit made to the outer regions of the profile
*   where sky noise is a significant effect (The alternative
*   procedure of excluding points close to or below the sky
*   from the new profile would produce a similar bias). The
*   bias is in the sense of making the profile appear brighter
*   than it actually is.
*
*
*  Usage:-
*     The user is first prompted for the filename of the input profile
*   held as relative intensity. Next he is prompted for the
*   filename to be given to the new profile held as log intensity
*   above the sky. The new profile is then computed and the
*   program terminates.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   PRFLOG.
*
*  A C Davenhall./ROE/                                   7/8/82.
*-
      CALL PRFLOG
      END
