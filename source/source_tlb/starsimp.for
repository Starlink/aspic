      PROGRAM STARSIMP
*+
*     Program:-  STARSIMP.
*
*     (part of the STARSIM suite).
*
*
*                      +------------+
*                      |            |
*                      |  STARSIMP. |
*                      |            |
*                      +------------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     Generate an image frame containing an artificial star field.
*
*
*  General Description:-
*     STARSIMP generates an image frame containing an artificial
*   star field, the constituent stars having known photometric
*   properties. STARSIMP obtains details of the simulation
*   required by reading values from a control file and also
*   requires two luminosity functions as input; one for the
*   general star field and the other for an optional globular
*   cluster in the field.
*     From the input files STARSIMP creates an image frame
*   containing the artificial star field, a "star catlogue"
*   containing the positions and magnitudes of all the stars
*   in the frame, an output listing giving details of the
*   generated frame and a file of plotting commands suitable for
*   the Versatec to generate a "finding chart" for the brighter
*   stars in the frame.
*     STARSIMP is part of the STARSIM suite and the online help
*   information and written documentation (SUN41)
*   for STARSIM should be consulted for further details of the
*   control parameter file and the output produced.
*
*
*  Usage:-
*     The program first prompts for the name of the control
*   parameter file and then the names of the field star
*   and globular cluster luminosity functions. It then prompts
*   for the names of the output files required; the simulated
*   image frame and the "star catalogue" file.
*     When all these files have been obtained the artificial
*   star field is generated automatically and the output produced.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   SIMSTR.
*
*  A C Davenhall./ROE/                                23/1/83.
*-
      CALL SIMSTR
      END
