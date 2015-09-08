      PROGRAM EQPROFASP
*+
*     Program:- EQPROFASP.
*
*
*                       +-------------+
*                       |             |
*                       |  EQPROFASP  |
*                       |             |
*                       +-------------+
*
*
*  Purpose:-
*   To compute an equivalent profile and photometric parameters
*   in the system due to de Vaucouleurs from an image frame
*   containing an image of a bright galaxy.
*
*.................................................................
*
*
*  General Information:-
*     Following the normal practice in galaxy photometry this
*   program expects an input image frame that has been converted
*   to intensity and normalised relative to a sky level of 1.0.
*   The sky level should not be subtracted.
*     Normally the galaxy to be studied will be the only object in
*   the frame, though this is not strictly necessary. What is
*   necessary, however, it that the object should be isolated
*   and surrounded by a large patch of blank sky.
*
*
*..................................................................
*
*
*  Usage:-
*     Before starting to use the program the frame to be analysed
*   should be displayed on an Args, eg. by using ADISP. EQPROFASP
*   is then run. Firstly the user will be prompted for the name
*   of the file to be analysed. Next he will be prompted for
*   various parameters concerning the scan; the sample spacing
*   (micron) and the plate scale (Arcsec/mm) and for details
*   concerning the separation and number of points in the
*   equivalent profile that is to be produced. Finally the
*   absolute brightness (mag./sq.arcsec) of the sky for this
*   frame will be requested.
*     A menu of commands then appears, which will be described
*   below. Generally there is no specific order associated with
*   these commands, however some of them must be used sequentially.
*
*   If a command is attempted without completing the necessary
*   precursors the command will not be executed and a message will
*   be displayed. The following commands are available;
*
*  EXTRACT  (EX)   Extract the equivalent profile from the image
*                  frame. A region around the galaxy is defined
*                  either using the Args cursor or from the keyboard.
*                  The importance of making this box considerably
*                  larger than the visible galaxy image cannot
*                  be over emphasised. Once the box is defined
*                  the equivalent profile within it is calculated.
*
*  EXTRACTC (EC)   Extract the equivalent profile from the image
*                  frame using the technique for correcting for
*                  sky noise due to C. P. Blackman 1979 (in
*                  "Photometry, kinematecs and dynamics of galaxies"
*                  ed. D. S. Evans, p135, Dept. Astr. Univ. of
*                  Texas at Austin, Austin). The largest and smallest
*                  apertures within which the equivalent profile
*                  is to be computed are defined using the cursor.
*
*  INTABLE (I)     Compute an integration table and photometric
*                  parameters in the de Vaucouleurs system from the
*                  current equivalent profile. The user is prompted
*                  for a title to appear on subsequent output, the
*                  integration table and parameters are computed
*                  and a listing automatically sent to the lineprinter.
*                  Files of Versatec plotting commands representing
*                  the equivalent profile and relative integrated
*                  luminosity curve are automatically created,
*                  ready for subsequent submission to the plotter
*                  if the user so desires.
*
*  INFILE (INF)    Read in a previously prepared equivalent profile
*                  from a VMS file. This was originally a diagnostic
*                  aid, but may prove useful for analysing
*                  equivalent profiles produced elsewhere. The user
*                  is prompted for the filename. The file should have
*                  been prepared with each data point occupying a
*                  separate line. Each point is specified by a radius
*                  (arcsec) and a Log I value, in that order. There
*                  is not specific formatting within the lines. The
*                  points should be ordered in increasing radius
*                  (decreasing Log I).
*
*  CHANGE (C)      Change some or all of the scan parameters
*                  previously input.
*
*  SAVE (S)        Save the current equivalent profile as a Starlink
*                  image.
*
*  HELP (H)        List the commands available.
*
*  EXIT (E)        Terminate the program.
*
*
*..................................................................
*
*
*
*  W. D. Pence./Univ.of Sussex/                             Oct 80.
*  A C Davenhall./ROE/                                      23/7/82.
*  A C Davenhall./ROE/   {Modified}                         12/9/82.
*-
      CALL TOPEQ
      END
