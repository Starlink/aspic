      PROGRAM PRIAXEASP
*+
*     Program:- PRIAXEASP.
*
*
*                    +-------------+
*                    |             |
*                    |  PRIAXEASP. |
*                    |             |
*                    +-------------+
*
*
*  Purpose:-
*      Extraction of the major and minor axes from a nebular image.
*
*
*  General Description:-
*      The purpose of PRIAXEASP is basically the interactive
*   extraction of the principle (major and minor) axes from
*   a nebular image. However it can also be used to extract
*   any desired axes.
*      PRIAXEASP differs from routines for extracting completely
*   arbitary axes, such as the ASPIC routine SECTOR in that
*   the set of axes extracted are constrained to pass through
*   the centre of the galaxy (or some other user defined
*   point, though usually the centre will be required) and
*   the set of axes extracted are forced to be mutually
*   perpendicular. Thus two possible sources of error are
*   elliminated.
*      In order to reduce the noise in the outer regions of the
*   extracted axes the extraction is performed along a "cake
*   slice", whose width is controlled by the user. The
*   extracted axes may be plotted and saved as files.
*      PRIAXEASP will accept an input frame where the
*   the brightness of the pixels are expressed in any
*   arbitary way. However the programs available for the
*   subsequent processing of the extracted axes assume that
*   the input frame had been converted to intensity relative
*   to a sky level of 1.0. The sky level of 1.0 should not
*   have been subtracted. This is consistent with the
*   normal practice of galaxy photometry.
*
*
*  Usage:-
*     Prior to running PRIAXEASP the image to be analysed should
*   be displayed on an Args, eg. by using ADISP. PRIAXEASP
*   is then run. Firstly the user will be prompted for the
*   image to be analysed and then a menu of commands appears.
*     The order in which the commands are carried out is
*   variable and if the user makes a mistake commands may
*   be repeated. Some commands require the prior successful
*   completion of other commands for their execution; if
*   such commands are attempted without having first
*   completed the precursors the command will not be
*   executed and a message will be printed.
*     A typical usage might be according to the following
*   pattern; the centre of the image is located and the
*   pixel size, in arcseconds, entered so that the profiles
*   are saved with their radii expressed in these units.
*   The cursor is then positioned interactively in the
*   required position over the image and the axes extracted.
*   Finally the axes are inspected and saved.
*     The following commands are available;
*
*  CENTRE (C) Automatically locate the centre of the galaxy
*             image. A box shaped cursor appears and is
*             placed over the centre of the galaxy and
*             adjusted to an appropriate size. The centre
*             is then found from intensity weighted moments
*             of all the points in the box.
*
*  MANCEN (M) Manually locate the centre of the galaxy image.
*             A cross-hair cursor appears and is positioned
*             over the required centre.
*
*  PIXSIZ (PI) The user is prompted for the pixel size, typically
*             in arcsec.
*
*  AXES (A)   Extract the required axes. A cursor corresponding
*             to the four principle axes appears, centred on the
*             previously located nucleus. The trackerball
*             buttons are used, following the instructions on
*             the bottom of the screen, to position the cursor
*             over the major and minor (or other) axes and
*             to adjust the length of these axes and the
*             required width of the "cake slice". When the
*             user is satisfied the axes are extracted.
*             Finally each of the axes will be labelled with
*             a number 1-4.
*
*  PLOT (P)   Plot a selected axis. The user is prompted for
*             the axis required, 1-4, corresponding to the
*             number displayed on the Args. A choice of
*             output device is also available.
*
*  SAVE (S)   Save a selected axis. The user is prompted for
*             the axis required, 1-4, corresponding to the
*             number displayed on the Args. He is then
*             prompted for the filename and title of the
*             saved profile. These should contain sufficient
*             information for subsequent identification of the
*             profile, eg. NGC 3333, Major Axis, NE of nucleus.
*
*  COLOUR (CO) Change the colour of the cursor.
*
*  HELP (H)   List the commands available.
*
*  EXIT (E)   Terminate the program.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*  Subroutine called;
*   PRIAXE.
*
*  A C Davenhall./ROE/                         31/7/82.
*
*  (Note: Code used to extract the axes is identical to that
*         used in ASPIC program SECTOR).
*-
      CALL PRIAXE
      END
