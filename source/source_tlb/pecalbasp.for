      PROGRAM PECALBASP
*+
*
*     Program:-  PECALBASP.
*
*
*                +-------------+
*                |             |
*                |  PECALBASP  |
*                |             |
*                +-------------+
*
*  Purpose:-
*
*   Absolute calibration of a bright galaxy image that has
*   been converted into intensity relative to the sky.
*   The calibration results in the determination of the
*   sky brightness in magnitudes/sq.arcsec. The calibration
*   is carried out by using multiaperture photoelectric photometry
*   available for the galaxy.
*
*.................................................................
*
*
*  General information:-
*
*   Following the normal practice in galaxy photometry
*   the program expects an input image that has been converted
*   into intensity and normalised to a sky brightness of 1.0.
*   The sky brightness should not be subtracted.
*    The sky brightness is evaluated for each photoelectric
*   aperture by computing the luminosity of the galaxy
*   inside a circle of corresponding radius and comparing this
*   luminosity to the magnitude measured in the aperture.
*    Prior to using the program the photoelectric photometry
*   available for the galaxy should be edited into a VMS
*   file. Data for each aperture occupy a single line of the
*   file. The required parameters for each aperture are
*   the diameter (arcsec) and aperture magnitude (magnitude),
*   in that order. The file is free format. Dubious
*   photometry should be included in the file; the program
*   allows any discrepant values to be dropped, at the
*   users discretion.
*
*...........................................................
*
*
*  Usage:-
*
*  1) Prepare a VMS file of the available multiaperture
*     photoelectric photometry and a Starlink image containing
*     the frame to be calibrated, normalised relative to the
*     sky (see above). This frame should be plotted on the
*     Args, eg. by using ADISP.
*
*  2) Run PECALBASP. The user is first prompted for the
*     filename of the frame to be calibrated and then
*     other necessary parameters; the stepsize used
*     to digitize the frame (micron) and the platescale
*     of the plates from which it was digitized (Arcsec/mm).
*
*  3) Next a menu of commands appears. These are described below
*     in the order in which they will probably be used. The
*     order of some commands is interchangeable and any
*     of the commands can be repeated if a mistake is made
*     (eg. reading in the wrong file of P.E. photometry).
*     Some of the commands are necessary precursors to
*     subsequent steps. If a command is attempted without
*     having previously completed the necessary precursors
*     the command will not be executed and a message will
*     be printed. The commands available are:-
*
*  CENTRE (C)      Locate the nucleus of the galaxy. The position
*                  thus found is used as the centre for both the
*                  photographic and photoelectric apertures.
*                  The cursor is used to define a box around
*                  the approximate centre, and the actual
*                  centre found from intensity weighted moments.
*
*  INFILE (INF)    Read in the file of multiaperture photoelectric
*                  photometry. The user is prompted for the
*                  filename.
*
*  DISPLAY (D)     Display circles centred on the nucleus with radii
*                  corresponding to the photoelectric apertures.
*
*  INTEGRATE (I)   Compute the luminosity inside circles corresponding
*                  to each of the photoelectric apertures.
*
*  RESULTS (R)     Display and manipulate the results to determine
*                  the sky brightness. The user is placed at a
*                  sub node where various commands are available.
*                  These are detailed below.
*
*  HELP (H)        List the commands available.
*
*  EXIT (E)        Terminate the session.
*
*
*  Commands available at the sub node RESULTS for displaying
*  and manipulating the results.
*
*  In principle each of the photoelectric apertures will give a
*  independant value for the sky brightness and these could be
*  averaged to give a final value for the sky brightness.
*  In practice, however, the user will wish to discard some
*  of the apertures because they are in error; small apertures
*  often give too bright a sky background due to the
*  saturation of the photographic plate in the nucleus,
*  large apertures are often in error due to centring
*  difficulties and some measures may simply be discrepant.
*  Thus facilities are provided to inspect the values obtained
*  and for the user to reject aberrant points. In the
*  following list apertures are considered to be either
*  "active" or "deleted". "Active" apertures will be used in
*  the determination of the sky brightness, "deleted" apertures
*  will be excluded from this determination.
*   The following commands are available:-
*
*  PLOT (P)       Plot all the active apertures as sky brightness
*                 against log aperture diameter. If the frame
*                 has been converted into intensity properly
*                 the sky brightness should not vary systematically
*                 with diameter (excepting saturation effects
*                 for small apertures, see above) and only
*                 random scatter should remain.
*
*  PLOTALL (PA)   The same as "PLOT", but both active and deleted
*                 apertures are plotted. These are diferentiated
*                 by using a different plotting symbol.
*
*  DELETE (D)     Delete a specified aperture.
*
*  REINSTATE (R)  Reinstate a specified aperture.
*
*  LIST (L)       List the diameters and sky brightness for
*                 all the apertures on the terminal. Deleted
*                 apertures are indicated.
*
*  MEAN (M)       Compute the mean sky brightness from all the
*                 active apertures. This is the end result of
*                 the calibration.
*
*  PRINT (PR)     Send to the lineprinter a listing showing
*                 the diameter and sky brightness for all
*                 the apertures, indicating the deleted
*                 apertures and giving the mean and standard
*                 deviation computed from the active apertures.
*                 This provides a hard copy of the calibration.
*
*  HELP (H)       List the commands available at this sub node.
*
*  EXIT (E)       Leave this sub node.
*
*
*
*.................................................................
*
*
*  Given;
*   None.
*
*  Returned;
*   None.
*
*  Subroutines called;
*   PECALB.
*
*  W. D. Pence./Univ. of Sussex/                          Oct. 1980.
*  A C Davenhall./ROE/                                    22/7/82.
*-
      CALL PECALB
      END
