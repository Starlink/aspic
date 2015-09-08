      PROGRAM SIMPAR
*+
*     Program:-  SIMPAR.
*
*     (Part of the STARSIM suite).
*
*
*                     +----------+
*                     |          |
*                     |  SIMPAR. |
*                     |          |
*                     +----------+
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Purpose:-
*     Generate a control parameter file for the starfield simulation
*   program STARSIMP.
*
*
*  General Description:-
*     The star field simulation program STARSIMP is driven by a
*   control parameter file constructed as a Starlink bulk data
*   frame. The purpose of SIMPAR is to contruct such a frame by
*   prompting the user for the required values and options.
*
*
*  Usage:-
*     When SIMPAR is run the following parameters are prompted
*   for;
*
*  1)  Select a Standard or Random image. Standard produces the
*      same frame each time, Random a different frame.
*
*  2)  X size of the image frame (pixels).
*
*  3)  Y size of the image frame (pixels).
*
*  4)  Plate scale (arcsec/mm). The default corresponds to the UK
*      Schmidt.
*
*  5)  Pixel size (micron).
*
*  6)  Diameter of the telescope aperture (cm).
*
*  7)  Exposure time (sec).
*
*  8)  Background sky brightness (mag./square arcsec).
*
*  9)  Star density/galactic latitude factor. A value of 1.0
*      corresponds to the star density at the Galactic poles,
*      larger values to lower Galactic latitudes. See LUN13
*      for further details.
*
*  10) Moffat's quantity beta.
*
*  11) Moffat's quantity R (arcsec).
*
*  12) Total absorption in the terrestrial atmosphere (magnitudes).
*
*  13) Fraction of the incident light lost in the telescope and
*      detector.
*
*  14) Threshold above which stars are to be plotted on the "finding
*      chart" automatically produced on the Versatec for the frame
*      (magnitudes).
*
*  15) Select whether a globular cluster is to be included in the
*      field. If so;
*
*  16)    No. of stars in the cluster.
*
*  17)    Distance to the cluster (pc).
*
*  18)    Total interstellar absorption along the line of sight to the
*         cluster.
*
*  19)    Core radius in King's formula (pc).
*
*  20)    Tidal radius in King's formula (pc).
*
*     Finally the name of the bulk data frame to hold the control
*   file is prompted for. The values input are copied to the
*   control file and the propgram terminates.
*
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Subroutine called;
*   SIMPARS.
*
*  A C Davenhall./ROE/                                    23/1/83.
*-
      CALL SIMPARS
      END
