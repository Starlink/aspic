      PROGRAM APERASP
*+
*     Program:- APERASP.
*
*                 +----------------------------------+
*                 |                                  |
*                 | APERASP: An Aperture Photometer. |
*                 |                                  |
*                 +----------------------------------+
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*   Usage:-
*
*   The ASPIC aperture photometer is a set of routines designed
*   for aperture photometry of images selected interactively by the
*   cursor. Before using the photometer the image frame to be
*   measured is displayed on an ARGS, for example by using ADISP.
*   The aperture photometer contains several functions,
*   which are obtained by the following commands:
*
*   SHAPE   Select the shape of the cursor. The available options
*           are square or ellipse (circular cursors are included
*           as a special case of an ellipse).
*
*   SIZE    Enter a new aperture size. In order to keep the
*           magnitudes on the same scale throughout a
*           measuring run the size should be kept constant
*           for all the stars measured.
*
*   STAR    Measure the signal from a star in the frame. The
*           cursor is positioned over the star to be
*           measured. If a square cursor is selected the trackerball
*           buttons can be used to increase or decrease the
*           size of the aperture. If an elliptical cursor
*           has been selected the trackerball buttons can
*           be used to change the ellipticity and orientation of
*           the cursor as well as its size.
*
*   SKY     This is used to measure the signal from a patch
*           of sky neighbouring the star measured. The
*           procedure and options are the same as for STAR,
*           however the aperture used need not be the same
*           size as that used in the STAR measurement.
*
*   MAG     This is used to derive a stellar magnitude from the
*           stellar and sky measures according to the formula;
*
*           mag = zero - 2.5log10(star-sky)
*
*           where zero is a zero point.
*
*   ZERO    Enter a value for the zero point from the
*           environment.
*
*   POSN    Specifies the starting position of the aperture
*           cursor in pixel coordinates.
*
*   GIVE    Displays all current aperture variables to the
*           environment for inspection.
*
*   WRITE   Writes the values obtained for the most recent star
*           measured to a VMS output file. This file is suitable
*           for printing as a permanent record of the session.
*           The first time that WRITE is used the user is prompted
*           for the name of the output file, the default being
*           APERASP.LIS. Next he is prompted to give a character
*           string identifying the star. Subsequent invocations
*           of WRITE will merely prompt for the identifier of the
*           star and append the details to the already existing
*           list.
*
*   E       Exit the aperture photometer and return to DSCL.
*
*
*   Output file.
*
*   Optionally an output file can be generated recording details
*   of the stars measured during a session using APERASP. This
*   file is a simple VMS formatted file suitable for printing.
*   The following information is recorded for each star;
*
*   1.  Running count of the image number in the output file.
*
*   2.  Identifier supplied by the user.
*
*   3.  Cursor X position marked for the star (pixels).
*
*   4.    "    Y    "       "     "   "   "   (  "   ).
*
*   5.  Summed intensity of star measurement.
*
*   6.  Area over which the star measurement was summed (pixels).
*
*   7.  Summed intensity of sky measurement.
*
*   8.  Area over which the sky measurement was summed (pixels).
*
*   9.  Zero point of magnitude scale (given by the user).
*
*   10. Computed magnitude.
*
*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*   Subroutines called :
*   APER
*
*   B. D. Kelly/ROE/ and J. A. Cooke/UOE/ 1981.
*   D.R.K.Brownrigg/ROE/29.1.1982
*   C.ASPIN/UOE/27.2.84
*   A C Davenhall/ROE/13.3.84.
*
*   User documentation; A C Davenhall./ROE/ 6/4/82.
*-
      CALL APER
      END
