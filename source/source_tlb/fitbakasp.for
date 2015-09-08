      PROGRAM FITBAKASP
*+
*     Program:- FITBAKASP.
*
*
*                   +--------------+
*                   |              |
*                   |  FITBAKASP.  |
*                   |              |
*                   +--------------+
*
*
*  Purpose:-
*      Fit a two dimensional polynomial to the sky background
*   surrounding a galaxy image and convert the image to
*   normalised intensity by dividing the origonal image by
*   the fitted background.
*
*
*  General Description:-
*      The normal practice of galaxy photometry requires the
*   conversion of a raster of intensity values into a grid
*   normalised relative to a sky level of 1.0. If the sky
*   level is constant over the raster this can be simply
*   achieved by measuring the mean sky value and dividing
*   this into the intensity converted image. A more
*   commonly occuring case, however, is that the sky is not
*   constant over the image, but shows small but significant
*   fluctuations. The normal procedure for handling this
*   case, following Jones et. al. (1967) , is to fit a
*   polynomial by least squares to the outer regions of the
*   background image, outside the galaxy, where the raster
*   is measuring the sky intensity. Whilst the present
*   program follows the precepts of Jones et. al. (1967)
*   their routines are not used, but rather a least squares fit
*   is made using a NAG routine (E02CAF).
*     When making the fit it is necessary to exclude regions
*   of the image containing objects. This is done by reading
*   a preprepared file containing the regions to be excluded.
*   Such a file can be generated using ABOXASP. The exclusion
*   zones should be sufficiently large to include the faint
*   outer regions of the galaxy. Foreground stars should
*   have been previously removed from the image.
*     Care should be taken that the fitted polynomial does
*   not "hump" beneath the galaxy; this can be caused by
*   fitting to the faint outer regions. Also the smallest
*   number of coefficients necessary to fit the data should
*   be used.
*     To permit evaluation of the fit contour plots of the
*   fitted polynomial and listings of the residuals are
*   automatically produced. In addition the fitted polynomial
*   is saved as a file as well as the normalised image, thus
*   permitting its detailed inspection. This also allows
*   unconventional uses of the routine, eg. the background can
*   be subtracted rather than divided.
*
*
*  Usage:-
*      An image file converted to intensity and with the foreground
*   stars removed should be prepared and displayed on an Args.
*   ABOXASP is then used to define a set of boxes enclosing the
*   galaxies in the image. It is most important to make the
*   boxes sufficiently large to include the faint outer regions
*   of the galaxies.
*     FITBAKASP can then be run. The user will first be prompted
*   for the image to be fitted and the file containing the
*   exclusion boxes. He will then be prompted for the filenames
*   for the polynomial background and normalised image that
*   are to be created.
*     The number of coefficients to be fit in the X and Y directions
*   are prompted for. These should be kept as small as is necessary
*   to give an adequate fit. The cell size to be used in averaging the
*   background is requested next. It is preferable, though not
*   necessary that this should be a factor of the X and Y extents.
*   Finally the title to appear on the listings and graphs is
*   prompted for.
*     Boxes are drawn on the Args corresponding to the cells
*   within which the median values of the background are
*   extracted in order to perform the fit. It will be noted
*   how these avoid the exclusion zones. The fit is then made,
*   a diagnostic listing is automatically sent to the
*   printer a file of Versatec plotting commands is created.
*     The Versatec output includes a contour map
*   of the fitted polynomial and strips extracted from the
*   polynomial, parallel to the X and Y axes, passing
*   through the centre of the image. The corresponding
*   original data are superposed on these later plots. The listing
*   contains the positions, observed background value,
*   fitted value, normalised intensity, residual and residual
*   expressed in magnitudes per square arcsec. In order to
*   give a value to points below the sky this is defined as;
*
*       residual(magnitudes) = 2.5*log10(modulus(residual))
*
*   If the absolute sky brightness of the frame is known these
*   residuals can be converted into absolute brightness using
*   the relation;
*
*          absolute brightness = sky - residual
*
*     After these plots have been produced the program
*   terminates. The hardcopies can be used in conjunction with the
*   evaluated polynomial image to adjudge the adequacy of the fit
*   and also provide a permanent record.
*
*
*  References:-
*   Jones W B, Obitts D L, Gallet R M & de Vaucouleurs G, 1967,
*      Pub. Dept. Astr. Univ. Texas, Austin, Ser.II,
*      Vol.1, No.8.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*
*  Given;
*   none.
*
*  Returned;
*   none.
*
*  Subroutines called;
*   E2D:-  FITBAK.
*
*  A C Davenhall./ROE/                              18/8/82.
*-
      CALL FITBAK
      END
