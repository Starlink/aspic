**********************************************************************
      PROGRAM DFFTASP
*+
*   PROGRAM DFFTASP
*
*   Dummy main program to use DFFT (Double Fast Fourier Transform)
*   subprogram on an image, with forward or inverse transform,
*   and option of progress report.
*   Program works on square images. If image is not square, it
*   is padded out, the user being given the median, mean and
*   mode of image values, and specifying a pad value.
*   The resulting image is IXEXT by IYEXT, IXEXT = IYEXT
*   The program is designed for the transform of a real image, of
*   which the transform is Hermitian, and so only half of the modes
*   need be retained for a complete description of the image.
*   The program proceeds as follows, for a forward transform.
*   (1) The row transform is carried out, positions 1 to IXEXT/2 + 1
*       holding the cosine harmonics and positions IXEXT/2 + 2 to
*       IEXT holding the IXEXT/2 down to 1 sine harmonics
*   (2) The partially transformed image is then rotated pi/2
*       clockwise to convert the columns to rows for efficiency of
*       access by the next stage of analysis
*   (3) Process (1) is repeated for the columns (in row layout)
*   (4) Small number harmonics, usually of most interest, are
*       shifted to the centre as follows.
*       (a) For rows (actually columns) 1 to IYEXT/2 + 1, row number
*           is increased by (IYEXT/2 - 1)
*       (b) For rows (columns) IYEXT/2 + 2 to IYEXT, row number is
*           decreased by (IYEXT/2 + 1)
*       (c) For columns (actually rows) 1 to IXEXT/2 + 1, column
*           number is increased by (IXEXT/2 - 1)
*       (d) For columns (rows) IXEXT/2 + 2 to IXEXT, column number
*           is decreased by (IXEXT/2 + 1)
*       The combination of rotation and shifting leaves (e.g.)
*       cosine,cosine harmonics in the bottom, right quadrant and
*       places the (0,0) harmonic at position (IXEXT/2,IYEXT/2 + 1)
*       in the array.
*   The back transform reverses this procedure, stages (4) down to
*   (1), so that forward followed by back transform gives the
*   original image, excepting the effects of cumulative roundoff.
*
*   Subroutines called :
*   DFFT               : E2DLIB
*
*   D.R.K.Brownrigg/ROE/18.Mar.1982
*-
      CALL DFFT
      END
**********************************************************************
