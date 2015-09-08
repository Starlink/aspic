      PROGRAM JONESASP
*+
*     Program:- JONESASP.
*
*
*                       +--------------+
*                       |              |
*                       |   JONESASP   |
*                       |              |
*                       +--------------+
*
*
*  Purpose:-
*      Smooth a two dimensional image frame containing a bright
*   galaxy image, subjecting the background sky and faint
*   extended outer regions to a higher degree of smoothing
*   than the nucleus.
*
*
*  General description:-
*      It is normally desirable to smooth a raster containing
*   an image of a bright galaxy in order to reduce the random
*   noise and allow the detection of the faint outer regions,
*   which are typically much fainter than the night
*   sky. However a simple smoothing uniformly
*   applied to the entire image is often inappropriate;
*   the bright central regions of the galaxy do not require
*   smoothing - indeed smoothing here would degrade the
*   quality of the image. The present routine is an
*   implementation of the procedure described by Jones et al
*   (1967) and discussed by de Vaucouleurs (1976). The
*   degree of smoothing to which a pixel is subjected
*   depends on its intensity. Bright pixels close to the
*   nucleus are not smoothed, pixels of moderate brightness
*   are subjected to 9 point smoothing and faint pixels,
*   comprising the faint outer regions of the galaxy and the
*   surrounding sky are 25 point smoothed. Weighted smoothing
*   is used, the grids, which are shown below, being taken
*   from Jones et al (1967).
*
*   1/ 3 point smoothing grid.
*
*             1/16   1/8   1/16
*             1/8    1/4   1/8
*             1/16   1/8   1/16
*
*   2/ 5 point smoothing grid.
*
*           1/256  1/64  3/128  1/64  1/256
*           1/64   1/16  3/32   1/16  1/64
*           3/128  3/32  9/64   3/32  3/128
*           1/64   1/16  3/32   1/16  1/64
*           1/256  1/64  3/128  1/64  1/256
*
*  Usage:-
*      Firstly the user is prompted for the filenames of the image
*   to be smoothed and the smoothed image to be output. He is
*   then required to supply the upper intensity thresholds
*   for 5 point and 3 point smoothing. These should have been
*   previously determined by inspection of the image
*
*  References:-
*   Jones W B, Obitts D L, Gallet R M & de Vaucouleurs G, 1967,
*      Pub. Dept. Astr. Univ. Texas, Austin, Ser.II,
*      Vol.1, No.8.
*   de Vaucouleurs G, 1976, Occ. Rep. ROE No.2.
*
*
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*
*  Subroutine called;
*   JMOOTH.
*
*  A C Davenhall./ROE/                                       29/7/82.
*-
      CALL JMOOTH
      END
