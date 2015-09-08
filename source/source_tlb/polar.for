      PROGRAM POLAR
*+
*
*    POLAR
*
*    Purpose : Given sky subtracted  Q and U polarization frames
*              then two frames containing the total polarization
*              and polarization angle are generated . Optionally
*              an  error  frame can be  provided and  the  error
*              estimates   for  each   pixel   applied   to  the
*              calculations . The  polarization vectors are then
*              then  plotted  on  one  of the  ARGS , VERSATEK ,
*              T4014 or T4010 .
*
*    Usage   : If the polarization vectors  are to be plotted on
*              top  of an image  displayed on  the ARGS then the
*              image  should be displayed such that it  occupies
*              a central  square of  320 by 320 pixels  . For an
*              image  of any other size  the SQORST command  can
*              be used  to generate a  320 by 320  pixel copy of
*              the  image  that can  then be displayed  by using
*              ADISP   for  example  .   POLAR  is  then  run  .
*              The filenames of the  Q and U frames are prompted
*              for and then the filename  and title is requested
*              for   each  of   the   total   polarization   and
*              polarization angle output frames. An instrumental
*              angle  to be added  to the polarization  angle is
*              is required  ,  the default is  0 degrees  .  The
*              option is given to provide an error frame  ,  the
*              default reply is Yes . If an error frame is to be
*              provided then its filename will be prompted for .
*              The  graphics device on  which the plot  is to be
*              produced is requested , the default device is the
*              ARGS  .  A scale factor  for the vectors  must be
*              provided  ,   the  default  value  is  1.0  which
*              produces  vectors with a  length of four pixels .
*              The  interval at which  the polarization  vectors
*              will be plotted depends on the extent of the plot
*              in pixels as follows :
*                    extent          interval
*                1 to  64 pixels      1 pixel
*               65 to 128   "         2 pixels
*              129 to 256   "         4   "
*                   > 256   "         8   "
*              If  the graphics device  specified  was the  ARGS
*              then  a request will  be made  as to  whether the
*              plot is to be overlayed  on an image displayed on
*              the  ARGS  ,  the  default  reply  is No  . If no
*              overlaying is to be done then the ARGS is cleared
*              otherwise  just the  overlay  plane  is cleared .
*
*    Subroutine called : POLARS
*
*    D.W.T.Baines/ROE/Feb 1983
*
*-
      CALL POLARS
      END
