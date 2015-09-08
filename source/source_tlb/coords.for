      PROGRAM COORDS
*+
*
*    COORDS
*
*    Purpose : To allow up  to 500 points  in the current image
*              displayed on the ARGS to be identified using the
*              cursor and stored as an output image.
*
*    Usage   : The image to be measured  should be displayed on
*              the  ARGS,  using  ADISP  for example,  and then
*              COORDS  run. The ARGS cursor will appear and the
*              two outermost trackerball buttons will light up.
*              If the  leftmost trackerball  button is  pressed
*              the coordinates will be stored. If the rightmost
*              button is pressed,  input of coordinates will be
*              terminated  and   COORDS   will  prompt  for the
*              filename  of  the  file  to  hold  the  measured
*              coordinates  and for a  title for the file.  The
*              program then terminates.
*
*    Subroutine called :
*    COORDS1           : E2DASP
*
*    D.W.T.Baines/ROE/April 1983
*
*-
      CALL COORDS1
      END
