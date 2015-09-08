      PROGRAM PRPLOT
*+
*
*     PRPLOT
*
*     Purpose : To produce a  plot of polarization  vectors given
*               two  frames  containing  total  polarization  and
*               polarization angle data .
*
*     Usage   : If the polarization vectors  are to be plotted on
*               top  of an image  displayed on  the ARGS then the
*               image  should be displayed such that it  occupies
*               a central  square of  320 by 320 pixels  . For an
*               image  of any other size  the SQORST command  can
*               be used  to generate a  320 by 320  pixel copy of
*               the  image  that can  then be displayed  by using
*               ADISP   for  example  .  PRPLOT  is  then  run  .
*               The  filenames  of  the  total  polarization  and
*               polarization  angle frames  will  be prompted for
*               followed by the X and Y ranges for the plot . The
*               default  values correspond  to the whole  frame .
*               The  interval at which  the polarization  vectors
*               will be plotted depends on the extent of the plot
*               in pixels as follows :
*                     extent          interval
*                 1 to  64 pixels      1 pixel
*                65 to 128   "         2 pixels
*               129 to 256   "         4   "
*                    > 256   "         8   "
*               The  graphics  device  options  are  the  ARGS  ,
*               versatek , T4014 and T4010  .  The default device
*               is the ARGS . A scale factor for the polarization
*               vectors is required  ,  the default value is  1.0
*               which  produces  vectors with  a  length of  four
*               pixels  .  If the device  specified was the  ARGS
*               then  an overlay option  is given  .  The default
*               reply is No  , this causes the ARGS to be cleared
*               If  Yes is specified  then only the overlay plane
*               is cleared .
*
*
*     Subroutine called : PRPLT
*
*     D.W.T.Baines/ROE/Feb 1983
*
*-
      CALL PRPLT
      END
