      PROGRAM CVPLOT
*+
*
*     CVPLOT
*
*     Purpose : To produce  an annotated  contour map of an image
*               with  the   corresponding   polarization  vectors
*               overlayed . The diagram  can be  produced  on any
*               one  of  the  T4014 , T4010 , ARGS  or VERSATEK .
*
*     Usage   : When  CVPLOT is run ,  three data frames  will be
*               requested  .  The first should  contain the image
*               from  which  the  contour  map is  produced , the
*               second  should contain  the total polarization at
*               each  point  in the  image  and the  third should
*               contain  the  angle of polarization corresponding
*               to each value of total polarization .
*
*               The  parameters  which  define the  layout of the
*               diagram will then be prompted for .
*
*               Polarization vector scale factor :
*               range 0.1 to 10.0 , default 1.0
*               a value  of 1.0  produces vectors  with a  length
*               corresponding to four pixels .
*               The  interval at which  the polarization  vectors
*               will be plotted depends on the extent of the plot
*               in pixels as follows :
*                     extent          interval
*                 1 to  64 pixels      1 pixel
*                65 to 128   "         2 pixels
*               129 to 256   "         4   "
*                    > 256   "         8   "
*
*               Angle to be added to the polarization angle :
*               range -180.0 degrees to +180.0 degrees
*               default 0.0 degrees
*               all  the polarization  vectors will be  offset by
*               this angle .
*
*               Title for the plot :
*               an  80  character string  which  will  be plotted
*               above the diagram .
*
*               Number of contours for the contour plot :
*               range 1 to 20 , default 10
*
*               Base contour level :
*               range -1E10 to +1E10 , default 0.0
*               this is the value of the first contour which will
*               be plotted .
*
*               Interval between contours :
*               range -1E10 to +1E10 , default 100.0
*               this  is the  separation of  successive  contours
*
*               Type of smoothing :
*               Options are NONE , BOX averge or GAUSSIAN
*               default is NONE
*               the  image  used  for  the  contour  map  can  be
*               smoothed prior to contouring .
*
*               If a  BOX  average is requested  then the size of
*               the  smoothing   box  will  be   prompted  for  :
*               range  3 pixels  to  15 pixels , default 5 pixels
*               this must be an odd number of pixels . if an even
*               number  is  given  it  is  incremented  by  one .
*
*               If a  GAUSSIAN is requested , the size of the box
*               over which  smoothing is to be performed  will be
*               requested  in   the  same  way  as  for  the  box
*               average  and then  the value  of  sigma  for  the
*               smoothing   gaussian   will  be  prompted   for :
*               range 1.0 pixels to 10.0 pixels
*               default 2.0 pixels
*
*               Graphics device for the plot :
*               options are T4014 , T4010 , ARGS or VERSATEK
*               the default is the T4014
*               The annotation for the  plot will only be plotted
*               correctly  on the T4014 and the  Versatek because
*               ( at the moment ) the  graphics package used uses
*               the nearest  size  hardware  characters for  each
*               device .
*
*               If the device requested was the  T4014 , T4010 or
*               ARGS  after the  plot is finished  the  option to
*               produce  a  copy on the  versatek is  available .
*               the default is No .
*
*               Another  plot using  the  same data  is  prompted
*               for , again the default is No .
*               If the reply is  Yes then the sequence will start
*               again from prompting for  the polarization vector
*               scale factor .
*               If it is  No  then the  program terminates  , any
*               plots  produced  for  the  versatek  can  now  be
*               submitted using the VPLOT command .
*
*     Subroutine called : CVPLOTS
*
*     D.W.T.Baines/ROE/Feb 1983/
*
*-
      CALL CVPLOTS
      END
