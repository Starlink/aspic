	SUBROUTINE CRB_GREYSCALE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   GREYSCALE *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C               GREYSCALE
C
C
C          FUNCTION:-
C               It generates output which, when plotted on  the  Printro
C               or  Versatec  printers, gives  a  greyscale  representat
C               of the input image.  The technique used generates a dens
C               of  dots in the  output  which is proportional to the in
C               data value. The pattern of dots is random.
C
C
C          USE:-
C               It gives a hard copy of image data.
C
C		When the hard copy is  for  the  Printronix it  is  written
C               to  a  file  called  GREYPLT.LIS  which  is   automatica
C               queued  to  the  printer   when   the   program   finish
C               Additional  copies  may  be obtained by typing the comma
C               PRINT/QUEUE=SYS_PRINTRONIX/PASSALL/NOFEED GREYPLT
C
C               When the hard copy is for the Versatec  it  is written t
C               file called DIT.DAT which is  automatically  queued  to
C               Versatec when the program  finishes.  A file called DIT.
C               is  also   queued  to  the   Versatec   and  this  conta
C               information on the image. Additional copies may be obtai
C               by typing the command:
C               PRINT/QUEUE=SYS_VERSATEC/PASSALL/NOFEED DIT.DAT,DIT.LIS
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This  is  the  2-D   starl
C                                             image which is to be plott
C
C
C         DEVICE          PRINTRONIX          Output device.  Default is
C                                             the Printronix, otherwise
C                                             Versatec printer.
C
C         DEFAULT         T                   If TRUE, will assume  defa
C                                             values  for  rotation, win
C                                             size, pixel size, grid lin
C                                             and intensity scaling.
C
C         ROTATE          F                   If TRUE, will rotate image
C                                             degrees on the printer pag
C
C         CHANGE          F                   If  TRUE,  will  change
C                                             default   window  size  wh
C                                             defines the area of  the
C                                             array to be printed.
C
C         IVALUES         1,MAXCOLS           The first and last column
C                                             the  2-D starlink image to
C                                             included in the window.
C
C         JVALUES         1,MAXROWS           The first and last row of
C                                             2-D   starlink  image  to
C                                             included in the window.
C
C
C         GRID            0                   Specifies  the  spacing
C                                             pixel   units)  between  g
C                                             lines to be superposed on
C                                             output  plot.  If  0, no g
C                                             lines will be generated.
C
C         PIXEL           WIDTH               Specifies the height of  e
C                                             image   pixel.   The  maxi
C                                             width   output   is    alw
C                                             produced      (within
C                                             constraint  that  each  im
C                                             pixel   must  be  an  inte
C                                             number of printer dots  wi
C                                             and  by  default square im
C                                             pixels     are      produc
C                                             (Actually the image height
C                                             compressed  by  a  factor
C                                             60/72  due  to the spacing
C                                             the rows on the Printronix
C
C         MAXINF          T                   If TRUE,  will  automatica
C                                             scale   the  image,  using
C                                             histogram        equalizat
C                                             technique,   to   convey
C                                             maximum amount of informat
C                                             in the output image.
C
C         WHITE           MIN                 If MAXINF = FALSE, then  t
C                                             defines  the  array  value
C                                             correspond to pure  white
C                                             the output plot.
C
C         BLACK           MAX                 IF MAXINF = FALSE, then  t
C                                             defines  the  array  value
C                                             correspond to pure  black
C                                             the output plot.
C
C         LOG             F                   If MAXINF = FALSE, then  t
C                                             defines    whether   or
C                                             logarithmic intensity scal
C                                             is to be used.
C
C         LABEL                               A title, up to 80 characte
C                                             to  be added at the bottom
C                                             the plot.
C
C
C
C
C         WDP                      AAO                             4-JUL
C
C
C-----------------------------------------------------------------------
 
 
 
      DIMENSION IDIMEN(2)
      CHARACTER DEVICE*10
      CALL RDIMAG('INPUT',204,2,IDIMEN,NDIM,NPOINT,ISTATUS)
      IF (ISTATUS .NE. 0)STOP
      NCOL=IDIMEN(1)
      NROW=IDIMEN(2)
      MINC=1
      MAXC=NCOL
      MINR=1
      MAXR=NROW
      CONVERT=1.
      DEVICE='PRINTRONIX'
      CALL RDKEYC('DEVICE3',.TRUE.,1,DEVICE,NV,IS)
      IF (DEVICE .EQ. 'PRINTRONIX')THEN
        NUNIT=-8
      ELSE
        NUNIT=8
      END IF
      CALL GREY(%VAL(NPOINT),NCOL,NROW,NCOL,NROW,MINC,MAXC,MINR,MAXR,
     1 CONVERT,NUNIT)
      END
