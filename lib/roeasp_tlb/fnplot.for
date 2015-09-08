      SUBROUTINE FNPLOT (MAXLEV,MAXBOX,NBOX,NPIX,FPIX,LEVEL,
     :                   WORKF,WORKN,WORKA,MINBOX)
C+
C     FNPLOT.
C
C     Subroutine to plot the computed values of F and N for
C     a range of apertures and to allow the user to choose the
C     minimum acceptable aperture from this plot.
C
C  Given;
C   MAXLEV (I)  Maximum levels in the integration table used
C               used to compute F and N.
C   MAXBOX (I)  Maximum number of apertures used to compute F and N.
C   NBOX   (I)  Actual number of apertures used to compute F and N.
C   NPIX   (RA) 2D array holding the values of N computed for each
C               minimum aperture and each intensity level.
C   FPIX   (RA) 2D array holding the values of F computed for each
C               minimum aperture and each intensity level.
C   LEVEL  (I)  Intensity level for which the values of F and N
C               are to be plotted.
C
C  Used;
C   WORKF  (RA) Work array, size = MAXBOX.
C   WORKN  (RA)  "     "  ,  "   =   "   .
C   WORKA  (RA)  "     "  ,  "   =   "   .
C
C  Returned;
C   MINBOX (I)  Minimum aperture that the user has selected.
C
C  Subroutines called;
C   Interfaces:- OUTPUT, READC, READI.
C   Fings:-      VUPORT.
C   Graphics:-   GDEVI1, WINDO, DRAXIS, TYPLO1, QLOT, STOPLOT.
C
C  Structure:-
C   Extract points corresponding to the chosen level into the
C    work arrays to be used for plotting.
C   Set up the work array of aperture size rerady for plotting.
C   Print message saying "clear screen and hit return to continue".
C   Set up for plotting to the T4010.
C   PLot N as a function of min. aperture in the top half of the screen.
C    "   F "  "    "     "   "      "     "   "   "   "   "   "    "   .C
C   Stop plotting.
C   Inquire which aperture is to be selected.
C
C  A C Davenhall./ROE/                                       24/8/82.
C-
      INTEGER MAXLEV,MAXBOX,NBOX,LEVEL,MINBOX
      REAL NPIX(MAXLEV,MAXBOX), FPIX(MAXLEV,MAXBOX)
      REAL WORKF(MAXBOX),WORKN(MAXBOX),WORKA(MAXBOX)
C
      INTEGER PTS,IOSTAT
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER REPLY*10
C
C
      PTS=NBOX-1
C
C    Extract points corresponding to the F and N values for the
C    isophotal levels to be plotted and set up the work array
C    of aperture sizes.
C
      DO I=1,PTS
        WORKF(I)=FPIX(LEVEL,I)
        WORKN(I)=NPIX(LEVEL,I)
        WORKA(I)=FLOAT(I)
      END DO
C
C    Print message.
C
      CALL OUTPUT 
     : (' Set terminal to T4010 mode prior to plotting,',IOSTAT)
      CALL OUTPUT
     : (' Clear the screen and hit return to continue.',IOSTAT)
      CALL READC ('REPLY','  ',' ',' ','~',REPLY,IOSTAT)
C
C    Set up for plotting to T4010.
C
      CALL GDEVI1 (1,.FALSE.)
C
C    Plot F to the top half of the screen.
C
      CALL VUPORT (0.0E0,1.3E0,5.0E-1,1.0E0)
      CALL WINDO (WORKA,WORKF,MAXBOX,PTS,XMIN,XMAX,YMIN,YMAX,
     :            .FALSE.)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'SMALLEST APERTURE','F')
      CALL TYPLO1 (1,8)
      CALL QLOT (WORKA,WORKF,MAXBOX,PTS)
C
C    Plot N in the bottom half of the screen.
C
      CALL VUPORT (0.0E0,1.3E0,0.0E0,5.0E-1)
      CALL WINDO (WORKA,WORKN,MAXBOX,PTS,XMIN,XMAX,YMIN,YMAX,
     :            .FALSE.)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'SMALLEST APERTURE','N')
      CALL TYPLO1 (1,6)
      CALL QLOT (WORKA,WORKN,MAXBOX,PTS)
C
C    Stop plotting.
C
      CALL STOPLOT
C
C    Obtain the required aperture from the user.
C
      CALL READI ('MINBOX',' Enter required aperture;',
     :             PTS,1,PTS,MINBOX,IOSTAT)
      END
