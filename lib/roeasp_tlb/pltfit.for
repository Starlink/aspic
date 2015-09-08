      SUBROUTINE PLTFIT (DEVICE,CLEAR,MAXPAR,MAXPOS,MAXCNT,
     :                   ELLPOS,PARMS,ELLPAR,WORKX,WORKY)
C+
C     PLTFIT.
C
C     Subroutine to plot a set of contours and ellipses fitted to
C     them superposed.
C
C  Given;
C   DEVICE (I)  Graphics device to be used.
C   CLEAR  (L)  Determines whether the device is to be cleared or
C               not; = .true.  - clear the screen.
C                    = .false. - do not clear the screen.
C   MAXPAR (I)  Max. no. of parameters stored for each point in
C               the extracted contours (must be .ge.2).
C   MAXPOS (I)  Max. permitted no. of points in each contour.
C   MAXCNT (I)  Max. permitted no. of contours.
C   ELLPOS (RA) Array holding contours.
C   PARMS  (I)  Max. no. of parameters stored for each fitted ellipse
C               (must be .ge. 6).
C   ELLPAR (RA) Array holding the parameters for the fitted ellises.
C
C  Used;
C   WORKX  (RA) Work array, size = MAXPOS.
C   WORKY  (RA)  "     "  ,  "   =   "   .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics:-  GDEVI1, WINDO, DRAXIS, TYPLO1, QLOT, ELLIPSE,
C               ELLIPSE.
C   Fings:-     CLICTL.
C
C  Structure:-
C   Initialise graphics device and clear screen.
C   Disable clipping.
C   Copy the faintest (& hence largest contour into the work arrays).
C   From this define the maxima and minima of the plotting space.
C   Set up this plotting space and draw a set of axes.
C   do for each contour
C     extract points into work arrays
C     plot extracted contour
C     plot fitted ellipse.
C   end do
C   disable clipping.
C   stop plotting.
C
C  A C Davenhall./ROE/                                  24/9/82.
C-
      INTEGER DEVICE,MAXPAR,MAXPOS,MAXCNT,PARMS
      REAL ELLPOS(MAXPAR,MAXPOS,MAXCNT),
     :     ELLPAR(PARMS,MAXCNT)
      REAL WORKX(MAXPOS),WORKY(MAXPOS)
      LOGICAL CLEAR
C
      INTEGER NPTS
      REAL XCEN,YCEN,A,B,ECC,THETA
      REAL XMIN,XMAX,YMIN,YMAX
C
C
C    Initialise the graphics device and disable clipping.
C
      CALL GDEVI1 (DEVICE,CLEAR)
      CALL CLICTL (1)
C
C    Copy the faintest contour into the work arrays and define
C    the plotting space.
C
      NPTS=1
      DO WHILE (ELLPOS(1,NPTS,1).GT.-9.0E3.AND.NPTS.LT.MAXPOS)
        WORKX(NPTS)=ELLPOS(1,NPTS,1)
        WORKY(NPTS)=ELLPOS(2,NPTS,1)
        NPTS=NPTS+1
      END DO
      NPTS=NPTS-1
      CALL WINDO (WORKX,WORKY,MAXPOS,NPTS,XMIN,XMAX,YMIN,YMAX,
     :            .FALSE.)
C
C    Draw axes.
C
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'X (PIXELS)','Y')
C
C    Process each contour.
C
      DO K=1,MAXCNT
C
C    Copy the Kth contour into the work arrays.
C
        NPTS=1
        DO WHILE (ELLPOS(1,NPTS,K).GT.-9.0E3.AND.NPTS.LT.MAXPOS)
          WORKX(NPTS)=ELLPOS(1,NPTS,K)
          WORKY(NPTS)=ELLPOS(2,NPTS,K)
          NPTS=NPTS+1
        END DO
        NPTS=NPTS-1
C
C    Plot as points.
C
        CALL TYPLO1 (1,0)
        CALL QLOT (WORKX,WORKY,MAXPOS,NPTS)
C
C    Extract the parameters for the kth fitted ellipse.
C
        XCEN=ELLPAR(2,K)
        YCEN=ELLPAR(3,K)
        A=ELLPAR(4,K)*2.0E0
        B=A*(1.0E0-ELLPAR(5,K))
        THETA=ELLPAR(6,K)
        ECC=SQRT((A**2)-(B**2))/A
C
C    Plot the ellipse.
C
        CALL ELLIPSE (XCEN,YCEN,A,ECC,THETA)
      END DO
C
C    Disable clipping and stop plotting.
C
      CALL CLICTL (0)
      CALL STOPLOT 
C
      END
