      SUBROUTINE KING (GLMFN,GNLMFN,GBIN,GMXBIN,DIST,ABSORP,
     :                 XSIZE,YSIZE,RC,RT,PIXSIZ,MAXSTR,NSTAR,
     :                 XCORD,YCORD,APMAG,RTPIX,ISTAT)
C+
C      KING.
C
C      Subroutine to generate a set of star positions & apparent
C     magnitudes within a globular cluster of known distance
C     and total number of stars. The cluster radial luminosity
C     distribution is assumed to obey a King (1962) law.
C
C  Reference; King I (1962) AJ vol.67 pp471-485.
C
C  Given;
C   GLMFN   (RA)  Magnitude bins for cluster luminosity function.
C   GNLMFN  (RA)  Number bins for cluster luminosity function.
C   GBIN    (I)   Number of bins in the cluster luminosity function.
C   GMXBIN  (I)   Max. permitted no. of bins in the cluster luminsity
C                 function ( = size of arrays GLMFN & GNLMFN).
C   DIST    (R)   Distance of the cluster in pc.
C   ABSORP  (R)   Total absorption to the cluster along the line
C                 of sight (magnitudes). NOT the absorption per kpc.
C   XSIZE   (R)   X size of field (pixels).
C   YSIZE   (R)   Y size of field (pixels).
C   RC      (R)   Core radius for cluster (pc).
C   RT      (R)   Tidal radius for cluster (pc).
C   PIXSIZ  (R)   Size of each pixel (arcsec).
C   MAXSTR  (I)   Max. permitted no. of stars ( = size of arrays
C                 XCORD, YCORD & APMAG, see below).
C   NSTAR   (I)   No. of stars the user requests.
C
C  Returned;
C   NSTAR   (I)   No. of stars the program generates (may be less
C                 then the number requested if either more than
C                 the permitted no. are requested or the cluster
C                 extends beyond the image.
C   XCORD   (RA)  X coords. of the positions of the stars generated.
C   YCORD   (RA)  Y   "   . "   "      "     "   "    "       "    .
C   APMAG   (RA)  Apparent magnitudes of the stars generated.
C   RTPIX   (R)   Tidal radius Rt expressed in pixels.
C   ISTAT   (I)   Return status.
C                 = 0 - Successfull (??!!) return.
C                 = 1 - Too many points in the luminosity function.
C                 = 2 - More than the permitted no. of stars requested.
C                 = 3 - Some stars fell outside the pixel grid.
C
C  Subroutines called;
C   None.
C
C  Structure:-
C   Force the no. of bins in the luminosity function to be sensible.
C   Compute the scaling factor between the grid size in pixels and
C   the cluster size in pc.
C   compute the scaling factor, k, for the King law.
C   Do while (more stars required)
C     Generate random magnitude.
C     generate random no. in luminosity function.
C     If (point falls inside envelope of luminosity function) then
C       generate star positions inside the cluster in (R,theta) coords.
C       Generate a random value the star density at this radius
C       If (star density falls inside the star density envelope) then
C         Convert (R,theta) to X,Y.
C         If (position of star falls inside pixel grid)
C           accept star
C           add positions & coords. to list.
C         end if
C       end if
C     end if
C   end do
C   If (some stars have fallen outside grid)
C     return status=3
C   end if
C   Convert absolute magnitudes to apparent magnitudes.
C
C  A C Davenhall./ROE/                                23/3/82.
C-
      INTEGER GBIN,GMXBIN
      REAL GLMFN(GMXBIN),GNLMFN(GMXBIN)
      INTEGER NSTAR,MAXSTR,ISTAT
      REAL DIST,XSIZE,YSIZE,RC,RT,PIXSIZ
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR)
C
C
      REAL PI,K,XT,XT1,RAD,THETA,F,FP
      REAL MAGMIN,MAGRANG,NUMMIM,NUMRANG,MAGSTR,NUMSTR,NUMINT
      INTEGER ISTAR,ICOUNT,ACCEPT,IK,IKK,GGBIN
      DOUBLE PRECISION RX
      REAL XPOS,YPOS,XCEN,YCEN,SCALE
      REAL CORE,TIDAL
      PARAMETER (PI=3.14159265E0)
C
C       Force the number of points in the luminosity profile
C       to be sensible.
C
      IF (GBIN.LE.GMXBIN) THEN
        GGBIN=GBIN
      ELSE
        GGBIN=GMXBIN
        ISTAT=1
      END IF
C
C      Set the centre of the cluster to the centre of the field.
C
      XCEN=XSIZE/2.0E0
      YCEN=YSIZE/2.0E0
C
C      Setup the scale factor between the distance in pc and the
C      observed position in pixels.
C
      SCALE=(3.6E2*6.0E1*6.0E1)/(DIST*2.0E0*PI*PIXSIZ)
C
C      Compute the tidal radius, Rt, in pixels.
C
      RTPIX=RT*SCALE
C
C      Check if there are more than the permitted no. of points
C      if so adopt the max. permitted as the working no. of pts. and
C      set a return status.
C
      ISTAT=0
      IF (NSTAR.LE.MAXSTR) THEN
        ISTAR=NSTAR
      ELSE
        ISTAR=MAXSTR
        ISTAT=2
      END IF
C
C      Compute the scaling constant for the King law, k.
C      (see eq.21 p476 of reference given above).
C
      XT=(RT/RC)**2
      XT1=XT+1.0E0
      K=FLOAT(ISTAR)/(PI*RC*RC*(ALOG(XT1)-
     :  (((3.0E0*SQRT(XT1))-1.0E0)*(SQRT(XT1)-1.0E0)/XT1)))
C
C      Perform Monte Carlo generation of Globular Cluster.
C
C      Setup the ranges for the luminosity function.
C
      MAGMIN=GLMFN(1)
      MAGRANG=GLMFN(GGBIN)-GLMFN(1)
      NUMMIN=GNLMFN(1)
      NUMRANG=GNLMFN(GGBIN)-GNLMFN(1)
C
      ICOUNT=0
      ACCEPT=0
      DO WHILE (ICOUNT.LT.ISTAR)
        RX=G05CAF (RX)
        MAGSTR=MAGMIN+(RX*MAGRANG)
        RX=G05CAF (RX)
        NUMSTR=NUMMIN+(RX*NUMRANG)
C
C      Interpolate to get the number density corresponding
C      to the generated magnitude.
C
        IF (MAGSTR.LE.GLMFN(1)) THEN
          NUMINT=GNLMFN(1)
        ELSE IF (MAGSTR.GE.GLMFN(GGBIN)) THEN
          NUMINT=GNLMFN(GGBIN)
        ELSE
          IK=1
          DO WHILE (MAGSTR.GT.GLMFN(IK))
            IK=IK+1
          END DO
          IKK=IK-1
          NUMINT=GNLMFN(IKK)+((MAGSTR-GLMFN(IKK))*
     :           (GNLMFN(IK)-GNLMFN(IKK))/
     :           (GLMFN(IK)-GLMFN(IKK)))
        END IF
C
C      Check if generated point falls inside envelope of
C      luminosity distribution.
C
        IF (NUMSTR.LE.NUMINT) THEN
C
C      Generate positions of star inside cluster in (R,theta) coords.
C
          RX=G05CAF (RX)
          RAD=RX*RT
          RX=G05CAF(RX)
          THETA=RX*2.0E0*PI
C
C      Generate value for star density.
C
          RX=G05CAF (RX)
          F=RX*K*4.0E0
C
C      Evaluate the stellar density expected for radius RAD.
C      (equation 14, 476 of reference above).
C 
          CORE=1.0E0/SQRT(1.0E0+((RAD/RC)**2))
          TIDAL=1.0E0/SQRT(1.0E0+((RT/RC)**2))
          FP=K*((CORE-TIDAL)**2)
C
C      If the evaluated point falls inside the stellar density 
C      envelope consider it a legitimate star for the cluster.
C
          IF (F.LE.FP) THEN
            ICOUNT=ICOUNT+1
C
C      Convert the polar cordinates of the point in kpc into
C      rectangular coords. in units of the pixel grid.
C
            XPOS=XCEN+(RAD*SCALE*COS(THETA))
            YPOS=YCEN+(RAD*SCALE*SIN(THETA))
C
C      Accept the point if it falls inside the pixel grid.
C
            IF (XPOS.GE.0.0E0.AND.XPOS.LE.XSIZE
     :         .AND.YPOS.GE.0.0E0.AND.YPOS.LE.YSIZE) THEN
              ACCEPT=ACCEPT+1
              XCORD(ACCEPT)=XPOS
              YCORD(ACCEPT)=YPOS
              APMAG(ACCEPT)=MAGSTR
            END IF
          END IF
        END IF
      END DO
C
C      Check whether any points fall outside the pixel grid.
C
      IF (ACCEPT.EQ.ICOUNT) THEN
        NSTAR=ISTAR
      ELSE
        NSTAR=ACCEPT
        ISTAT=3
      END IF
C
C      Convert the absolute magnitudes into apparent magnitudes.
C
      DO I=1,NSTAR
        APMAG(I)=APMAG(I)-5.50E0+(5.0E0*ALOG10(DIST))+ABSORP
      END DO
      END
