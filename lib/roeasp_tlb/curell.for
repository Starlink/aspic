      SUBROUTINE CURELL(AXMAJ,ECCEN,THETA,ISIZE,JSIZE,CURSOR)
*+
*   CURELL
*
*   Generates an elliptical cursor for the ARGS by setting
*   bits in a square bit pattern. The centre of the ellipse
*   is set to the centre of the bit pattern.
*
*   Given         (arguments)
*   AXMAJ     R    major axis length
*   ECCEN     R    eccentricity
*   THETA     R    angle of major axis above the horizontal,
*                  measured anticlockwise in radians.
*   ISIZE     I    X-dimension of I*2 array containing bit pattern.
*   JSIZE     I    Y-dimension of bit pattern array.
*
*   Returned      (arguments)
*   CURSOR    I*2A array containing bit pattern.
*
*   Subroutines called :
*   PUTHEX             : E2DLIB
*
*       If (major axis ~ 0 ) then
*         Draw a dot.
*       else
*       If (eccentricity ~ 1 ) then
*         Draw a line of length equal to the major axis along
*         the direction of tilt.
*       else
*         Have a real ellipse;
*         Compute semi-major axis.
*         Compute smei-minor axis.
*         For array of points,
*           Compute an angle phi
*           Compute radius of the ellipse at this angle.
*           Compute corresponding X & Y positions.
*         end do
*         For array of points
*           Rotate to desired inclination.
*         end do
*         Plot points.
*       end if
*       end if
*
*   A.C.Davenhall/ROE/12.11.81.
*   B.D.Kelly/ROE/4.12.1981
*-
      REAL XPOS,YPOS,AXMAJ,ECCEN,THETA
      INTEGER ISIZE,JSIZE
      INTEGER*2 CURSOR(ISIZE,JSIZE)
      REAL PHI,DEPHI,PI,AA,BB,R,XX,YY
      INTEGER NPTS
      REAL X(100),Y(100)

      PARAMETER (PI=3.1415927E0)
      PARAMETER (NPTS=60)

*
*   Position of centre of ellipse
*
      XPOS=REAL(ISIZE*8)
      YPOS=REAL(JSIZE/2)
*
*   BLANK CURSOR ARRAY
*
      DO J=1,JSIZE
         DO I=1,ISIZE
            CURSOR(I,J)=0
         ENDDO
      ENDDO
*
*   Take care of the case where the major axis is sensibly
*   zero. Draw a dot.
*
      IF (AXMAJ.LE.1.0E-7) THEN
        NX=NINT(XPOS)
        NY=NINT(YPOS)
        CALL PUTHEX(NX,NY,ISIZE,JSIZE,CURSOR)
      ELSE IF ((1.0E0-ECCEN).LE.1.0E-7) THEN
*
*       Take care of the case where the eccentricity is sensibly
*       1.0. Draw a line.
*
        DELR=AXMAJ/REAL(NPTS*2)
        DO I=1,NPTS
          RPOS1=DELR*I
          NX=NINT(XPOS)+NINT(RPOS1*COS(THETA))
          NY=NINT(YPOS)+NINT(RPOS1*SIN(THETA))
          CALL PUTHEX(NX,NY,ISIZE,JSIZE,CURSOR)
          NX=NINT(XPOS)-NINT(RPOS1*COS(THETA))
          NY=NINT(YPOS)-NINT(RPOS1*SIN(THETA))
          CALL PUTHEX(NX,NY,ISIZE,JSIZE,CURSOR)
        ENDDO
      ELSE
*
*       Now do the case where a real ellipse has been input.
*
*
*       Compute the semi-major axis, AA & the semi-minor
*       axis, BB.
*
        AA=AXMAJ/2.0E0
        BB=AA*SQRT(1.0E0-(ECCEN**2))
*
*       Compute an array of points along a horizontal
*       Centred on the origin.
*
        DEPHI=(2.0E0*PI)/FLOAT(NPTS)
        DO I=1,NPTS+1
          PHI=DEPHI*FLOAT(I-1)
          R=SQRT(((AA*BB)**2)/(((AA*SIN(PHI))**2)+
     :                ((BB*COS(PHI))**2)))
          X(I)=R*COS(PHI)
          Y(I)=R*SIN(PHI)
        END DO
*
*       Now rotate through an angle THETA,
*       & translate the centre to point XPOS,YPOS.
*
        NPPTS=NPTS+1
        DO I=1,NPPTS
          XX=X(I)
          YY=Y(I)
          X(I)=XPOS+(XX*COS(THETA))-(YY*SIN(THETA))
          Y(I)=YPOS+(XX*SIN(THETA))+(YY*COS(THETA))
        END DO
*
*       Plot points,
*
        DO I=1,NPTS
          NX=NINT(X(I))
          NY=NINT(Y(I))
          CALL PUTHEX(NX,NY,ISIZE,JSIZE,CURSOR)
        ENDDO
      END IF

      END
