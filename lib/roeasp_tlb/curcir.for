      SUBROUTINE CURCIR (RAD,ISIZE,JSIZE,CURSOR)
C+
C     CURCIR.
C
C     Subroutine to return an integer*2 array holding values
C     appropriate for displaying as a circular cursor on
C     the Args. The centre and radius of the circle are given
C     as arguments to the subroutine.
C
C  Given;
C   RAD     (R)  Radius of circle.
C   ISIZE   (I)  X size of cursor array.
C   JSIZE   (I)  Y size of cirsor array.
C
C  Returned;
C   CURSOR  (I2) Cursor array (integer*2).
C
C  Subroutines called;
C   E2D:-   PUTHEX.
C
C  A C Davenhall./ROE/                                        12/11/81.
C  A C Davenhall./ROE/    {Modified from routine "CIRCLE"}    26/9/82.
C-
      REAL RAD
      INTEGER ISIZE,JSIZE
      INTEGER*2 CURSOR(ISIZE,JSIZE)
C
      REAL DETHETA,THETA,XPOS,YPOS
      INTEGER NX,NY
C
      INTEGER NPTS
      PARAMETER (NPTS=100)
      REAL X(NPTS),Y(NPTS)
C
      REAL PI
      PARAMETER (PI=3.1415927E0)
C
C
C    Blank out the cursor array.
C
      DO J=1,JSIZE
        DO I=1,ISIZE
          CURSOR(I,J)=0
        END DO
      END DO
C
C    Position the centre of the circle.
C
      XPOS=REAL(ISIZE*8)
      YPOS=REAL(JSIZE/2)
C
C    Take care of the case where the radius is sensibly zero.
C    Draw a dot.
C
      IF (RAD.LE.1.0E-7) THEN
        NX=NINT(XPOS)
        NY=NINT(YPOS)
        CALL PUTHEX (NX,NY,ISIZE,JSIZE,CURSOR)
      ELSE
C
C    Case where have a real circle.
C
C    Set up array of points.
C
        DETHETA=(2.0E0*PI)/FLOAT(NPTS)
        DO I=1,NPTS
          THETA=DETHETA*(FLOAT(I-1))
          X(I)=XPOS+(RAD*COS(THETA))
          Y(I)=YPOS+(RAD*SIN(THETA))
        END DO
C
C    Generate the cursor.
C
        DO I=1,NPTS
          NX=NINT(X(I))
          NY=NINT(Y(I))
          CALL PUTHEX (NX,NY,ISIZE,JSIZE,CURSOR)
        END DO
      END IF
      END
