      SUBROUTINE STAZAP (IX,IY,IRAD,IXEXT,IYEXT,A,ISTAT)
C+
C      STAZAP.
C
C      Subroutine to remove a Star image or other blemish given its
C      pixel coordinates and radius. The image is replaced by points
C      interpolated in a 2D polynomial fitted by least squares to
C      a region around its periphery.
C
C    Given;
C    IX   (I)   X coord. of centre of star image.
C    iy   (I)   Y   "  . "    "    "   "     "  .
C    IRAD (I)   Radius totally enclosing image, in pixels.
C    IXEXT (I)  X size of total image array.
C    IYEXT (I)  Y  "   "    "     "     "  .
C    A    (R)   Array containing image.
C
C    Returned;
C    A    (R)   Image modified by removal of offending star.
C    ISTAT (I)  Return status
C               = 0 - OK.
C               = 1 - Periphery too large to permit extraction of all
C                     points for background fit. Fit may therefore
C                     be biased.
C
C    Subroutine called;  FITSUR.
C    Function called;    FUNC.
C
C      Structure:-
C      Form a box around the star equal in size to the radius + 
C         5 pixels on either side.
C      Prevent the box overflowing the image edge.
C      Do for (all points inside the box)
C        Compute the distance of the pt. from the star centre.
C        If radius > star radius
C          If array to hold background pts. is not full.
C            increment no. of background pts.
C            add this pt. to the list of background pts.
C          else
C            set return status to 1.
C          end if
C        end if
C      end do
C      Solve for background polynomial using lst. squares.
C      Compute a box around the star equal in size to its radius.
C      Do for (pts. inside box)
C        If radius < star radius then
C          replace the point with the value of the polynomial evaluated
C            here.
C        end if
C      end do
C
C      Based on a routine by;
C      C. P. Blackman./Univ.St.And./                         1975.
C
C      A C Davenhall./ROE/                                   21/1/82.
C-
      INTEGER IX,IY,IRAD,IXEXT,IYEXT,ISTAT
      REAL A(IXEXT,IYEXT)
      REAL X(3000),Y(3000),F(3000)
      INTEGER MAXPTS,NPTS,MAXORD
      INTEGER IX1,IY1,IX2,IY2,PTRAD
      REAL XX,YY
      REAL COEF(9)
      PARAMETER (MAXPTS=1000)
      PARAMETER (MAXORD=9)
C
C      Form a box around the star equal in size to the radius
C      + 5 pixels on either side, preventing it from
C      overlapping the edge of the picture.
C
      IX1=IX-IRAD-5
      IY1=IY-IRAD-5
      IX2=IX+IRAD+5
      IY2=IY+IRAD+5
      IX1=MAX(1,IX1)
      IY1=MAX(1,IY1)
      IX2=MIN(IX2,IXEXT)
      IY2=MIN(IY2,IYEXT)
C
      NPTS=0
      ISTAT=0
      DO J=IY1,IY2
        DO I=IX1,IX2
C
C      Compute the distance of each pt. from the star centre, &
C      if > star radius include it as one of the pts. for the
C      background fit.
C
          PTRAD=IFIX(SQRT(FLOAT(((I-IX)*(I-IX))+((J-IY)*(J-IY)))))
          IF (PTRAD.GT.IRAD) THEN
            IF (NPTS.LT.MAXPTS-1) THEN
              NPTS=NPTS+1
              X(NPTS)=FLOAT(I)
              Y(NPTS)=FLOAT(J)
              F(NPTS)=A(I,J)
            ELSE
              ISTAT=1
            END IF
          END IF
        END DO
      END DO
C
C      Solve for the background polynomial.
C
      CALL FITSUR (X,Y,F,MAXPTS,NPTS,COEF)
C
C      Form a box around the star equal in size to its radius.
C
      IX1=IX-IRAD-1
      IY1=IY-IRAD-1
      IX2=IX+IRAD+1
      IY2=IY+IRAD+1
      IX1=MAX(1,IX1)
      IY1=MAX(1,IY1)
      IX2=MIN(IX2,IXEXT)
      IY2=MIN(IY2,IYEXT)
      DO J=IY1,IY2
        DO I=IX1,IX2
C
C      If the radius of the pt. is less than the star radius
C      replace the point with the polynomial evaluated
C      at the point.
C
          PTRAD=IFIX(SQRT(FLOAT(((I-IX)*(I-IX))+((J-IY)*(J-IY)))))
          IF (PTRAD.LE.IRAD) THEN
            XX=FLOAT(I)
            YY=FLOAT(J)
            A(I,J)=FUNC(XX,YY,COEF,MAXORD)
          END IF
        END DO
      END DO
      END
