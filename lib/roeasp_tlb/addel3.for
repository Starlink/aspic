      SUBROUTINE ADDEL3(X01,Y01,A1,B1,ALPHA1,ARR,ID1,ID2,SUM)
C+
C
C   SUBROUTINE ADDEL3(X01,Y01,A1,B1,ALPHA1,ARR,ID1,ID2,SUM)
C
C   Sums pixel values in an ellipse {centre X01,Y01; semiaxes A1,B1;
C   A1 axis tilted ALPHA1 radians (anticlockwise) with respect to
C   the positive X direction} overlaying a pixel array ARR(ID1,ID2).
C   The centre of pixel i,j is position x=i,y=j and the pixels are
C   one unit square.
C   All calculations are carried out in double precision and the 
C   result returned in single precision in SUM.
C   Where a pixel is cut by the ellipse, the fraction
C   within the ellipse is calculated.
C
C   Given       (arguments)
C   X01     R   X coordinate of centre of ellipse
C   Y01     R   Y coordinate of centre of ellipse
C   A1      R   Length of first semi major axis of ellise
C   B1      R   Length of second semi major axis of ellipse
C   ALPHA1  R   Tilt(anticlockwise) of A1 axis w.r.t. X direction
C   ARR     RA  Image array
C   ID1     I   X dimension of image array
C   ID2     I   Y dimension of image array
C
C   Returned    (arguments)
C   SUM     R   Integrated value within ellipse
C
C   Subroutines called:
C   COMCAL            :E2DLIB
C   ELFR3             :E2DLIB
C   INTCEP            :E2DLIB
C  
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*4 ARR(ID1,ID2),X01,Y01,A1,B1,ALPHA1,SUM,FRAC
      X0=X01
      Y0=Y01
      A=A1
      B=B1
      ALPHA=ALPHA1
      CALL COMCAL(X0,Y0,A,B,ALPHA,F1,F2,F3,F4,XEL,YEL,
     :            XER,YER,XEB,YEB,XET,YET)
      ILEFT=JIDNNT(XEL-0.5)
      IRIGHT=JIDNNT(XER+0.5)
      JLOW=JIDNNT(YEB-0.5)
      JHIGH=JIDNNT(YET+0.5)
      DSUM=0.0
      IF(ILEFT.LT.1.OR.IRIGHT.GT.ID1.OR.JLOW.LT.1.OR.
     +   JHIGH.GT.ID2) THEN
          WRITE(6,'('' ELLIPSE RUNS OUT OF PICTURE'')')
      ELSE
          DO J=JLOW,JHIGH
              CALL INTCEP(J,X0,Y0,F1,F2,F3,F4,YEB,YET,
     :                    XE1,XE2,XE3,XE4)
              DO I=ILEFT,IRIGHT
                  CALL ELFR3(I,J,X0,Y0,F1,F2,F3,F4,XEL,YEL,XER,YER,
     :                       YEB,YET,XE1,XE2,XE3,XE4,DFRAC)
                  DVAL=ARR(I,J)
                  DSUM=DSUM+DFRAC*DVAL
              ENDDO
          ENDDO
      ENDIF
      SUM=DSUM
      RETURN
      END
