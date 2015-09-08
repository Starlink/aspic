      SUBROUTINE INTERPOLATE(ID,LINDAT,NPTS,DATA,NX,NY,LINE)
      INTEGER STATUS
      REAL LINDAT(0:1023,2),LINE(0:1023),DATA(0:NX-1,0:NY-1),XVAL(2),
     :    YVAL(2)
C
C   SET COLOUR OF LINE AND PLOT IT
C
      XVAL(1) = LINDAT(0,1)
      YVAL(1) = LINDAT(0,2)
      XVAL(2) = LINDAT(NPTS-1,1)
      YVAL(2) = LINDAT(NPTS-1,2)
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
C   LINDAT CONTAINS THE X,Y PAIRS OF POINTS TO BE INTERPOLATED
C
      DO I=0,NPTS-1
          IX1 = LINDAT(I,1)
          IY1 = LINDAT(I,2)
          IX2 = IX1 + 1
          IY2 = IY1 + 1
C
C   GET THE FOUR SURROUNDING POINTS IN THE DATA ARRAY
C
         VAL1 = DATA(IX1,IY1)
         VAL2 = DATA(IX2,IY1)
         VAL3 = DATA(IX1,IY2)
         VAL4 = DATA(IX2,IY2)
C
C   F & G ARE THE FRACTIONAL PIXEL DISPLACEMENTS OF THE
C   INTERPOLATION POINT
C
         F = LINDAT(I,1) - IX1
         G = LINDAT(I,2) - IY1
C
C   ON EXIT, 'LINE' WILL CONTAIN THE ARRAY OF INTERPOLATED VALUES
C   READY FOR PLOTTING. BILINEAR INTERPOLATION IS USED.
C
         LINE(I) = F*(VAL2-VAL1) + F*G*(VAL1+VAL4-VAL2-VAL3)
     1             + G*(VAL3-VAL1) + VAL1
      ENDDO
C
C
      END
C
C