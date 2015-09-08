      SUBROUTINE INTCEP(J,X0,Y0,F1,F2,F3,F4,YEB,YET,XE1,XE2,XE3,XE4)
C+
C
C   SUBROUTINE INTCEP(J,X0,Y0,F1,F2,F3,F4,YEB,YET,XE1,XE2,XE3,XE4)
C
C   Calculates intercepts of the ellipse with y=j+0.5 and y=j-0.5
C
C   Given       (arguments)
C   J       I   Y coordinate of centre of current line of pixels
C   X0      D   X coordinate of centre of ellipse
C   Y0      D   Y coordinate of centre of ellipse
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   F3      D   Parameter of equation of ellipse
C   F4      D   Parameter of equation of ellipse
C   YEB     D   Y coordinate of lowest point on ellipse
C   YET     D   Y coordinate of highest point on ellipse
C
C   Returned    (arguments)
C   XE1     D   Left intercept of ellipse with line y=j+0.5
C   XE2     D   Right intercept of ellipse with line y=j+0.5
C   XE3     D   Right intercept of ellipse with line y=j-0.5
C   XE4     D   Left intercept of ellipse with line y=j-0.5
C  
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      FJPH=FLOAT(J)+0.5
      FJMH=FLOAT(J)-0.5
      IF(YEB.LT.FJPH.AND.FJPH.LT.YET) THEN
          TEMP=FJPH-Y0
          ROOT2A=(F2*F2-4.0*F1*F3)*TEMP*TEMP+4.0*F3*F4
          ROOTA=DSQRT(ROOT2A)
          XE1=X0+(-F2*TEMP-ROOTA)/(2.0*F3)
          XE2=X0+(-F2*TEMP+ROOTA)/(2.0*F3)
      ELSE
          XE1=0.0
          XE2=0.0
      ENDIF
      IF(YEB.LT.FJMH.AND.FJMH.LT.YET) THEN
          TEMP=FJMH-Y0
          ROOT2B=(F2*F2-4.0*F1*F3)*TEMP*TEMP+4.0*F3*F4
          ROOTB=DSQRT(ROOT2B)
          XE4=X0+(-F2*TEMP-ROOTB)/(2.0*F3)
          XE3=X0+(-F2*TEMP+ROOTB)/(2.0*F3)
      ELSE
          XE4=0.0
          XE3=0.0
      ENDIF
      RETURN
      END
