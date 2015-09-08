      SUBROUTINE ELLPSE(A,B,PA,XO,YO,X,Y,N)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE ELLPSE 
C
C
C         This  routine  generates  a  set  of  (x,y)  co-ordinates   which 
C         represent  an  ellipse  at  any angle. The output is suitable for 
C         plotting by any polyline routine. 
C
C         A           REAL  IN    This is the size of the longest  diameter 
C                                 of the ellipse. 
C
C         B           REAL  IN    This is the size of the shortest diameter 
C                                 of the ellipse. 
C
C         PA          REAL  IN    This is the position angle of  the  major 
C                                 axis in radians. 
C
C         XO          REAL  IN    This is the x co-ordinate of  the  centre 
C                                 of the ellipse. 
C
C         YO          REAL  IN    This is the y co-ordinate of the ellipse. 
C
C         X           REAL  OUT   This is the array (of dimension N)  which 
C                                 contains the x co-ordinates whi have been 
C                                 generated. 
C
C         Y           REAL  OUT   This is the array (of dimension N)  which 
C                                 contains the y co-ordinates whi have been 
C                                 generated. 
C
C         N           INT   IN    This defines the number of points  to  be 
C                                 generated. 
C
C
C         M J Currie               RGO-Starlink                   24-MAY-82 
C
C-------------------------------------------------------------------------- 

      REAL X(N),Y(N)                       
      DATA PI2/6.2831853/
      STEP=PI2/REAL(N-1)
      X(1)=XO+A*COS(PA)
      Y(1)=YO+A*SIN(PA)
      DO 100 I=2,N
         TH=STEP*REAL(I-1)
         X(I)=XO+A*COS(PA)*COS(TH)-B*SIN(PA)*SIN(TH)
         Y(I)=YO+A*SIN(PA)*COS(TH)+B*COS(PA)*SIN(TH)
  100 CONTINUE
      END
