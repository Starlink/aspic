      SUBROUTINE RECTAN(ID,X,Y)
C
C
C
      REAL X(2),Y(2)
      REAL XP(5),YP(5)
C
C
C
      XP(1)=X(1)
      YP(1)=Y(1)
      XP(2)=X(2)
      YP(2)=Y(1)
      XP(3)=X(2)
      YP(3)=Y(2)
      XP(4)=X(1)
      YP(4)=Y(2)
      XP(5)=XP(1)
      YP(5)=YP(1)
      CALL ARGS_POLYL(ID,5,XP,YP,KSTAT)
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R CROSS *
C      *           *
C      *************
C
C S/R TO DRAW CROSS ON ARGS AT POSITION (USER UNITS) (X,Y) OF WIDTH 5 PI
C   (ID IS ID OF IMAGE)
C
C --------------------------------------------------------
C
C
C
