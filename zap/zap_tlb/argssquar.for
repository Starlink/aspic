      SUBROUTINE ARGS_SQUARE(X,Y,ID)
      INTEGER X(2),Y(2)
      REAL XP(5),YP(5)
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
C     CALL ARGS_VSRRST
      CALL ARGS_OVOP(8,'G')
C     CALL ARGS_CLS(8)
      CALL ARGS_POLYL(ID,5,XP,YP,ISTAT)
      CALL ARGS_OVCL(8,.FALSE.)
      END
