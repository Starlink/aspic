      SUBROUTINE MOVEABS (X,Y)
*+
*
*
*     ---------
*       MOVEABS
*     ---------
*
*
*	GKS - absolute move
*
*  Given (arguments)
*	X,Y (R)		coords of where to move
*
*  D. Tudhope. ROE.  March 1983.
*-

      REAL X,Y
      REAL AX(2),AY(2)

      AX(1)=X
      AX(2)=X
      AY(1)=Y
      AY(2)=Y
      CALL GKS_POLYL(2,AX,AY)
      END
