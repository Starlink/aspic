      SUBROUTINE CROSS(X,Y)

C+
C    CROSS
C
C	draws a cross  at (x,y) using gks
C	size is a constant fraction of current window
C
C    Given (arguments)
C	X,Y (R)			bottom of cross
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      REAL X,Y
      REAL AX(2),AY(2)
C*  vars for getting window size
      LOGICAL B
      REAL R1,R2,R3,R4
      REAL WXP,WYP,WXQ,WYQ,DX,DY

      CALL GKS_IWVWT(WXP,WYP,WXQ,WYQ,R1,R2,R3,R4,B)
      DX=(WXQ-WXP)/20.0
      DY=(WYQ-WYP)/9.0
      AX(1)=X-DX
      AY(1)=Y
      AX(2)=X+DX
      AY(2)=Y+DY
      CALL GKS_POLYL(2,AX,AY)
      AX(1)=X-DX
      AY(1)=Y+DY
      AX(2)=X+DX
      AY(2)=Y
      CALL GKS_POLYL(2,AX,AY)
      END
