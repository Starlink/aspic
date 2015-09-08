      SUBROUTINE ARROW(X,Y)

C+
C    ARROW
C
C	draws an arrow at (x,y) using gks
C	size of constant fraction of window
C
C    Given (arguments)
C	X,Y (R)			bottom of arrow
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      REAL X,Y
      REAL AX(5),AY(5)
C*  vars for getting window size
      LOGICAL B
      REAL R1,R2,R3,R4
      REAL WXP,WYP,WXQ,WYQ,DX,DY1,DY2

      CALL GKS_IWVWT(WXP,WYP,WXQ,WYQ,R1,R2,R3,R4,B)
      DX=(WXQ-WXP)/25.0
      DY1=(WYQ-WYP)/9.0
      DY2=(WYQ-WYP)/15.0
      AX(1)=X
      AY(1)=Y
      AX(2)=X
      AY(2)=Y+DY1
      AX(3)=X-DX
      AY(3)=Y+DY2
      AX(4)=X+DX
      AY(4)=Y+DY2
      AX(5)=X
      AY(5)=Y+DY1
      CALL GKS_POLYL(5,AX,AY)
      END
