      SUBROUTINE LINFIT(NP,X,Y,GRAD,CONS)                                       
C+
C   LINFIT
C
C     STRAIGHT LINE FIT:  OBTAINS GRAD AND CONS ONLY
C
C   Given      (arguments)
C   NP          NUMBER OF POINTS
C   X           REAL ARRAY OF X
C   Y           REAL ARRAY OF Y
C
C   Returned   (arguments)
C   GRAD        GRADIENT
C   CONS        CONSTANT   ( Y = GRAD*X + CONS )
C
C   J.A.Cooke/UOE/1981
C-
      REAL X(NP),Y(NP)
      XM=0.
      YM=0.
      RP=NP
      DO10I=1,NP
      XM=XM+X(I)
 10   YM=YM+Y(I)
      XM=XM/RP
      YM=YM/RP
      RNUM=0.
      RDEN=0.
      DO20I=1,NP
      XIMXM=X(I)-XM
      RNUM=RNUM+XIMXM*Y(I)
 20   RDEN=RDEN+XIMXM*XIMXM
      GRAD=RNUM/RDEN
      CONS=YM-GRAD*XM
      RETURN
      END
C
C
C
C
C
C***************************************************************
C***************************************************************
