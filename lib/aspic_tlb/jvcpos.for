      SUBROUTINE JVCPOS(XA,YA,XX,YY,STSAM,STREC,
     1                 ENSAM,ENREC)
      COMMON/JBJUD/K,MISS,XL,X1,X2,XS,YL,Y1,Y2,YS,
     1    F,S,MS(2),GAP,IPOL,G(2)
      COMMON/JBPT/X,Y,INSIDE,M
      CALL JBNRNG(STSAM,STREC)
      AX = X
      AY = Y
      CALL JBNRNG(ENSAM,ENREC)
      BX = X
      BY = Y
      XX = (XA-AX)/(BX-AX)*(ENSAM-STSAM)+STSAM
      YY = (YA-AY)/(BY-AY)*(ENREC-STREC)+STREC
      RETURN
      END