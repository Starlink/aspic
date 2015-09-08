      SUBROUTINE MTOWT (WKID,FX,FY)
*+
*
*     ---------
*       MTOWT
*     ---------
*
*     Get metres to world coords. scaling transformation.
*
*  Given;
*   WKID   (I)  Work station identifier.
*
*  Returned;
*   FX,FY  (R)  Metres --> world scaling facors.
*
*  Calculates scaling factor from metres to world coords. and stores
*  in FX, FY. Have to get the scaling factor from the workstation
*  window to workstation viewport that preserves the aspect ratio
*  but uses as much of the workstation viewport as possible.
*
*  D. Tudhope. ROE.   March 1982.
*-
      INTEGER WKID
      REAL    FX,FY
*
*    Window bounds.
*
      REAL WX1,WX2,WY1,WY2
*
*    Workstation window dimensions and bounds.
*
      REAL WWX,WWY,WWX1,WWX2,WWY1,WWY2
*
*    Scaling factor from workstation window to workstation viewport.
*
      REAL WSC
*
*    Dummy variables for parameters not interested in.
*
      LOGICAL B1
      REAL R1,R2,R3,R4,R5,R6
*
*
*    Get window.
*
      CALL GKS_IWVWT (WX1,WY1,WX2,WY2,R1,R2,R3,R4,B1)
*
*    Get workstation window and worstation viewport -
*    (use "requested" Args).
*
      CALL GKS_IWKT (WKID,WWX1,WWY1,WWX2,WWY2,R1,R2,R3,R4,WVX,WVY,
     :               R5,R6)
*
*    Find the ratio for scaling the workstation window to workstation
*    viewport preserving the aspect.
*
      WWX=WWX2-WWX1
      WWY=WWY2-WWY1
      WSC=MIN(WVX/WWX,WVY/WWY)
*
*    Find the X and Y conversion factors for metres --> world coords.
*
      FX=(WX2-WX1)/(WSC*WWX)
      FY=(WY2-WY1)/(WSC*WWY)
*
      END
