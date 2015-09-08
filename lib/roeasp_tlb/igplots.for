      SUBROUTINE IGPLOTS(IXEXT,IYEXT,PARAMS,MAXDIM,IXR,IYR)
C+
C   IGPLOTS
C
C	does graphics plot of IAM images on args quadrant
C    called from IGPLOT
C
C   Given         (arguments)
C   IXEXT,IYEXT    (I)   dimensions of PARAMS
C   PARAMS         (RA)  array with images found by TVANAL
C   MAXDIM    (R)    larger dimension of original image for versatek window
C   IXR,IYR     (I)    quadrant on args
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

      INTEGER IXEXT,IYEXT,IXR,IYR
      REAL PARAMS(IXEXT,IYEXT)
      REAL MAXDIM
      INTEGER K
      REAL XPOS,YPOS,AXMAJ,AXMIN,ECC,THETA

*   Plot ellipses matching IAM parameters
*   Images of area less than 2 are not plotted.

      CALL SRINIT(0,.FALSE.,ISTATUS)
      CALL ARGSET(0)
C*  try and set up args with a viewport on correct quadrant (hopefully !)
C*  assume TVANAL's parameters are in range 1..maxdim not 0..maxdim-1
      CALL WINDOL(1.0,MAXDIM,1.0,MAXDIM)
      XR=IXR/512.0
      YR=IYR/512.0
C*  ixr,iyr either 0 or 256, so xr,yr either 0.0 or 0.5 - args vport 0..1
      CALL VUPORT(XR,XR+0.5,YR,YR+0.5)
      DO K=1,IYEXT
        IF(PARAMS(11,K).GT.2.0) THEN
          XPOS=PARAMS(1,K)
          YPOS=PARAMS(2,K)
          AXMAJ=PARAMS(3,K)*2.0
          AXMIN=PARAMS(4,K)*2.0
          ECC=SQRT(AXMAJ**2-AXMIN**2)/AXMAJ
          THETA=PARAMS(10,K)/57.2958
          CALL ELLIPSE(XPOS,YPOS,AXMAJ,ECC,THETA)
        ENDIF
      ENDDO
      CALL DEVEND
      END
