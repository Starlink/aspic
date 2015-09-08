      SUBROUTINE GAUXY(ARRAY,NX,NY,XO,YO,SX,SY,ITX,ITY,
     :                 XMARGE,YMARGE)
*+
*     GAUXY
*
*     performs gaussian fit on a stellar image
*
*     Given      (arguments)
*     ARRAY   RA  Data array containing stellar image near its centre
*     NX      I   X-dimension of data array (typically 40)
*     NY      I   Y-dimension of data array (typically 40)
*     XMARGE  RA  Workspace array for X-marginal
*     YMARGE  RA  Workspace array for Y-marginal
*
*     Returned      (arguments)
*     XO      R      X-coordinate of image centre
*     YO      R      Y-coordinate of image centre
*     SX      R      Error estimate for XO
*     SY      R      Error estimate for YO
*     ITX     I      No. of iterations for XO
*     ITY     I      No. of iterations for YO
*
*     Calls : ASTROM,LOCATE
*
*     B.D.Kelly/ROE/1981
*     D.W.T.Baines/ROE/JAN 1983
*-
*
*     program lifted from Astronomical Image Processing
*     Circular 5
*     Jan 80
*     Peter B Stetson
*     Yale
*     New Haven Connecticut USA
*
*     this program inputs scans in and around stellar images,
*     locates the stars in the scan arrays and computes
*     astrometric positions in two coordinates 
*
      INTEGER NX,NY,ITX,ITY
      INTEGER JX,JY,LX,LY,ICROWDX,ICROWDY
      REAL ARRAY(NX,NY)
      REAL XO,YO,SX,SY
      REAL XMARGE(NX),YMARGE(NY)
      REAL VARX,VARY,DOX,DOY,BX,BY
*
*     compute X-location with the upper and lower Y-limits for
*     first determination of X-marginal set at the 1/4 and 3/4 points.
*     then iterate, computing new Y- and X-locations.
*
      YO=NY/2
      SY=NY/8
      JY=1
      LY=NX
      DO JITER=1,2
         CALL LOCATE(ARRAY,NX,NY,XMARGE,YO,SY,JY,LY,
     :               XO,SX,JX,LX,ICROWDX,0)
         CALL LOCATE(ARRAY,NX,NY,YMARGE,XO,SX,JX,LX,
     :               YO,SY,JY,LY,ICROWDY,1)
      ENDDO
*
*     perform astrometric fits in X and Y
*
      CALL ASTROM(XMARGE,JX,LX,XO,VARX,SX,DOX,BX,ITX)
      CALL ASTROM(YMARGE,JY,LY,YO,VARY,SY,DOY,BY,ITY)
*
*     ITX and ITY are the number of iterations required in the astrometric 
*     fits .
*     if ITX or ITY = 0 the solution failed to converge.
*     if neither solution converged, the star is worthless.
*     if one coordinate converged but not the other, the program makes one
*     last attempt to compute, search, and fit the missing marginal.
*     if the star now has two good coordinates for the centre, processing
*     continues; otherwise, it sets SX=SY=40.0, and exits.
*
      IF((ITX.EQ.0).AND.(ITY.NE.0)) THEN
         CALL LOCATE(ARRAY,NX,NY,XMARGE,YO,SY,JY,LY,
     :               XO,SX,JX,LX,ICROWDX,0)
         CALL ASTROM(XMARGE,JX,LX,XO,VARX,SX,DOX,BX,ITX)
         IF(ITX.EQ.0) ITY=0
      ELSE IF((ITY.EQ.0).AND.(ITX.NE.0)) THEN
         CALL LOCATE(ARRAY,NX,NY,YMARGE,XO,SX,JX,LX,
     :               YO,SY,JY,LY,ICROWDY,1)
         CALL ASTROM(YMARGE,JY,LY,YO,VARY,SY,DOY,BY,ITY)
         IF(ITY.EQ.0) ITX=0
      ENDIF
      IF((ITX.EQ.0).AND.(ITY.EQ.0)) THEN
         SX=40.0
         SY=40.0
      ENDIF
      END
