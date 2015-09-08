      SUBROUTINE MOMCAL(ANALYSER,ICAT,KK1,MXLEN,NUMLEN,NUMPAR,
     :                  STORE,YMAX)
*+
*   MOMCAL
*
*   Calculate the normalized, centralized moments of an image
*
*   Given      (arguments)
*   ICAT    I   catalogue number of the object
*   KK1     I   index of the object in the STORE array
*   MXLEN   I   X-dimension of image frame
*   NUMLEN  I   maximum possible number of objects
*   NUMPAR  I   maximum possible number of object parameters
*   STORE   DA  store for accumulating object parameters
*   YMAX    I   maximum Y-coordinate in object
*
*   Returned   (arguments)
*   ANALYSER RA final object parameters
*
*   B.D.Kelly/ROE/15.3.1982
*-

      INTEGER ICAT,KK1,MXLEN,NUMLEN,NUMPAR,YMAX
      REAL ANALYSER(NUMPAR,NUMLEN)
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2)

      DOUBLE PRECISION XCEN,YCEN,XICEN,YICEN,SNXX,SNXY,SNYY
      DOUBLE PRECISION SNIXX,SNIXY,SNIYY,ADXY,ASXY,AXY,ZXY
      DOUBLE PRECISION ADIXY,ASIXY,AIXY,ZIXY,EPSLON

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)

      PARAMETER (EPSLON=1.0D-4,RADDEG=57.2958)

      XCEN=STORE(SUMX,KK1)/STORE(AREA,KK1)
      YCEN=STORE(SUMY,KK1)/STORE(AREA,KK1)
      XICEN=STORE(SIX,KK1)/STORE(SUMI,KK1)
      YICEN=STORE(SIY,KK1)/STORE(SUMI,KK1)
      SNXX=STORE(SXX,KK1)/STORE(AREA,KK1)-XCEN*XCEN
      SNXY=STORE(SXY,KK1)/STORE(AREA,KK1)-XCEN*YCEN
      SNYY=STORE(SYY,KK1)/STORE(AREA,KK1)-YCEN*YCEN
      SNIXX=STORE(SIXX,KK1)/STORE(SUMI,KK1)-XICEN*XICEN
      SNIXY=STORE(SIXY,KK1)/STORE(SUMI,KK1)-XICEN*YICEN
      SNIYY=STORE(SIYY,KK1)/STORE(SUMI,KK1)-YICEN*YICEN
*
*
*   calculate the unweighted semi - major & semi - minor axes &
*   orientation from the unweighted second moments .
*     EPSLON is used to check for singular points or indeterminacy
*     and has the value 0.0001
*  --------------------------------------------------------------------
*
*
      ADXY=ABS(SNXX-SNYY)
      ASXY=ABS(SNXX+SNYY)
      AXY=ABS(SNXY)
      IF((ABS(SNXY).GT.EPSLON).OR.(ABS(SNXX-SNYY).GT.EPSLON)) THEN
         THETAU=0.5*RADDEG*ATAN2(2.0D0*SNXY,(SNXX-SNYY))
      ELSE
         THETAU= 0.0
      ENDIF
      IF(THETAU.LT.0.0) THETAU=THETAU+180.0
      ANALYSER(5,ICAT)=THETAU

      ZXY=SQRT(ADXY*ADXY+4.0D0*AXY*AXY)
      ANALYSER(3,ICAT)=SQRT(2.0D0*(ASXY+ZXY))
      IF(ASXY.GT.ZXY) THEN
         ANALYSER(4,ICAT)=SQRT(2.0D0*(ASXY-ZXY))
      ELSE
         ANALYSER(4,ICAT)=0.0
      ENDIF
*
*
*  calculate the weighted semi - major & semi - minor axes and
*  orientation from the weighted second moments .
* -------------------------------------------------------------
*
*
      ADIXY=ABS(SNIXX-SNIYY)
      ASIXY=ABS(SNIXX+SNIYY)
      AIXY=ABS(SNIXY)
      IF((ABS(SNIXY).GT.EPSLON).OR.(ABS(SNIXX-SNIYY).GT.EPSLON))
     :                                  THEN
         THETAI=0.5*RADDEG*ATAN2((2.0D0*SNIXY),(SNIXX-SNIYY))
      ELSE
         THETAI=0.0
      ENDIF
      IF(THETAI.LT.0.0) THETAI=THETAI+180.0
      ANALYSER(10,ICAT)=THETAI

      ZIXY=SQRT(ADIXY*ADIXY+4.0D0*AIXY*AIXY)
      ANALYSER(8,ICAT)=SQRT(2.0D0*(ASIXY+ZIXY))
      IF(ASIXY.GT.ZIXY) THEN
         ANALYSER(9,ICAT)=SQRT(2.0D0*(ASIXY-ZIXY))
      ELSE
         ANALYSER(9,ICAT)=0.0
      ENDIF
*
*   centroids, area, sum of intensities and maximum intensity
*
      ANALYSER(1,ICAT)=-0.5+XCEN
      ANALYSER(2,ICAT)=0.5+FLOAT(YMAX)-STORE(YEXT,KK1)+YCEN
      ANALYSER(6,ICAT)=-0.5+XICEN
      ANALYSER(7,ICAT)=0.5+FLOAT(YMAX)-STORE(YEXT,KK1)+YICEN
      ANALYSER(12,ICAT)=STORE(SUMI,KK1)
      ANALYSER(11,ICAT)=STORE(AREA,KK1)
      ANALYSER(13,ICAT)=STORE(INTMAX,KK1)

      END
