      SUBROUTINE UPDATE(KK1,KKL,KKS,MXLEN,STORE)
*+
*   UPDATE
*
*   Merge object parameters of two previously separate objects
*
*   Given      (arguments)
*   KK1     I   index to set of data parameters to be overwritten
*               by merged values
*   KKL     I   index to data parameters with largest Y-extent
*   KKS     I   index to data parameters with smallest Y-extent
*   MXLEN   I   X-dimension of image frame
*   STORE   DA  data storage area
*
*   Returned   (arguments)
*   STORE   DA  data storage area
*
*   B.D.Kelly/ROE/15.3.1982
*-
      INTEGER KK1,KKL,KKS,MXLEN
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2),YDIFF

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)

*
*   In general, KK1 is equal to either KKL or KKS, therefore
*   the image parameters have to be updated in the correct order
*
      STORE(XMAX,KK1)=MAX(STORE(XMAX,KKL),STORE(XMAX,KKS))
      STORE(XMIN,KK1)=MIN(STORE(XMIN,KKL),STORE(XMIN,KKS))
      STORE(INTMAX,KK1)=MAX(STORE(INTMAX,KKL),STORE(INTMAX,KKS))
      YDIF=STORE(YEXT,KKL)-STORE(YEXT,KKS)
      STORE(SIXY,KK1)=STORE(SIXY,KKL)+STORE(SIXY,KKS)+
     :                STORE(SIX,KKS)*YDIF
      STORE(SIYY,KK1)=STORE(SIYY,KKL)+STORE(SIYY,KKS)+
     :                STORE(SUMI,KKS)*YDIF**2+2.0*STORE(SIY,KKS)*YDIF
      STORE(SIY,KK1)=STORE(SIY,KKL)+STORE(SIY,KKS)+STORE(SUMI,KKS)*YDIF
      STORE(SXY,KK1)=STORE(SXY,KKL)+STORE(SXY,KKS)+STORE(SUMX,KKS)*YDIF
      STORE(SYY,KK1)=STORE(SYY,KKL)+STORE(SYY,KKS)+
     :               STORE(AREA,KKS)*YDIF**2+2.0*STORE(SUMY,KKS)*YDIF
      STORE(SUMY,KK1)=STORE(SUMY,KKS)+STORE(SUMY,KKL)+
     :                STORE(AREA,KKS)*YDIF
      STORE(AREA,KK1)=STORE(AREA,KKL)+STORE(AREA,KKS)
      STORE(SUMI,KK1)=STORE(SUMI,KKL)+STORE(SUMI,KKS)
      STORE(SUMX,KK1)=STORE(SUMX,KKL)+STORE(SUMX,KKS)
      STORE(SIX,KK1)=STORE(SIX,KKL)+STORE(SIX,KKS)
      STORE(SXX,KK1)=STORE(SXX,KKL)+STORE(SXX,KKS)
      STORE(SIXX,KK1)=STORE(SIXX,KKL)+STORE(SIXX,KKS)
      STORE(YEXT,KK1)=STORE(YEXT,KKL)

      END
