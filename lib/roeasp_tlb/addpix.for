      SUBROUTINE ADDPIX(BCKGND,I,MXLEN,VALPIX,STORE)
*+
*   ADDPIX
*
*   Merge the values for a single pixel with the top image
*   on the object stack
*
*   Given      (arguments)
*   BCKGND  R   sky background value
*   I       I   pixel number along current scan
*   MXLEN   I   X-dimension of image frame
*   VALPIX  R   value of current pixel
*   STORE   DA  store for accumulating object parameters
*
*   Given      (stacks)
*   OBSTAK      objects currently being traversed
*
*   Returned   (arguments)
*   STORE   DA  store for accumulating object parameters
*
*   Subroutines called :
*   GETSTK
*
*   B.D.Kelly/ROE/15.3.1982
*-


      INTEGER OBJECT(3),I,MXLEN
      DOUBLE PRECISION STORE(16,(MXLEN+1)/2),XPOS,PIXVAL,DIFINT
      REAL BCKGND,VALPIX

      INTEGER AREA,INTMAX,SIX,SIXX,SIXY,SIY,SIYY,SUMI
      INTEGER SUMX,SUMY,SXX,SXY,SYY,XMAX,XMIN,YEXT

      PARAMETER (AREA=1,INTMAX=2,SIX=3,SIXX=4)
      PARAMETER (SIXY=5,SIY=6,SIYY=7,SUMI=8)
      PARAMETER (SUMX=9,SUMY=10,SXX=11,SXY=12)
      PARAMETER (SYY=13,XMAX=14,XMIN=15,YEXT=16)

      XPOS=DBLE(I)
      PIXVAL=DBLE(VALPIX)
      DIFINT=PIXVAL-DBLE(BCKGND)
*
*   Find index to parameters for current object
*
      CALL GETSTK('OBSTAK',OBJECT)
      KK1=OBJECT(3)
*
*   Update parameter values
*
      STORE(XMAX,KK1)=MAX(STORE(XMAX,KK1),XPOS)
      STORE(XMIN,KK1)=MIN(STORE(XMIN,KK1),XPOS)
      STORE(INTMAX,KK1)=MAX(STORE(INTMAX,KK1),PIXVAL)
      STORE(AREA,KK1)=STORE(AREA,KK1)+1.0
      STORE(SUMI,KK1)=STORE(SUMI,KK1)+DIFINT
      STORE(SUMX,KK1)=STORE(SUMX,KK1)+XPOS
      STORE(SUMY,KK1)=STORE(SUMY,KK1)+STORE(YEXT,KK1)
      STORE(SIX,KK1)=STORE(SIX,KK1)+DIFINT*XPOS
      STORE(SIY,KK1)=STORE(SIY,KK1)+STORE(YEXT,KK1)*DIFINT
      STORE(SXX,KK1)=STORE(SXX,KK1)+XPOS**2
      STORE(SXY,KK1)=STORE(SXY,KK1)+XPOS*STORE(YEXT,KK1)
      STORE(SYY,KK1)=STORE(SYY,KK1)+STORE(YEXT,KK1)**2
      STORE(SIXX,KK1)=STORE(SIXX,KK1)+DIFINT*XPOS**2
      STORE(SIXY,KK1)=STORE(SIXY,KK1)+STORE(YEXT,KK1)*XPOS*DIFINT
      STORE(SIYY,KK1)=STORE(SIYY,KK1)+DIFINT*STORE(YEXT,KK1)**2

      END
