      SUBROUTINE CIRFIL(XCEN,YCEN,RAD,RVAL,IXEXT,IYEXT,ARRAY)
*+
*   CIRFIL
*
*   Fills a circular area of an image with a given value
*
*   Given         (arguments)
*   XCEN     R     X-coordinate of circle centre
*   YCEN     R     Y-coordinate of circle centre
*   RAD      R     Radius of circle
*   RVAL     R     value to be put into pixels within the circle
*   IXEXT    I     X-dimension of image array
*   IYEXT    I     Y-dimension of image array
*   ARRAY    RA    image array
*
*   Returned      (arguments)
*   ARRAY    RA    image array
*
*   B.D.Kelly/ROE/9.12.1981  mod. D. Tudhope to add 1
*-

      INTEGER IXEXT,IYEXT,IXCEN,IYCEN,IRAD
      REAL ARRAY(IXEXT,IYEXT)
      REAL XCEN,YCEN,RAD,RVAL,KR


C*  arrays start at 1 not 0
      IXCEN=NINT(XCEN+1)
      IYCEN=NINT(YCEN+1)
      IRAD=NINT(RAD)
      JMIN=MAX(1,IYCEN-IRAD)
      JMAX=MIN(IYEXT,IYCEN+IRAD)
      IMIN=MAX(1,IXCEN-IRAD)
      IMAX=MIN(IXEXT,IXCEN+IRAD)

      DO J=JMIN,JMAX
         DO I=IMIN,IMAX
            KR=SQRT(REAL((I-IXCEN)**2+(J-IYCEN)**2))
            IF(KR.LE.RAD) ARRAY(I,J)=RVAL
         ENDDO
      ENDDO

      END
