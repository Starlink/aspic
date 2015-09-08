      SUBROUTINE SUBIM1(NXIN,NYIN,ARRIN,NXST,NXFN,NYST,NYFN,
     :                  NXOUT,NYOUT,ARROUT)
*+
*   SUBIM1
*
*   Extracts a rectangular area from an image
*
*   Given      (arguments)
*   NXIN    I   X-dimension of given image
*   NYIN    I   Y-dimension of given image
*   ARRIN   RA  given image
*   NXST    I   X-coord of leftmost pixel
*   NXFN    I   X-coord of rightmost pixel
*   NYST    I   Y-coord of bottom pixel
*   NYFN    I   Y-coord of top pixel
*   NXOUT   I   X-dimension of output array
*   NYOUT   I   Y-dimension of output array
*
*   Returned   (arguments)
*   ARROUT  RA  output array
*
*   B.D.Kelly/ROE/4.3.1982
*-

      INTEGER NXIN,NYIN,NXST,NXFN,NYST,NYFN,NXOUT,NYOUT
      REAL ARRIN(NXIN,NYIN),ARROUT(NXOUT,NYOUT)

      DO J=NYST,NYFN
         DO I=NXST,NXFN
            ARROUT(I-NXST+1,J-NYST+1)=ARRIN(I,J)
         ENDDO
      ENDDO

      END
