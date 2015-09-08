      SUBROUTINE THRESH2(NXIN,NYIN,ARRIN,THRLO,THRHI,NXOUT,NYOUT,ARROUT)
*+
*   THRESH2
*
*   Sets values of an array below and above given thresholds to zero.
*
*   Given      (arguments)
*   NXIN    I   X-dimension of input array
*   NYIN    I   Y-dimension of input array
*   ARRIN   RA  input array
*   THRLO   R   threshold below which values are set to zero
*   THRHI   R   threshold above which values are set to zero
*   NXOUT   I   X-dimension of output array
*   NYOUT   I   Y-dimension of output array
*
*   Returned   (arguments)
*   ARROUT  RA  output array
*
*   B.D.Kelly/ROE/29.1.1982
*-
      INTEGER NXIN,NYIN,NXOUT,NYOUT
      REAL ARRIN(NXIN,NYIN),ARROUT(NXOUT,NYOUT),THRLO,THRHI
*
*    go through the whole image replacing all elements outside
*    the range THRLO to THRHI by 0.0
*
      DO J=1,MIN(NYIN,NYOUT)
         DO I=1,MIN(NXIN,NXOUT)
            IF((ARRIN(I,J).GT.THRLO).AND.(ARRIN(I,J).LT.THRHI)) THEN
               ARROUT(I,J)=ARRIN(I,J)
            ELSE
               ARROUT(I,J)=0.0
            ENDIF
         ENDDO
      ENDDO
*
*    zero any remaining uninitialized elements in output array.
*
      IF(NY2.GT.MIN(NY1,NY2)) THEN
        DO J=MIN(NY1,NY2)+1,NY2
          DO I=1,NX2
            ARROUT(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
      IF(NX2.GT.MIN(NX1,NX2)) THEN
        DO J=1,NY2
          DO I=MIN(NX1,NX2)+1,NX2
            ARROUT(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
      END
