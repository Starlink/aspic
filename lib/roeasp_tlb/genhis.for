      SUBROUTINE GENHIS(ARRAY,ILEN,JLEN,ISTART,IFIN,JSTART,JFIN,
     &                  RMAX,RMIN,JHIST,NBIN)
*+
*     GENHIS
*
*     Computes truncated histogram of a rectangular area within
*     2-D array.
*
*     Given      (arguments)
*     ARRAY       data array
*     ILEN        X-extent of array
*     JLEN        Y-dimension of array
*     ISTART      X start of area within array
*     IFIN        X finish of area within array
*     JSTART      Y start of area within array
*     JFIN        Y finish of area within array
*     RMAX        value of maximim bin in histogram
*     RMIN        zero point of histogram
*     NBIN        number of bins in histogram
*
*     Returned      (arguments)
*     JHIST          histogram
*
*     B.D.Kelly/ROE/18/9/1981
*-
      INTEGER ILEN,JLEN,ISTART,IFIN,JSTART,JFIN,NBIN
      REAL ARRAY(ILEN,JLEN)
      INTEGER JHIST(NBIN)
      REAL RMAX,RMIN,SCALE
 
      DO I=1,NBIN
        JHIST(I)=0
      ENDDO
*
*   Set scaling factor
*
      IF(ABS(RMAX-RMIN).GT.1.0E-20) THEN
         SCALE=RMAX-RMIN
      ELSE
         SCALE=1.0
      ENDIF
*
*   Calculate histogram
*
      DO J=JSTART,JFIN
        DO I=ISTART,IFIN
          KTEMP=NBIN*(ARRAY(I,J)-RMIN)/SCALE
          KTEMP=MAX(1,KTEMP)
          KTEMP=MIN(NBIN,KTEMP)
          JHIST(KTEMP)=JHIST(KTEMP)+1
        ENDDO
      ENDDO
 
      END
