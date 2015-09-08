      SUBROUTINE ASP_HGRAM (PARAM,INPUT,SIZE,HMIN,HMAX,NUMBIN,HGRAM,
     :    STATUS)

*+  ASP_HGRAM
*
*   Calculate histogram of real BDF in integer array. If it has already
*   been calculated, values are retrieved from frame descriptors.
*   Otherwise, they are calculated and, if possible, the frame descriptors
*   are written. (If the histogram has already been written, but a
*   different range of intensity (HMIN,HMAX) or number of bins NUMBIN
*   is requested, it will have to be recalculated.
*
*   Given:
*     PARAM   C   program parameter name corresponding to BDF
*     INPUT   RA  BDF data (arbitrary dimension)
*     SIZE    I   size of INPUT
*     HMIN    R   intensity corr. to lh end of first bin
*     HMAX    R   intensity corr. to rh end of last bin
*     NUMBIN  I   no of bins in histogram (size of HGRAM)
*
*  Returned:
*     HGRAM   IA  histogram
*     STATUS  I   return status (Starlink)
*
*  Called:
*     ASP_RDDSI, ASP_RDDSR, ASP_WRDSI: ASPIC
*
*  W.F.Lupton RGO 23 Oct 1981
*-

      INCLUDE 'INTERIM(ERRPAR)'

      INTEGER SIZE,NUMBIN,HGRAM(NUMBIN),STATUS,OBIN,NVALS,TSTAT
      REAL INPUT(SIZE),HMIN,HMAX,OHMIN,OHMAX
      CHARACTER PARAM*(*)

*   Try and read histogram from descriptor HGRAM
      CALL ASP_RDDSI (PARAM,'HGRAM',NUMBIN,HGRAM,OBIN,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
          CALL ASP_RDDSR (PARAM,'HMIN',1,OHMIN,NVALS,TSTAT)
          CALL ASP_RDDSR (PARAM,'HMAX',1,OHMAX,NVALS,TSTAT)
      ELSE
          OBIN = 0
          OHMIN = 0.0
          OHMAX = 0.0
      ENDIF

*   If stored histogram is wrong size or has wrong intensity range
*   then must recalculate
      IF (OBIN.NE.NUMBIN.OR.ABS(OHMIN-HMIN).GT.1E-3.OR.
     :    ABS(OHMAX-HMAX).GT.1E-3) THEN

*       calculate histogram (first clear out bins)
          DO I=1,NUMBIN
              HGRAM(I) = 0
          ENDDO
          SCALE = NUMBIN / MAX (HMAX-HMIN,1E-18)
          DO I=1,SIZE
              V = INPUT(I)
              J = (V-HMIN) * SCALE + 1
              IF (1.LE.J.AND.J.LE.NUMBIN) THEN
                  HGRAM(J) = HGRAM(J) + 1
              ENDIF
          ENDDO

*       write descriptors
          CALL ASP_WRDSR (PARAM,'HMIN',HMIN,1,STATUS)
          CALL ASP_WRDSR (PARAM,'HMAX',HMAX,1,STATUS)
          CALL ASP_WRDSI (PARAM,'HGRAM',HGRAM,NUMBIN,STATUS)
          STATUS = ERR_NORMAL
      ENDIF

      END
