      SUBROUTINE HISDATA(NX,NY,ARRAY,ISTART,IFIN,JSTART,JFIN)
*+
*   HISDATA
*
*     Calculate histogram of current image, compute statistical
*     properties from it.
*
*   Given      (arguments)
*   NX          X-dimension of image
*   NY          Y-dimension of image
*   ARRAY       image
*   ISTART      index of left edge of area to be analysed
*   IFIN        index of right edge of area
*   JSTART      index of bottom edge of area
*   JFIN        index of top edge of area
*
*     Subroutine calls :
*      HISTPROP,GENHIS,READC               : E2DLIB
*      WRUSER   : STARLINK
*
*       D.R.K.BROWNRIGG/ROE/1982 FROM B.D.KELLY-HIST1
*-
      CHARACTER*10 TBUF
      CHARACTER*60 PROCH
      INTEGER NX,NY,ISTART,IFIN,JSTART,JFIN
      INTEGER JHIST(2048)
      REAL ARRAY(NX,NY)
*
*     CALCULATE RANGE OF DATA
*
      VALMAX=ARRAY(ISTART,JSTART)
      VALMIN=VALMAX
      DO J=JSTART,JFIN
        DO I=ISTART,IFIN
          VALMAX=MAX(VALMAX,ARRAY(I,J))
          VALMIN=MIN(VALMIN,ARRAY(I,J))
        ENDDO
      ENDDO
*
*     CALCULATE DEFAULT HISTOGRAM
*
      CALL GENHIS(ARRAY,NX,NY,ISTART,IFIN,JSTART,JFIN,
     &            VALMAX,VALMIN,JHIST,2048)
      ARMAX=VALMAX
      ARMIN=VALMIN
      CALL HISTPROP(JHIST,2048,VALMAX,VALMIN,RSUM,RMEAN,RMED,RMODE)
*
*   Write out properties of the distribution
*
      WRITE(PROCH,'(''MAXIMUM VALUE'',F20.3)') VALMAX
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''MINIMUM VALUE'',F20.3)') VALMIN
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''SUM'',F20.3)') RSUM
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''MEAN'',F20.3)') RMEAN
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''MEDIAN'',F20.3)') RMED
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''MODE'',F20.3)') RMODE
      CALL WRUSER(PROCH,ISTS)
*
*     FINISHED
*
 
      END
