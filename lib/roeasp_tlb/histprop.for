      SUBROUTINE HISTPROP(JHIST,NBINS,VALMAX,VALMIN,                            
     &                    RSUM,RMEAN,RMED,RMODE)
C+
C     HISTPROP
C
C     Calculates the statistical properties of an image from
C     its histogram.
C
C     Given      (arguments)
C     JHIST       histogram
C     NBINS       size of histogram
C     VALMAX      maximum value in original image
C     VALMIN      minimum value in original image
C
C     Returned   (arguments)
C     RSUM        sum of all values in image
C     RMEAN       mean value of image
C     RMED        median value of image
C     RMODE       mode of image
C
C      B.D.Kelly/ROE/4.9.1981
C-
C
C     It is assumed that the histogram bins are narrow enough
C     that computing the sum of the pixel values directly from
C     the histogram produces negligible error.
C     The median will be accurate to one bin-width.
C     The mode is calculated from the approximation
C       mode=3*mean-2*median
C
C
      INTEGER NBINS
      INTEGER JHIST(NBINS)
      REAL VALMAX,VALMIN,RSUM,RMEAN,RMED,RMODE
C
C   Calculate sum and mean of data
C
      RNUM=0.0
      RSUM=0.0
      DO I=1,NBINS
        RSUM=RSUM+FLOAT(I)*FLOAT(JHIST(I))
        RNUM=RNUM+FLOAT(JHIST(I))
      ENDDO
 
      RSUM=RNUM*VALMIN+RSUM*(VALMAX-VALMIN)/NBINS
      RMEAN=RSUM/RNUM
C
C   Find median and estimate mode
C
      HSUM=0.0
      I=1
      DO WHILE((HSUM.LT.(RNUM/2.0)).AND.(I.LT.NBINS))
        HSUM=HSUM+JHIST(I)
        I=I+1
      ENDDO
 
      RMED=VALMIN+I*(VALMAX-VALMIN)/NBINS
      RMODE=3.0*RMED-2.0*RMEAN
 
      END
C***************************************************************
C***************************************************************
