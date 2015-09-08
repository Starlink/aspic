      SUBROUTINE RAPGAU(SIGMA,NPIX,ISIZE,JSIZE,ARRAY,
     &                  ROLL,RBUF,WEIGHT,RMARGE)
C+
C      RAPGAU
C
C      RAPID 2-D GAUSSIAN SMOOTH
C
C      Replaces each pixel of a 2-D array by the weighted mean of
C      the NPIX*NPIX pixels centred on it. NPIX is odd.
C
C
C      Given       (arguments)
C      SIGMA        sigma of gaussian smooth, in pixels
C      NPIX         size of square smoothing area; odd no.
C      ARRAY        data array
C      ISIZE        array X-dimension
C      JSIZE        array Y-dimension
C
C      Returned    (arguments)
C      ARRAY        smoothed data array
C      ROLL         workspace for 2-D rolling buffer
C      RBUF         workspace for 1-D rolling buffer
C      WEIGHT       workspace for weights
C      RMARGE       workspace for marginal sum of ROLL
C
C      B.D.Kelly/ROE/5.8.1981
C-
C
C
C      The algorithm is :
C
C
C      initialize 2-D rolling buffer as the bottom part of
C      the data array, centred on the row 'below' the bottom
C      of the data array, padded by duplicating the bottom
C      row of the array
C
C      for each row of the data array
C        if clear of top edge
C          update 2-D rolling buffer to contain the NPIX
C          original data rows centred on the current row
C        else
C          propogate latest row added to 2-D rolling buffer
C          by 1 row
C        endif
C
C        put the column weighted means of the 2-D rolling buffer
C        into a 1-D array
C        initialize 1-D rolling buffer
C        for each point in the 1-D array
C          if clear of end
C            update 1-D rolling buffer to contain the NPIX
C            elements centred on the current element
C          else
C            propogate latest element added to 1-D rolling buffer
C            by 1 element
C          endif
C          replace the corresponding point in the output data array
C          by weighted mean of 1-D rolling buffer
C        endfor
C      endfor
C
C
C
      INTEGER NPIX,ISIZE,JSIZE,ISTAT
      REAL ARRAY(ISIZE,JSIZE),ROLL(ISIZE,NPIX)
      REAL RMARGE(ISIZE),WEIGHT(NPIX),RBUF(NPIX)
 
C
C   Initialize weighting function and normalize
C
      SUM=0.0
      DO I=1,NPIX
         WEIGHT(I)=EXP(-0.5*(I-NPIX/2-1)**2/SIGMA**2)
         SUM=SUM+WEIGHT(I)
      ENDDO
      DO I=1,NPIX
         WEIGHT(I)=WEIGHT(I)/SUM
      ENDDO
C
C   initialize 2-D rolling buffer
C
      DO J=1,(NPIX+1)/2+1
        DO I=1,ISIZE
          ROLL(I,J)=ARRAY(I,1)
        ENDDO
      ENDDO
 
      DO J=(NPIX+1)/2+2,NPIX
        DO I=1,ISIZE
          ROLL(I,J)=ARRAY(I,1+J-(NPIX+1)/2)
        ENDDO
      ENDDO
C
C   calculate the means row-by-row
C
      DO J=1,JSIZE
C
C     calculate index of oldest row in 2-D buffer
C
        JROLL=MOD(J-1,NPIX)+1
C
C     roll the 2-D buffer
C
        IF(J.LE.(JSIZE-NPIX/2)) THEN
          DO I=1,ISIZE
            ROLL(I,JROLL)=ARRAY(I,J+NPIX/2)
          ENDDO
        ELSE
          JOLD=MOD(J-2,NPIX)+1
          DO I=1,ISIZE
            ROLL(I,JROLL)=ROLL(I,JOLD)
          ENDDO
        ENDIF
C
C   Calculate weighted means in Y-direction
C
         DO I=1,ISIZE
            RMARGE(I)=0.0
         ENDDO
         DO JPOS=JROLL+1,JROLL+NPIX
            DO I=1,ISIZE
               RMARGE(I)=RMARGE(I)+WEIGHT(JPOS-JROLL)*
     :                             ROLL(I,MOD(JPOS-1,NPIX)+1)
            ENDDO
         ENDDO
*
*   Do 1-D smooth in X-direction. The ends of the data are padded
*   by pixel replication
*
         DO I=1,NPIX/2
            ARRAY(I,J)=0.0
            DO ITEMP=1,NPIX/2-I+1
               ARRAY(I,J)=ARRAY(I,J)+WEIGHT(ITEMP)*RMARGE(1)
            ENDDO
            DO ITEMP=NPIX/2+2-I,NPIX
               ARRAY(I,J)=ARRAY(I,J)
     :                    +WEIGHT(ITEMP)*RMARGE(I-NPIX/2-1+ITEMP)
            ENDDO
         ENDDO

         DO I=NPIX/2+1,ISIZE-NPIX/2
            ARRAY(I,J)=0.0
            DO ITEMP=1,NPIX
               ARRAY(I,J)=ARRAY(I,J)
     :                    +WEIGHT(ITEMP)*RMARGE(I-NPIX/2-1+ITEMP)
            ENDDO
         ENDDO

         DO I=ISIZE-NPIX/2+1,ISIZE
            ARRAY(I,J)=0.0
            DO ITEMP=NPIX/2+2+ISIZE-I,NPIX
               ARRAY(I,J)=ARRAY(I,J)+WEIGHT(ITEMP)*RMARGE(ISIZE)
            ENDDO
            DO ITEMP=1,NPIX/2+1+ISIZE-I
               ARRAY(I,J)=ARRAY(I,J)
     :                    +WEIGHT(ITEMP)*RMARGE(I-NPIX/2-1+ITEMP)
            ENDDO
         ENDDO
 
      ENDDO
 
      END
