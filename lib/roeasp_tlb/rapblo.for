      SUBROUTINE RAPBLO(NPIX,ISIZE,JSIZE,ARRAY1,                                
     &                  ARRAY2,ROLL,RMARGE,ISTAT)
C+
C      RAPBLO
C
C      RAPID 2-D BLOCK SMOOTH
C
C      Replaces each pixel of a 2-D array by the mean of
C      the NPIX*NPIX pixels centred on it. NPIX is odd.
C
C
C      Given       (arguments)
C      NPIX         size of square smoothing area; odd no.
C      ARRAY1       data array
C      ISIZE        array X-dimension
C      JSIZE        array Y-dimension
C
C      Returned    (arguments)
C      ARRAY2       smoothed data array
C      ROLL         workspace for 2-D rolling buffer
C      RMARGE       workspace for marginal sum of ROLL
C      ISTAT        status return
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
C        put the column sum of the 2-D rolling buffer into a 1-D array
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
C          by (sum of 1-D rolling buffer)/(NPIX**2)
C        endfor
C      endfor
C
C
C
      INTEGER NPIX,ISIZE,JSIZE,ISTAT
      REAL ARRAY1(ISIZE,JSIZE),ARRAY2(ISIZE,JSIZE),ROLL(ISIZE,NPIX)
      REAL RMARGE(ISIZE)
 
      ISTAT=0
 
C
C   initialize 2-D rolling buffer
C
      DO J=1,(NPIX+1)/2+1
        DO I=1,ISIZE
          ROLL(I,J)=ARRAY1(I,1)
        ENDDO
      ENDDO
 
      DO J=(NPIX+1)/2+2,NPIX
        DO I=1,ISIZE
          ROLL(I,J)=ARRAY1(I,1+J-(NPIX+1)/2)
        ENDDO
      ENDDO
C
C   initialize the sum of the columns of the rolling buffer.
C
      DO I=1,ISIZE
        RMARGE(I)=0.0
      ENDDO
 
      DO JTEM=1,NPIX
        DO I=1,ISIZE
          RMARGE(I)=RMARGE(I)+ROLL(I,JTEM)
        ENDDO
      ENDDO
C
C   calculate the means row-by-row
C
      DO J=1,JSIZE
C
C     calculate index of oldest row in 2-D buffer
C
        JROLL=MOD(J,NPIX)+1
C
C     subtract the oldest row of the buffer from the column sum.
C
        DO I=1,ISIZE
          RMARGE(I)=RMARGE(I)-ROLL(I,JROLL)
        ENDDO
C
C     roll the 2-D buffer
C
        IF(J.LE.(JSIZE-NPIX/2)) THEN
          DO I=1,ISIZE
            ROLL(I,JROLL)=ARRAY1(I,J+NPIX/2)
          ENDDO
        ELSE
          JOLD=MOD(J-1,NPIX)+1
          DO I=1,ISIZE
            ROLL(I,JROLL)=ROLL(I,JOLD)
          ENDDO
        ENDIF
C
C     add the newest row of the buffer to the column sum
C
        DO I=1,ISIZE
          RMARGE(I)=RMARGE(I)+ROLL(I,JROLL)
        ENDDO
C
C     calculate means from 1-D array
C
C     initialize sum
C
        SUM=RMARGE(1)*FLOAT((NPIX+1)/2)
        DO I=1,NPIX/2
          SUM=SUM+RMARGE(I)
        ENDDO
C
C     move through data updating sum and calculating means
C
        DO I=1,ISIZE
          IF(I.LE.(NPIX+1)/2) THEN
            SUM=SUM+RMARGE(I+NPIX/2)-RMARGE(1)
          ELSE IF(I.GT.(ISIZE-NPIX/2)) THEN
            SUM=SUM+RMARGE(ISIZE)-RMARGE(I-(NPIX+1)/2)
          ELSE
            SUM=SUM+RMARGE(I+NPIX/2)-RMARGE(I-(NPIX+1)/2)
          ENDIF
 
          ARRAY2(I,J)=SUM/NPIX**2
        ENDDO
 
      ENDDO
 
      END
C
C
C
C
C
C***************************************************************
C***************************************************************
