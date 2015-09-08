      SUBROUTINE COPY(NX1,NY1,ARRAY1,NX2,NY2,ARRAY2)                            
C+
C   COPY
C
C   Copies one 2-D array into another
C
C   Given      (arguments)
C   NX1         X-dimension of input array
C   NY1         Y-dimension of input array
C   ARRAY1      input array
C   NX2         X-dimension of output array
C   NY2         Y-dimension of output array
C
C   Returned   (arguments)
C   ARRAY2      output array
C
C   B.D.Kelly/ROE/25.9.1981
C-
C
C   If the input and output arrays are of different sizes,
C   the area in common is copied.
C
      INTEGER NX1,NY1,NX2,NY2
      REAL ARRAY1(NX1,NY1),ARRAY2(NX2,NY2)
 
      DO J=1,MIN(NY1,NY2)
        DO I=1,MIN(NX1,NX2)
          ARRAY2(I,J)=ARRAY1(I,J)
        ENDDO
      ENDDO
C
C   Zero uninitialized elements of output array.
C
      IF(NY2.GT.MIN(NY1,NY2)) THEN
        DO J=MIN(NY1,NY2)+1,NY2
          DO I=1,NX2
            ARRAY2(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      IF(NX2.GT.MIN(NX1,NX2)) THEN
        DO J=1,NY2
          DO I=MIN(NX1,NX2)+1,NX2
            ARRAY2(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      END
C
C
C
C
C
C****************************************************************
C****************************************************************
