      SUBROUTINE DIV1(NX1,NY1,ARRAY1,NX2,NY2,ARRAY2,NX3,NY3,ARRAY3)             
C+
C   DIV1
C
C   Divide one 2-D array by another
C
C   Given      (arguments)
C   NX1         X-dimension of first input array
C   NY1         Y-dimension of first input array
C   ARRAY1      first input array
C   NX2         X-dimension of second input array
C   NY2         Y-dimension of second input array
C   ARRAY2      second input array
C   NX3         X-dimension of output array
C   NY3         Y-dimension of output array
C
C   Returned   (arguments)
C   ARRAY3      output array
C
C   B.D.Kelly/ROE/25.9.1981
C-
C
C   If the arrays are of different sizes, then the areas in
C   common are divided.
C
      INTEGER NX1,NY1,NX2,NY2,NX3,NY3
      REAL ARRAY1(NX1,NY1),ARRAY2(NX2,NY2),ARRAY3(NX3,NY3)
 
      DO J=1,MIN(NY1,NY2,NY3)
        DO I=1,MIN(NX1,NX2,NX3)
          IF(ARRAY2(I,J).GT.0.01) THEN
            ARRAY3(I,J)=ARRAY1(I,J)/ARRAY2(I,J)
          ELSE
            ARRAY3(I,J)=0.0
          ENDIF
        ENDDO
      ENDDO
C
C   Zero any remaining unitialized elements in output array.
C
      IF(NY3.GT.MIN(NY1,NY2,NY3)) THEN
        DO J=MIN(NY1,NY2,NY3)+1,NY3
          DO I=1,NX3
            ARRAY3(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      IF(NX3.GT.MIN(NX1,NX2,NX3)) THEN
        DO J=1,NY3
          DO I=MIN(NX1,NX2,NX3)+1,NX3
            ARRAY3(I,J)=0.0
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
