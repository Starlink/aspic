      SUBROUTINE BOXSUM(IXCUR,IYCUR,IBSIZE,NX,NY,ARRAY,SUMVAL)                  
C+
C   BOXSUM
C
C     SUMS THE VALUES IN THE CURRENT IMAGE WITHIN THE SQUARE
C     AREA OF SIZE IBSIZE, CENTRED ON (IXCUR,IYCUR).
C	
C   Given      (arguments)
C   IYCUR	CURSOR Y-POSITION
C   IXCUR	CURSOR X-POSITION
C   IBSIZE	CURSOR BOX SIZE MAX=19
C   NX          X-dimension of image
C   NY          Y-dimension of image
C   ARRAY       image
C
C   Returned   (arguments)
C   SUMVAL      sum of pixel values within box.
C
C	B.D KELLY/ROE/1981
C-
      INTEGER IXCUR,IYCUR,IBSIZE,NX,NY
      REAL SUMVAL
      REAL ARRAY(NX,NY)
 
      SUMVAL=0.0
      DO J=IYCUR-IBSIZE/2,IYCUR+IBSIZE/2
        DO I=IXCUR-IBSIZE/2,IXCUR+IBSIZE/2
          SUMVAL=SUMVAL+ARRAY(I,J)
        ENDDO
      ENDDO
 
      END
C****************************************************************
C****************************************************************
