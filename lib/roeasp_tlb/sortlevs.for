      SUBROUTINE SORTLEVS(NLEVS,LEVELS,SLEVELS)

C+
C    SORTLEVS
C
C	part of calibration suite.
C	crudely sorts LEVELS into SLEVELS on (*,1) - VALUEs ascending order
C
C    Given (arguments)
C	LEVELS(NLEVS,2)	(RA)		unsorted array
C	NLEVS		(I)		actual number of levels in array
C
C    Returned (arguments)
C	SLEVELS(NLEVS,2)	(RA)		sorted array
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      INTEGER NLEVS
      REAL LEVELS(NLEVS,2),SLEVELS(NLEVS,2)
      INTEGER I,J
      REAL TEMP
 
C*  copy levels first
        DO I=1,NLEVS
          SLEVELS(I,1)=LEVELS(I,1)
          SLEVELS(I,2)=LEVELS(I,2)
        ENDDO

         DO I=1,NLEVS-1
            XMIN=SLEVELS(I,1)
            NMIN=I
            DO J=I+1,NLEVS
               IF (SLEVELS(J,1).LT.XMIN) THEN
                  XMIN=SLEVELS(J,1)
                  NMIN=J
               ENDIF
            ENDDO
            TEMP=SLEVELS(I,1)
            SLEVELS(I,1)=SLEVELS(NMIN,1)
            SLEVELS(NMIN,1)=TEMP
            TEMP=SLEVELS(I,2)
            SLEVELS(I,2)=SLEVELS(NMIN,2)
            SLEVELS(NMIN,2)=TEMP
         ENDDO
 
      RETURN
      END
