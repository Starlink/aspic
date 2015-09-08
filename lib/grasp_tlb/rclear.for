C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R RCLEAR *
C      *            *
C      **************
C
C
C   PURPOSE
C       Sets the values in a section of a real array to zero.
C
C   ARGUMENTS
C  IN
C    NX            Integer   The X size of the array
C    NY            Integer   The Y size of the array
C    NXA           Integer   The X start of the section to be zeroed
C    NXB           Integer   The X end of the section to be zeroed
C    NYA           Integer   The Y start of the section to be zeroed
C    NYB           Integer   The Y end of the section to be zeroed
C  IN/OUT
C    DATA(NX,NY)   Real      The array
C  OUT
C
C   A.J.PENNY                   RGO                    83-3-17
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE RCLEAR(DATA,NX,NY,NXA,NXB,NYA,NYB)
C
C
C
      REAL DATA(NX,NY)
C
C
C
      DO K = NYA,NYB
         DO J = NXA,NXB
            DATA(J,K) = 0.0
         ENDDO
      ENDDO
C
C
C
      END



