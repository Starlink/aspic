C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      SUBROUTINE COPYRGO(KWHOLE,N,M,PART,N1,M1,IX1,IY1,IX2,IY2,
     +                BS,BZ,INVAL,NINVAL,KADD)
C
C   This subroutine copies part of the array KWHOLE
C   into the array PART.
C
C   Parameters
C      INPUT
C         KWHOLE  An INTEGER*2 array containing the full image
C         N       The first dimension of KWHOLE
C         M       The second dimension of KWHOLE
C         N1      The first dimension of PART
C         M1      The second dimension of PART
C         IX1     The first column to be stored
C         IY1     The first row to be stored
C         IX2     The last column to be stored
C         IY2     The last row to be stored.
C         BS      The scale factor to be used in converting the
C                 integers into reals.
C         BZ      The zero offset to be used in converting the
C                 integers into reals.
C         INVAL   The flag value for an INVALID pixel
C         KADD    The flag for adding (1) into PART or overwriting (0)
C
C      OUTPUT
C         PART    The real array used to store the selected part
C                 of KWHOLE
C         NINVAL  The number of INVALID pixels found whilst copying

C   Written by K F Hartley, modified 12/4/82
C   Further modified by A J Penny, april 82
C
      REAL PART(N1,M1)
      INTEGER*2 KWHOLE(N,M)
C
C  Find the first valid number
C
      DO K = IY1,IY2
         DO J = IX1,IX2
            L = KWHOLE(J,K)
            IF(L.NE.INVAL) GO TO 1
         ENDDO
      ENDDO
    1 LAST = L
C
C  Transfer pixels values from KWHOLE to PART,using
C  BS and BZ to convert from integer to real
C  Set any Invalid number to value of pixel before
C
      NINVAL = 0
      J1=1
      DO J=IY1,IY2
         I1=1
         DO I=IX1,IX2
            L = KWHOLE(I,J)
            IF (L.EQ.INVAL) THEN
                L = LAST
                NINVAL = NINVAL + 1
            ENDIF
            LAST = L
            S = REAL(L)*BS + BZ
            IF (KADD.EQ.0) THEN
               PART(I1,J1) = S
            ELSE
               PART(I1,J1) = PART(I1,J1) + S
            ENDIF
            I1=I1+1
         ENDDO
         J1=J1+1
      ENDDO
C
C
C
      END



