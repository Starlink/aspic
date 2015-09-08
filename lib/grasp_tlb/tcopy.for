C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      **************
C      *            *
C      * S/R TCOPY  *
C      *            *
C      **************
C
C
C
C  PURPOSE
C   This subroutine extraxts part of the array KWHOLE
C   which is in I*2 with BSCALE and BZERO and copies it into the
C   real array PART.
C   INVALID pixels are copied over with a value of BS*INVAL + BZ
C   The copying can either add to the input values in PART or replace
C   them.
C
C
C
C   ARGUMENTS
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
C      INPUT/OUTPUT
C         PART    The real array used to store the selected part
C                 of KWHOLE
C
C      OUTPUT
C         NINVAL  The number of INVALID pixels found whilst copying
C
C
C    CALLS
C      None
C
C
C
C    A J PENNY                      RGO                    83-2-23
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE TCOPY(KWHOLE,N,M,PART,N1,M1,IX1,IY1,IX2,IY2,
     +                BS,BZ,INVAL,NINVAL,KADD)
C
C
C
      REAL PART(N1,M1)
      INTEGER*2 KWHOLE(N,M)
C
C  Transfer pixels values from KWHOLE to PART,using
C  BS and BZ to convert from integer to real
C  Set INVALID pixels to 1.0 higher than INVALID value
C
      NINVAL = 0
      J1=1
      DO J=IY1,IY2
         I1=1
         DO I=IX1,IX2
            L = KWHOLE(I,J)
            S = REAL(L)*BS + BZ
            IF (L.EQ.INVAL) THEN
                NINVAL = NINVAL + 1
            ENDIF
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



