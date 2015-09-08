C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DISPMM *
C      *            *
C      **************
C
C
*  PURPOSE
*       This finds the minimum and maximum of a section of a 2-D
*       I*2 array that contains values that are to be ignored
*
*  METHOD
*
*  ARGUMENTS
*  (IN)
*
*    IDAT
*        The input array
*    NX
*        The X size of the array
*    NY
*        The Y size of the array
*    INVAL
*        If array elements have this value they are ignored
*    KXR
*        The X range of the array to be looked at
*    KYR
*        The Y range of the array to be looked at
*
*  (OUT)
*
*    KLOW
*        The lowest value in the area
*    KHI
*        The highest value in the area
*
*
*  CALLS
*      None
*
*  WRITTEN BY
*     A.J. PENNY                                           82-6-21
* ----------------------------------------------------------------



      SUBROUTINE DISPMM(IDAT,NX,NY,KLOW,KHI,INVAL,KXR,KYR)
C
C      Subroutine finds minimum and maximum of an I*2 array
C
      INTEGER*2 IDAT(NX,NY)
      INTEGER KXR(2),KYR(2)
C
      IF (IDAT(KXR(1),KYR(1)).EQ.INVAL) THEN
         KLOW = 32767
         KHI = -32768
      ELSE
         KLOW=IDAT(KXR(1),KYR(1))
         KHI=KLOW
      ENDIF
      DO K = KYR(1),KYR(2)
         DO J = KXR(1),KXR(2)
            IF (IDAT(J,K).NE.INVAL) THEN
            KV = IDAT(J,K)
               IF (KV.LT.KLOW) KLOW=KV
               IF (KV.GT.KHI) KHI=KV
            ENDIF
         ENDDO
      ENDDO
C
C
C
      END



