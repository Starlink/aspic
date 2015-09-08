C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R DISPK *
C      *           *
C      *************
C
C
C  PURPOSE
*        To make an array suitable for the ARGS display.
*        A defined section of the input array is copied to the output
*        array, and scaled to the range 0 to 255 by the parameters;
*        lower,upper limits, Log(or not), Trim(or not),
*        Average(or not).
*        The copying is done compressing the array as specified.
*
*  METHOD
*
*
*  ARGUMENTS
*    (IN)
*
*    PIC
*       The input array
*     NX
*       The X size of PIC
*     NY
*       The Y size of PIC
*
*    KFACT
*       The factor by which PIC is to be squeezed down
*    TRIM
*       Flag for wether values outside range are to be wrapped round
*       or not
*    LLOG
*       Flag for wether values are to be scaled logarithmetically or not
*    AVERAG
*       Flag for wether the compression (if any) is to be done by
*       averaging all values in the box to be compressed, or by taking
*       merely the first value alone. If averaging and any values are
*       'invalid', then the average value is set to invalid.
*    KVLO
*       The value that is to be scaled to 0
*    KVHI
*       The value that is to be scaled to 255
*    KX
*       The X size of the output array
*    KY
*       The Y size of the output array
*    INVAL
*       The value that is to taken as being an 'invalid' value
*    KDSINV
*       The value invalid values are to be displayed as
*    KXR
*       The X range of the input array that is to output
*    KYR
*       The Y range of the input array that is to be output
*
*   (OUT)
*    IPIC
*       The output array
*
*  CALLS
*       None
*
*  WRITTEN BY
*      A.J. PENNY                                      82-6-21
* ----------------------------------------------------------------
C
C
C
      SUBROUTINE DISPK(PIC,NX,NY,KFACT,TRIM,WRAP,LLOG,AVERAG,
     +                  KVLO,KVHI,IPIC,KX,KY,INVAL,KDSINV,KXR,KYR)
C
C
C
      REAL VLO,VHI,VL,D,SCALE,V
      INTEGER NX,NY,KFACT,KX,KY,IXC,IYC,INVAL
      INTEGER*2 IPIC(KX,KY)
      INTEGER*2 PIC(NX,NY)
      INTEGER KXR(2),KYR(2)
      LOGICAL TRIM,LLOG,AVERAG,WRAP
C
C  Produce scale factor and low value
C
      IF (LLOG) THEN
         D = REAL(KVHI-KVLO)
         IF (ABS(D).LT.1.0E-10) D = SIGN(1.0,D)
         DIR = SIGN(1.0,D)
         VLO = REAL(KVLO)
         IF (KVLO.EQ.0) THEN
            VLO = -1.0E-10*DIR
         ELSE
            VLO = REAL(KVLO)
         ENDIF
         IF (KVHI.EQ.0) THEN
            VHI = 1.0E-10*DIR
         ELSE
            VHI = REAL(KVHI)
         ENDIF
         VLO = VLO - 1.0E-11*DIR
         SCALE = 255.0/LOG(ABS(D))
      ELSE
         D = REAL(KVHI-KVLO)
         IF (ABS(D).LT.1.0E-10) D = SIGN(1.0,D)
         SCALE = 255.0/D
         VLO = REAL(KVLO)
      ENDIF
C
C  Produce the scaled array
C
      IF (KFACT.EQ.1) THEN
C
C  Do for no compression case
C
         JS = KXR(1) - 1
         KS = KYR(1) - 1
         DO KA = 1,KY
            K = KA + KS
            DO JA = 1,KX
               J = JA + JS
C
C  Extract the value from the input array, allowing for 
C  invalid points
C
               KVAL = PIC(J,K)
               IF (KVAL.EQ.INVAL) KVAL = KDSINV
C
C  Do the scaling, either linear or log
C
               IF (LLOG) THEN
                  VL = ABS(REAL(KVAL)-VLO)
                  IF (VL.LT.1.0E-10) VL = 1.0E-10
                  KV = SCALE*LOG(VL)
               ELSE
                  KV = SCALE*(REAL(KVAL)-VLO)
               ENDIF
C
C  If WRAPping round do so
C
               IF (WRAP) THEN
                  IF (ABS(REAL(KV)).GT.255.0) THEN
                     KV = MOD(KV,256)
                  ENDIF
               ENDIF
C
C  Store in output array, in limits 0 to 255
C
               IPIC(JA,KA) = MIN(MAX(KV,0),255)
C
C  If TRIM=FALSE, then store data directly from input array
C
               IF (.NOT.TRIM) THEN
                  IF (ABS(REAL(KVAL)).GT.32767.0) THEN
                     IPIC(JA,KA) = MOD(KVAL,32768)
                  ELSE
                     IPIC(JA,KA) = KVAL
                  ENDIF
               ENDIF
C
C
C
            ENDDO
         ENDDO
C
C
C
      ELSE
C
C  Do for compression case
C
         NTOTP = KFACT*KFACT
         KKLIM = KYR(1) + (KY-1)*KFACT
         JJLIM = KXR(1) + (KX-1)*KFACT
         KA = 1
         DO K = KYR(1),KKLIM,KFACT
            JA = 1
            DO J = KXR(1),JJLIM,KFACT
C
C  Extract the value from the input array, allowing for binning and
C  invalid points
C
               IF (AVERAG) THEN
                  L = 0
                  NUMINV = 0
                  DO KB = 1,KFACT
                     KC = K + KB - 1
                     DO JB = 1,KFACT
                        JC = J + JB - 1
                        IF (PIC(JC,KC).NE.INVAL) THEN
                           L = L + PIC(JC,KC)
                        ELSE
                           NUMINV = NUMINV + 1
                        ENDIF
                     ENDDO
                  ENDDO
                  IF (NUMINV.EQ.0) THEN
                     KVAL = L/NTOTP
                  ELSE
                     KVAL = KDSINV
                  ENDIF
               ELSE
                  KVAL = PIC(J,K)
                  IF (KVAL.EQ.INVAL) KVAL = KDSINV
               ENDIF
C
C  Do the scaling, either linear or log
C
               IF (LLOG) THEN
                  VL = ABS(REAL(KVAL)-VLO)
                  IF (VL.LT.1.0E-10) VL = 1.0E-10
                  KV = SCALE*LOG(VL)
               ELSE
                  KV = SCALE*(REAL(KVAL)-VLO)
               ENDIF
C
C  If WRAPping round do so
C
               IF (WRAP) THEN
                  IF (ABS(REAL(KV)).GT.255.0) THEN
                     KV = MOD(KV,256)
                  ENDIF
               ENDIF
C
C  Store in output array, in limits 0 to 255
C
               IPIC(JA,KA) = MIN(MAX(KV,0),255)
C
C  If TRIM=FALSE, then store data directly from input array
C
               IF (.NOT.TRIM) THEN
                  IF (ABS(REAL(KVAL)).GT.32767.0) THEN
                     IPIC(JA,KA) = MOD(KVAL,32768)
                  ELSE
                     IPIC(JA,KA) = KVAL
                  ENDIF
               ENDIF
C
C
C
               JA = JA + 1
            ENDDO
            KA = KA + 1
         ENDDO
C
C
C
      ENDIF
C
C
C
      END



