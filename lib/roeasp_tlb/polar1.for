      SUBROUTINE POLAR1(THETA,NQX,NQY,ARRQ,NUX,NUY,ARRU,
     :                  NTX,NTY,NAX,NAY,NEX,NEY,ARRE,ARRT,ARRA)
*+
*   POLAR1
*
*   Given Q and U frames, and instrumental offset angle THETA,
*   calculate total polarization and polarization angle.
*
*   Given      (arguments)
*   THETA   R   instrumental offset angle, radians.
*   NQX     I   X-dimension of Q-array
*   NQY     I   Y-dimension of Q-array
*   ARRQ    RA  array of Q-values
*   NUX     I   X-dimension of U-array
*   NUY     I   Y-dimension of U-array
*   ARRU    RA  array of U-values
*   NTX     I   X-dimension of polarization array
*   NTY     I   Y-dimension of polarization array
*   NAX     I   X-dimension of angle array
*   NAY     I   Y-dimension of angle array
*   NEX     I   X-dimension of error array
*   NEY     I   Y-dimension of error array
*   ARRE    RA  error array
*
*   Returned   (arguments)
*   ARRT    RA  array for total polarization
*   ARRA    RA  array for polarization angles
*
*   B.D.Kelly/ROE/29.1.1982
*-

      INTEGER NQX,NQY,NUX,NUY,NTX,NTY,NAX,NAY,NEX,NEY
      REAL ARRQ(NQX,NQY),ARRU(NUX,NUY),ARRT(NTX,NTY),ARRA(NUX,NUY)
      REAL ARRE(NEX,NEY)

      IF((NEX.GE.NQX).AND.(NEY.GE.NQY)) THEN
*
*      Correct polarizations by the error frame
*
         DO J=1,MIN(NQY,NUY,NTY,NAY)
            DO I=1,MIN(NQX,NUX,NTX,NAX)
               IF((ABS(ARRQ(I,J)).GT.1.0E-20).AND.
     :            (ABS(ARRU(I,J)).GT.1.0E-20).AND.
     :            (ABS(ARRE(I,J)).LT.(ARRQ(I,J)**2+ARRU(I,J)**2))) THEN
                  ARRT(I,J)=SQRT(ARRQ(I,J)**2+ARRU(I,J)**2
     :                           -ARRE(I,J))
                  ARRT(I,J)=MAX(ARRT(I,J),0.0)
                  IF(ARRT(I,J).GT.100.0) ARRT(I,J)=0.0
                  ARRA(I,J)=0.5*ATAN2(ARRU(I,J),ARRQ(I,J))+THETA
                  IF(ARRA(I,J).LT.0.0) ARRA(I,J) = 3.14159+ARRA(I,J)
               ELSE
                  ARRT(I,J)=0.0
                  ARRA(I,J)=0.0
               ENDIF
            ENDDO
         ENDDO

      ELSE
*
*      No error frame provided
*
         DO J=1,MIN(NQY,NUY,NTY,NAY)
            DO I=1,MIN(NQX,NUX,NTX,NAX)
               IF((ABS(ARRQ(I,J)).GT.1.0E-20).AND.
     :            (ABS(ARRU(I,J)).GT.1.0E-20)) THEN
                  ARRT(I,J)=SQRT(ARRQ(I,J)**2+ARRU(I,J)**2)
                  ARRT(I,J)=MAX(ARRT(I,J),0.0)
                  IF(ARRT(I,J).GT.100.0) ARRT(I,J)=0.0
                  ARRA(I,J)=0.5*ATAN2(ARRU(I,J),ARRQ(I,J))+THETA
                  IF(ARRA(I,J).LT.0.0) ARRA(I,J) = 3.14159+ARRA(I,J)
               ELSE
                  ARRT(I,J)=0.0
                  ARRA(I,J)=0.0
               ENDIF
            ENDDO
         ENDDO

      ENDIF
*
*   Zero any remaining unitialized elements in output arrays.
*
      IF(NTY.GT.MIN(NQY,NUY,NTY,NAY)) THEN
        DO J=MIN(NQY,NUY,NTY,NAY)+1,NTY
          DO I=1,NTX
            ARRT(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      IF(NTX.GT.MIN(NQX,NUX,NTX,NAX)) THEN
        DO J=1,NTY
          DO I=MIN(NQX,NUX,NTX,NAX)+1,NTX
            ARRT(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      IF(NAY.GT.MIN(NQY,NUY,NTY,NAY)) THEN
        DO J=MIN(NQY,NUY,NTY,NAY)+1,NAY
          DO I=1,NAX
            ARRA(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      IF(NAX.GT.MIN(NQX,NUX,NTX,NAX)) THEN
        DO J=1,NAY
          DO I=MIN(NQX,NUX,NTX,NAX)+1,NAX
            ARRA(I,J)=0.0
          ENDDO
        ENDDO
      ENDIF
 
      END
