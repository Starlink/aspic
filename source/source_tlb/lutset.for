*
*+  LUTSET RANGE RGB1 [RGB2]
*
*   Set up all or part of ARGS lookup table by linear interpolation from
*   RGB1 to RGB2
*
*   WFL Aug 81

      INCLUDE 'INTERIM(ERRPAR)'

      INTEGER RANGE(2),RGB1(3),RGB2(3),NVALS,STATUS
      DATA RANGE,RGB1,RGB2/0,255,0,0,0,255,255,255/

      CALL RDKEYI('RANGE',.TRUE.,2,RANGE,NVALS,STATUS)
      IF (STATUS.GT.ERR_PARNUL.OR.
     :  MIN(RANGE(1),RANGE(2)).LT.0.OR.
     :  MAX(RANGE(1),RANGE(2)).GT.255) THEN
          CALL WRERR('BADRANGE')
          CALL EXIT
      ENDIF

      CALL RDKEYI('RGB1',.TRUE.,3,RGB1,NVALS,STATUS)
      IF (STATUS.GT.ERR_PARNUL.OR.
     :  MIN(RGB1(1),RGB1(2),RGB1(3)).LT.0.OR.
     :  MAX(RGB1(1),RGB1(2),RGB1(3)).GT.255) THEN
          CALL WRERR('BADRGB1')
          CALL EXIT
      ENDIF

      IF (RANGE(1).NE.RANGE(2)) THEN
          CALL RDKEYI('RGB2',.TRUE.,3,RGB2,NVALS,STATUS)
          IF (STATUS.GT.ERR_PARNUL.OR.
     :      MIN(RGB2(1),RGB2(2),RGB2(3)).LT.0.OR.
     :      MAX(RGB2(1),RGB2(2),RGB2(3)).GT.255) THEN
              CALL WRERR('BADRGB2')
              CALL EXIT
          ENDIF
      ENDIF

      CALL SRINIT(0,.FALSE.,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
          CALL LUTSET(RANGE,RGB1,RGB2)
      ELSE
          CALL WRERR('NOARGS')
      ENDIF

      END

      SUBROUTINE LUTSET(RANGE,RGB1,RGB2)

*   NB ARGS is assumed to be assigned

      INTEGER RANGE(2),RGB1(3),RGB2(3),R1,R2,RLO,RHI,C1(3),
     :   LUT(3,0:255),I,J
      REAL CFAC(3)

      R1=RANGE(1)
      R2=RANGE(2)
      RLO=MIN(R1,R2)
      RHI=MAX(R1,R2)

      DO I=1,3
          C1(I)=RGB1(I)
          IF (R1.EQ.R2) THEN
              CFAC(I)=0.0
          ELSE
              CFAC(I)=FLOAT((RGB2(I)-RGB1(I)))/(R2-R1)
          ENDIF
      ENDDO

      DO I=RLO,RHI
          DO J=1,3
              LUT(J,I)=C1(J)+(I-R1)*CFAC(J)
          ENDDO
      ENDDO
      CALL SRCOLS(RLO,RHI-RLO+1,LUT(1,RLO))

      END
