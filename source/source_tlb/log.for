*
*+  LOG IN OUT
*
*   Calculate natural logarithm of IN and put in OUT (element by element)
*   Any values in IN less than exp(-10) will be set to -10 and an error
*   message communicated to the environment.
*
*   WFL Aug 81

      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'

      INTEGER AXIS(99),NAXIS,INPTR,STATUS,OUTPTR,NELS,I

      CALL RDIMAG('IN',FMT_R,99,AXIS,NAXIS,INPTR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('BADIN')
          CALL EXIT
      ENDIF

      CALL WRIMAG('OUT',FMT_R,AXIS,NAXIS,OUTPTR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('BADOUT')
          CALL EXIT
      ENDIF

      NELS=1
      DO I=1,NAXIS
          NELS=NELS*AXIS(I)
      ENDDO
      CALL LOG2(%VAL(INPTR),NELS,%VAL(OUTPTR),STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('LITVAL')
      ENDIF

      END

      SUBROUTINE LOG2(IN,NELS,OUT,STATUS)

      INTEGER NELS,STATUS
      REAL IN(NELS),OUT(NELS)

      STATUS=0
      DO I=1,NELS
          IF (IN(I).GE.EXP(-10.0)) THEN
              OUT(I)=LOG(IN(I))
          ELSE
              OUT(I)=-10.0
              STATUS=1
          ENDIF
      ENDDO

      END
