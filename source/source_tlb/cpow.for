*
*+  CPOW SCALAR IN OUT
*
*   Calculate IN**SCALAR and put in OUT (element by element)
*   If any element of IN is negative with SCALAR not an integer or if
*   any element is zero regardless of the value of SCALAR, a value of
*   zero will be given to the corresponding element of OUT and an error
*   message will be communicated to the environment
*
*   WFL Aug 81

      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'

      INTEGER NVALS,STATUS,AXIS(99),NAXIS,INPTR,OUTPTR,NELS,I
      REAL SCALAR

      CALL RDKEYR('SCALAR',.FALSE.,1,SCALAR,NVALS,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('BADSCAL')
          CALL EXIT
      ENDIF

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
      CALL CPOW2(SCALAR,%VAL(INPTR),NELS,%VAL(OUTPTR),STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('NEGVAL')
      ENDIF

      END

      SUBROUTINE CPOW2(SCALAR,IN,NELS,OUT,STATUS)

      INTEGER NELS,STATUS
      REAL SCALAR,IN(NELS),OUT(NELS)

      STATUS=0
      DO I=1,NELS
          IF (IN(I).LT.0.0) THEN
              IF (SCALAR.EQ.INT(SCALAR)) THEN
                  OUT(I)=IN(I)**INT(SCALAR)
              ELSE
                  OUT(I)=0.0
                  STATUS=1
              ENDIF
          ELSE IF (IN(I).EQ.0.0) THEN
              OUT(I)=0.0
              IF (SCALAR.LE.0.0) THEN
                  STATUS=1
              ENDIF
          ELSE
              OUT(I)=IN(I)**SCALAR
          ENDIF
      ENDDO

      END
