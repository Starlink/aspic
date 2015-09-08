*
*+  SQRT IN OUT
*
*   Calculate square root of IN and put in OUT (element by element)
*   Any negative values in IN will be set to zero and an error message
*   communicated to the environment.
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
      CALL SQRT2(%VAL(INPTR),NELS,%VAL(OUTPTR),STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('NEGVAL')
      ENDIF

      END

      SUBROUTINE SQRT2(IN,NELS,OUT,STATUS)

      INTEGER NELS,STATUS
      REAL IN(NELS),OUT(NELS)

      STATUS=0
      DO I=1,NELS
          IF (IN(I).GE.0.0) THEN
              OUT(I)=SQRT(IN(I))
          ELSE
              OUT(I)=0.0
              STATUS=1
          ENDIF
      ENDDO

      END
