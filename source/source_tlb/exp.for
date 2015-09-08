*
*+  EXP IN OUT
*
*   Calculate EXP of IN and put in OUT (element by element)
*   Any values greater than 88.028 cause overflow. Thus, any values greater
*   than 69 will be replaced by exp(69) (about 1E30) and an error message
*   will be communicated to the environment.
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
      CALL EXP2(%VAL(INPTR),NELS,%VAL(OUTPTR),STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('BIGVAL')
      ENDIF

      END

      SUBROUTINE EXP2(IN,NELS,OUT,STATUS)

      INTEGER NELS,STATUS
      REAL IN(NELS),OUT(NELS)

      STATUS=0
      DO I=1,NELS
          IF (IN(I).LE.69.0) THEN
              OUT(I)=EXP(IN(I))
          ELSE
              OUT(I)=EXP(69.0)
              STATUS=1
          ENDIF
      ENDDO

      END
