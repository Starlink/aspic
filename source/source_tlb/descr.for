C+
C    STARLINK PROGRAM TO ACCESS THE DESCRIPTORS OF A BDF FILE
C
C    INPUTS REQUIRED:
C                      DESNAME - DESCRIPTOR NAME (IF RESPONSE IS NULL
C                                THEN ALL DESCRIPTORS WILL BE OUTPUT)
C
C                      IMAGE -  THE BDF FILE NAME
C
C     CDP/RGO
C-
      LOGICAL MORE
      INTEGER STATUS,COUNT
      CHARACTER*80 NAME,DESCR,TEXT(16384)
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'


      CALL RDKEYC('DESNAME',.FALSE.,1,DESCR,NVAL,STATUS)
      IF(STATUS.GT.ERR_PARNUL)  GO TO 999
      IF(STATUS.EQ.ERR_PARNUL)  THEN
         MORE = .TRUE.
         COUNT = 0
         NVAL=1
         DO WHILE (MORE)
         COUNT = COUNT + NVAL
         CALL RDDSCN('IMAGE',COUNT,DESCR,STATUS)
         IF(STATUS.EQ.ERR_DSCNPR)   GO TO 999
         CALL RDDSCR('IMAGE',DESCR,16384,TEXT,NVAL,STATUS)
         DO I=1,NVAL
         CALL WRUSER(DESCR(1:8)//' '//TEXT(I)(1:68),STATUS)
         END DO
         ENDDO
      ELSE
         CALL RDDSCR('IMAGE',DESCR,16384,TEXT,NVAL,STATUS)
         IF(STATUS.EQ.ERR_DSCNPR)  THEN
            CALL WRUSER('DESCRIPTOR OF THAT NAME NOT THERE',STATUS)
            GO TO 999
         ENDIF
         DO I=1,NVAL
         CALL WRUSER(DESCR(1:8)//' '//TEXT(I)(1:68),STATUS)
         END DO
      ENDIF

  999 CONTINUE
      CALL CNPAR('DESNAME',STATUS)
      CALL CNPAR('IMAGE',STATUS)

      CALL EXIT
      END

