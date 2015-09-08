      PROGRAM EXTRACT
C+
C
C   Program EXTRACT
C
C   This program is a less sophisticated version of SELECT,
C   in that it only allows selection of subsets of the data
C   defined by a range of epochs. However it does not need a
C   a graphics terminal and is easier to use.
C
C   Written by K.F.Hartley at RGO on 2-January-1984
C   (Based on EX by C.D.Pike)
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AXIN(2),AXOU(2),STATUS,PIN,POUT
      DOUBLE PRECISION LIMS(2),EP1,EP2
      CHARACTER*72 TEXT
      LOGICAL AGAIN
C
C   First obtain the input dataset
C
      CALL RDIMAG('INPUT',FMT_DP,2,AXIN,NDIM,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('HELLIN',STATUS)
      ELSE
         AGAIN=.TRUE.
         DO WHILE (AGAIN)
C
C         Obtain estimates of the range of epochs.
C
            CALL PER_GETLIM(%VAL(PIN),AXIN,EP1,EP2)
            LIMS(1)=EP1
            LIMS(2)=EP2
C
C         and ask the user to select a range
C
            CALL RDKEYD('EPOCHS',.TRUE.,2,LIMS,NVAL,STATUS)
            IF(STATUS.GT.ERR_PARNUL)  GO TO 999
C
C         Find out how many samples lie between those limits
C
            CALL PER_GETNUM(%VAL(PIN),AXIN,LIMS,NPTS)
C
C         and obtain an output dataset for them
C
            AXOU(1) = 3
            AXOU(2) = NPTS
            CALL WRIMAG('OUTPUT',FMT_DP,AXOU,2,POUT,STATUS)
            IF (STATUS.EQ.ERR_NORMAL) THEN
C
C         Copy out the required data
C
               CALL PER_PICK(%VAL(POUT),AXOU,%VAL(PIN),AXIN,LIMS)
C
C            and obtain a title for the new dataset
C
               TEXT=' '
               CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
               CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
            ELSE
               CALL WRERR('HELLOUT',STATUS)
            END IF
            CALL WRUSER('Do you want to extract another subset?',
     :                   STATUS)
            CALL RDKEYL('AGAIN',.TRUE.,1,AGAIN,I,STATUS)
            CALL CNPAR('AGAIN',STATUS)
            CALL CNPAR('EPOCHS',STATUS)
            CALL FRDATA('OUTPUT',STATUS)
            CALL CNPAR('OUTPUT',STATUS)
            CALL CNPAR('TITLE',STATUS)
         END DO
      END IF
  999 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END


