      PROGRAM CONCAT
C+
C
C      Program CONCAT
C
C      Allows several datasets of the type used by the period finding
C      programs to be concatanated. The only limit is that
C      the total number of samples to be handled must be less
C      than 10000.
C
C      Written by C.D.Pike at RGO on 25-Jan-1984
C
C-
      INTEGER PIN,POUT,PTEMP,STATUS,AXES(2)
      CHARACTER*72 TEXT
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C   Obtain workspace - enough to store 10000 sets of epoch,
C   value and weight.
C
      CALL GETDYN('TEMP',FMT_DP,30000,PTEMP,STATUS)
      CALL PER_STOP(STATUS,'ERRDYN')
C
C   Now start reading the input datasets and storing them in the
C   workspace provided.
C
   10 CONTINUE
         CALL RDIMAG('INPUT',FMT_DP,2,AXES,NDIM,PIN,STATUS)
         IF(STATUS.EQ.ERR_PARNUL) GO TO 998
         IF(STATUS.NE.ERR_NORMAL) THEN
            CALL CNPAR('INPUT',STATUS)
            GO TO 10
         ENDIF
         CALL PER_SHOVE(%VAL(PIN),AXES,%VAL(PTEMP),NPTS)
         CALL CNPAR('INPUT',STATUS)
         CALL FRDATA('INPUT',STATUS)
         GO TO 10
C
C   Comes down to here on exit from the input loop.
C
  998 CONTINUE
      IF (NPTS.LE.0) THEN
C
C   If for some reason no points have been obtained, then say so.
C
         CALL WRUSER('No samples obtained',STATUS)
      ELSE
C
C   Otherwise arrange to output the finished product
C
         AXES(1) = 3
         AXES(2) = NPTS
         CALL WRIMAG('OUTPUT',FMT_DP,AXES,2,POUT,STATUS)
         CALL PER_STOP(STATUS,'ERROUT')
C
C   Now copy the workspace into the output dataset.
C
         CALL PER_COPOUT(%VAL(POUT),AXES,%VAL(PTEMP))
C
C      and add a title
C
         TEXT=' '
         CALL RDKEYC('TITLE',.TRUE.,1,TEXT,I,STATUS)
         CALL WRDSCR('OUTPUT','TITLE',TEXT,1,STATUS)
      END IF
C
C   Tidy up and exit
C
  999 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
