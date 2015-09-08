      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),STATUS
C
C   Obtain the 2 input images, and the corresponding masks.
C
      CALL RDIMAG('IMAGE1',FMT_R,2,AX,I,IP1,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL RDIMAG('MASK1',FMT_R,2,AX,I,IP2,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL RDKEYR('AVE1',.FALSE.,1,AVE1,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL RDIMAG('IMAGE2',FMT_R,2,AX,I,IP3,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL RDIMAG ('MASK2',FMT_R,2,AX,I,IP4,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL RDKEYR('AVE2',.FALSE.,1,AVE2,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
C
C   Then get the output image, and the output mask.
C
      CALL WRIMAG('OUTPUT',FMT_R,AX,2,IP5,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GO TO 899
      CALL WRIMAG('OUTMASK',FMT_R,AX,2,IP6,STATUS)
C
C   Do the work.
C
      CALL ADDMSK (%VAL(IP1),%VAL(IP2),%VAL(IP3),%VAL(IP4),
     :             AVE1,AVE2,
     :             AX(1),AX(2),%VAL(IP5),%VAL(IP6))
C
C   Tidy up and go home.
C
  899 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE ADDMSK(IN1,MSK1,IN2,MSK2,AV1,AV2,N,M,OUT,MSKO)
C
C      Subroutine ADDMSK
C
C      It forms the sum of the arrays IN1 and IN2 weighted
C      THEIR MEAN VALUES AND MASKS.
C
C      Input parameters:-
C         IN1        The first 2-D array.
C         MSK1       The correspopnding mask.
C         IN2        The second 2-D array.
C         MSK2       The corresponding mask.
C         AV1        THE AVERAGE VALUE OF IN1
C         AV2        THE AVERAGE VALUE OF IN2
C         N          The x-dimension of all 6 arrays.
C         M          The y-dimension of all 6 arrays.
C
C      Output parameters:-
C         OUT        The 2-D output array containing the sum.
C         MSKO       The corresponding mask.
C
C      Written by K F Hartley at RGO on 22/12/81
C
      REAL IN1(N,M),IN2(N,M),MSK1(N,M),MSK2(N,M)
      REAL OUT(N,M),MSKO(N,M)
C
C   Loop through all the pixels.
C
      DO J=1,M
         DO I=1,N
C
C   First form the new mask.
C
            MSKO(I,J)=MSK1(I,J)+MSK2(I,J)
            IF (MSKO(I,J).LT.0.5) THEN
C
C         If it is zero, then pick the first pixel, for want
C         of anything better.
C
               OUT(I,J)=IN1(I,J)
            ELSE
C
C         Otherwise form the weighted sum of the input pixels.
C
            IF (MSK1(I,J).NE.0.AND.MSK2(I,J).NE.0) THEN
               OUT(I,J)=IN1(I,J)+IN2(I,J)
            END IF
            IF (MSK1(I,J).EQ.0.AND.MSK2(I,J).NE.0) THEN
               OUT(I,J)=IN2(I,J)*(AV1+AV2)/AV2
            END IF
            IF (MSK1(I,J).NE.0.AND.MSK2(I,J).EQ.0) THEN
               OUT(I,J)=IN1(I,J)*(AV1+AV2)/AV1
            END IF
            END IF
         END DO
      END DO
      END
