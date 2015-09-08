      SUBROUTINE DFFT2D(INVERS,REPORT,IXEXT,IYEXT,ARRAY,WORK,DTEMP)
*+
*   DFFT2D
*
*   2-D FFT for real, square arrays
*
*   Given         (arguments)
*   INVERS    C    'FORWARD' or 'INVERSE' transform specification.
*   REPORT    C    'Y' causes progress reports to be sent to user.
*   IXEXT     I    X-dimension of image array
*   IYEXT     I    Y-dimension of image array
*   ARRAY     RA   image array (must be square)
*
*   Returned      (arguments)
*   ARRAY     RA   transformed array
*   WORK      RA   work array, same size as ARRAY
*   DTEMP     DA   double precision 1-D work array, 2*IXEXT long.
*
*   Subroutines called :
*   C06EAF,-EBF,-GBF   : NAGLIB
*   WRUSER             : STARLINK
*
*   D.R.K.Brownrigg/ROE/1981
*-

      INTEGER IXEXT,IYEXT
      REAL ARRAY(IXEXT,IYEXT),WORK(IXEXT,IYEXT)
      CHARACTER INVERS*10,REPORT*1
      REAL*8 DTEMP(1024)
      CHARACTER MESSAGE*20,NUM*6
      IFREQ=100
      N1=IXEXT
      MRANGE=N1
      IRANGE=MRANGE/2
      NHALF=N1/2
      NHP1=NHALF+1
      NHP2=NHALF+2
      NHM1=NHALF-1
      IF(INVERS.EQ.'INVERSE') GO TO 1
*
*   FIRST OF ALL TRANSFORM ALL THE ROWS
*
      MESSAGE=' ROWS TRANSFORMED'
      INFO=0
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=ARRAY(J,I)
         ENDDO
         CALL C06EAF(DTEMP,N1,IFAIL)
         DO J=1,N1
            WORK(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF (REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE (NUM,'(I6)') I
            CALL WRUSER(NUM//MESSAGE,ERROR)
         ENDIF
      ENDDO
*
*   NOW (SLIGHTLY MESSIER) TRANSFORM THE COLUMNS
*
      MESSAGE=' COLUMNS TRANSFORMED'
      INFO=0
      DO I=1,IRANGE
         DO J=I,MRANGE-I
            VAL=WORK(MRANGE+1-I,J)
            WORK(MRANGE+1-I,J)=WORK(MRANGE+1-J,MRANGE+1-I)
            WORK(MRANGE+1-J,MRANGE+1-I)=WORK(I,MRANGE+1-J)
            WORK(I,MRANGE+1-J)=WORK(J,I)
            WORK(J,I)=VAL
         ENDDO
      ENDDO
      CALL WRUSER(NUM//'ROTATE COMPLETED',ERROR)
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=WORK(J,I)
         ENDDO
         CALL C06EAF(DTEMP,N1,IFAIL)
         DO J=1,N1
            WORK(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF(REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE(NUM,'(I6)') I
            CALL WRUSER(NUM//MESSAGE,ERROR)
         ENDIF
      ENDDO
*
*   PUT TRANSFORM INTO ARRAY WITH SMALL FREQS. IN CENTRE
*   NB WILL STILL BE ROTATED
*
      DO J=1,NHM1
         DO I=1,NHP1
            ARRAY(NHM1+I,NHP1+J)=-WORK(I,J)
         ENDDO
         DO I=NHP2,N1
            ARRAY(I-NHP1,NHP1+J)=WORK(I,J)
         ENDDO
      ENDDO
      DO J=NHALF,N1
         DO I=1,NHP1
            ARRAY(NHM1+I,J-NHM1)=WORK(I,J)
         ENDDO
         DO I=NHP2,N1
            ARRAY(I-NHP1,J-NHM1)=-WORK(I,J)
         ENDDO
      ENDDO
      CALL WRUSER('TRANSFER END',ERROR)
      RETURN

 1    CONTINUE
*
*   INVERSE TRANSFORM
*   TRANSFORMED, FILTERED AND ROTATED IMAGE IS IN ARRAY
*   FIRST DETRANSFER TO WORK
*
      DO J=1,NHP1
         DO I=1,NHM1
            WORK(NHP1+I,J+NHM1)=-ARRAY(I,J)
         ENDDO
         DO I=NHALF,N1
            WORK(I-NHM1,J+NHM1)=ARRAY(I,J)
         ENDDO
      ENDDO
      DO J=NHP2,N1
         DO I=1,NHM1
            WORK(I+NHP1,J-NHP1)=ARRAY(I,J)
         ENDDO
         DO I=NHALF,N1
            WORK(I-NHM1,J-NHP1)=-ARRAY(I,J)
         ENDDO
      ENDDO
      CALL WRUSER('SHIFT TRANSFER END',ERROR)
      INFO=0
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=WORK(J,I)
         ENDDO
         CALL C06GBF(DTEMP,N1,IFAIL)
         CALL C06EBF(DTEMP,N1,IFAIL)
         DO J=1,N1
            ARRAY(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF(REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE(NUM,'(I6)') I
            CALL WRUSER(NUM//'COLUMNS TRANSFORMED',ERROR)
         ENDIF
      ENDDO
*
*   NOW ROTATE BACK
*
      DO I=1,IRANGE
         DO J=I,MRANGE-I
            VAL=ARRAY(J,I)
            ARRAY(J,I)=ARRAY(I,MRANGE+1-J)
            ARRAY(I,MRANGE+1-J)=ARRAY(MRANGE+1-J,MRANGE+1-I)
            ARRAY(MRANGE+1-J,MRANGE+1-I)=ARRAY(MRANGE+1-I,J)
            ARRAY(MRANGE+1-I,J)=VAL
         ENDDO
      ENDDO
      CALL WRUSER('ROTATED BACK',ERROR)
      INFO=0
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=ARRAY(J,I)
         ENDDO
         CALL C06GBF(DTEMP,N1,IFAIL)
         CALL C06EBF(DTEMP,N1,IFAIL)
         DO J=1,N1
            ARRAY(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF(REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE(NUM,'(I6)') I
            CALL WRUSER(NUM//'ROWS TRANSFORMED',ERROR)
         ENDIF
      ENDDO

      END
