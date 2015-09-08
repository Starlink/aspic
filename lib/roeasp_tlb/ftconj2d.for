      SUBROUTINE FTCONJ2D(REPORT,IXEXT,IYEXT,ARRAY,WORK,DTEMP)
C+
C   FTCONJ2D
C
C   Complex conjugation for Fourier Transforms produced by
C   the DFFT routines
C
C   Given         (arguments)
C   REPORT    C    'Y' causes progress reports to be sent to user.
C   IXEXT     I    X-dimension of image array
C   IYEXT     I    Y-dimension of image array
C   ARRAY     RA   image array (must be square)
C
C   Returned      (arguments)
C   ARRAY     RA   transformed array
C   WORK      RA   work array, same size as ARRAY
C   DTEMP     DA   double precision 1-D work array, 2*IXEXT long.
C
C   Subroutines called :
C   C06GBF             : NAGLIB
C   WRUSER             : STARLINK
C
C   B.V.McNally ROE/DEC-1983
C-

C  Variable type definitions

      INTEGER IXEXT,IYEXT
      CHARACTER REPORT*1
      DOUBLE PRECISION DTEMP(1024)
      REAL ARRAY(IXEXT,IYEXT),WORK(IXEXT,IYEXT)
      CHARACTER MESSAGE*32

C  Set up control variables

      IFREQ=100
      N1=IXEXT
      MRANGE=N1
      IRANGE=MRANGE/2
      NHALF=N1/2
      NHP1=NHALF+1
      NHP2=NHALF+2
      NHM1=NHALF-1

C  Transfer the data array from its packed format into a work array

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
      CALL WRUSER('Data unpacking complete',ERROR)

C  Rotate the work array to put rows back in place

      DO I=1,IRANGE
         DO J=I,MRANGE-I
            VAL=WORK(J,I)
            WORK(J,I)=WORK(I,MRANGE+1-J)
            WORK(I,MRANGE+1-J)=WORK(MRANGE+1-J,MRANGE+1-I)
            WORK(MRANGE+1-J,MRANGE+1-I)=WORK(MRANGE+1-I,J)
            WORK(MRANGE+1-I,J)=VAL
         ENDDO
      ENDDO
      CALL WRUSER('Back rotation complete',ERROR)

C  Perform the complex conjugate of the rows

      INFO=0
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=WORK(J,I)
         ENDDO
         CALL C06GBF(DTEMP,N1,IFAIL)
         DO J=1,N1
            WORK(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF(REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE(MESSAGE,'(I6,''Rows conjugated'')') I
            CALL WRUSER(MESSAGE,ERROR)
         ENDIF
      ENDDO

C  Rotate the work array to put columns into row position for
C  faster access during processing

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
      CALL WRUSER('Forward rotation completed',ERROR)

C  Perform the complex conjugate of the columns (in row order)

      INFO=0
      DO I=1,N1
         DO J=1,N1
            DTEMP(J)=WORK(J,I)
         ENDDO
         CALL C06GBF(DTEMP,N1,IFAIL)
         DO J=1,N1
            WORK(J,I)=DTEMP(J)
         ENDDO
         INFO=MOD(INFO+1,IFREQ)
         IF(REPORT.EQ.'Y'.AND.(INFO.EQ.0.OR.I.EQ.N1)) THEN
            WRITE(MESSAGE,'(I6,''Columns conjugated'')') I
            CALL WRUSER(MESSAGE,ERROR)
         ENDIF
      ENDDO

C  Pack the complex conjugate into ARRAY with small frequencies in
C  the center. NB. The data will still be in the rotated form as input
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
      CALL WRUSER('Conjugation completed',ERROR)

      END
