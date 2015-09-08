      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER AX(2),STATUS
      CHARACTER*80 VALUE
C
C   Iniitialize the ARGS and exit if not available
C
      CALL SRINIT(0,.FALSE.,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRERR('ERRARGS')
         CALL EXIT
      END IF
C
C   Read the input (ie the displayed) image.
C
      STATUS=1
      DO WHILE (STATUS.NE.0)
         CALL RDIMAG('INPUT',FMT_R,2,AX,I,IPIN,STATUS)
      END DO
      NVAL=AX(1)*AX(2)
C
C   Find out what values of the low and high cut-offs were
C   used when the image was displayed.
C   (It assumes that the image in question was the last one displayed)
C
      CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
      CALL ASP_DZTOF('PVLO',VALUE,ZMIN,JSTAT)
      IF (JSTAT.NE.0) ZMIN=0.0
      CALL ASP_DZTOF('PVHI',VALUE,ZMAX,JSTAT)
      IF (JSTAT.NE.0) ZMAX=255.0
      NHIST=INT(ZMAX+1-ZMIN)
C
C   Get some work space for the histogram
C
      CALL GETDYN('HIST',FMT_SW,NHIST,IPH,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRERR('ERRGD')
         CALL EXIT
      END IF
      CALL HIST(%VAL(IPIN),NVAL,%VAL(IPH),NHIST,ZMIN,ZMAX)
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE HIST(DATA,N,IHIST,NH,ZMIN,ZMAX)
C
C   This subroutine does all th ework as described above.
C   All the parameters are of type INPUT.
C
C      DATA	The input data array (real)
C      N	The size of DATA
C      IHIST	A work array for the histogram.
C      NH	The size of the histogram
C      ZMIN	The low cut-off for the histogram
C      ZMAX     The high cut-off for the histogram.
C
C   Written by K F Hartley at RGO October 1982
C
      REAL DATA(N)
      INTEGER IHIST(NH),LIMITS(2,8)
      INTEGER LUT(3,0:255),ICOLS(3,8)
C
C   This array defines the 8 colours used for the LUT.
C
      DATA ICOLS/0,0,0,255,0,255,0,0,255,0,255,255,0,255,0,
     :           255,255,0,255,0,0,255,255,255/
C
C   First clear, then calculate the histogram for all data
C   samples lying between the two limits.
C
      DO I=1,NH
         IHIST(I)=0
      END DO
      NUSED=0
      DO I=1,N
         NP=INT(DATA(I)-ZMIN) + 1
         IF (NP.GT.0.AND.NP.LE.NH) THEN
            IHIST(NP)=IHIST(NP)+1
C
C      Note that NUSED counts the number of samples between the limits.
C
            NUSED=NUSED+1
         END IF
      END DO
C
C   The array LIMITS is going to be used to store the boundaries
C   where the colours will change in the LUT.
C
C   First set up default values in case of slightly pathological
C   histograms, when not all the colours will be used.
C
      DO I=1,8
         LIMITS(1,I)=255
         LIMITS(2,I)=255
      END DO
      LIMITS(1,1)=0
C
C   In principle each colour should be used for the same number
C   of samples - the total in the range divided by the
C   number of colours being used.
C
      NBIN=NUSED/8
      ICO=1
      ITOT=0
C
C   Now run through the histogram, setting the next limit
C   every time the appropriate number of entries has been counted.
C
C   Note that this is only approximate as no re-distribution of
C   samples takes place.
C
      DO I=1,NH
         ITOT=ITOT+IHIST(I)
         IF (ITOT.GE.NBIN) THEN
            LIMITS(2,ICO)=INT((REAL(I))*255.0/(ZMAX-ZMIN))
            LIMITS(1,ICO+1)=LIMITS(2,ICO)+1
            ICO=ICO+1
            ITOT=0
         END IF
      END DO
C
C   Now fill the LUT with the colours in turn, in the ranges
C   defined from the histogram.
C
      DO I=1,8
         DO L=LIMITS(1,I),LIMITS(2,I)
            DO J=1,3
               IF (ICOLS(J,I).GT.255) THEN
                  LUT(J,L)=255
               ELSE IF (ICOLS(J,I).LT.0) THEN
                  LUT(J,I)=0
               ELSE
                  LUT(J,L)=ICOLS(J,I)
               END IF
            END DO
         END DO
      END DO
C
C   Write the LUT to the ARGS.
C
      CALL SRCOLS(0,256,LUT)
      END
