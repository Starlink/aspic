      PROGRAM STORE
C+
C     Program STORE
C
C      This program copies data from a formatted file, as created
C      by an editor or any Fortran program, into the format required
C      by all the other programs in the period finding package.
C
C         The input file may consist of up to 10000 records, each
C         of which consists of an epoch followed up to 25 data values,
C         all of which are in free format (separated by spaces, written
C         in I, F or E format).
C
C         The output file will be a .BDF file as described in SUN 4,
C         consisting of a 3xN array of double precision numbers.
C            The first will be the epoch;
C            The second will be the average of several input values
C            (each with its own constant offset);
C            The third will be a weight, which at this stage will all
C            have the value 1.
C
C         Options are available to sort the input data into increasing
C         order of epoch, and to exclude all occurrences of some
C         "invalid value".
C
C         A title may also be stored with the output dataset.
C
C      Written by K.F.Hartley at RGO on 23-Jan-1984
C      (Based heavilly on MSTORE by C.D.Pike)
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),POUT,STATUS,IMAG(100)
      DOUBLE PRECISION T(10000),VIN(25,10000),V(10000)
      REAL VAL(2),CORR(100)
      CHARACTER*50 FNAME
      CHARACTER*72 TITLE
      LOGICAL SORT
C
C   Obtain name of input VMS file, from which the data are to be
C   read, using standard FORTRAN input routines.
C
C
  100 CONTINUE
      CALL WRUSER('Enter name of input file',ISTAT)
      CALL WRUSER('(.DAT is assumed if no type is specified)',ISTAT)
      CALL RDKEYC('FNAME',.FALSE.,1,FNAME,I,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRUSER('Failed to understand input',ISTAT)
         CALL CNPAR('FNAME',ISTAT)
         FNAME=' '
         GO TO 100
      END IF
C
C   Now open the file requested.
C
      OPEN (UNIT=99,TYPE='OLD',NAME=FNAME)
C
C   Because of the wish for free format it is desirable to specify
C   how many numbers occur in each record. Note that the user is
C   asked for the number of data values, so that each record must
C   contain an epoch plus that number.
C
      CALL WRUSER('Enter number of data samples at each epoch',ISTAT)
      NMAG=1
      CALL RDKEYI('NUMVAL',.TRUE.,1,NMAG,I,ISTAT)
C
C   Loop around reading records until an error (usually EOF)
C   is detected.
C
      DO I=1,10000
         READ  (99,*,ERR=110) T(I),(VIN(K,I),K=1,NMAG)
      END DO
  110 NSAMP=I-1
      CLOSE (UNIT=99)
C
C   Note that a value of 0 for NSAMP implies that the file was bad,
C   so report and exit.
C
      IF (NSAMP.LE.0) THEN
         CALL WRUSER('No valid entries found',ISTAT)
         CALL FRDATA(' ',ISTAT)
         CALL EXIT
      END IF
C
C   The user is also allowed to reject all entries which have
C   a specified "invalid" value.
C
      RUBBISH=-999
      CALL RDKEYR('INVVAL',.TRUE.,1,RUBBISH,I,ISTAT)
C
C   Now obtain the entries in each record which are to be averaged.
C   Each column may have its own offset specified.
C
      DO I=1,NMAG
         CALL WRUSER('Enter column and its offset value',ISTAT)
         CALL WRUSER('End by hitting <CR>',ISTAT)
         CALL RDKEYR('COLUMN',.FALSE.,2,VAL,IN,ISTAT)
         IF (ISTAT.NE.ERR_NORMAL) GO TO 205
         IMAG(I)=INT(VAL(1))
         IF (IN.EQ.2) THEN
            CORR(IMAG(I))=VAL(2)
         ELSE
            CORR(I)=0.0
         END IF
         CALL CNPAR('COLUMN',ISTAT)
      ENDDO

  205 CONTINUE
      NUMMAG = I-1

C
C   Perform the averaging, writing the result into array V
C
      DO I=1,NSAMP
  210    CONTINUE
         V(I) = 0.0
         ITOT = 0
         DO J=1,NUMMAG
            ILINE = IMAG(J)
            IF(VIN(ILINE,I).NE.RUBBISH)  THEN
               V(I) = V(I) + VIN(ILINE,I) + CORR(ILINE)
               ITOT = ITOT + 1
            END IF
         END DO
C
C      Note that if ITOT is still zero then no valid values
C      were found in that record, so ignore it.
C
         IF (ITOT.EQ.0) THEN
            GO TO 210
         ELSE
            V(I) = V(I) /REAL(ITOT)
         END IF
      END DO
C
C   Having read in the data the user is allowed the option of
C   sorting them into chronological order (ie increasing epoch).
C
      SORT=.FALSE.
      CALL RDKEYL('SORT',.TRUE.,1,SORT,I,ISTAT)
      IF (SORT) THEN
         CALL PER_LIMSORT(T,V,NSAMP)
      END IF
C
C   Having performed all the arithmetic, obtain an output dataset.
C   Note that the results are stored as double precision.
C
  300 CONTINUE
      AX(1)=3
      AX(2)=NSAMP
      CALL WRIMAG('OUTPUT',FMT_DP,AX,2,POUT,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         CALL CNPAR('OUTPUT',STATUS)
         CALL FRDATA('OUTPUT',STATUS)
         GO TO 300
      END IF
      CALL PER_INSTOR(%VAL(POUT),AX(1),AX(2),T,V,RUBBISH)
C
C   Finally obtain a title for this dataset
C
      TITLE=' '
      CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,ISTAT)
      CALL WRDSCR('OUTPUT','TITLE',TITLE,1,ISTAT)
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END 
