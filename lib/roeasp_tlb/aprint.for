      SUBROUTINE APRINT (TITLE,MAXAP,NAP,DIA,LOGDIA,SKYBRI,
     :                   DELMARK,STATUS)
C+
C     APRINT.
C
C     Subroutine to send to the lineprinter a listing of the
C     values for the sky brightness determined from a set
C     of photoelectric measures.
C
C  Given;
C   TITLE   (C)   Title for the measures, to appear at the top of the
C                 page.
C   MAXAP   (I)   Max. permitted no. of measures; equal to size of 
C                 arrays, below.
C   NAP     (I)   Actual no. of measures.
C   DIA     (RA)  Diameter of the photoelectric aperture.
C   LOGDIA  (RA)  Log (dia) for each aperture.
C   SKYBRI  (RA)  Sky brightness determined from each aperture.
C   DELMARK (LA)  Deletion marker for each aperture.
C                 = .TRUE. - aperture deleted.
C                 = .FALSE. - aperture not deleted.
C
C  Returned;
C   STATUS  (I)   Return status, = 0 for successful return, otherwise
C                 non-zero.
C
C  Subroutines called;
C   System routines:-  DATE, TIME.
C
C  A C Davenhall./ROE/                                      21/7/82.
C-
      INTEGER MAXAP,NAP,STATUS
      REAL DIA(MAXAP),LOGDIA(MAXAP),SKYBRI(MAXAP)
      LOGICAL DELMARK(MAXAP)
      CHARACTER TITLE*20
C
      CHARACTER DATEB*9, TIMEB*8
C
C    Fortran unit no. for I/O.
C
      INTEGER UNIT
      PARAMETER (UNIT=28)
C
      INTEGER NPTS,NAPOK,OSTAT,CSTAT
      REAL SUM,SUMALL,STD,STDALL
C
C    
C    Attempt to open the output file.
C
      OSTAT=0
      CSTAT=0
      OPEN (UNIT=UNIT,FILE='PECALIB.TMP',STATUS='NEW',
     :      IOSTAT=OSTAT)
      IF (OSTAT.EQ.0) THEN
C
C    Obtain the time and date from the system and write the title.
C
        CALL DATE (DATEB)
        CALL TIME (TIMEB)
        WRITE(UNIT,2000) TITLE,TIMEB,DATEB
 2000   FORMAT(1H1,56X,A20,30X,A8,1X,A9//35X,
     :   'Absolute Calibration by Multiaperture Photoelectric ',
     :   'Photometry.',4(/))
C
C    Force the number of apertures to be less than the array size.
C
        NPTS=MIN(NAP,MAXAP)
        WRITE(UNIT,2001) NPTS
 2001   FORMAT(48X,'Number of apertures input = ',I3///)
C
C    Print out the apertures input.
C
        WRITE(UNIT,2002)
 2002   FORMAT(48X,'Diameter',4X,'Log Dia.',1X,'Sky Bright.'/)
        DO I=1,NPTS
          IF (.NOT.DELMARK(I)) THEN
            WRITE(UNIT,2003) I,DIA(I),LOGDIA(I),SKYBRI(I)
 2003       FORMAT(39X,I5,0PF10.1,2X,F10.3,F10.2)
          ELSE
            WRITE(UNIT,2004) I,DIA(I),LOGDIA(I),SKYBRI(I)
 2004       FORMAT(39X,I5,0PF10.1,2X,F10.3,F10.2,2X,'<-Deleted-<<-')
          END IF
        END DO
C
C    Compute the mean and standard deviation of the sky brightness
C    for both the "active" apertures and all the apertures
C    ("active" and "deleted") if there is more than one aperture.
C
        IF (NPTS.GT.1) THEN
          SUM=0.0E0
          SUMALL=0.0E0
          NAPOK=0
          DO I=1,NPTS
            IF (.NOT.DELMARK(I)) THEN
                SUM=SUM+SKYBRI(I)
                NAPOK=NAPOK+1
            END IF
            SUMALL=SUMALL+SKYBRI(I)
          END DO
          SUM=SUM/FLOAT(NAPOK)
          SUMALL=SUMALL/FLOAT(NPTS)
          STD=0.0E0
          STDALL=0.0E0
          DO I=1,NPTS
            IF (.NOT.DELMARK(I))
     :        STD=STD+((SKYBRI(I)-SUM)**2)
            STDALL=STDALL+((SKYBRI(I)-SUM)**2)
          END DO
          STD=SQRT(STD/FLOAT(NAPOK*(NAPOK-1)))
          STDALL=SQRT(STDALL/FLOAT(NPTS*(NPTS-1)))
C
C    Write out the means and standard deviations.
C
          WRITE(UNIT,2005) SUM,STD,SUMALL,STDALL
 2005     FORMAT(////35X,'  FOR the selected measures only:-'//
     :   35X,'Mean = ',0PF10.3,2X,
     :  'Standard deviation of the mean = ',F10.3////35X,
     :  'For all the measures (including deleted ones):-'//35X,
     :  'Mean = 'F10.3,2X,'Standard deviation of the mean = ',
     :   F10.3///)
        END IF
C
C    Attempt to close the output file, disposing of it by sending
C    it to the printer and then deleting it.
C
        CLOSE (UNIT=UNIT,DISPOSE='PRINT/DELETE',IOSTAT=CSTAT)
      END IF
C
C    Set the return status.
C
      STATUS=MAX(OSTAT,CSTAT)
      END
