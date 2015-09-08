      SUBROUTINE MEANS
C+
C     MEANS.
C
C     Subroutine to compute the mean and standard error of
C     a user defined patch of an array. The results are
C     written to both the environment and the user.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interface:-  INPICR, READI, WRITER, OUTPUT, CLEARIM.
C   E2D:-        AVER, STDEV.
C
C  Structure:-
C   Attempt to obtain a pointer to the array.
C   If pointer obtained OK
C     read in coords. of region
C     if coords read Ok
C       compute mean and error
C       write results to the environment
C       write results to the user.
C     else
C       print error message.
C     end if
C   else
C     print error message.
C   end if
C   tidy up input image.
C
C  A C Davenhall./ROE/                               28/10/82.
C-
      INTEGER XEXT,YEXT,AXIS(2),ARRPTR
      INTEGER IOSTAT,ARSTAT,CRSTAT
      INTEGER XBASE,YBASE,XTOP,YTOP
      REAL    MEAN,STDERR
      CHARACTER BUFFER*60
C
C
      IOSTAT=0
C
C    Attempt to obtain a pointer to the required array.
C   
      ARSTAT=0
      CALL INPICR ('ARRAY',
     :  ' Enter file name of the required image;',
     :    2,AXIS,ARRPTR,ARSTAT)
C
C    Proceed if the pointer has been obtained Ok.
C
      IF (ARSTAT.EQ.0) THEN
        XEXT=AXIS(1)
        YEXT=AXIS(2)
C
C    Attempt to obtain the coords. for the required region.
C
        CRSTAT=0
        CALL READI ('XBASE',
     :   ' Enter the lower X coord. of the selected region;',
     :     1,1,XEXT,XBASE,CRSTAT)
        CALL READI ('YBASE',
     :   ' Enter the lower Y coord. of the selected region;',
     :     XBASE,1,YEXT,YBASE,CRSTAT)
        CALL READI ('XTOP',
     :   ' Enter the upper X coord. of the selected region;',
     :     XEXT,XBASE,XEXT,XTOP,CRSTAT)
        CALL READI ('YTOP',
     :   ' Enter the upper Y coord. of the selected region;',
     :     XTOP,YBASE,YEXT,YTOP,CRSTAT)
C
C    Proceed if these values have been obtained Ok.
C
        IF (CRSTAT.EQ.0) THEN
C
C    Compute the mean.
C
          CALL AVER (XEXT,YEXT,%VAL(ARRPTR),XBASE,YBASE,
     :               XTOP,YTOP,MEAN)
C
C    Compute the standard error of the mean.
C
          CALL STDEV (XEXT,YEXT,%VAL(ARRPTR),XBASE,YBASE,
     :                XTOP,YTOP,MEAN,STDERR)
C
C    Write out the mean and standard error to the environment.
C
          CALL WRITER ('MEAN',MEAN,IOSTAT)
          CALL WRITER ('STDERR',STDERR,IOSTAT)
C
C    Write out the mean and standard error to the user.
C
          CALL OUTPUT ('  ',IOSTAT)
          WRITE(BUFFER,2000) MEAN
 2000     FORMAT(1X,'Mean = ',1PE12.3)
          CALL OUTPUT (BUFFER,IOSTAT)
          WRITE(BUFFER,2001) STDERR
 2001     FORMAT(1X,'Standard error of the mean = ',1PE12.3)
          CALL OUTPUT (BUFFER,IOSTAT)
          CALL OUTPUT ('  ',IOSTAT)
        ELSE
          CALL OUTPUT (
     :  ' ***ERROR Unable to obtain coords. of required region.',
     :      IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain image successfully.',IOSTAT)
      END IF
C
C    Tidy up the image.
C
      CALL CLEARIM ('ARRAY')
      END
