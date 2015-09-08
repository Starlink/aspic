      FUNCTION AXINCR (RANGE)
C+
C      Compute a suitable increment for putting tick
C      marks on an axis.
C
C  Given;
C  RANGE (R)  Total range of axis to be marked.
C
C  Returned;
C  AXINCR (R) Increment between tick marks.
C
C    Structure:-
C
C    Take log10 of range.
C    seperate log into mantissa & characteristic.
C    subtract 1 from characteristic.
C    form a number; 1.0 + mantissa.
C    antilog this number.
C    divide by 10 & fix the result to integer.
C    float & multiply by 10.
C    take logarithm
C    extract mantissa.
C    pair up new mantissa with new characteristic.
C    antilog the result.
C
C  A C Davenhall./ROE/                       1/2/82.
C-
      REAL RANGE
      REAL LOGRAN,MANTIS,XX,LOGINC
      INTEGER CHARAC
      LOGRAN=ALOG10(RANGE)
      CHARAC=INT(LOGRAN)
      IF (LOGRAN.LT.0) CHARAC=CHARAC-1
      MANTIS=LOGRAN-FLOAT(CHARAC)
      CHARAC=CHARAC-1
      XX=1.0+MANTIS
      XX=1.0E1**XX
      NN=INT(XX/1.0E1)
      XX=ALOG10(FLOAT(NN)*1.0E1)
      NN=INT(XX)
      MANTIS=XX-FLOAT(NN)
      LOGINC=FLOAT(CHARAC)+MANTIS
      AXINCR=1.0E1**LOGINC
      END
