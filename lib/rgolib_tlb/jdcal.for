      SUBROUTINE JDCAL(JD,DATE)
C
 
C
 
C
 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C
 
C   SUBROUTINE JDCAL
 
C
 
C
 
C   To convert from Julian Date and  decimal  to  Calendar  Date  and
 
C   Time.   N.B.   This  routine has been amended - please check your
 
C   results carefully and report any errors to Miss B M Harris.   For
 
C   fuller  details  of the subroutine please see the comments in the
 
C   source file following the line commencing C-
 
C
 
C
 
C   JD          R*8   In
 
C                             Julian Date and decimals
 
C
 
C   DATE        I*6   Out
 
C                             Array containing Calendar Date etc.  in
 
C                             the following form:  (1) Year (2) Month
 
C                             (3) Day (4) Hour (5)  Minute  (nearest)
 
C                             (6)    Zero    -   only   present   for
 
C                             compatibility with CALJD.  etween  U.T.
 
C                             and G.M.A.T.  before 1925.0
 
C
 
C
 
C   LVM (BMH)
 
C   RGO             
 
C   1-OCT-82
 
C
 
C--------------------------------------------------------------------
 
        
        
        
C                              
C     Routine to convert from Julian Date and decimal to
C     Calendar Date and Time.
C
C     Parameters needed are:-
C
C         JD      - Real*8 (Double precision) for input of Julian Date
C
C         DATE(6) - Integer array for output of:-
C
C                   (1) Year
C                   (2) Month
C                   (3) Day
C                   (4) Hour
C                   (5) Minute
C                   (6) Zero - only present for compatibility with
C                       CALJD which may have U.T. or G.M.A.T. input
C                       and uses this as an indicator (0/1 respectively)
C
C     Accuracy:-
C
C         Input  : Approximately 9 decimal places
C         Output : Nearest minute
C
      INTEGER DATE(6)
      DOUBLE PRECISION M(48),JD,SJD,REM,DIFF,TEMPA
C
C     Set up number of days in month for 4-year cycle (leap year first).
C
      DATA M(1),M(2),M(3),M(4),M(5),M(6),M(7),M(8),M(9),M(10),M(11),M(12
     1),M(13),M(14),M(15),M(16),M(17),M(18),M(19),M(20),M(21),M(22),M(23
     2),M(24),M(25),M(26),M(27),M(28),M(29),M(30),M(31),M(32),M(33),M(34
     3),M(35),M(36),M(37),M(38),M(39),M(40),M(41),M(42),M(43),M(44),M(45
     4),M(46),M(47),M(48)/31.0D0,29.0D0,31.0D0,30.0D0,31.0D0,30.0D0,31.0
     5D0,31.0D0,30.0D0,31.0D0,30.0D0,31.0D0,31.0D0,28.0D0,31.0D0,30.0D0,
     631.0D0,30.0D0,31.0D0,31.0D0,30.0D0,31.0D0,30.0D0,31.0D0,31.0D0,28.
     70D0,31.0D0,30.0D0,31.0D0,30.0D0,31.0D0,31.0D0,30.0D0,31.0D0,30.0D0
     8,31.0D0,31.0D0,28.0D0,31.0D0,30.0D0,31.0D0,30.0D0,31.0D0,31.0D0,30
     9.0D0,31.0D0,30.0D0,31.0D0/
C
C     Copy original 'JD' into 'SJD' to enable it to be manipulated
C
      J = 1
      SJD = JD
C
C     Test whether date is prior to change from Julian to Gregorian
C     Calendar. If not, add 10-day difference between the two, and sort
C     out the dropping of the extra day at the turn of the century when
C     the year is not divisible by 400 from 1700 onwards.
C
      IF (SJD.GE.2299160.5D0) THEN
        SJD = SJD + 10.0D0
        REM = SJD - 2342041.5D0
        IF (REM.GT.0.0D0) THEN
          DO 100 J=1,100
            REM = REM - FLOAT(36524 + (J/4 - ((J-1)/4)))
            IF (REM.LT.0.0D0) THEN
              SJD = SJD + FLOAT(J - (J/4))
              GO TO 101
            ENDIF
  100     CONTINUE
        ELSE
          IF (REM.EQ.0.0D0) SJD = SJD + FLOAT(J - (J/4))
        ENDIF
      ENDIF
 101  SJD = SJD + 0.5D0
C
C     Calculate the year
C
      IY = INT(SJD/1461.0D0)
      DIFF = SJD - FLOAT(IY*1461)
      DO 102 JM=1,48
        DIFF = DIFF - M(JM)
        IF (DIFF.LT.0.0D0) GO TO 103
  102 CONTINUE
  103 DATE(1) = IY*4 - 4712 + (JM-1)/12
C
C     Calculate the month, day, hour and minute. Also set up indicator.
C
      DATE(2) = JM - ((JM-1)/12)*12
      DIFF = DIFF + M(JM) + 1.0D0
      DATE(3) = INT(DIFF)
      TEMPA = (DIFF - FLOAT(DATE(3)))*24.0D0
      DATE(4) = INT(TEMPA)
      DATE(5) = NINT((TEMPA - FLOAT(DATE(4)))*60.0D0)
      IF (DATE(5).EQ.60) THEN
        DATE(5) = 0
        DATE(4) = DATE(4) + 1
        IF (DATE(4).EQ.24) THEN
          DATE(4) = 0
          DATE(3) = DATE(3) + 1
          IF (DATE(3).EQ.32) THEN
            DATE(3) = 1
            DATE(2) = DATE(2) + 1
            IF (DATE(2).EQ.13) THEN
              DATE(2) = 1
              DATE(1) = DATE(1) + 1
            ENDIF
          ELSE
            IF (DATE(3).EQ.31) THEN
              IF (DATE(2).EQ.4.OR.DATE(2).EQ.6.OR.DATE(2).EQ.9.
     1          OR.DATE(2).EQ.11) THEN
                DATE(3) = 1
                DATE(2) = DATE(2) + 1
              ENDIF
            ELSE
              IF (DATE(3).EQ.30.AND.DATE(2).EQ.2) THEN
                DATE(3) = 1
                DATE(2) = 3
              ELSE
                IF (DATE(3).EQ.29.AND.DATE(2).EQ.2.AND.(MOD(DATE(1),4)
     1            .NE.0)) THEN
                  DATE(3) = 1
                  DATE(2) = 3
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      DATE(6) = 0
      RETURN
      END
