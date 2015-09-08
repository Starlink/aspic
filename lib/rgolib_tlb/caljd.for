      SUBROUTINE CALJD(DATE,JD)



C
 
C
 
C
 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C
 
C   SUBROUTINE CALJD
 
C
 
C
 
C   To convert from  Calendar  Date  and  Time  to  Julian  Date  and
 
C   decimals.   N.B.   This  routine  has been amended - please check
 
C   your results carefully and report any errors to Miss B  M  Harris
 
C   (Ext.3431).   For fuller details of the subroutine please see the
 
C   comments in the source file following the line commencing C-
 
C
 
C
 
C   DATE        I*6   In
 
C                             Array containing Calendar Date etc.  in
 
C                             the following form:  (1) Year (2) Month
 
C                             (3) Day (4) Hour (5)  Minute  (nearest)
 
C                             (6)  =0 (for U.T.) or =1 (for G.M.A.T.)
 
C                             - essential for dates prior to 1925.0
 
C
 
C   JD          R*8   Out
 
C                             Julian  Date  and  decimals  of  day  -
 
C                             accurate to approx.  9 places.
 
C
 
C
 
C   LVM (BMH)             
 
C   RGO             
 
C   19-APR-83
 
C
 
C--------------------------------------------------------------------
 
        
        
        

C     Routine to convert from Calendar Date and Time to Julian
C     Date and decimals.
C
C     Parameters needed are :-
C
C         DATE(6) - Integer array for input of :-
C
C                   (1) Year
C                   (2) Month
C                   (3) Day
C                   (4) Hour
C                   (5) Minute
C                   (6) U.T. (=0) or G.M.A.T. (=1)
C
C         JD      - Double precision (REAL*8) for output of Julian Date.
C
C     Notes:
C
C       (1)  Accepts any Calendar Date (or G.M.A.T) starting from
C            -4712 Astronomical Year and less than 9999.
C       (2)  Dates after 1582 October 4 must be in Gregorian Calendar.
C       (3)  Prior to 1925.0 it is essential to specify in DATE(6)
C            whether the date required is U.T. or G.M.A.T.
C
C     Accuracy:
C
C        Input  : Nearest minute.
C        Output : Approximately 9 decimal places of Julian Day.
C
      DOUBLE PRECISION JD,FRACTN,CY
      INTEGER DATE(6),M(2,12),DIFF
C
C     When converting from Calendar Date to Julian Date, it is necessary
C     to convert Years, Months, Days, Hours and Minutes to days and
C     decimals of a Julian Day.
C
C     When dealing with the conversion of the month, what is actually
C     required is the number of days in the year which precede the start
C     of that particular month. This is most easily dealt with by
C     storing in an array the appropriate value for the month. E.g.
C
C         No. of days in year preceding January  =  0 :  ARRAY(1) =  0
C          "   "   "   "   "      "     February = 31 :  ARRAY(2) = 31
C
C     To deal with the difference between leap years and non-leap years,
C     two version are stored in a 2-dimensional array, so that non-leap
C     years are in (1,n), and leap years are in (2,n), where n=1,12.
C     It is then a simple matter to decide whether the year in question
C     is a leap year or not, and set a pointer to 1 or 2 as appropriate
C     to pick up the relevant part of the table.
C
      DATA M(1,1),M(1,2),M(1,3),M(1,4),M(1,5),M(1,6),M(1,7),M(1,8)/
     1          0,    31,    59,    90,   120,   151,   181,   212/,
     2     M(1,9),M(1,10),M(1,11),M(1,12)/
     3        243,    273,    304,    334/,
     4     M(2,1),M(2,2),M(2,3),M(2,4),M(2,5),M(2,6),M(2,7),M(2,8)/
     5          0,    31,    60,    91,   121,   152,   182,   213/,
     6     M(2,9),M(2,10),M(2,11),M(2,12)/
     7        244,    274,    305,    335/
C
C     Decide whether or not year is a leap year, and set the appropriate
C     pointer for the 'month' table.
C
      IDAT = MOD(DATE(1),4)
      IF (IDAT.EQ.0) THEN
        L = 2
      ELSE
        L = 1
      ENDIF
C
C     Check for Centurial years after 1600 which would be leap years but
C     are not divisible by 400, and so should not be. Set pointer for
C     non-leap year part of 'month' table.
C
      IF (DATE(1).GE.1700) THEN
        IDAT = MOD(DATE(1),100)
        IF (IDAT.EQ.0) THEN
          DIFF = DATE(1)-1600
          IF (DIFF.LE.0) THEN
            L = 1
          ELSE
            IDIF = MOD(DIFF,400)
            IF (IDIF.EQ.0) THEN
              L = 2
            ELSE
              L = 1
            ENDIF
          ENDIF
        ENDIF
C
C     Calculate increase/decrease in divergence of Julian/Gregorian
C     calendars from each other.
C
        NY =-(1601-DATE(1))/100
        CY = FLOAT(NY-(NY/4))
      ELSE
        CY = 0.0D0
      ENDIF
C
C     Calculate Julian Date, removing the 10-day difference between
C     the Julian/Gregorian calendars where required.
C
      N = DATE(2)
      FRACTN = (FLOAT(DATE(4))+FLOAT(DATE(5))/60.0D0)/24.0D0
     1    + 0.0000000005D0
      JD = FLOAT(INT(FLOAT(DATE(1)+4712)*365.25D0-0.25D0)+M(L,N)
     1    +DATE(3))+FRACTN-CY-0.5D0
      IF (JD.GE.2299169.5D0) JD = JD - 10.0D0
C
C     If year is prior to 1925, check whether G.M.A.T. is required
C     and adjust result accordingly.
c
      IF (DATE(1).LT.1925) THEN
        IF (DATE(6).GT.0) JD = JD + 0.5D0
      ENDIF
      RETURN
      END
