      SUBROUTINE CDJD(IYEAR,M,IDAY,JD)
C
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C   SUBROUTINE CDJD 
C
C
C   To convert Civil Date to Julian Date in range 1925 to 1999. 
C
C
C   IYEAR       I     In 
C                             Year 
C
C   M           I     In 
C                             Month 
C
C   IDAY        I     In 
C                             Day 
C
C   JD          R*8   Out 
C                             Julian Date 
C
C
C   AEC (BE) (BMH) 
C   RGO 
C   28-SEP-82 
C
C-------------------------------------------------------------------- 
        
        
        
C                                     
C     Routine to convert a Civil Date to a Julian Date.
C
C     Parameters needed are:-
C
C          IYEAR  - Integer for input of 4 digit year
C          M      - Integer for input of 2 digit month
C          IDAY   - Integer for input of 2 digit day
C
C          JD     - (Double precision) Real*8 for output of Julian Date
C                   in days and decimals
C
C     Range of Civil Date:
C
C          Original version says range 1925-1999, but the calculation
C          should be correct up to 2099 unless any changes are introduced
C          from 2000 onwards, as has been discussed at some point.
C

      DOUBLE PRECISION JD
      DIMENSION N(12)
     0DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10),N(11),
     1N(12)/  0,  31,  59,  90, 120, 151, 181, 212, 243,  273, 304, 334/
C
      JD = 2415019.5D0 + FLOAT((IYEAR-1900)*365+(IYEAR-1900)/4
     1    + N(M)+IDAY)
      IF(MOD(IYEAR,4).EQ.0.AND.M.LE.2) JD = JD - 1.0D0
      RETURN
      END
