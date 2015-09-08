      SUBROUTINE JDCD (DJ,IY,M,ID)
C
 
C
 
C
 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
C
 
C   SUBROUTINE JDCD 
 
C
 
C
 
C   To convert Julian Date and decimal to Civil Date and Time.
 
C
 
C
 
C   DJ          R*8   In
 
C                             Julian Date and decimals.
 
C
 
C   IY          I     Out
 
C                             Year
 
C
 
C   M           I     Out
 
C                             Month
 
C
 
C   ID          I     Out
 
C                             Day
 
C
 
C
 
C   AEC (BMH)       
 
C   RGO             
 
C   28-SEP-82
 
C
 
C--------------------------------------------------------------------
 
        
        
        
C                                 
C     Routine to convert Julian Date to Civil Date
C
C     Parameters needed are:-
C
C         DJ - Real*8 (Double precision) for input of Julian Date
C
C         IY - Integer for output of Year
C
C         M  - Integer for output of Month
C
C         ID - Integer for output of Day
C
C     Accuracy:-
C
C         Julian Date includes fraction of day, but the output will
C         only be given to the day.
C
      DIMENSION N(12)
      DOUBLE PRECISION DJ
     0DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10),N(11),
     1    N(12)/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     Remove from Julian Date the equivalent of 1900.0 and convert the
C     remainder to integer
C
      J = DJ-2415019.5D0
      J = J*100
C
C     Calculate Year
C
      IY = J/36525 + 1900
C
C     Test for Leap Year, then calculate Month and Day
C
      LR = MOD(IY,4)
      ID = (MOD(J,36525) + 25*LR - 100)/100
      M = 1
      IF ((MOD(IY,4)).EQ.0) THEN
        N(2) = 29
        ID = ID + 1
      ELSE
        N(2) = 28
      ENDIF
 100  ID = ID - N(M)
      IF (ID.GE.0) THEN
        M = M + 1
        GO TO 100
      ELSE
        ID = ID + N(M) + 1
      ENDIF
      RETURN
      END
