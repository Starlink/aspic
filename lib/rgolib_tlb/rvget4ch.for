      SUBROUTINE RV_GET4CH( VAL, CHARS)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE RV_GET4CH
C
C
C         It convets an integer into the 4 ICL characters it contains, when 
C         used as a substitute for character variables. 
C
C         VAL         I*4   In    This is  the  integer  which  stores  the 
C                                 input value. 
C
C         CHARS       Char  Out   This contains the four  characters  which 
C                                 were extracted from VAL. 
C
C
C         J V Carey                RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                      
                                                                                
                                                                                
                                                                                
C This subroutine converts the number stored in VAL
C into the 4 ICL characters corresponding to it
C and stores them in the character*4 variable CHARS
      CHARACTER CHARS*4
      CHARACTER*1 CHARVAL(0:63)
      INTEGER VAL, CH
C
      COMMON/RVTRAN/CHARVAL
      DO I = 4,1,-1
          CH = MOD(VAL,64)
          VAL =  VAL/64
          IF(CH.LT.0) THEN
               CH = CH + 64
               VAL = VAL - 1
          ENDIF
          CHARS(I:I) = CHARVAL(CH)
      ENDDO
      END
