      SUBROUTINE GAP(LU,NL,NR)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE GAP 
C
C
C         It writes a  specified  number  of  blank  lines  to  a  Versatec 
C         plotting  file.  It  is part of theGreyscale package described in 
C         LUN 14. 
C
C         LU          I*4   In    The logical unit to which output is to be 
C                                 sent. 
C
C         NL          I*4   In    The length of the records to be output  . 
C                                 A  value  of  264  corresponds  to a full 
C                                 width line. 
C
C         NR          I*4   In    The number of records to be output.  Note 
C                                 that  200 records are required to produce 
C                                 an inch of blank paper. 
C
C
C         K F Hartley              RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                             
                                                                                
                                                                                
                                                                                
C      USED TO OUTPUT BLANK PAPER TO A VERSATEC PLOTTING FILE
C
      INTEGER*4 LU,NL,NR
      LOGICAL*1 IOUT(264)
      COMMON /LINKF/IOUT
C
C      SET ARRAY TO ZEROS
C
      DO 100 I=1,NL
         IOUT(I)=0
  100 CONTINUE
C
C      NOW WRITE THE BLANK RECORD NR TIMES
C
      DO 200 I=1,NR
         CALL WRITBLOK(LU,NL)
  200 CONTINUE
      RETURN
      END
