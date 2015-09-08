      SUBROUTINE WRITBLOK(LU,NL)                                                
C                                                                               
C      THIS OUTPUTS BINARY RECORDS TO A FILE                                    
C      ASSIGNED TO LOGICAL UNIT LU                                              
C      EACH RECORD IS NL BYTES LONG ( UP TO 264 BYTES OR 2112 BITS)             
C                                                                               
      LOGICAL*1 IOUT(264)                                                       
      BYTE BYTE1
      DATA BYTE1/4/
      COMMON /LINKF/IOUT                                                        
      WRITE (LU) BYTE1,(IOUT(J),J=1,NL)                                           
      RETURN                                                                    
      END                                                                       
