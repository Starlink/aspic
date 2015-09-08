      SUBROUTINE GETHETA(X1,Y1,X,Y,ANGLE)                                       
C+
C     GETHETA.
C
C     ASPIC Subroutine inserted into E2D by
C     A C Davenhall./ROE/                             5/7/82.
C
C     Associated with the "cake slice" extracting routines.
C-
C                                                                               
      IF(X.EQ.X1)  THEN                                                         
         IF(Y.GE.Y1)  THEN                                                      
            ANGLE = 1.57079                                                     
         ELSE                                                                   
            ANGLE = 4.712389                                                    
         ENDIF                                                                  
         RETURN                                                                 
      ENDIF                                                                     
      ANGLE = ATAN((Y-Y1)/(X-X1))                                               
      IF(ANGLE.LT.0.0)   THEN                                                   
           IF(X.LT.X1) THEN                                                     
               ANGLE = 3.141593 + ANGLE                                         
           ELSE                                                                 
               ANGLE = 6.283185 + ANGLE                                         
           ENDIF                                                                
      ELSE                                                                      
           IF(X.LT.X1)  ANGLE = 3.141593 + ANGLE                                
      ENDIF                                                                     
C                                                                               
      END                                                                       
