      SUBROUTINE GETSECTOR(DATA,NX,NY,                                          
     1                     IXSTART,IXEND,IYSTART,IYEND,LINE,IRAD,               
     2                     X1,Y1,X2,Y2,X3,Y3)                                   
C+
C     GETSECTOR.
C
C     ASPIC routine inserted into E2D by
C     A C Davenhall./ROE/                             5/7/82.
C
C     Concerned with extracting cake slices from an image
C-
C                                                                               
      REAL DATA(0:NX-1,0:NY-1),LINE(0:1023),SECTOR(0:1023,2)                    
C                                                                               
C  CALC RADIUS                                                                  
C                                                                               
      RADIUS = SQRT((X2-X1)**2 + (Y2-Y1)**2)                                    
      IRAD   = NINT(RADIUS)                                                     
C                                                                               
C   SET FLAG FOR X-AXIS CROSSING                                                
C                                                                               
      ICROSS = 0                                                                
C                                                                               
C  INITIALISE OUTPUT, WRITE IMAGE TO WINDOW                                     
C                                                                               
      DO 50 I=0,1023                                                            
      SECTOR(I,1) = 0.0                                                         
      SECTOR(I,2) = 0.0                                                         
   50 CONTINUE                                                                  
      NPIX = 0                                                                  
      DO 51 J=IYSTART,IYEND                                                     
      DO 51 I=IXSTART,IXEND                                                     
      NPIX = NPIX + 1                                                           
   51 CONTINUE                                                                  
C                                                                               
C   GET ANGLES OF VECTORS                                                       
C                                                                               
      CALL GETHETA(X1,Y1,X2,Y2,THETA1)                                          
      CALL GETHETA(X1,Y1,X3,Y3,THETA2)                                          
      IF(THETA1.EQ.THETA2) THEN                                                 
         THETA1 = 0.0                                                           
         THETA2 = 6.283185                                                      
      ENDIF                                                                     
C                                                                               
C   TEST FOR CROSSING +VE X-AXIS. IF SO DO IN 2 BITS                            
C                                                                               
      IF(THETA2.LT.THETA1)  THEN                                                
          STORE = THETA2                                                        
          THETA2 = 6.283185                                                     
          ICROSS = 1                                                            
      ENDIF                                                                     
C                                                                               
C   SET CENTRE POINT OF SECTOR LINE IN CASE IT FALLS THRO                       
C   THE LATER TESTS FOR INCLUSION IN THE SECTOR!                                
C                                                                               
      SECTOR(0,1) = DATA(NINT(X1),NINT(Y1))                                     
      SECTOR(0,2) = 1.0                                                         
C                                                                               
C   LOOP THRO EXO-REGION, TEST THETA AND RADIUS FOR INCLUSION IN SECTOR         
C                                                                               
C                                                                               
C     SET PIXEL COUNTER                                                         
C                                                                               
  200 NPIX = 0                                                                  
      DO 100 J=IYSTART,IYEND                                                    
      Y = FLOAT(J)                                                              
      DO 100 I=IXSTART,IXEND                                                    
      X = FLOAT(I)                                                              
      NPIX = NPIX + 1                                                           
C                                                                               
      CALL GETHETA(X1,Y1,X,Y,THETAX)                                            
C                                                                               
C  TEST IF ANGLE IN SECTOR                                                      
C                                                                               
      IF(THETAX.GE.THETA1.AND.THETAX.LE.THETA2) THEN                            
          DIST = SQRT((X-X1)**2 + (Y-Y1)**2)                                    
          IF(DIST.LE.RADIUS)   THEN                                             
              IDIST = NINT(DIST)                                                
              SECTOR(IDIST+1,1) = DATA(I,J) + SECTOR(IDIST+1,1)                 
              SECTOR(IDIST+1,2) = SECTOR(IDIST+1,2) + 1                         
          ENDIF                                                                 
      ENDIF                                                                     
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
C  CHECK ICROSS AND RETURN TO 100 LOOP FOR SECOND BIT IF FLAG SET               
C                                                                               
      IF(ICROSS.EQ.1)   THEN                                                    
         THETA1 = 0.0                                                           
         THETA2 = STORE                                                         
         ICROSS = 0                                                             
         GO TO 200                                                              
      ENDIF                                                                     
C                                                                               
C   TAKE MEAN OF ACCUMULATED VALUES                                             
C                                                                               
      DO 201 K=0,IRAD-1                                                         
      IF(SECTOR(K,2).GT.0.0)  THEN                                              
         LINE(K) = SECTOR(K,1)/SECTOR(K,2)                                      
      ENDIF                                                                     
  201 CONTINUE                                                                  
C
C;;;;;;
C
C   Frig to linearly interpolate over output pixels that have no
C   data.
C   Modified by B McNally 21-OCT-1982 to remove a compilation
C   warning due to assigning loop variable I within the loop
C
C   Inserted by C. D. Pike   4/10/82.
C
C;;;;;;
C
      I = 0
      DO WHILE (I .LT. IRAD-1)
        I = I + 1
      IF (LINE(I).NE.0) GOTO 202
C
      DO K=1,100
      IF (LINE(I+K).NE.0) GOTO 204
      END DO
C
  204 IST=I-1
      IEND=I+K
C
      DO K=IST+1,IEND-1
      LINE(K)=LINE(IST) + (LINE(IEND)-LINE(IST))*(K-IST)/(IEND-IST)
      END DO
C
      I=IEND
C
  202 CONTINUE
      ENDDO
C                                                                               
C   NORMAL EXIT                                                                 
C                                                                               
      END
