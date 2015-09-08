      SUBROUTINE SECTOR(DATA,NX,NY,X1,Y1,X2,Y2,X3,Y3,LINE,IRAD)              
C+
C     SECTOR.
C
C     Subroutine for extracting a "cake slice" from an
C     image array.
C
C  Based on ASPIC routine DEFREGION.
C
C  Provisional argument list.
C
C  Given;
C   DATA   (RA)   Image array.
C   NX     (I)    X extent of the image.
C   NY     (I)    Y   "    "   "    "  .
C   X1     (R)    X coord. of the centre of the line.
C   Y1     (R)    Y   "  . "   "    "    "   "   "  .
C   X2     (R)   | Coords. determining clockwise bound
C   Y2     (R)   | of the slice and its radius.
C   X3     (R)   | Coords. determining the anticlockwise
C   Y3     (R)   | bound of the slice.
C  Returned;
C   LINE   (RA)   1024 element array containing the extracted 
C                 slice.
C   IRAD   (I)    No. of points in the extracted profile.
C
C  Subroutines called;
C   E2D:- GETHETA, GETSECTOR, WITHIN.
C
C  Inserted into E2D from the APSPIC library by;
C  A C Davenhall./ROE/                                 7/5/82.
C-
      INCLUDE 'INTERIM(FMTPAR)'                        
      INCLUDE 'INTERIM(ERRPAR)'                        
      REAL DATA(0:NX-1,0:NY-1),LINE(0:1023)                                     
      LOGICAL INCL,INCL1
C                                                                               
C   CALCULATE RADIUS OF VECTOR                                                  
C                                                                               
      RADIUS = SQRT((X2-X1)**2 + (Y2-Y1)**2)                                    
C                                                                               
C   DEFINE EXO-REGION                                                           
C                                                                               
      IXSTART = NINT(X1-RADIUS)                                                 
      IXEND   = NINT(X1+RADIUS)                                                 
      IYSTART = NINT(Y1-RADIUS)                                                 
      IYEND   = NINT(Y1+RADIUS)                                                 
C                                                                               
C    CHECK BOUNDS                                                               
C                                                                               
      IXSTART = MAX(0,IXSTART)                                                  
      IXEND   = MIN(NX-1,IXEND)                                                 
      IYSTART = MAX(0,IYSTART)                                                  
      IYEND   = MIN(NY-1,IYEND)                                                 
C                                                                               
C  NOW A MESSY BIT TO TRY AND CUT DOWN THE SEARCH AREA                          
C                                                                               
      CALL GETHETA(X1,Y1,X2,Y2,THETA1)                                          
      CALL GETHETA(X1,Y1,X3,Y3,THETA2)                                          
C                                                                               
      IF(THETA1.EQ.THETA2) THEN                                                 
         THETA1 = 0.0                                                           
         THETA2 = 6.283185                                                      
         GO TO 300                                                              
      ENDIF                                                                     
      DELTHETA =( THETA2-THETA1)*57.29578                                       
C                                                                               
      IF(ABS(DELTHETA).LE.90.) THEN                                             
         CALL WITHIN (THETA1,0,90,INCL)
         IF (INCL) THEN
           CALL WITHIN (THETA2,0,90,INCL1) 
           IF (INCL1) THEN
               IXSTART = X1                                                     
               IYSTART = Y1                                                     
            ELSE                                                                
               CALL WITHIN (THETA2,90,180,INCL1)
               IF (INCL1) IYSTART=Y1
            ENDIF                                                               
         ENDIF                                                                  
         CALL WITHIN (THETA1,90,180,INCL)
         IF (INCL) THEN
            IF (THETA2.LE.3.141593E0) THEN
               IXEND = X1                                                       
               IYSTART = Y1                                                     
            ELSE                                                                
               IF(THETA2.GT.3.141593)   IXEND = X1                              
            ENDIF                                                               
         ENDIF                                                                  
         CALL WITHIN (THETA1,180,270,INCL)
         IF (INCL) THEN
            IF (THETA1.LE.4.712389E0) THEN
               IYEND = Y1                                                       
               IXEND = X1                                                       
            ELSE                                                                
               IF(THETA2.GT.4.712389)  IYEND = Y1                               
            ENDIF                                                               
         ENDIF                                                                  
         CALL WITHIN (THETA1,270,360,INCL)
         IF (INCL) THEN
            CALL WITHIN (THETA2,270,360,INCL1)
            IF (INCL1) THEN
               IXSTART = X1                                                     
               IYEND = Y1                                                       
            ELSE                                                                
               IF(THETA2.LE.1.57079)  IXSTART = X1                              
            ENDIF                                                               
         ENDIF                                                                  
      ELSE                                                                      
C                                                                               
C  TEST FOR LESSER CASE OF ANGLE >90 <180                                       
C                                                                               
      CALL WITHIN (THETA1,0,180,INCL)
      CALL WITHIN (THETA2,0,180,INCL1)
      IF (INCL.AND.INCL1) IYSTART=Y1
      CALL WITHIN (THETA1,180,360,INCL)
      CALL WITHIN (THETA2,180,360,INCL1)
      IF (INCL.AND.INCL1) THEN
         IF(THETA2.GT.THETA1)  IYEND = Y1                                       
      ENDIF                                                                     
      CALL WITHIN (THETA1,90,270,INCL)
      CALL WITHIN (THETA2,90,270,INCL1)
      IF (INCL.AND.INCL1) THEN
         IF(THETA2.GT.THETA1) IXEND = X1                                        
      ENDIF                                                                     
      CALL WITHIN (THETA1,270,360,INCL)
      CALL WITHIN (THETA2,0,90,INCL1)
      IF (INCL.AND.INCL1) IXSTART=X1
      ENDIF    ! FROM THE FIRST IF                                              
  300 CONTINUE                                                                  
C                                                                               
C   SET UP SECTOR                                                               
C                                                                               
      CALL GETSECTOR(DATA,NX,NY,                                                
     1               IXSTART,IXEND,IYSTART,IYEND,LINE,IRAD,                     
     2               X1,Y1,X2,Y2,X3,Y3)                                         
C                                                                               
      END                                                                       
