      SUBROUTINE ITPLBV(IU,LX,LY,X,Y,N,U,V,W,Z,N1,N2)                                   
C                                                                               
C ===============================================================               
C                                                                               
C                        BIVARIATE INTERPOLATION                                
C ===============================================================               
C                                                                               
C                                                                               
C DESCRIPTION                                                                   
C --------------                                                                
C THIS SUBROUTINE INTERPOLATES,FROM THE VALUES OF THE FUNCTION                  
C GIVEN AT INPUT GRID POINTS IN AN X-Y PLANE,AND FOR A GIVEN SET                
C OF POINTS IN THE PLANE,THE VALUES OF A SINGLE VALUED BIVARIATE                
C FUNCTION Z=Z(X,Y).                                                            
C THE METHOD IS BASED ON PIECE-WISE FUNCTIONS COMPOSED OF A SET                 
C OF BICUBIC POLYNOMIALS IN X AND Y.EACH POLYNOMIAL SET IS APPLICABLE TO        
C A RECTANGLE OF THE INPUT POINYS IN THE X-Y PLANE.EACH POLYNOMIAL              
C IS DETERMINED LOCALLY.                                                        
C --------------------------------------------------------------                
C                                                                               
C                                                                               
C INPUT PARAMETERS                                                              
C -------------------                                                           
C          IU = LOGICAL UNIT NUMBER OF OUTPUT CHANNEL                           
C         LX = NUMBER OF GRID POINTS IN X COORDINATE(GREATER THAN2)             
C          LY = NUMBER OF INPUT POINTS IN Y COORDINATE(=2)                      
C          X = ARRAY OF DIMENSIOND LX STORES X-COORDINATES                      
C              OF INPUT GRID POINTS(IN ASSCENDING ORDER)                        
C          Y = ARRAY OF DIMENSION LY STORES Y COORDINATE OF                     
C               INPUT GIRD POINTS(IN ASSCENDING ORDER)                          
C          Z = 2D ARRAY(LX,LY) STORES FUNCTION AT GRID                         
C          N = NUMBER OF PIONTS TO BE INTERPOLATED                              
C          U = ARRAY OF DIMENSION N STORES X COORDINATE OF DESIRID              
C                POINRS.                                                        
C        V = ARRAY DIMENISON N   Y COORDINATE OF DESIRED POINTS.                
C                                                                               
C OUTPUT PARAMETER                                                              
C ------------------                                                            
C         V = ARRAY DIMENSION N STORING INTERPOLATED Z VALUES.                  
C                                                                               
C INTERNAL VARIABLES                                                            
C --------------                                                                
C          ZA = DIVIDED DIFFERENCE OF Z WITH RESPECT TO X                       
C          ZB = DIVIDED DIFFERENCE OF Z WITH RESPECT TO Y                       
C          ZAB = SECOND DIVIDED DIFFERENCE WITH RESPECT TO X AND Y              
C         ZX = PARTIAL DERIVATIVE OF Z WOTH RESPECT TO X                        
C          ZY = PARTIAL DERIVATIVE OF Z WITH RESPECT TO Y                       
C        ZXY = SECOND PARTIAL DEROVATIVE OF Z REPSECT TO X ANDY                 
      DIMENSION Z(N1,N2)
      DIMENSION X(LX),Y(LY),U(N),V(N),W(N)                                      
      DIMENSION ZA(5,2),ZB(2,5),ZAB(3,3),ZX(4,4),ZY(4,4),                       
     *ZXY(4,4)                                                                  
      EQUIVALENCE (Z3A1,ZA(1)),(Z3A2,ZA(2)),(Z3A3,ZA(3)),                       
     *(Z3A4,ZA(4)),(Z3A5,ZA(5)),(Z4A1,ZA(6)),(Z4A2,ZA(7)),                      
     *(Z4A3,ZA(8)),(Z4A4,ZA(9)),(Z4A5,ZA(10)),(Z3B1,ZB(1)),                     
     *(Z3B2,ZB(3)),(Z3B3,ZB(5)),(Z3B4,ZB(7)),(Z3B5,ZB(9)),                      
     *(Z4B1,ZB(2)),(Z4B2,ZB(4)),(Z4B3,ZB(6)),(Z4B4,ZB(8)),                      
     *(Z4B5,ZB(10)),(ZA2B2,ZAB(1)),(ZA3B2,ZAB(2)),                              
     *(ZA4B2,ZAB(3)),(ZA2B3,ZAB(4)),(ZA3B3,ZAB(5)),                             
     *(ZA4B3,ZAB(6)),(ZA2B4,ZAB(7)),(ZA3B4,ZAB(8)),                             
     *(ZA4B4,ZAB(9)),(ZX33,ZX(6)),(ZX43,ZX(7)),                                 
     *(ZX34,ZX(10)),(ZX44,ZX(11)),(ZY33,ZY(6)),                                 
     *(ZY43,ZY(7)),(ZY34,ZY(10)),(ZY44,ZY(11)),                                 
     *(ZXY33,ZXY(6)),(ZXY43,ZXY(7)),(ZXY34,ZXY(10)),                            
     *(ZXY44,ZXY(11)),(P00,Z33),(P01,ZY33),(P10,ZX33),                          
     *(P11,ZXY33)                                                               
      EQUIVALENCE (LX0,ZX(1)),(LXM1,ZX(4)),(LXM2,ZX(13)),                       
     *(LXP1,ZX(16)),(LY0,ZY(1)),(LYM1,ZY(4)),(LYM2,ZY(13)),                     
     *(LYP1,ZY(16)),(IX,ZXY(1)),(IY,ZXY(4)),(IXPV,ZXY(13)),                     
     *(IYPV,ZXY(16)),(UK,DX),(VK,DY),(A1,A5,B1,B5,ZX(2),A,Q0)   ,               
     *(A2,ZX(5),B,Q1),(A4,ZX(8),C,Q2),(B2,ZY(2),D,Q3),                          
     *(B4,ZY(14),E),(X2,ZX(3),A3SQ),(X4,ZX(9)),(X5,ZX(12)),                     
     *(Y2,ZX(14)),(Y4,ZY(3),B3SQ),(Y5,ZX(15),P02),                              
     *(Z23,ZY(5),P03),(Z24,ZY(8),P12),(Z32,ZY(9),P13),                          
     *(Z34,ZY(12),P20),(Z35,ZY(15),P21),(Z42,ZXY(2),P22),                       
     *(Z43,ZXY(5),P23),(Z44,ZXY(3),P30),(Z45,ZXY(8),P31),                       
     *(Z53,ZXY(9),P32),(Z54,ZXY(12),P33),(W2,WY2,W4),                           
     *(W3,WY3,W1,W5),(WX2,ZXY(14)),(WX3,ZXY(15))                                
C  PRELIMINARY PROCESSING                                                       
C  SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES                          
      IU0=IU                                                                    
      LX0=LX                                                                    
      LXM1=LX0-1                                                                
      LXM2=LXM1-1                                                               
      LXP1=LX0+1                                                                
      LY0=LY                                                                    
      LYM1=LY0-1                                                                
      LYM2=LYM1-1                                                               
      LYP1=LY0+1                                                                
      N0=N                                                                      
C    CHECK FOR ERRORS                                                           
      IF (LXM2.LT.0) GO TO 710                                                  
      IF (LYM2.LT.0) GO TO 720                                                  
      IF (N0.LT.1) GO TO 730                                                    
      DO 10 IIX=2,LX0                                                           
      IF(X(IIX-1)-X(IIX)) 10,740,750                                            
   10 CONTINUE                                                                  
      DO 20 IIY=2,LY0                                                           
      IF(Y(IIY-1)-Y(IIY)) 20,770,780                                            
   20 CONTINUE                                                                  
C INITIAL SETTING OF PREVIOUS VALUES OF IX AND IY                               
      IXPV=0                                                                    
      IYPV=0                                                                    
C  MAIN LOOP                                                                    
      DO 700 K=1,N0                                                             
      UK=U(K)                                                                   
      VK=V(K)                                                                   
C  ROUTINES TO LOCATE THE DESIRED POINT                                         
C  TO FIND OUT THE IX VALUE FOR WHICH                                           
C (U(K).GE.X(IX-1)).AND.(U(K).LT.X(IX))                                         
      IF (LXM2.EQ.0) GO TO 80                                                   
      IF (UK.GE.X(LX0)) GO TO 70                                                
      IF (UK.LT.X(1)) GO TO 60                                                  
      IMN=2                                                                     
      IMX=LX0                                                                   
   30 IX=(IMN+IMX)/2                                                            
      IF (UK.GE.X(IX)) GO TO 40                                                 
      IMX=IX                                                                    
      GO TO 50                                                                  
   40 IMN=IX+1                                                                  
   50 IF (IMX.GT.IMN) GO TO 30                                                  
      IX=IMX                                                                    
      GO TO 90                                                                  
   60 IX=1                                                                      
      GO TO 90                                                                  
   70 IX=LXP1                                                                   
      GO TO 90                                                                  
   80 IX=2                                                                      
C TO FIND OUT THE IY VALUE FOR WHICH                                            
C (V(K).GE.Y(IY-1)).AND.(V(K).LT.Y(IY))                                         
   90 IF (LYM2.EQ.0) GO TO 150                                                  
      IF (VK.GE.Y(LY0)) GO TO 140                                               
      IF (VK.LT.Y(1)) GO TO 130                                                 
      IMN=2                                                                     
      IMX=LY0                                                                   
  100 IY=(IMN+IMX)/2                                                            
      IF(VK.GE.Y(IY)) GO TO 110                                                 
      IMX=IY                                                                    
      GO TO 120                                                                 
  110 IMN=IY+1                                                                  
  120 IF (IMX.GT.IMN) GO TO 100                                                 
      IY=IMX                                                                    
      GO TO 160                                                                 
  130 IY=1                                                                      
      GO TO 160                                                                 
  140 IY=LYP1                                                                   
      GO TO 160                                                                 
  150 IY=2                                                                      
C TO CHECK IF THE DESIRED POINT IS IN THE SAME TECTANGLE                        
C AS THE PREVIOUS POINT.IF YES SKIP TO THE COMPUTATION                          
C OF THE POLYNOMIAL.                                                            
  160 IF (IX.EQ.IXPV.AND.IY.EQ.IYPV) GO TO 690                                  
      IXPV=IX                                                                   
      IYPV=IY                                                                   
C ROUTINES TO PICK UP NECESSARY X,Y,AND Z VALUES,TO                             
C COMPUTE THE ZA,ZB,ZAB, VALUES AND TO ESTIMATE THEM                            
C WHEN NECCESSARY                                                               
      JX=IX                                                                     
      IF (JX.EQ.1) JX=2                                                         
      IF (JX.EQ.LXP1) JX=LX0                                                    
      JY=IY                                                                     
      IF (JY.EQ.1) JY=2                                                         
      IF (JY.EQ.LYP1) JY=LY0                                                    
      JXM2=JX-2                                                                 
      JXML=JX-LX0                                                               
      JYM2=JY-2                                                                 
      JYML=JY-LY0                                                               
C THE CORE AREA, IE IN THE RECTANGLE THAT CONTAINS                              
C THE DESIRED POINT                                                             
      X3=X(JX-1)                                                                
      X4=X(JX)                                                                  
      A3=1.0/(X4-X3)                                                            
      Y3=Y(JY-1)                                                                
      Y4=Y(JY)                                                                  
      B3=1.0/(Y4-Y3)                                                            
      Z33=Z(JX-1,JY-1)                                                          
      Z43=Z(JX,JY-1)                                                            
      Z34=Z(JX-1,JY)                                                            
      Z44=Z(JX,JY)                                                              
      Z3A3=(Z43-Z33)*A3                                                         
      Z4A3=(Z44-Z34)*A3                                                         
      Z3B3=(Z34-Z33)*B3                                                         
      Z4B3=(Z44-Z43)*B3                                                         
      ZA3B3=(Z4B3-Z3B3)*A3                                                      
C IN THE X DIRECTION                                                            
      IF (LXM2.EQ.0) GO TO 230                                                  
      IF (JXM2.EQ.0) GO TO 170                                                  
      X2=X(JX-2)                                                                
      A2=1.0/(X3-X2)                                                            
      Z23=Z(JX-2,JY-1)                                                          
      Z24=Z(JX-2,JY)                                                            
      Z3A2=(Z33-Z23)*A2                                                         
      Z4A2=(Z34-Z24)*A2                                                         
      IF (JXML.EQ.0) GO TO 180                                                  
  170 X5=X(JX+1)                                                                
      A4=1.0/(X5-X4)                                                            
      Z53=Z(JX+1,JY-1)                                                          
      Z54=Z(JX+1,JY)                                                            
      Z3A4=(Z53-Z43)*A4                                                         
      Z4A4=(Z54-Z44)*A4                                                         
      IF (JXM2.NE.0) GO TO 190                                                  
      Z3A2=Z3A3+Z3A3-Z3A4                                                       
      Z4A2=Z4A3+Z4A3-Z4A4                                                       
      GO TO 190                                                                 
  180 Z3A4=Z3A3+Z3A3-Z3A2                                                       
      Z4A4=Z4A3+Z4A3-Z4A2                                                       
  190 ZA2B3=(Z4A2-Z3A2)*B3                                                      
      ZA4B3=(Z4A4-Z3A4)*B3                                                      
      IF (JX.LE.3) GO TO 200                                                    
      A1=1.0/(X2-X(JX-3))                                                       
      Z3A1=(Z23-Z(JX-3,JY-1))*A1                                                
      Z4A1=(Z24-Z(JX-3,JY))*A1                                                  
      GO TO 210                                                                 
  200 Z3A1=Z3A2+Z3A2-Z3A3                                                       
      Z4A1=Z4A2+Z4A2-Z4A3                                                       
  210 IF (JX.GE.LXM1) GO TO 220                                                 
      A5=1.0/(X(JX+2)-X5)                                                       
      Z3A5=(Z(JX+2,JY-1)-Z53)*A5                                                
      Z4A5=(Z(JX+2,JY)-Z54)*A5                                                  
      GO TO 240                                                                 
  220 Z3A5=Z3A4+Z3A4-Z3A3                                                       
      Z4A5=Z4A4+Z4A4-Z4A3                                                       
      GO TO 240                                                                 
  230 Z3A2=Z3A3                                                                 
      Z4A2=Z4A3                                                                 
      GO TO 180                                                                 
C IN THE Y DIRECTION                                                            
  240 IF (LYM2.EQ.0) GO TO 310                                                  
      IF (JYM2.EQ.0) GO TO 250                                                  
      Y2=Y(JY-2)                                                                
      B2=1.0/(Y3-Y2)                                                            
      Z32=Z(JX-1,JY-2)                                                          
      Z42=Z(JX,JY-2)                                                            
      Z3B2 = (Z33-Z32)*B2
      Z4B2=(Z43-Z42)*B2                                                         
      IF (JYML.EQ.0) GO TO 260                                                  
  250 Y5=Y(JY+1)                                                                
      B4=1.0/(Y5-Y4)                                                            
      Z35=Z(JX-1,JY+1)                                                          
      Z45=Z(JX,JY+1)                                                            
      Z3B4=(Z35-Z34)*B4                                                         
      Z4B4=(Z45-Z44)*B4                                                         
      IF (JYM2.NE.0) GO TO 270                                                  
      Z3B2=Z3B3+Z3B3-Z3B4                                                       
      Z4B2=Z4B3+Z4B3-Z4B4                                                       
      GO TO 270                                                                 
  260 Z3B4=Z3B3+Z3B3-Z3B2                                                       
      Z4B4=Z4B3+Z4B3-Z4B2                                                       
  270 ZA3B2=(Z4B2-Z3B2)*A3                                                      
      ZA3B4=(Z4B4-Z3B4)*A3                                                      
      IF (JY.LE.3) GO TO 280                                                    
      B1=1.0/(Y2-Y(JY-3))                                                       
      Z3B1=(Z32-Z(JX-1,JY-3))*B1                                                
      Z4B1=(Z42-Z(JX,JY-3))*B1                                                  
      GO TO 290                                                                 
  280 Z3B1=Z3B2+Z3B2-Z3B3                                                       
      Z4B1=Z4B2+Z4B2-Z4B3                                                       
  290 IF (JY.GE.LYM1) GO TO 300                                                 
      B5=1.0/(Y(JY+2)-Y5)                                                       
      Z3B5 = (Z(JX-1,JY+2)-Z35)*B5
      Z4B5=(Z(JX,JY+2)-Z45)*B5                                                  
      GO TO 320                                                                 
  300 Z3B5=Z3B4+Z3B4-Z3B3                                                       
      Z4B5=Z4B4+Z4B4-Z4B3                                                       
      GO TO 320                                                                 
  310 Z3B2=Z3B3                                                                 
      Z4B2=Z4B3                                                                 
      GO TO 260                                                                 
C IN THE DIAGONAL DIRECTIONS                                                    
  320 IF (LXM2.EQ.0) GO TO 400                                                  
      IF (LYM2.EQ.0) GO TO 410                                                  
      IF (JXML.EQ.0) GO TO 350                                                  
      IF (JYM2.EQ.0) GO TO 330                                                  
      ZA4B2=((Z53-Z(JX+1,JY-2))*B2-Z4B2)*A4                                     
      IF (JYML.EQ.0) GO TO 340                                                  
  330 ZA4B4=((Z(JX+1,JY+1)-Z54)*B4-Z4B4)*A4                                     
      IF (JYM2.NE.0) GO TO 380                                                  
      ZA4B2=ZA4B3+ZA4B3-ZA4B4                                                   
      GO TO 380                                                                 
  340 ZA4B4=ZA4B3+ZA4B3-ZA4B2                                                   
      GO TO 380                                                                 
  350 IF (JYM2.EQ.0) GO TO 360                                                  
      ZA2B2=(Z3B2-(Z23-Z(JX-2,JY-2))*B2)*A2                                     
      IF (JYML.EQ.0) GO TO 370                                                  
  360 ZA2B4=(Z3B4-(Z(JX-2,JY+1)-Z24)*B4)*A2                                     
      IF (JYM2.NE.0) GO TO 390                                                  
      ZA2B2=ZA2B3+ZA2B3-ZA2B4                                                   
      GO TO 390                                                                 
  370 ZA2B4=ZA2B3+ZA2B3-ZA2B2                                                   
      GO TO 390                                                                 
  380 IF (JXM2.NE.0) GO TO 350                                                  
      ZA2B2 = ZA3B2+ ZA3B2-ZA4B2                                                
      ZA2B4 = ZA3B4 + ZA3B4-ZA4B4                                               
      GO TO 420                                                                 
390   IF (JXML.NE.0) GO TO 420                                                  
      ZA4B2 = ZA3B2 + ZA3B2 - ZA2B2                                             
      ZA4B4=ZA3B4+ZA3B4-ZA2B4                                                   
      GO TO 420                                                                 
  400 ZA2B2=ZA3B2                                                               
      ZA4B2=ZA3B2                                                               
      ZA2B4=ZA3B4                                                               
      ZA4B4=ZA3B4                                                               
      GO TO 420                                                                 
  410 ZA2B2=ZA2B3                                                               
      ZA2B4=ZA2B3                                                               
      ZA4B2=ZA4B3                                                               
      ZA4B4=ZA4B3                                                               
C NUMERICAL DIFFERENTIATION.....TO DETERMINE PARTIAL                            
C DERIVATIVES ZX,ZY,AND ZXY AS WEIGHTED MEANS OF DIVIDED                        
C DIFFERANCES ZA,ZB,AND ZAB,RESPECTIVELY                                        
  420 DO 480 JY=2,3                                                             
      DO 470 JX=2,3                                                             
      W2=ABS(ZA(JX+2,JY-1)-ZA(JX+1,JY-1))                                       
      W3=ABS(ZA(JX,JY-1)-ZA(JX-1,JY-1))                                         
      WS=W2+W3                                                                  
      IF (WS.EQ.0.0) GO TO 430                                                  
      WX2=W2/WS                                                                 
      WX3=W3/WS                                                                 
      GO TO 440                                                                 
  430 WX2=0.5                                                                   
      WX3=0.5                                                                   
  440 ZX(JX,JY)=WX2*ZA(JX,JY-1)+WX3*ZA(JX+1,JY-1)                               
      W2=ABS(ZB(JX-1,JY+2)-ZB(JX-1,JY+1))                                       
      W3=ABS(ZB(JX-1,JY)-ZB(JX-1,JY-1))                                         
      WS=W2+W3                                                                  
      IF (WS.EQ.0.0) GO TO 450                                                  
      WY2=W2/WS                                                                 
      WY3=W3/WS                                                                 
      GO TO 460                                                                 
  450 WY2=0.5                                                                   
      WY3=0.5                                                                   
  460 ZY(JX,JY)=WY2*ZB(JX-1,JY)+WY3*ZB(JX-1,JY+1)                               
      ZXY(JX,JY)=                                                               
     *WY2*(WX2*ZAB(JX-1,JY-1)+WX3*ZAB(JX,JY-1))+                                
     *WY3*(WX2*ZAB(JX-1,JY)+WX3*ZAB(JX,JY))                                     
  470 CONTINUE                                                                  
  480 CONTINUE                                                                  
C WHEN (U(K).LT.X(1)).ORU(K).GT.X(LX))                                          
      IF (IX.EQ.LXP1) GO TO 530                                                 
      IF (IX.NE.1) GO TO 590                                                    
      W2=A4*(3.0*A3+A4)                                                         
      W1=2.0*A3*(A3-A4)+W2                                                      
      DO 500 JY=2,3                                                             
      ZX(1,JY)=(W1*ZA(1,JY-1)+W2*ZA(2,JY-1))/(W1+W2)                            
      ZY(1,JY)=ZY(2,JY)+ZY(2,JY)-ZY(3,JY)                                       
      ZXY(1,JY)=ZXY(2,JY)+ZXY(2,JY)-ZXY(3,JY)                                   
      DO 490 JX1=2,3                                                            
      JX=5-JX1                                                                  
      ZX(JX,JY)=ZX(JX-1,JY)                                                     
      ZY(JX,JY)=ZY(JX-1,JY)                                                     
      ZXY(JX,JY)=ZXY(JX-1,JY)                                                   
  490 CONTINUE                                                                  
  500 CONTINUE                                                                  
      X3=X3-1.0/A4                                                              
      Z33=Z33-Z3A2/A4                                                           
      DO 510 JY=1,5                                                             
      ZB(2,JY)=ZB(1,JY)                                                         
  510 CONTINUE                                                                  
      DO 520 JY=2,4                                                             
       ZB(1,JY)=ZB(1,JY)-ZAB(1,JY-1)/A4                                         
  520 CONTINUE                                                                  
      A3=A4                                                                     
      JX=1                                                                      
      GO TO 570                                                                 
  530 W4=A2*(3.0*A3+A2)                                                         
      W5=2.0*A3*(A3-A2)+W4                                                      
      DO 550 JY=2,3                                                             
      ZX(4,JY)=(W4*ZA(4,JY-1)+W5*ZA(5,JY-1))/(W4+W5)                            
      ZY(4,JY)=ZY(3,JY)+ZY(3,JY)-ZY(2,JY)                                       
      ZXY(4,JY)=ZXY(3,JY)+ZXY(3,JY)-ZXY(2,JY)                                   
      DO 540 JX=2,3                                                             
      ZX(JX,JY)=ZX(JX+1,JY)                                                     
      ZY(JX,JY)=ZY(JX+1,JY)                                                     
      ZXY(JX,JY)=ZXY(JX+1,JY)                                                   
  540 CONTINUE                                                                  
  550 CONTINUE                                                                  
      X3=X4                                                                     
      Z33=Z43                                                                   
      DO 560 JY=1,5                                                             
      ZB(1,JY)=ZB(2,JY)                                                         
  560 CONTINUE                                                                  
      A3=A2                                                                     
      JX=3                                                                      
  570 ZA(3,1)=ZA(JX+1,1)                                                        
      DO 580 JY=1,3                                                             
      ZAB(2,JY)=ZAB(JX,JY)                                                      
  580 CONTINUE                                                                  
C WHEN (V(K).LT.Y(1)).OR.(V(K).GT.Y(LY))                                        
  590 IF (IY.EQ.LYP1) GO TO 630                                                 
      IF (IY.NE.1) GO TO 680                                                    
      W2=B4*(3.0*B3+B4)                                                         
      W1=2.0*B3*(B3-B4)+W2                                                      
      DO 620 JX=2,3                                                             
      IF (JX.EQ.3.AND.IX.EQ.LXP1) GO TO 600                                     
      IF (JX.EQ.2.AND. IX.EQ.1) GO TO 600                                       
      ZY(JX,1)=(W1*ZB(JX-1,1)+W2*ZB(JX-1,2))/(W1+W2)                            
      ZX(JX,1)=ZX(JX,2)+ZX(JX,2)-ZX(JX,3)                                       
      ZXY(JX,1)=ZXY(JX,2)+ZXY(JX,2)-ZXY(JX,3)                                   
  600 DO 610 JY1=2,3                                                            
      JY=5-JY1                                                                  
      ZY(JX,JY)=ZY(JX,JY-1)                                                     
      ZX(JX,JY)=ZX(JX,JY-1)                                                     
      ZXY(JX,JY)=ZXY(JX,JY-1)                                                   
  610 CONTINUE                                                                  
  620 CONTINUE                                                                  
      Y3=Y3-1.0/B4                                                              
      Z33=Z33-Z3B2/B4                                                           
      Z3A3=Z3A3-ZA3B2/B4                                                        
      Z3B3=Z3B2                                                                 
      ZA3B3=ZA3B2                                                               
      B3=B4                                                                     
      GO TO 670                                                                 
  630 W4=B2*(3.0*B3+B2)                                                         
      W5=2.0*B3*(B3-B2)+W4                                                      
      DO 660 JX=2,3                                                             
      IF (JX.EQ.3.AND.IX.EQ.LXP1) GO TO 640                                     
      IF (JX.EQ.2.AND.IX.EQ.1) GO TO 640                                        
      ZY(JX,4)=(W4*ZB(JX-1,4)+W5*ZB(JX-1,5))/(W4+W5)                            
      ZX(JX,4)=ZX(JX,3)+ZX(JX,3)-ZX(JX,2)                                       
      ZXY(JX,4)=ZXY(JX,3)+ZXY(JX,3)-ZXY(JX,2)                                   
  640 DO 650 JY=2,3                                                             
      ZY(JX,JY)=ZY(JX,JY+1)                                                     
      ZX(JX,JY)=ZX(JX,JY+1)                                                     
      ZXY(JX,JY)=ZXY(JX,JY+1)                                                   
  650 CONTINUE                                                                  
  660 CONTINUE                                                                  
      Y3=Y4                                                                     
      Z33=Z33+Z3B3/B3                                                           
      Z3A3=Z3A3+ZA3B3/B3                                                        
      Z3B3=Z3B4                                                                 
      ZA3B3=ZA3B4                                                               
      B3=B2                                                                     
  670 IF (IX.NE.1.AND.IX.NE.LXP1) GO TO 680                                     
      JX=IX/LXP1+2                                                              
      JX1=5-JX                                                                  
      JY=IY/LYP1+2                                                              
      JY1=5-JY                                                                  
      ZX(JX,JY)=ZX(JX1,JY)+ZX(JX,JY1)-ZX(JX1,JY1)                               
      ZY(JX,JY)=ZY(JX1,JY)+ZY(JX,JY1)-ZY(JX1,JY1)                               
      ZXY(JX,JY)=ZXY(JX1,JY)+ZXY(JX,JY1)-ZXY(JX1,JY1)                           
C     DETERMINATION OF COEFFICIENTS OF POLYNOMIAL                               
  680 ZX3B3=(ZX34-ZX33)*B3                                                      
      ZX4B3=(ZX44-ZX43)*B3                                                      
      ZY3A3=(ZY43-ZY33)*A3                                                      
      ZY4A3=(ZY44-ZY34)*A3                                                      
      A=ZA3B3-ZX3B3-ZY3A3+ZXY33                                                 
      B=ZX4B3-ZX3B3-ZXY43+ZXY33                                                 
      C=ZY4A3-ZY3A3-ZXY34+ZXY33                                                 
      D=ZXY44 -ZXY43 -ZXY34+ZXY33                                               
      E=A+A-B-C                                                                 
      A3SQ=A3*A3                                                        ÿÿÿÿÿÿÿ
      B3SQ=B3*B3
      P02=(2.0*(Z3B3-ZY33)+Z3B3-ZY34)*B3
      P03=(-2.0*Z3B3+ZY34+ZY33)*B3SQ
      P12=(2.0*(ZX3B3-ZXY33)+ZX3B3-ZXY34)*B3
      P13=(-2.0*ZX3B3+ZXY34+ZXY33)*B3SQ
      P20=(2.0*(Z3A3-ZX33)+Z3A3-ZX43)*A3
      P21=(2.0*(ZY3A3-ZXY33)+ZY3A3-ZXY43)*A3
      P22=(3.0*(A+E)+D)*A3*B3
      P23=(-3.0*E-B-D)*A3*B3SQ
      P30=(-2.0*Z3A3+ZX43+ZX33)*A3SQ
      P31=(-2.0*ZY3A3+ZXY43+ZXY33)*A3SQ
      P32=(-3.0*E-C-D)*B3*A3SQ
      P33=(D+E+E)*A3SQ*B3SQ
C COMPUTATION OF POLYNOMIAL
  690 DY=VK-Y3
      Q0=P00+DY*(P01+DY*(P02+DY*P03))
      Q1=P10+DY*(P11+DY*(P12+DY*P13))
      Q2=P20+DY*(P21+DY*(P22+DY*P23))
      Q3=P30+DY*(P31+DY*(P32+DY*P33))
      DX=UK-X3
      W(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
  700 CONTINUE
C NORMAL EXIT
      RETURN
C ERROR EXIT
  710 WRITE(IUO,99999)
      CALL WRUSER(IUO,IST)
      GO TO 800
  720 WRITE (IUO,99998)
      CALL WRUSER(IUO,IST)
      GO TO 800
  730 WRITE (IUO,99997)
      CALL WRUSER(IUO,IST)
      GO TO 800
  740 WRITE (IUO,99996)
      CALL WRUSER(IUO,IST)
       GO TO 800
  750 WRITE (IUO,99995)
      CALL WRUSER(IUO,IST)
  760 WRITE (IUO,99994) IX,X(IX)
      CALL WRUSER(IUO,IST)
      GO TO 800
  770 WRITE (IUO,99993)
      CALL WRUSER(IUO,IST)
      GO TO 800
  780 WRITE (IUO,99992)
      CALL WRUSER(IUO,IST)
  790 WRITE (IUO,99991) IY,Y(IY)
      CALL WRUSER(IUO,IST)
  800 WRITE (IUO,99990) LX0,LY0,N0
      CALL WRUSER (IUO,IST)
      RETURN
99999 FORMAT('  ***  LX = 1 OR LESS.')
99998 FORMAT('  ***  LY = 1 OR LESS.')
99997 FORMAT('  ***  N = 0 OR LESS.')
99996 FORMAT('  ***  IDENTICAL X VALUES.')
99995 FORMAT('  ***  X VALUES OUT OF SEQUENCE.')
99994 FORMAT(7H   IX =, I6, 10X, 7HX(IX) =, E12.3)
99993 FORMAT('  ***  IDENTICAL Y VALUES.')
99992 FORMAT('  ***  Y VALUES OUT OF SEQUENCE.')
99991 FORMAT(7H   IY =,I6, 10X, 7HY(IY) =, E12.3)
99990 FORMAT(7H   LX =,I6, 10X, 4HLY =,I6, 10X, 3HN =, I7/
     * ' ERROR DETECTED IN ROUTINE    ITPLBV')
      END
