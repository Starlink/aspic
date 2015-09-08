      SUBROUTINE TRANS(COEFF,Z,ZZ,N1,N2,N3,N4,NCOEFF)
C ===============================================================               
C                                                                               
C                   REGISTAR TWO IMAGES                                         
C                                                                               
C ================================================================              
C                                                                               
C DESCRIPTION                                                                   
C -----------                                                                   
C             PERFORMS RESTRUCRURING OF AN IMAGE FOR PICTURE REGISTRATION.      
C             THE PROGRAM ASSUMES THAT ALL OF THE ACTIVE PICTURE IS IN          
C             CORE(IE THE ONE WHICH THE INTERPOLATION LOOKUPS ARE BEING         
C             MADE BY ITPLBV).                                                  
C             HOWEVER THE INTERPOLATION OF THE REFERENCE PICTURE PIXELS IS      
C             CARRIED OUT IN SMALL BLOCKS OF NPIXEL*100 WHERE NPIXEL            
C             IS THE NUMBER OF PIXELS IN THE X-DIRECTION.                       
C             THE REGISTRATION POLYNOMIALS ARE USED IN TWO FUNCTION             
C             SUBROUTINES XFUN AND YFUN.                                        
C             AUTOMATIC CHICE OF THE CORRECT ORDER IS MADE.                     
C             IF THE COMPUTED INTERPOLATON POINT IS OUTSIDE THE IMAGE           
C             ITS VALE IS SET TO 4095.0                                         
C                                                                               
C PARAMETERS                                                                    
C ----------                                                                    
C                     X        ARRAY STORING X COORDINATES OF GRID POINTS.      
C                     Y        ARRAY STORING Y COORDINATES OF GRID POINTS.      
C                     Z        DOUBLY DIMENSIONED ARRAY STORES ACTIVE           
C                              PICTURE.                                         
C                     U        ARRAY STORING X COORDS OF DESIRED POINTS.        
C                     V        ARRAY STORING Y COORDS OF DESIRED POINTS.        
C                     W        ARRAY STORING THE INTERPOLATED VALUES.           
C                     N        NUMBER OF POINTS TO BE INTERPOLATED PER CALL     
C                 NYLOW        ARRAY CONTAINING LOWER Y BOUNDARY OF SEGMENT     
C                 NYHIGH                                                        
C                NYHIGH         ARRAY STORES UPPER SEGMENT BOUNDARY             
C                NPIXEL         NUMBER OF X PIXELS                              
C                NSCANS         NUMBER OF Y PIXELS                              
C                XCOEFF         ARRAY STORES X REGISTRATION POLY                
C                YCOEFF         ARRAY STORES Y REGISTRATION POLY                
C ----------------------------------------------------------------------------  
      DIMENSION Z(N1,N2),ZZ(N3,N4)
      DIMENSION COEFF(2,8)
      DIMENSION X(2048),Y(2048),U(2048),V(2048),W(2048)                         
      DIMENSION IFLAG(2048),XCOEFF(8),YCOEFF(8)                                
      DIMENSION NYLOW(20),NYHIGH(20)                        
      INTEGER*2 JW(2048)
      CHARACTER*72 OUTPUT
      DO I = 1,8
      XCOEFF(I) = COEFF(1,I)
      YCOEFF(I) = COEFF(2,I)
      ENDDO
C DO Y SEGMENTATION                                                             
           IYSEGS=0                                                             
    1      IYSEGS=IYSEGS+1                                                      
           NYLOW(IYSEGS)=1+100*(IYSEGS-1)                                       
           NYHIGH(IYSEGS)=NYLOW(IYSEGS)+100-1                                   
           LEFT=N2-NYHIGH(IYSEGS)                                           
           IF(LEFT) 3,2,1                                                       
    3 NYHIGH(IYSEGS)=N2                                                     
    2 WRITE(OUTPUT,102) IYSEGS                                                       
      CALL WRUSER(OUTPUT,IS)
  102 FORMAT(1H ,'THERE ARE ',I4,' Y-SEGMENTS')                                 
C ----------------------------------------------------------------------------- 
C FILL IN X AND Y ARRAYS                                                        
      DO 4 I=1,N2                                                           
           Y(I)=FLOAT(I)                                                        
    4 CONTINUE                                                                  
      DO 5 J=1,N1                                                           
           X(J)=FLOAT(J)                                                        
    5 CONTINUE                                                                  
C---------------------------------------------------------------------------??? 
C READ IN Z ARRAY                                                               
C-------------------------------------------------------------------------      
C READ IN REGISTRATION POLYNOMIAL
C ------------------------------------------------------------------------      
C MAIN DO LOOP OVER Y SEGMENTS                                                  
      WRITE(OUTPUT,103)                                                              
      CALL WRUSER(OUTPUT,IS)
  103 FORMAT(1H ,'DATA INITIALIZATION IS COMPLETE')                               
      XMAX=FLOAT(N1)                                                        
      YMAX=FLOAT(N2)                                                        
      N8 = 1
      DO 7 K=1,IYSEGS                                                           
           DO 8 J=NYLOW(K),NYHIGH(K)                                            
                YY=FLOAT(J)                                                     
                DO 9 L=1,N1                                                 
                     XX=FLOAT(L)                                                
C DO TRANSFORMATION FOR EACH X AND Y                                            
                     U(L)=XFUN(XCOEFF,NCOEFF,XX,YY)                               
                     V(L)=YFUN(YCOEFF,NCOEFF,XX,YY)                               
                           IF(U(L).GE.1.0) GO TO 12                             
                           IFLAG(L)=1                                           
                           U(L)=1.0                                             
			   GOTO 9
   12                      IF(U(L).LE.XMAX) GO TO 13                             
                           IFLAG(L)=1                                           
                           U(L)=XMAX                                            
			   GOTO 9
   13			   IF(V(L).GE.1.0)GOTO 14
			   IFLAG(L)=1
			   GOTO 9
   14			   IF(V(L).LE.YMAX)GOTO 9
			   IFLAG(L)=1
    9 CONTINUE                                                                  
      CALL ITPLBV(6,N1,N2,X,Y,N1,U,V,W,Z,N1,N2)                             
      DO 17 JJ=1,N1                                                         
      IF(IFLAG(JJ).EQ.1) W(JJ)=0.0                                           
      IF (ABS(W(JJ)).GT.4095.0) W(JJ) =4095.0
   17 CONTINUE
      DO 432 JJ = 1,N1
  432 JW(JJ) = W(JJ)
      DO JJ = 1,N1
      ZZ(JJ,N8) = JW(JJ)
      ENDDO
      N8 = N8 + 1
      WRITE(OUTPUT,20) J
      CALL WRUSER(OUTPUT,IS)
   20 FORMAT(1H ,'ROW NO.',I4,' TRANSFORMED.')
      DO 21 JJ = 1,N1
   21 IFLAG(JJ) = 0
    8 CONTINUE                                                                  
      WRITE(OUTPUT,104) K                                                            
      CALL WRUSER(OUTPUT,IS)
  104 FORMAT(1H ,1X,I2)                                                         
    7 CONTINUE                                                                  
      CLOSE(UNIT=6)
      RETURN
      END                                                                       
