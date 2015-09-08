      FUNCTION XFUN(XC,NCOEFF,XX,YY)                                              
      DIMENSION XC(NCOEFF)                                                        
      XX2=XX*XX                                                                 
      YY2=YY*YY                                                                 
      RC=XX+XC(1)+XC(2)*XX+XC(3)*YY                                             
      IF(NCOEFF.LE.3) GO TO 9                                                     
      RC=RC+XC(4)*XX2+XC(5)*XX*YY+XC(6)*YY2                                     
      IF(NCOEFF.LE.6) GO TO 9                                                     
      RC=RC+XC(7)*XX*(XX2+YY2)+XC(8)*XX*((XX2+YY2)**2)                          
    9 XFUN=RC                                                                   
      RETURN                                                                    
      END                                                                       
