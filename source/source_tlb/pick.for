C                                                                               
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
C                                                                               
C                                                                               
C                                                                               
C                     ******************                                        
C                     *                *                                        
C                     * Program   PICK *                                        
C                     *                *                                        
C                     ******************                                        
C                                                                               
C                                                                               
C                                                                               
C          CALLING SEQUENCE:-                                                   
C               PICK                                                          
C                                                                               
C                                                                               
C          FUNCTION:-                                                           
C               It may be used to define a rectangular  sub-set  of  a  2-D     
C               Starlink image for storage as a new image.                      
C                                                                               
C                                                                               
C          USE:-                                                                
C               It may be used with or without the ARGS cursor. For a  more     
C               general  extraction  program  use  MANIC,  which  works  on     
C               1,2,and 3-D images.                                             
C                                                                               
C                                                                               
C                                                                               
C         USER PARAMETERS:-                                                     
C                                                                               
C         INPUT                               This   is   the   input   2-D     
C                                             Starlink image.                   
C                                                                               
C         OUTPUT                              This is the name of  the  new     
C                                             image picked out of INPUT.        
C                                                                               
C         XYPOS1                              By default the program  tries     
C                                             to  read  the ARGS cursor. If     
C                                             this is not possible, or  the     
C                                             cursor  is not moved within 5     
C                                             secs of appearing,  then  the     
C                                             user is prompted for keyboard     
C                                             input. In this case XYPOS1 is     
C                                             a  pair of (x,y) co-ordinates     
C                                             defining one  corner  of  the     
C                                             sub-image.                        
C                                                                               
C         XYPOS2                              If keyboard input is required     
C
C                                             (see  above)  then XYPOS2 are     
C                                             the (x,y) co-ordinates  of  a     
C                                             second  corner. The two pairs     
C                                             are sorted out,  and  printed     
C                                             on    the    terminal    with     
C                                             bottom-left-hand first.           
C                                                                               
C         XYOUT1                              Output parameter used to 
C                                             return the xy position of
C                                             the BLH point of the selected
C                                             region.
C
C         XYOUT2                              Output parameter used to
C                                             store the xy position of the
C                                             TRH point of the selected
C                                             region.
C                                                                               
C         USE OF TRACKER-BALL BUTTONS:-                                         
C                                                                               
C                                                                               
C                                                                               
C         GREEN 1     Read the cursor position  - only valid if the  cursor     
C                     was  moved  within  5 secs. but then there is no time     
C                     limit.                                                    
C                                                                               
C         WHITE 2     As 1                                                      
C                                                                               
C         WHITE 3     As 1                                                      
C                                                                               
C         RED   4     As 1                                                      
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C         K F Hartley              RGO                             7-JAN-82     
C                                                                               
C                                                                               
C--------------------------------------------------------------------------     



C      Written by K F Hartley at RGO on 30/11/81                                
                                                                                
C                                                                               
C      Subroutines called:-                                                     
C                                                                               
      INCLUDE 'INTERIM(ERRPAR)'                    
      INCLUDE 'INTERIM(FMTPAR)'                    
      INTEGER AX(2),AXO(2),STATUS                                               
      INTEGER LIMX(2),LIMY(2)                                                   
      INTEGER XY1(2),XY2(2)
C                                                                               
C   First get the input frame.                                                  
C                                                                               
      CALL RDIMAG('INPUT',FMT_R,2,AX,I,IPIN,STATUS)                             
      IF (STATUS.EQ.ERR_NORMAL.AND.I.EQ.2) THEN                                 
C                                                                               
C      If the image is OK then get the limits                                   
C                                                                               
         CALL GET2XY(LIMX,LIMY)                                                 
C      and write them to the environment.
C
         XY1(1)=LIMX(1)
         XY1(2)=LIMY(1)
         XY2(1)=LIMX(2)
         XY2(2)=LIMY(2)
         CALL WRKEYI('XYOUT1',XY1,2,STATUS)
         CALL WRKEYI('XYOUT2',XY2,2,STATUS)
C                                                                               
C      Now work out size of output frame.                                       
C                                                                               
         AXO(1)=LIMX(2)-LIMX(1)+1                                               
         AXO(2)=LIMY(2)-LIMY(1)+1                                               
C                                                                               
C      Then get the output frame.                                               
C                                                                               
         CALL WRIMAG('OUTPUT',FMT_R,AXO,2,IPOUT,STATUS)                         
         IF (STATUS.EQ.ERR_NORMAL) THEN                                         
C                                                                               
C         If the response was OK then make the copy.                            
C                                                                               
            CALL COPYIT(%VAL(IPIN),AX(1),AX(2),LIMX,LIMY,                       
     :                  AXO(1),AXO(2),%VAL(IPOUT))                              
C                                                                               
         END IF                                                                 
                                                                                
      END IF                                                                    
                                                                                
C                                                                               
C   Now tidy up and exit.                                                       
C                                                                               
      CALL FRDATA(' ',STATUS)                                                   
      CALL EXIT                                                                 
      END                                                                       
      SUBROUTINE GET2XY(XPOS,YPOS)                                              
C                                                                               
C   This subroutine gets 2 pairs of (x,y) co-ordinates from the                 
C   environment. It use the ARGS cursor and/or RDKEY to do this.                
C                                                                               
C   Parameters :-                                                               
C                                                                               
C      Output   XPOS    A 2 element array which will store the lower             
C                       and the higher of the 2 x values input.                 
C               YPOS    A similar array storing the 2 y values.                 
C                                                                               
C                                                                               
      INTEGER XY(2),XPOS(2),YPOS(2)                                             
      INTEGER X1,X2,Y1,Y2                                                       
      CHARACTER*72 TEXT                                                         
      CALL SRINIT(0,.FALSE.,ISTAT)                                              
C                                                                               
C   IF THE ARGS IS AVAILABLE TRY THE CURSOR                                     
C                                                                               
      IF (ISTAT.EQ.0) THEN                                                      
         CALL ASP_XYPOS(X1,Y1,IVAL,ID1)                                         
C                                                                               
C   IVAL=0 MEANS SUCCESS , ID1 IS THE IMAGE POINTED TO                          
C                                                                               
      END IF                                                                    
C                                                                               
C   IF NO GOOD TRY ASKING FOR NUMBERS                                           
C                                                                               
      IF (ISTAT.NE.0.OR.IVAL.NE.0) THEN                                         
        ISTA=2                                                                  
         DO WHILE (ISTA.GT.1)                                                   
            CALL RDKEYI('XYPOS1',.FALSE.,2,XY,I,ISTA)                           
            CALL CNPAR('XYPOS1',IST)                                            
         END DO                                                                 
         X1=XY(1)                                                               
         Y1=XY(2)                                                               
      ELSE
         WRITE (TEXT,'(A,2I5)') 'First set of co-ordinates is ',X1,Y1
         CALL WRUSER(TEXT,ISTAT)
      END IF                                                                    
C                                                                               
C   NOW REPEAT FOR SECOND POINT                                                 
C                                                                               
C   ONLY TRY CURSOR IF FIRST WAS OK                                             
C                                                                               
      IF (ISTAT.EQ.0.AND.IVAL.EQ.0) THEN                                        
         CALL ASP_XYPOS(X2,Y2,IVAL,ID2)                                         
      END IF                                                                    
      IF (ISTAT.NE.0.OR.IVAL.NE.0) THEN                                         
         ISTA=2                                                                 
         DO WHILE (ISTA.GT.1)                                                   
            CALL RDKEYI('XYPOS2',.FALSE.,2,XY,I,ISTA)                           
            CALL CNPAR('XYPOS2',IST)                                            
         END DO                                                                 
         X2=XY(1)                                                               
         Y2=XY(2)                                                               
      ELSE
         WRITE (TEXT,'(A,2I5)') 'Second set of co-ordinates is',X2,Y2
         CALL WRUSER(TEXT,ISTAT)
      END IF                                                                    
C                                                                               
C   Now unscramble the bottom left hand corner and                              
C   the top right hand one.                                                     
C                                                                               
      XPOS(1)=MIN(X1,X2)                                                        
      XPOS(2)=MAX(X1,X2)                                                        
      YPOS(1)=MIN(Y1,Y2)                                                        
      YPOS(2)=MAX(Y1,Y2)                                                        
C                                                                               
C   IF THE ARGS IS AVAILABLE, DRAW A SQUARE ON IT                               
C                                                                               
      IF (ISTAT.EQ.0) THEN                                                      
         CALL ARGS_SQUARE(XPOS,YPOS,ID2)                                        
      END IF                                                                    
C                                                                               
C   FINALLY, WRITE THE VALUES BACK TO THE ENVIRONMENT                           
C                                                                               
      WRITE (TEXT,'(A,2I5)') 'Bottom left hand corner ',                        
     :                        XPOS(1),YPOS(1)                                   
      CALL WRUSER(TEXT,IST)                                                     
      WRITE (TEXT,'(A,2I5)') 'Top right hand corner ',                          
     :                        XPOS(2),YPOS(2)                                   
      CALL WRUSER(TEXT,IST)                                                     
                                                                                
      END                                                                       
      SUBROUTINE COPYIT(IN,NI,MI,LIMX,LIMY,NO,MO,OUT)                           
C                                                                               
C      This subroutine copies part of an array from in to out.                  
C                                                                               
C      Parameteers :-                                                           
C                                                                               
C         Input                                                                 
C 		IN	Real 2D input array                                                      
C 		NI	X-dimension of IN                                                        
C 		MI	Y-dimension of IN                                                        
C 		LIMX	X limits of selected region.                                           
C 		LIMY	Y limits of selected region.                                           
C 		NO	X dimension of OUT                                                       
C 		MO	Y dimension of OUT                                                       
C                                                                               
C         Output                                                                
C 		OUT	Real 2D array to hold the output.                                       
C                                                                               
C      written by K F Hartley at RGO on 30/11/81                                
C                                                                               
      REAL IN(0:NI-1,0:MI-1) , OUT(0:NO-1,0:MO-1)                               
      INTEGER LIMX(2),LIMY(2)                                                   
                                                                                
C                                                                               
C   Do the copy.                                                                
C                                                                               
      J1=0                                                                      
      DO J=LIMY(1),LIMY(2)                                                      
         I1=0                                                                   
         DO I=LIMX(1),LIMX(2)                                                   
            OUT(I1,J1)=IN(I,J)                                                  
            I1=I1+1                                                             
         END DO                                                                 
         J1=J1+1                                                                
      END DO                                                                    
                                                                                
      END                                                                       
      SUBROUTINE ARGS_SQUARE(X,Y,ID)                                            
      INTEGER X(2),Y(2)                                                         
      REAL XP(5),YP(5)                                                          
      XP(1)=X(1)                                                                
      YP(1)=Y(1)                                                                
      XP(2)=X(2)                                                                
      YP(2)=Y(1)                                                                
      XP(3)=X(2)                                                                
      YP(3)=Y(2)                                                                
      XP(4)=X(1)                                                                
      YP(4)=Y(2)                                                                
      XP(5)=XP(1)                                                               
      YP(5)=YP(1)                                                               
C     CALL ARGS_VSRRST                                                          
      CALL ARGS_OVOP(8,'G')                                                     
C     CALL ARGS_CLS(8)                                                          
      CALL ARGS_POLYL(ID,5,XP,YP,ISTAT)                                         
      CALL ARGS_OVCL(8,.FALSE.)                                                 
      END                                                                       
      SUBROUTINE ASP_XYPOS(IX,IY,ISTAT,ID)                                      
C                                                                               
C  THESE 3 ARRAYS ARE USED TO SET UP THE ARGS OVERLAYS.                         
C   IN GENERAL THEY SHOULD NOT BE NEEDED, BUT IN THIS CASE                      
C   IT IS DESIRABLE THAT THE CURSOR SHOULD NOT CLEAR THE OVERLAY                
C   PLANES.                                                                     
C                                                                               
      INTEGER*2 IOFF(3),ION(3),IFLIP(3)                                         
      DATA IOFF,ION,IFLIP/3*0,'0010'X,'0008'X,'0003'X,3*0/                      
C                                                                               
C   ROUTINE WHICH CALLS THE TIME-OUT CURSOR                                     
C   IF THE ARGS CURSOR DOES NOT MOVE IN 5 SECS.                                 
C   IT IS AS IF BUTTON "0" HAD BEEN PRESSED.                                    
C   AND A BAD STATUS IS RETURNED                                                
C                                                                               
C   THIS ROUTINE WOULD NORMALLY BE REPLACED BY A CALL TO                        
C      ARGS_CUROP('1234','G')                                                   
C                                                                               
      CALL ARGS_VSR(IOFF,ION,IFLIP)                                             
      CALL ARGS_CURTO(' ',ID,IB,UX,UY)                                          
      IF (IB.EQ.0) THEN                                                         
         ISTAT=1                                                                
      ELSE                                                                      
         ISTAT=0                                                                
      END IF                                                                    
      CALL ARGS_CURCL                                                           
      IX=UX                                                                     
      IY=UY                                                                     
      END                                                                       
