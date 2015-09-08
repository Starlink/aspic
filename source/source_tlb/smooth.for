C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   SMOOTH *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               SMOOTH
C
C 
C          FUNCTION:-
C               It convolves an image with a  2-D  function  which  may  be
C               constant (a top hat) or a Gaussian, and of variable size.
C
C
C          USE:-
C               It gives several options for smoothing an image.
C
C
C
C         USER PARAMETERS:-      
C
C         INPUT                               This   is   the   input
C                                             Starlink image.
C
C         OUTPUT                              This  is   the   output   2-D
C                                             Starlink image.
C
C         BOXSIZ          5                   The size  of  the  convolving
C                                             box - between 3 and 33
C                                             and odd.
C
C         TYPE            GAUSS               This  may   be   Tophat   (or
C                                             anything starting with T) for
C                                             convolution with  a  top  hat
C                                             (constant);     any     other
C                                             response    gives    Gaussian
C                                             smoothing.
C
C         SIGMA           1                   If the TYPE  is  Gauss,  then
C                                             this  is  the  sigma  of  the
C                                             convolving Gaussian
C
C
C         M.J.Currie               RAL                            27-Apr-84
C
C
C--------------------------------------------------------------------------



      PARAMETER (MAXSIZ=33)

C      This version by M.J.Currie at RAL 27/4/84
C      based on earlier versions by K.F.Hartley and C.D.Pike at RGO
      INTEGER*4 AXIS(2),KIND,SIZE,PIN,POUT,ISTAT      
      REAL*4 SIGMA          
      CHARACTER*6 TYPE,BUFFER*32
      INCLUDE 'INTERIM(FMTPAR)' 
      INCLUDE 'INTERIM(ERRPAR)' 
C 
C      SET UP INPUT AND OUTPUT IMAGES    
C 
      CALL RDIMAG('INPUT',FMT_R,2,AXIS,I,PIN,ISTAT)  
      IF (ISTAT.NE.0.OR.I.NE.2) THEN     
            CALL WRERR ('DAMNIN')        
            CALL EXIT       
      END IF   
      CALL WRIMAG('OUTPUT',FMT_R,AXIS,2,POUT,ISTAT)  
      IF (ISTAT.NE.0.OR.I.NE.2) THEN     
            CALL WRERR('DAMNOU')         
            CALL EXIT       
      END IF   
C 
C      NOW PICK UP THE BOXSIZE           
C 
      SIZE=5   
  100 CALL RDKEYI('BOXSIZ',.TRUE.,1,SIZE,I,ISTAT)     
      IF (ISTAT.GT.1.OR.I.GT.1.OR.SIZE.LT.3.OR.SIZE.GT.MAXSIZ
     :    .OR. MOD(SIZE,2) .EQ. 0 ) THEN    
         CALL WRERR('AGAIN')
         CALL CNPAR('BOXSIZ',ISTAT)      
         IF (ISTAT.NE.0) THEN            
            CALL EXIT       
         ELSE  
            IF ( MOD(SIZE,2) .EQ. 0 ) CALL WRUSER('BOXSIZ must be odd',
     :          ISTAT)
            IF ( SIZE .LT. 3 .OR. SIZE .GT. MAXSIZ ) THEN
               WRITE(BUFFER,'(''BOXSIZ must be between 3 and'',I3)')
     :                                                          MAXSIZ
               CALL WRUSER(BUFFER,ISTAT)
            END IF
            SIZE=5
            GO TO 100       
         END IF
      END IF   
C 
C      AND THE TYPE OF PROFILE           
C 
      TYPE='GAUSS '         
      CALL WRUSER('The options are GAUSS or TOPHAT',STATUS)
  200 CALL RDKEYC('TYPE',.TRUE.,1,TYPE,I,ISTAT)       
      IF (ISTAT.GT.1) THEN  
         CALL WRERR('AGAIN')
         CALL CNPAR('TYPE',ISTAT)        
         IF (ISTAT.NE.0) THEN            
            CALL EXIT       
         ELSE  
            GO TO 200       
         END IF
      END IF   
      CALL STR$UPCASE(TYPE,TYPE)
      IF (TYPE(1:1).EQ.'T') THEN         
         KIND=1
      ELSE     
         KIND=2
      END IF   
C 
C      PICK UP SIGMA IF A GAUSSIAN PROFILE IS TO BE USED           
C 
      SIGMA=1  
      IF (KIND.NE.1) THEN   
  300    CALL RDKEYR('SIGMA',.TRUE.,1,SIGMA,I,ISTAT)  
         IF (ISTAT.GT.1.OR.I.GT.1) THEN  
            CALL WRERR('AGAIN')          
            CALL CNPAR('SIGMA',ISTAT)    
            IF (ISTAT.NE.0) THEN         
               CALL EXIT    
            ELSE            
               GO TO 300    
            END IF          
         END IF
      END IF   
C 
C      NOW CALL THE WORKING SUBROUTINE   
C 
      CALL SMOOTH(%VAL(PIN),%VAL(POUT),AXIS(1),AXIS(2),            
     1            KIND,SIZE,SIGMA)       
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END      
      SUBROUTINE SMOOTH(IN,OUT,NX,NY,KIND,NB,SIGMA)   
C 
C 
C      CONVOLVES A 2-D ARRAY WITH A BOX WITH GAUSSIAN OR TOP-HAT SHAPE          
C 
C      IN IS THE INPUT ARRAY , AND OUT THE OUTPUT ARRAY BOTH OF SIZE            
C      NX BY NY
C      KIND = 1 GIVES TOP-HAT KIND = 2 (OR ANYTHING EXCEPT 1) GIVES GAUSSIAN    
C      THE CONVOLVING BOX IS OF SIZE NB BY NB         
C      IF KIND=2 , SIGMA IS THE STANDARD DEVIATION OF THE GAUSSIAN 
C 
C -            
      REAL WT(33,33)
      REAL IN(NX,NY),OUT(NX,NY)   
      INTEGER*4 KIND,NB     
      REAL*4 SIGMA          
      CHARACTER*72 LIST     
      CHARACTER*13 MESS     
C 
C      FIRST GENERATE THE CONVOLVING FUNCTION         
C 
C      THE SUM OF THE ELEMENTS SHOULD BE 1 , SO FIND TOTAL         
C 
      TOT=0    
      IF (KIND.EQ.1) THEN
         DO 20 J=1,NB
            DO 10 I=1,NB
               WT(I,J)=1.0
   10       CONTINUE
   20    CONTINUE       
      ELSE     
         ALPHA=1.0/(1.4142*SIGMA)  
         A2=ALPHA*ALPHA     
         C=REAL(NB+1)/2.0  
         DO 40 J=1,NB       
            Y=REAL(J)
            DO 30 I=1,NB    
               X=REAL(I)   
               R2=(X-C)*(X-C)+(Y-C)*(Y-C)
               WT(I,J)=EXP(-A2*R2) 
   30       CONTINUE    
   40    CONTINUE       
      END IF   

      IEDGE=(NB-1)/2

      DO J=1,NY
C
C   JM,JP ARE THE MINIMUM AND MAXIMUM ROWS USED IN THE SMOOTH
C   IM,IP ARE THE MINIMUM AND MAXIMUM COLUMNS USED IN THE SMOOTH
C
         JM=MAX(1,J-IEDGE)
         JP=MIN(NY,J+IEDGE)
         DO I=1,NX
            IM=MAX(1,I-IEDGE) 
            IP=MIN(NX,I+IEDGE) 
            S=0. 
C
C   JJ IS THE NUMBER OF ROWS OF THE BOX CLIPPED BY THE IMAGE'S EDGE
C   II IS THE NUMBER OF COLUMNS OF THE BOX CLIPPED BY THE IMAGE'S EDGE
C
            JJ=MIN(JM-J+IEDGE,IEDGE)
            WTS=0.
            DO KJ=JM,JP 
               JJ=JJ+1
               II=MIN(IM-I+IEDGE,IEDGE)
               DO KI=IM,IP
                  II=II+1
                  S=S+IN(KI,KJ)*WT(II,JJ)
C
C   WTS SUMS THE WEIGHTS FOR NORMALISATION
C
                  WTS=WTS+WT(II,JJ)
               END DO
            END DO
            OUT(I,J)=S/WTS
         END DO
C                                                                               
C   OUTPUT TO ENCOURAGE THE USER                                                
C                                                                               
         IF (MOD(J,30) .EQ. 0) THEN
            MESS='FINISHED LINE'
            WRITE (LIST,'(A,2X,I4)') MESS,J
            CALL WRUSER (LIST,ISTAT)
         END IF
      END DO
C         

      END 
