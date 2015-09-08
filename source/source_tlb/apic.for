C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C                ******************
C                * Program   APIC *
C                ******************
C
C
C          CALLING SEQUENCE:- 
C               APIC IMAGE [ XC YC SATRGB SCALE ZERO ] 
C
C
C          FUNCTION:-
C                        Displays an integer*2 image on the ARGS by  copying
C          the  image  file  into the ARGS memory, then allows  the  user to
C          interactively change the scale and zero point of the  display  to
C          produce he best looking image.  This is faster than other display
C          routines and allows the user to quickly investigate the  range of
C          intensity values within an image.   Also  contains option to  pan
C          and zoom the image.
C
C
C          USE:-
C                   Useful for displaying 16 bit integer images,  especially
C          when one  does  not  know beforehand what scaling parameters will
C          produce the best display. The program will repeatedly ask for new
C          scale  and zero  point  parameters  for  the  display.   Exit  by
C          returning  3 successive null responses (return key). 
C
C
C          USER PARAMETERS:- 
C
C          IMAGE                               The 2-D Starlink image  (I*2)
C                                              to be displayed. 
C
C          SCALE          8                    Determines   the    intensity
C                                              scaling.  The intensity scale
C                                              will  range   from   ZERO  to
C                                              ZERO + 2**(SCALE).         If
C                                              SCALE  <  0,  the   intensity
C                                              scale is inverted so that the
C                                              lowest value is displayed  as
C                                              white and the highest value as
C                                              black (for a linear greyscale
C                                              LUT in the ARGS).   SCALE may
C                                              range from 1-15 or -1 to -15.
C
C          ZERO           0                    The intensity zero point for
C                                              the image.
C
C
C          NORMALLY DEFAULTED PARAMETERS:- 
C
C          XC              256                 The X coordinate on the  ARGS
C                                              of the image centre. 
C
C          YC              256                 The Y coordinate on the  ARGS
C                                              of the image centre. 
C
C          SATRGB          255,100,100         Determines the colour  to  be
C                                              used to  display pixels which
C                                              are brighter than the maximum
C                                              scaling   value.  The   three
C                                              integers (range = 0  to  255)
C                                              determine he intensity of the
C                                              red, green, and blue guns. 
C
C
C          W D Pence, D Terrett          AAO and RAL             22-NOV-83
C           Based on program ADISP
C
C-------------------------------------------------------------------------- 
C
      PROGRAM APIC
      INTEGER IDIMN(99),SATRGB(3)
      LOGICAL TRIM,LLOG
      CHARACTER VALUE*80
      INCLUDE 'INTERIM(FMTPAR)'                    
      INCLUDE 'INTERIM(ERRPAR)'
c
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
      ELSE
         CALL RDIMAG('IMAGE',102,99,IDIMN,NDIMS,IPNT1,ISTAT)                   
         IF (NDIMS.NE.2.OR.ISTAT.NE.ERR_NORMAL) THEN                                                   
            CALL WRUSER('Image not found or not 2D',J)                                  
         ELSE                                                                   
C
C         GET CENTRE OF IMAGE
C
            CALL RDKEYI('XC',.TRUE.,1,IXC,NVALS,JSTAT)   
            CALL RDKEYI('YC',.TRUE.,1,IYC,NVALS,JSTAT)   
C
C         GET COLOUR TO BE USED TO INDICATE SATURATED PIXELS
C
            CALL RDKEYI('SATRGB',.TRUE.,3,SATRGB,NVALS,JSTAT)   
C
C         GET WORK SPACE FOR NEW IMAGE
C
            CALL GETDYN('QQQ',102,IDIMN(1)*IDIMN(2),IPNT2,ISTAT)
C
            CALL DISP2(%VAL(IPNT1),%VAL(IPNT2),IDIMN(1),IDIMN(2),
     &          IXC,IYC,SATRGB,ILOW,IHI)
            CALL FRDATA(' ',JSTAT)                                              
C
C           UPDATE THE ARGS DATABASE
C
	    CALL ARGS_NUMIM(IDMAX)                                                     
	    IF (IDMAX.GE.1) THEN                                                       
		CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)                               
		CALL ASP_DZTOI('ZXC',VALUE,IZXC,JSTAT)                                        
		CALL ASP_DZTOI('ZYC',VALUE,IZYC,JSTAT)                                        
		CALL ASP_DZTOI('ZXF',VALUE,IXF,JSTAT)                                         
		CALL ASP_DZTOI('ZYF',VALUE,IYF,JSTAT)                                         
		CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)                                     
	    ELSE                                                                       
		IZXC=256                                                                      
		IZYC=256                                                                      
		IXF=1                                                                         
		IYF=1                                                                         
	    ENDIF                                                                      

            CALL ARGS_WRIM (IXC,IYC,IDIMN(1),IDIMN(2),IDIMN(1),IDIMN(2),        
     &         JSTAT)                                                           
            IF(JSTAT.NE.0)THEN                                                  
               CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)              
            ELSE                                                                
               CALL ARGS_RDPAR ('DISPZOOM',1,VALUE,NVALS,JSTAT)                 
               VLO=ILOW
               VHI=IHI
               TRIM=.TRUE.
               LLOG=.FALSE.
               CALL ASP_LTODZ ('TRIM',TRIM,VALUE,JSTAT)                          
               CALL ASP_LTODZ ('LOG',LLOG,VALUE,JSTAT)                          
               CALL ASP_FTODZ ('PVLO',VLO,VALUE,JSTAT)                          
               CALL ASP_FTODZ ('PVHI',VHI,VALUE,JSTAT)                          
	       CALL ASP_ITODZ ('ZXC',IZXC,VALUE,JSTAT)                                 
	       CALL ASP_ITODZ ('ZYC',IZYC,VALUE,JSTAT)                                 
	       CALL ASP_ITODZ ('ZXF',IXF,VALUE,JSTAT)                                  
	       CALL ASP_ITODZ ('ZYF',IYF,VALUE,JSTAT)                                  
               CALL ARGS_WRPAR ('DISPZOOM',VALUE,1,JSTAT)                       
            ENDIF                                                               
         END IF                                                                 
      ENDIF                                                                     
      END                                                                       
C**************************************************************************
      SUBROUTINE DISP2(IPIC,IWORK,NX,NY,IXC,IYC,SATRGB,LOW,HIGH)               
      INTEGER LUTPOS(3,256),LUTNEG(3,256),LUT(3,256)
      INTEGER NX,NY,IXC,IYC,HIGH,SATRGB(3)
      INTEGER*2 IPIC(NX*NY),IWORK(NX*NY),IDUMMY,SHORT(2),IFACTOR
      INTEGER*2 ARGSIN(4),ARGSOUT(3)
      CHARACTER TEXT*70
      EQUIVALENCE (SHORT(1),ISHIFT)
C
C     ENABLE WRITING TO ALL 16 Z PLANES
C
        CALL ZENABLE
C
C     SET DEFAULT Z PLANE ASSIGNMENTS IN THE ARGS
C
        CALL ZDEFALT
C
C     READ CURRENT ARGS LOOK UP TABLE
C
        CALL ASP_GETLUT(LUT)
C
C     SET UP SPECIAL LOOKUP TABLES
C   
C       COMPACT THE CURRENT LUT INTO LEVELS 1-64
C       REVERSE THE ORDER FOR THE NEGATIVE LUT
C
      DO J=1,3
        LUTPOS(J,1)=0
        LUTNEG(J,1)=0
      END DO
      DO I=2,64
        K=I*4
        KK=(66-I)*4
        DO J=1,3
          LUTPOS(J,I)=LUT(J,K)
          LUTNEG(J,I)=LUT(J,KK)
        ENDDO
      ENDDO
C
C     LEVELS 65-128
C       POSITIVE LUT: SET TO SATURATION LEVEL
C       NEGATIVE LUT: SET TO BLACK
C
        DO I=65,128
           LUTPOS(1,I)=SATRGB(1)
           LUTPOS(2,I)=SATRGB(2)
           LUTPOS(3,I)=SATRGB(3)
           DO J=1,3
             LUTNEG(J,I)=0
           END DO
        END DO
C
C      LEVELS 129-256
C       POSITIVE LUT: BLACK  (NEGATIVE IMAGE VALUES)
C       NEGATIVE LUT: SET TO SATURATION COLOUR
C
        DO I=129,256
           DO J=1,3
              LUTPOS(J,I)=0
           END DO
           LUTNEG(1,I)=SATRGB(1)
           LUTNEG(2,I)=SATRGB(2)
           LUTNEG(3,I)=SATRGB(3)
        END DO
C
C     TURN OFF DISPLAY WHILE MESSING AROUND
C
      CALL ARGSOFF
C
C     RESET VIDIO STATUS REGISTERS
C
        CALL ARGS_VSRRST
C
C     SEND LUT TO ARGS
C
        CALL SRCOLS(0,256,LUTPOS)
        NEG=0
C
C     SEND RAW 16 BIT IMAGE TO ARGS
C
      NX1=IXC-NX/2
      NY1=IYC-NY/2
      CALL SRPXI2(IPIC,NX,NX,NY,NX1,NY1,16,.FALSE.,IDUMMY,1)
C
C     SET DEFAULT PAN/ZOOM PARAMETERS
C
      ARGSIN(1)=0
      ARGSIN(2)=0
      ARGSIN(3)=IXC
      ARGSIN(4)=IYC
C
C     SET DEFAULT DISPLAY PARAMETERS
C
      NULL=0
      NEWBASE=0
      ISCALE=8
      CALL SCALE(ISCALE)
      CALL ARGSON
      LOW=NEWBASE
      HIGH=2**ISCALE+NEWBASE-1
      CALL WRUSER(' ',IST)
      CALL WRUSER(' SCALE controls image contrast',IST)
      CALL WRUSER('    range =  1 to  15 (positive image)',IST)
      CALL WRUSER('      or  = -1 to -15 (negative image).',IST)
      CALL WRUSER('    SCALE =  0 selects trackerball pan/zoom.',IST)
      CALL WRUSER(' ',IST)
      CALL WRUSER(' ZERO selects lower limit of intensity range.',IST)
      CALL WRUSER(' ',IST)
      CALL WRUSER(' Exit by hitting RETURN three times.',IST)
      CALL WRUSER(' ',IST)
      CALL WRUSER('                         Intensity Range',IST)
      WRITE(TEXT,1071)LOW,HIGH
1071  FORMAT(20X,I7,'  to ',I7)
      CALL WRUSER(TEXT,IST)
C
C   Beginning of main loop of program:
C
10    CONTINUE

      IOLD=ISCALE
C
C     FIND SCALE FACTOR FOR IMAGE
C
      CALL RDKEYI('SCALE',.TRUE.,1,ISCALE,NVALS,JSTAT)   
      CALL CNPAR('SCALE',ISTATUS)
C
      IF (ISCALE .EQ. 0)THEN
C
C        RUN THE PAN/ZOOM PROGRAM
C
         CALL LOAD_PANZ
         CALL WRITE_PANZ(ARGSIN)
         CALL ARGS_LAMPS(1,1,1,1)
         CALL RUN_PANZ
         CALL READ_PANZ(ARGSOUT)
         CALL ARGS_LAMPS(0,0,0,0)
         ARGSIN(3)=ARGSOUT(1)
         ARGSIN(4)=ARGSOUT(2)
         IXF=(ARGSOUT(3).AND. 'FF'X)+1
         ARGSIN(1)=(IXF-1)*256+IXF-1
         IF (IXF .LT. 1)THEN
            ARGSIN(2)=0
         ELSE IF (IXF .LT. 3)THEN
            ARGSIN(2)=1
         ELSE IF (IXF .LT. 7)THEN
            ARGSIN(2)=2
         ELSE IF (IXF .LT. 15)THEN
            ARGSIN(2)=3
         ELSE
            ARGSIN(2)=4
         END IF
         ISCALE=IOLD
         NULL=0
         GO TO 10
      END IF
C
      IF (ABS(ISCALE) .LT. 1 .OR. ABS(ISCALE).GT.15)THEN
        CALL WRUSER('  !!! Invalid SCALE factor !!!',IST)
        GO TO 10
      END IF
C
C       IF USER HAS RETURNED NULL VALUE 3 TIMES IN ROW, JUMP OUT OF LOOP
C
      IF (JSTAT .EQ. 1)THEN
        NULL=NULL+1
        IF (NULL .EQ. 3)GO TO 20
      ELSE
        NULL=0
      END IF
      IF (ISCALE .LT. 0)THEN
        MSCALE=-ISCALE
        IF (NEG .NE. 1)THEN
          CALL SRCOLS(0,256,LUTNEG)
          NEG=1                  
        END IF
      ELSE
        MSCALE=ISCALE
        IF (NEG .NE. 0)THEN
          CALL SRCOLS(0,256,LUTPOS)
          NEG=0
        END IF
      END IF  
      CALL SCALE(MSCALE)
      LOW=NEWBASE
      HIGH=2**MSCALE+NEWBASE-1
      WRITE(TEXT,1071)LOW,HIGH
      CALL WRUSER(TEXT,IST)
C
C     FIND ZEROPOINT FOR IMAGE
C
15    CALL RDKEYI('ZERO',.TRUE.,1,NEWBASE,NVALS,JSTAT)
      CALL CNPAR('ZERO',ISTATUS)
      IF (ABS(NEWBASE) .GT. 16383)THEN
        CALL WRUSER('  !!! Invalid ZERO point !!!',IST)
        GO TO 15
      END IF
      IF (JSTAT .EQ. 1)THEN
        NULL=NULL+1
        IF (NULL .EQ. 3)GO TO 20
      ELSE
        NULL=0
      END IF
      ISHIFT=IZERO-NEWBASE
      IF (ISHIFT .NE. 0)THEN
C
C     ADD ISHIFT TO THE IMAGE IN THE ARGS; TURN DISPLAY OFF DURING OPERATION
C
        CALL ARGSOFF
        CALL ZDEFALT
        CALL ZERO(SHORT(1),SHORT(2),NX,NY,NX1,NY1)
        CALL SCALE(MSCALE)
        CALL ARGSON
        IZERO=NEWBASE
        LOW=NEWBASE
        HIGH=2**MSCALE+NEWBASE-1
      END IF
      WRITE(TEXT,1071)LOW,HIGH
      CALL WRUSER(TEXT,IST)
      GO TO 10
C
C   Exit from loop
C
20    CONTINUE
C
C     MAKE NEW SCALED 8 BIT IMAGE TO WRITE TO LOWER 8 Z PLANES
C
      NHIGH=HIGH-LOW
      IF (ISCALE .GT. 8)THEN
        KSCALE=2**(ISCALE-8)
        DO I=1,NX*NY
          IWORK(I)=MIN(NHIGH,MAX(0,(IPIC(I)-LOW)))/KSCALE
        END DO
      ELSE IF (ISCALE .GT. 0)THEN
        KSCALE=2**(8-ISCALE)
        DO I=1,NX*NY
          IWORK(I)=MIN(NHIGH,MAX(0,(IPIC(I)-LOW)))*KSCALE
        END DO
      ELSE IF (ISCALE .LT. -8)THEN
        KSCALE=2**(-ISCALE-8)
        DO I=1,NX*NY
         IWORK(I)=255-MIN(NHIGH,MAX(0,(IPIC(I)-LOW)))/KSCALE
        END DO
      ELSE 
        KSCALE=2**(8+ISCALE)
        DO I=1,NX*NY
         IWORK(I)=255-MIN(NHIGH,MAX(0,(IPIC(I)-LOW)))*KSCALE
        END DO
      END IF
C
C     SET DEFAULT ARGS Z PLANE ASSIGNMENTS
C
      CALL ARGSOFF
      CALL ZDEFALT
C
C     DISPLAY NEW IMAGE
C
      CALL SRPXI2(IWORK,NX,NX,NY,NX1,NY1,
     &               16,.FALSE.,IDUMMY,1)
C
C	SEND ORIGINAL LUT TO ARGS
C
      CALL SRCOLS(0,256,LUT)
      CALL ARGSON
C
      END
C************************************************************************
      SUBROUTINE SCALE(SC)
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 ZPIP(40)
C
C     ASSIGN Z PLANES IN ARGS TO APPROPRIATE IMAGE PLANES
C
C     FIRST, ASSIGN THE LOWER 6 Z PLANES
C      IF THE IMAGE PLANE IS LESS THAN 0 OR GREATER THAN 14,
C       THEN SET TO NON-EXISTENT 16TH PLANE
C
      IDUMMY=16
      DO I=0,5
        IP=SC-6+I
        IF (IP .LT. 0 .OR. IP .GT. 14)THEN
          IP=IDUMMY
          IDUMMY=IDUMMY+1
        END IF
        ZPIP(I+1)=256*I+IP
      END DO
C
C    .OR. ANY HIGHER IMAGE PLANES (UP TO #14) ONTO Z PLANE 6
C
      NASSIGN=6
      DO I=SC,14
        NASSIGN=NASSIGN+1
        ZPIP(NASSIGN)=6*256+I
      END DO
C
C     ASSIGN IMAGE PLANE 15 (SIGN BIT) TO ZPLANE 7
C
      NASSIGN=NASSIGN+1
      ZPIP(NASSIGN)=7*256+15
C
C     DEASSIGN ANY LOWER IMAGE PLANES BY ASSIGNING THEM
C       TO THE NON-EXISTENT Z PLANE 8
C
      DO I=0,SC-7
        NASSIGN=NASSIGN+1
        ZPIP(NASSIGN)=8*256+I
      END DO
C
C     NOW SEND THE INSTRUCTIONS TO THE ARGS
C
      CALL ARGS_PUT1('5400'X+NASSIGN)
      DO I=1,NASSIGN
        CALL ARGS_PUT1(ZPIP(I))
      END DO
      CALL ARGS_FLUSH(-1)
C
      END
C************************************************************************
      SUBROUTINE ZERO(LSBITS,MSBITS,NX,NY,NX1,NY1)
C
C     ADD A CONSTANT TO THE ARGS IMAGE
C      CONSTANT IS A 32 BIT INTEGER, BROKEN INTO THE MOST SIGNIFICANT
C      BITS (MSBITS), AND THE LEAST SIGN. BITS (LSBITS)
C
      INTEGER*2  MSBITS,LSBITS
      CALL ARGS_PUT1('9C10'X)
      CALL ARGS_PUT1(0)
      CALL ARGS_PUT1(NX)
      CALL ARGS_PUT1(NY)
      CALL ARGS_PUT1(NX1)
      CALL ARGS_PUT1(NY1)
      CALL ARGS_PUT1('0F00'X)
      CALL ARGS_PUT1(NX1)
      CALL ARGS_PUT1(NY1)
      CALL ARGS_PUT1(LSBITS)
      CALL ARGS_PUT1(MSBITS)
      CALL ARGS_FLUSH(-1)
      END
C************************************************************************
      SUBROUTINE ARGSOFF
C
C     TURN ARGS PICTURE OFF
C
      CALL ARGS_PUT1('4101'X)
      CALL ARGS_FLUSH(-1)
      END
C************************************************************************
      SUBROUTINE ARGSON
C
C     TURN ARGS PICTURE ON
C
      CALL ARGS_PUT1('4100'X)
      CALL ARGS_FLUSH(-1)
      END
C************************************************************************
      SUBROUTINE ZENABLE
C
C     ENABLE WRITING TO ALL 16 Z PLANES
C
      CALL ARGS_PUT1('2D01'X)
      CALL ARGS_PUT1('FFFF'X)
      CALL ARGS_FLUSH(-1)
      END
C************************************************************************
      SUBROUTINE ZDEFALT
C
C     RESET Z PLANE ASSIGNMENTS TO DEFAULT VALUES
C
      CALL ARGS_PUT1('5410'X)
      CALL ARGS_PUT1('0000'X)
      CALL ARGS_PUT1('0101'X)
      CALL ARGS_PUT1('0202'X)
      CALL ARGS_PUT1('0303'X)
      CALL ARGS_PUT1('0404'X)
      CALL ARGS_PUT1('0505'X)
      CALL ARGS_PUT1('0606'X)
      CALL ARGS_PUT1('0707'X)
      CALL ARGS_PUT1('0808'X)
      CALL ARGS_PUT1('0909'X)
      CALL ARGS_PUT1('0A0A'X)
      CALL ARGS_PUT1('0B0B'X)
      CALL ARGS_PUT1('0C0C'X)
      CALL ARGS_PUT1('0D0D'X)
      CALL ARGS_PUT1('0E0E'X)
      CALL ARGS_PUT1('0F0F'X)
      CALL ARGS_FLUSH(-1)
      END
