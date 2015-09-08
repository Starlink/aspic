      PROGRAM LUTTWEAK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     **********************
C                     *                    *
C                     * Program   LUTTWEAK *
C                     *                    *
C                     **********************
C
C
C
C          CALLING SEQUENCE:-
C               LUTTWEAK
C
C
C          FUNCTION:-
C               Allows the user to interactively  alter  the  look-up-table
C               (LUT)   of  the  Args   by  using  the  Trackerball.  Three
C               different LUTs are available: a linear greyscale, a  series
C               of  solid  colours,  and a series of colours which linearly
C               increase in intensity.  The X  motion  of  the  trackerball
C               shifts  the  whole  LUT to the left or right (thus changing
C               the intensity of the image when the greyscale LUT is  used)
C               and  the  Y  motion  expands  or  compresses  the LUT (thus
C               changing the contrast of the image).
C
C
C          USE:-
C               Useful for tweaking the LUT to give the best looking  image
C               and to enhanse subtle features in the image.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Resets the current LUT to its initial standard value.
C
C         WHITE 2     Cycles through the three available LUTs.
C
C         WHITE 3     Does nothing.
C
C         RED   4     Exits from program.
C
C
C
C
C
C
C         W D Pence                AAO                            13-JAN-83
C
C
C--------------------------------------------------------------------------



      PARAMETER NLUT=3,DELORG=1.,SFACTOR=100.
      INTEGER LUT1(3,0:255),LUT2(3,0:255),LUT3(3,0:255)
      INTEGER TAB(3,0:255),NCOL(3,8)
      DATA NCOL/255,0,0, 0,255,0, 255,255,128, 0,0,255, 255,255,0,
     1 255,0,255, 0,255,255, 255,255,255/
C
C      FIRST INITIALIZE THE ARGS , AND STOP ON ERROR
C
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT.NE.0)THEN
        CALL WRUSER('ARGS NOT AVAILABLE',ISTAT)
        STOP
      END IF
C
C     TELL ARGS TO TIMEOUT IN FOUR SECONDS IF THERE IS A HANGUP
C
      CALL ARGS_TIMEOUT(4)
C
C     INITIALIZE THE LOOK UP TABLES (LUT)
C      A GRAYSCALE:
C
      DO I=0,255
      DO J=1,3
        LUT1(J,I)=I
      END DO
      END DO
C
C      A RAMPED COLOUR SCALE:
C
      I=0
      DO KCOL=1,8
      DO KINT=1,32
        I=I+1
        DO J=1,3
          LUT2(J,I)=NCOL(J,KCOL)*KINT/32.
        END DO
      END DO
      END DO
C
C     A BLOCKED COLOUR SCALE
C
      I=0
      DO KCOL=1,8
      DO KINT=1,32
        I=I+1
        DO J=1,3
          LUT3(J,I)=NCOL(J,KCOL)
        END DO
      END DO
      END DO
C
C   LOAD THE DEFAULT LUT INTO THE ARGS
C
      CALL SRCOLS(0,256,LUT1)
C
C   SET-UP INITIAL VALUES
C
      ORIG=1.
      SLOPE=1.
      JLUT=1
      IXLAST=256
      IYLAST=256
C
C   SET CURSOR TO PRESET POSITION
C
      CALL ARGS_CURP(0,IXLAST,IYLAST)
C
C   LOAD THE TRACKERBALL PROGRAM
C     THIS IS A MODIFIED VERSION OF THE STANDARD TRACKERBALL ROUTINE
C     WHICH IMMEDIATELY RETURNS WITH THE CURRENT X,Y CURSOR POSITION,
C     REGARDLESS OF WHETHER OR NOT ANY OF THE BUTTONS HAVE BEEN PRESSED.
C
      CALL ARGS_TBSTL
C
C   TURN ON TRACKERBALL LAMPS
C
      CALL ARGS_LAMPS(1,1,0,1)
C
C   DO A DUMMY READ TO CLEAR ANY BUTTONS
C
      CALL ARGS_TBSTX(IXX,IYY,IB,IB,IB,IB)
C
C   READ TRACKERBALL
C
10    CALL ARGS_TBSTX(IX,IY,IB1,IB2,IB3,IB4)
C
      IF (IX .NE. IXLAST .OR. IY .NE. IYLAST)THEN
C
C   FIND THE ORIGIN AND SLOPE OF THE LINEAR FUNCTION
C   WHICH DEFINES THE NEW LUT FROM THE ORIGINAL LUT
C
         ORIG=ORIG+(IX-IXLAST)*DELORG*SLOPE
         SLOPE=SLOPE*(1.+(IY-IYLAST)/SFACTOR)
C
C        IF CURSOR IS ON THE BOUNDARY, WRAP IT AROUND TO THE OTHER SIDE
C
         IF (IX .EQ. 512)THEN
           IXLAST=0
           CALL ARGS_CURP(0,IXLAST,IY)
         ELSE IF (IX .EQ. -1)THEN
           IXLAST=511
           CALL ARGS_CURP(0,IXLAST,IY)
         ELSE
           IXLAST=IX
         END IF
         IF (IY .EQ. 512)THEN
           IYLAST=0
           CALL ARGS_CURP(0,IXLAST,IYLAST)
         ELSE IF (IY .EQ. -1)THEN
           IYLAST=511
           CALL ARGS_CURP(0,IXLAST,IYLAST)
         ELSE
           IYLAST=IY
         END IF
      ELSE IF (IB1 .EQ. 1) THEN
C
C      BUTTON 1 WAS PRESSED, SO RESET THE LUT TO INITIAL STATE
C
         ORIG=1.
         SLOPE=1.
      ELSE IF (IB2 .EQ. 1)THEN
C
C      BUTTON 2 WAS PRESSED, SO LOAD A DIFFERENT LUT
C
         JLUT=JLUT+1
         IF (JLUT .GT. NLUT)JLUT=1
      ELSE IF (IB4 .EQ. 1)THEN
C
C      BUTTON 4 WAS PRESSED, SO TURN OFF LAMPS AND EXIT
C
         CALL ARGS_LAMPS(0,0,0,0)
         STOP
      ELSE
C
C      NOTHING HAS CHANGED FROM THE LAST T/B READ SO
C        READ THE T/B AGAIN
C
         GO TO 10
      END IF
C
C   NOW GO AND FILL THE LUT AND WRITE IT TO THE ARGS
C
      DO I=1,255
         K=ORIG+I*SLOPE
         IF (JLUT .EQ. 1)THEN
           K=MIN(MAX(K,0),255)
           DO J=1,3
              TAB(J,I)=LUT1(J,K)
           END DO
         ELSE IF (JLUT .EQ. 2)THEN
           K=MOD(K,256)
           IF (K .LT. 0)K=K+256
           DO J=1,3
              TAB(J,I)=LUT2(J,K)
           END DO
         ELSE IF (JLUT .EQ. 3)THEN
           K=MOD(K,256)
           IF (K .LT. 0)K=K+256
           DO J=1,3
              TAB(J,I)=LUT3(J,K)
           END DO
         END IF
      END DO
      DO J=1,3
        TAB(J,0)=0
      END DO
C
C    SEND COLOUR TABLE TO THE ARGS
C
      CALL SRCOLS(0,256,TAB)
C
C    MAKE DUMMY CALL TO TRACKERBALL TO ELIMINATE DOUBLE BOUNCE
C
      IF (IB2 .EQ. 1)THEN
         CALL ARGS_TBSTX(IXX,IYY,IB,IB,IB,IB)
      END IF
      GO TO 10
      END
