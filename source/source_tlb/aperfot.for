      PROGRAM APERFOT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   APERFOT *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               APERFOT
C
C
C          FUNCTION:-
C               Interactive program for doing simple aperture photometry on
C               objects in an image displayed on the Args.  The trackerball
C               is used to control two concentric circles superposed on the
C               image.   The  center  of the circles and their radii can be
C               controlled independently. When the first (Green) button  is
C               pressed,  the  magnitude  of  the  object  is calculated by
C               summing the total intensity of the pixels within the  inner
C               circle,  and  subtracting  the  background level determined
C               from the  mean  value  (after  going  through  one  2*sigma
C               rejection cycle) in the outer annulus. The magnitude of the
C               object (defined as M = 30 - 2.5*log(Net Counts)  and  other
C               related  statistics  are displayed on the user terminal and
C               also written to a Log file which may be printed later.  The
C               calculated  mean  error  of  the  magnitude  assumes photon
C               statistics and should not be taken too seriously.
C
C
C          USE:-
C               Useful for making quick  and  simple  measurements  of  the
C               intensity  of  objects  in an image.  The main advantage of
C               this program over other photometry programs is that  it  is
C               fast,  interactive,  and the object being measured does not
C               have to have  any  specific  structure  (e.g.,  a  Gaussian
C               profile).
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               Name of the 2D Starlink image
C                                             which contains the objects to
C                                             be   measured.    The   image
C                                             should  already  be displayed
C                                             on the Args.
C
C         TITLE                               Title to be written to the log
C                                             file (APERFOT.LIS).
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C
C
C         GREEN 1     Accept the current cursor position  and  compute  the
C                     intensity  of  the  object within the smaller circle,
C                     subtracting the mean value within the outer annulus.
C
C         WHITE 2     Enable the trackerball to control the position of the
C                     centre of the two concentric circles.
C
C         WHITE 3     Enable the trackerball to control the  radii  of  the
C                     two circles.
C
C         RED   4     Exit from the program.
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



C
      DIMENSION IDIM(2)
C
C     INITIALIZE ARGS
C
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
         CALL WRUSER('ARGS NOT AVAILABLE',ISTATUS)
         STOP
      END IF
C
C     TIME OUT AFTER 4 SECONDS IF ARGS HANGS UP
C
      CALL ARGS_TIMEOUT(4)
C
C     CHECK THAT AT LEAST ONE IMAGE IS DISPLAYED
C
      CALL ARGS_NUMIM(ID)
      IF (ID .EQ. 0)THEN
         CALL WRERR('NOIMS')
         STOP
      END IF
C
C     READ INPUT FILE
C
5     CALL RDIMAG('IMAGE',204,2,IDIM,NDIM,IPNT,ISTATUS)
      IF (ISTATUS .NE. 0)THEN
         CALL WRUSER('ERROR IN READING FILE. TRY AGAIN',ISTATUS)
         CALL CNPAR('IMAGE',ISTATUS)
         GO TO 5
      END IF
C
C     INITIALIZE ARGS REGISTERS
C
      CALL ARGS_VSRRST
C
C     SET UP DOT CURSOR
C
      CALL ARGS_CURS('.')
      CALL ARGS_CURC('G')
C
C     TURN ON LAMPS
C
      CALL ARGS_LAMPS(1,1,1,1)
C
C     LOAD TRACKERBALL READING PROGRAM INTO ARGS
C
      CALL ARGS_TBSTL
C
C     ENABLE OVERLAY
C
      CALL ARGS_OVOP(8,'G')
C
C     TURN ON LAMPS
C
      CALL ARGS_LAMPS(1,1,1,1)
      CALL APER_2(%VAL(IPNT),IDIM(1),IDIM(2))
C
C     CLOSE DOWN ARGS
C
      CALL ARGS_LAMPS(0,0,0,0)
      CALL DRCIRC(0,0,0,0)
      CALL ARGS_OVCL(8,.FALSE.)
      END
C
      SUBROUTINE APER_2(RAY,NX,NY)
      DIMENSION RAY(NX,NY)
      CHARACTER TITLE*80,BUFFER*80
      OPEN(UNIT=8,NAME='APERFOT.LIS',TYPE='NEW')
C
      CALL WRUSER('Title for logfile = ?',ISTATUS)
      CALL RDKEYC('TITLE',.FALSE.,1,TITLE,NVALS,ISTATUS)
      WRITE(8,2001)TITLE
2001  FORMAT(1X,A/)
      WRITE(8,1000)
      CALL WRUSER('APERFOT TRACKERBALL FUNCTIONS:',IS)
      CALL WRUSER('     BUTTON 1 (GREEN)  ACCEPT POSITION',IS)
      CALL WRUSER('       "    2 (WHITE)  CHANGE POSITION',IS)
      CALL WRUSER('       "    3 (WHITE)  CHANGE RADII',IS)
      CALL WRUSER('       "    4 (RED)    EXIT',IS)
C
C     GET POSITION AND RADII OF CIRCLES
C
      IC=1
      JC=1
      IR1=10
      IR2=20
C
C     DISPLAY TWO CONCENTRIC CIRCLES ON THE ARGS AND
C      LET THE USER POSITION THEM AND VARY THEIR RADII
C
10    CALL ARGSANUL(IC,JC,IR1,IR2,IBUTTON)
      IF (IBUTTON .EQ. 4)GO TO 999
      IRA=MIN(IR1,IR2)
      IRB=MAX(IR1,IR2)
      RA2=IRA*IRA
      RB2=IRB*IRB
C
C     FIND MINIMUM SIZED RECTANGLE ENCLOSING THE CIRCLES
C
      LX1=MAX(1,IC-IRB)
      LX2=MIN(NX,IC+IRB)
      LY1=MAX(1,JC-IRB)
      LY2=MIN(NY,JC+IRB)
C
C     FIND MEAN INTENSITY WITHIN INNER CIRCLE AND ANNULI
C
      SUM1=0.
      NS1=0
      SUM2=0.
      SUM22=0.
      NS2=0
      DO J=LY1,LY2
         DO I=LX1,LX2
            R2=(I-IC)**2+(J-JC)**2
            IF (R2 .LE. RA2)THEN
                SUM1=SUM1+RAY(I,J)
                NS1=NS1+1
            ELSE IF (R2 .LE. RB2)THEN
                SUM2=SUM2+RAY(I,J)
                SUM22=SUM22+RAY(I,J)*RAY(I,J)
                NS2=NS2+1
            END IF
         END DO
      END DO
      IF (NS1 .GT. 0)THEN
        XINNER=SUM1/NS1
      ELSE
        XINNER=0.
      END IF
      IF (NS2 .GT. 0)THEN
        SKYMEAN=SUM2/NS2
      ELSE
        SKYMEAN=0.
      END IF
      IF (NS2 .GT. 1)THEN
       TWOSIG=2.*SQRT(ABS((SUM22-SUM2*SUM2/NS2)/(NS2-1)))
      ELSE
       TWOSIG=0.
      END IF
C
C     FIND MEAN SKY AFTER 2 SIGMA REJECTION:
C
      NS2=0
      SUM2=0.
      SUM22=0.
      DO J=LY1,LY2
         DO I=LX1,LX2
            R2=(I-IC)**2+(J-JC)**2
            IF (R2 .GT. RA2 .AND. R2 .LE. RB2)THEN
                IF (ABS(RAY(I,J)-SKYMEAN) .LT. TWOSIG)THEN
                   SUM2=SUM2+RAY(I,J)
                   SUM22=SUM22+RAY(I,J)*RAY(I,J)
                   NS2=NS2+1
                END IF
            END IF
         END DO
      END DO
C
      IF (NS2 .GT. 0)THEN
        SKYMEAN=SUM2/NS2
      ELSE
        SKYMEAN=0.
      END IF
      IF (NS2 .GT. 1)THEN
C
C      mean error of the MEAN:
C
        SIGN=SQRT(ABS((SUM22-SUM2*SUM2/NS2)/(NS2-1))/NS2)
      ELSE
        SIG=0.
      END IF
C
C     FIND TOTAL COUNTS, MAG. ETC. AND PRINT OUT RESULTS
C
      AREA=3.14159*IRA*IRA
      STARSKY=XINNER*AREA
      SKY=SKYMEAN*AREA
      STAR=STARSKY-SKY
      IF (STAR .GT. 0.)THEN
        XMAG=-2.5*LOG10(STAR)+30.
C
C     CALCULATE ERROR ASSUMING SHOT NOISE STATISTICS
C
        ERRSTAR=SQRT(STARSKY)
        ERRSKY=SIGN*AREA
        ERRTOT=SQRT(ERRSTAR*ERRSTAR+ERRSKY*ERRSKY)
        ERR=2.5*LOG10((STAR+ERRTOT)/STAR)
      ELSE
        XMAG=0.
      END IF
      WRITE(BUFFER,1002)
      CALL WRUSER(BUFFER,ISTATUS)
      WRITE(BUFFER,1001)IC,JC,IRA,IRB,STARSKY,SKY,SKYMEAN,STAR,XMAG,ERR
      CALL WRUSER(BUFFER,ISTATUS)
      WRITE(8,1003)IC,JC,IRA,IRB,STARSKY,SKY,SKYMEAN,SIGN,
     1 STAR,XMAG,ERR
1000  FORMAT('   COORD.     RADII      TOTAL       SKY       <SKY>',
     1'     M.E.      STAR    MAG     M.E.')
1002  FORMAT('   COORD.     RADII      TOTAL       SKY       <SKY>',
     1'     STAR    MAG     M.E.')
1001  FORMAT(1X,2I4,2X,2I4,1X,2F11.1,F10.4,F11.1,F8.3,F7.3)
1003  FORMAT(1X,2I4,2X,2I4,1X,2F11.1,F10.4,F10.5,F11.1,F8.3,F7.3)
C
      GO TO 10
999   CONTINUE
      END
      SUBROUTINE ARGSANUL(IXCEN,IYCEN,IR1,IR2,IBUTTON)
C
C     SUBROUTINE FOR DEFINING TWO CONCENTIC CIRCLES (AN ANNULI)
C         ON THE ARGS
C
C   BUTTONS SELECT THE FOLLOWING FUNCTION FOR THE TRACKERBALL:
C        1  EXIT
C        2  SELECT X,Y POSITION OF APEX
C        3  CHANGE RADIUS OF THE TWO CIRCLES
C        4  EXIT
C
      PARAMETER TWOPIE=6.28318,PIEFAC=81.
C
      IXLAST=0
      IYLAST=0
C
C     FIND SIZE AND POSITION OF IMAGE ON ARGS
C
      CALL ARGS_RDIM(IXPOS,IYPOS,ISX,ISY,I,I,ISTATUS)
C
C     SUBTRACT 1 SO THAT COORD OF FIRST PIXEL IS (1,1) NOT (0,0)
C
      IXOR=IXPOS-ISX/2-1
      IYOR=IYPOS-ISY/2-1
C
C     POSITION CENTER OF CIRCLE
C
      IXC=IXCEN+IXOR
      IYC=IYCEN+IYOR
C
C     SET POSITION OF CURSOR TO INPUT VALUE
C
100   CALL ARGS_CURP(0,IXC,IYC)
C
C     EXECUTE TRACKERBALL PROGRAM
C      THIS ROUTINE RETURNS IMMEDIATELY WITH THE CURRENT CURSOR POSITION
C      REGARDLESS OF WHETHER OR NOT ANY OF THE BUTTONS HAVE BEEN PRESSED
C
110   CALL ARGS_TBSTX(IXC,IYC,IB1,IB2,IB3,IB4)
C
      IF (IXC .NE. IXLAST .OR. IYC .NE. IYLAST)THEN
C
C        NEW POSITION, SO REDRAW CIRCLES
C
         CALL DRCIRC(IXC,IYC,IR1,IR2)
         IXLAST=IXC
         IYLAST=IYC
      ELSE IF (IB1 .EQ. 1)THEN
         GO TO 300
      ELSE IF (IB2 .EQ. 1)THEN
         GO TO 100
      ELSE IF (IB3 .EQ. 1)THEN
         GO TO 200
      ELSE IF (IB4 .EQ. 1)THEN
         GO TO 400
      END IF
      GO TO 110
C
C     CHANGE RADIUS OF CIRCLES
C
200   IXLAST=IR1
      IYLAST=IR2
      CALL ARGS_CURP(0,IXLAST,IYLAST)
210   CALL ARGS_TBSTX(IX,IY,IB1,IB2,IB3,IB4)
      IF (IX .NE. IXLAST .OR. IY .NE. IYLAST)THEN
         IR1=MAX(0,IX)
         IR2=MAX(0,IY)
         CALL DRCIRC(IXC,IYC,IR1,IR2)
         IXLAST=IX
         IYLAST=IY
      ELSE IF (IB1 .EQ. 1)THEN
         GO TO 300
      ELSE IF (IB2 .EQ. 1)THEN
         GO TO 100
      ELSE IF (IB3 .EQ. 1)THEN
         GO TO 200
      ELSE IF (IB4 .EQ. 1)THEN
         GO TO 400
      END IF
      GO TO 210
C
300   CONTINUE
      IBUTTON=1
      GO TO 410
C
400   CONTINUE
      IBUTTON=4
410   IXCEN=IXC-IXOR
      IYCEN=IYC-IYOR
      END
      SUBROUTINE DRCIRC(IX,IY,IR1,IR2)
      PARAMETER TWOPIE=6.28318
      PARAMETER NARC=50
      DIMENSION IX1(100),IY1(100),IX2(100),IY2(100)
C
C     ERASE PREVIOUS  CIRCLE ON ARGS
C
C       SET TO ERASE MODE:
C
      MODE=0
      CALL APOLYL(IX1,IY1,NP,MODE)
      CALL APOLYL(IX2,IY2,NP,MODE)
      ISUM=IX+IY+IR1+IR2
      IF (ISUM .EQ. 0)RETURN
C
C       SET TO DRAW MODE
C
      MODE=1
C
      DELANG=TWOPIE/NARC
C
C     CALCULATE LOCUS OF POINTS DEFINING THE CIRCLES
C
      NP1=0
      NP2=0
      DO I=1,NARC
         ANG=I*DELANG
         ANGCOS=COS(ANG)
         ANGSIN=SIN(ANG)
         ITEMP=IX+IR1*ANGCOS
         IX1(I)=MAX(0,ITEMP)
         ITEMP=IY+IR1*ANGSIN
         IY1(I)=MAX(0,ITEMP)
         ITEMP=IX+IR2*ANGCOS
         IX2(I)=MAX(0,ITEMP)
         ITEMP=IY+IR2*ANGSIN
         IY2(I)=MAX(0,ITEMP)
      END DO
      NP=NARC+1
      IX1(NP)=IX1(1)
      IY1(NP)=IY1(1)
      IX2(NP)=IX2(1)
      IY2(NP)=IY2(1)
C
C     DRAW CIRCLES ON ARGS
C
      CALL APOLYL(IX1,IY1,NP,MODE)
      CALL APOLYL(IX2,IY2,NP,MODE)
      END
      SUBROUTINE APOLYL(IXRAY,IYRAY,NP,MODE)
C
C     ROUTINE TO DRAW LINES ON THE ARGS CONNECTING
C      THE NP POINTS (IXRAY,IYRAY)
C
C     DRAWS IMAGE ON AN OVERLAY PLANE
C
C     IT IS UP TO THE CALLING PROGRAM TO INITIALIZE OVERLAYS,
C        SET COLOUR, ETC.
C
C     WRITTEN BY W PENCE, AAO, NOV 29, 1982
C
      INTEGER IXRAY(*),IYRAY(*)
C
      IF (MODE .EQ. 0)THEN
C       SET TO ERASE MODE:
        CALL ARGS_S1('ZDI1','0000'X)
      ELSE
C       SET TO LINE DRAWING MODE
        CALL ARGS_S1('ZDI1','0100'X)
      END IF
C
C     SET LINE TYPE TO DRAW SOLID LINES
C
      CALL ARGS_PUT1('0500'X)
C
      DO I=1,NP-1
        CALL ARGS_PUT1('C000'X+IXRAY(I))
        CALL ARGS_PUT1('A000'X+IYRAY(I))
        CALL ARGS_PUT1('C000'X+IXRAY(I+1))
        CALL ARGS_PUT1('E000'X+IYRAY(I+1))
      END DO
C
C     MAKE SURE ARGS IS RESET TO NORMAL DRAWING MODE
C
      CALL ARGS_PUT1('0500'X)
      CALL ARGS_S1('ZDI1','0100'X)
C
      CALL ARGS_FLUSH(-1)
      END
