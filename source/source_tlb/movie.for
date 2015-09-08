      PROGRAM KFH_MOVIE
C+
C
C      Program MOVIE
C
C    Displays a movie made up of 1-D plots of
C    successive rows of a 2-D BDF file.
C
C    The program will plot up to 500 pixels in the X-direction
C    (ie the rows, ie first index)
C    with a delay between each row which is controlled by
C    the trackerball.
C
C    It continues to loop around repeating the display until the
C    RED button (4) is pressed.
C
C    PARAMETERS are :-
C			INPUT		the 2-D image
C			PVLO		the lowest value plotted
C			PVHI		the highest value plotted
C			FIRST		first row element to be
C					plotted.
C
C    TRACKERBALL :-
C			Button 1 (GREEN)	go to fastest display
C			Button 2 (WHITE)	hold display instantly
C			Button 3 (WHITE)	continue with display
C			Button 4 (RED)		exit
C
C			Move trackerball UP (Y increasing) for SLOWER
C			and DOWN (Y decreasing) for FASTER
C			(Movement in X is ignored)
C
C    Written by K.F.Hartley at RGO on 28-11-83
C    for C.D.Pike
C
C-



C *****MUST BE LINKED WITH SPECIALLY MODIFIED ARGS LIBRARY
C *****AS IN U2:[KFH]SLINK.COM
C *****LIBRARY IS IN [DJK.ARGSLIB]ARGS.OLB
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER LUT(3,0:255)
      INTEGER STATUS
      INTEGER AX(2)
      CALL RDIMAG('INPUT',FMT_R,2,AX,I,IP,STATUS)
      IF (STATUS.EQ.0.AND.I.EQ.2) THEN
         CALL SRINIT('1,0',.TRUE.,ISTAT)
         IF (STATUS.EQ.0) THEN
            NSAMP=AX(1)*AX(2)
            CALL GETDYN('TEMP',FMT_SL,NSAMP,IPT,STATUS)
C
C         A white LUT is loaded so that there is no colour
C         or intensity change as planes are swapped around.
C
            DO J=1,255
               DO I=1,3
                  LUT(I,J)=255
               END DO
            END DO
C
C         The background is set to black
C
            DO I=1,3
               LUT(I,1)=0
            END DO
C
C         and the plane used for axes to cyan (ie no red)
C
            LUT(1,2)=0
            CALL SRCOLS(0,256,LUT)
C
C         Set the ARGS timeout to 4 secs if there is a hangup
C
            CALL ARGS_TIMEOUT(4)
C
C         This is where most of the work is done
C
            CALL MOVIE(%VAL(IP),%VAL(IPT),AX(1),AX(2))
         ELSE
            CALL WRUSER('Failed to get the ARGS',ISTAT)
         END IF
      ELSE
         CALL WRUSER('Failed to get image',ISTAT)
      END IF
      END
      SUBROUTINE MOVIE(DATA,TEMP,N,M)
C
C   This subroutine does all the hard work
C
C   DATA contains the input (real) array
C   TEMP is a working (integer) array of the same size
C
      EXTERNAL IWAIT
      REAL DATA(N,M)
      INTEGER TEMP(N,M)
      CHARACTER XLAB*4,YLABB*8,YLABT*8
      CHARACTER FRLAB*4
C
C   First scale the data into TEMP, in the range 0 to 500
C   chosen so that the plot fits onto the 512x512 ARGS screen
C
      DMIN=DATA(1,1)
      DMAX=DMIN
      DO J=1,M
         DO I=1,N
            IF (DATA(I,J).GT.DMAX) DMAX=DATA(I,J)
            IF (DATA(I,J).LT.DMIN) DMIN=DATA(I,J)
         END DO
      END DO
C
C   Prompt for low and high scaling limits using min. and max.
C   as defaults.
C
      CALL RDKEYR('PVLO',.TRUE.,1,DMIN,I,ISTAT)
      CALL RDKEYR('PVHI',.TRUE.,1,DMAX,I,ISTAT)
C
C   and the first pixel to be plotted
C
      IFL=1
      CALL RDKEYI('FIRST',.TRUE.,1,IFL,I,ISTAT)
      FACTOR=500.0/(DMAX-DMIN)
      DO J=1,M
         DO I=1,N
            IF (DATA(I,J).LT.DMIN) THEN
               TEMP(I,J)=0
            ELSE IF (DATA(I,J).GT.DMAX) THEN
               TEMP(I,J)=500
            ELSE
               TEMP(I,J) = INT((DATA(I,J)-DMIN)*FACTOR)
            END IF
         END DO
      END DO

C
C   Sort out the x-scaling
C
      IROW=MIN(N,511)+IFL-1
      ISTEP=512/N
      CALL WRUSER('Starting to display',ISTAT)
C
C   Set up trackerball reading
C   (to allow spped-up/slow-down)
C
      CALL ARGS_CURP(0,256,256)
      CALL ARGS_TBSTL
      CALL ARGS_LAMPS(1,1,1,1)
      CALL ARGS_TBSTX(IX,IY,IB1,IB2,IB3,IB4)
C
C   Drawing axes - the hard way!
C
C   First a square box (in plane 2)
C
      CALL ARGS_PUT1('2D01'X)
      CALL ARGS_PUT1('0202'X)
      CALL ARGS_PUT1('A000'X)
      CALL ARGS_PUT1('C000'X)
      CALL ARGS_PUT1('C000'X+511)
      CALL ARGS_PUT1('E000'X)
      CALL ARGS_PUT1('E000'X+511)
      CALL ARGS_PUT1('C000'X)
      CALL ARGS_PUT1('E000'X+511)
      CALL ARGS_PUT1('E000'X)
      CALL ARGS_FLUSH(1)
C
C   Then some crude labels
C
C   First set them up as characters
C
       WRITE (XLAB,'(I4)') IROW
       WRITE (YLABT,'(F8.1)') DMAX
       WRITE (YLABB,'(F8.1)') DMIN
C
C    Then set up size, direction and auto-increment
C
       CALL ARGS_S1('SSB',13)
       CALL ARGS_S1('SSZ',0)
       CALL ARGS_S1('SDD',0)
C
C   Then do the bottom Y axis label
C
C   First set "pen" to start point
C
       CALL ARGS_S1('XMA',5)
       CALL ARGS_S1('YMA',5)
C
C      Find out how many leading spaces there are
C      and suppress them!
C
       IFIRST=1
       DO I=1,8
          IF (YLABB(I:I).EQ.' ') IFIRST=IFIRST+1
       END DO
C
C    Then write the characters, 1 at a time.
C    (Note use of ICHAR)
C
       DO I=IFIRST,8
         CALL ARGS_S1('JSI',ICHAR(YLABB(I:I)))
       END DO
C
C    Repeat for label at top of Y-axis
C
       CALL ARGS_S1('XMA',5)
       CALL ARGS_S1('YMA',490)
       IFIRST=1
       DO I=1,8
          IF (YLABT(I:I).EQ.' ') IFIRST=IFIRST+1
       END DO
       DO I=IFIRST,8
          CALL ARGS_S1('JSI',ICHAR(YLABT(I:I)))
       END DO
C
C   Finally for the left hand end of the X axis
C   (Note that this is an integer less than 512)
C
       CALL ARGS_S1('XMA',475)
       CALL ARGS_S1('YMA',5)
       DO I=1,4
          CALL ARGS_S1('JSI',ICHAR(XLAB(I:I)))
       END DO
       CALL ARGS_FLUSH(-1)
C
C   This is teh basic unit used for changing the delay between
C   successsive plots and hence the overall spped of the display
C
       BASICDELAY=1.0/256.0
       IXLAST=256
       IYLAST=256
C
C   Now loop through the rows
C
  100 CONTINUE
      DO J=1,M
C
C       This code is designed to speed-up/slow-down the display
C       using the trackerball
C
         CALL ARGS_TBSTX(ITX,ITY,IB1,IB2,IB3,IB4)
C
C      If button 4 (RED) is pressed then exit
C
         IF (IB4.NE.0) GO TO 200
C
C      If button 1 (GREEN) is pressed then set delay to zero
C
         IF (IB1.NE.0) THEN
            DELAY=0.0
         ELSE
C
C      otherwise use the trackerball reading and adjust the delay accordingly
C
            DELAY = DELAY + BASICDELAY*(REAL(ITY-256))
            IF (DELAY.LE.0.0) DELAY=0.0
         END IF
C
C      This is th eoutcome of some trials in the best way
C      of allowing interaction
C
         CALL ARGS_CURP(0,256,256)
C
C      This function does a wait of DELAY seconds
C      without wasting CPU time (see below)
C
         ITEMP = IWAIT(DELAY)
C
C      If the second button was pressed loop around waiting
C      for the third button to be pressed.
C
         IF (IB2.NE.0) THEN
             DO WHILE (IB3.EQ.0)
                CALL ARGS_TBSTX(ITX,ITY,IB1,IB2,IB3,IB4)
             END DO
         END IF
C
C      If ther first element of the current row is zero then
C      the colour is changed for that plot - this allows
C      special rows to be flagged
C      (eg set first element to -1 and choses PVLO=0)
C
         IF (TEMP(1,J).EQ.0) THEN
            CALL SRCOL1(8,O,255,0)
            CALL SRCOL1(128,0,255,0)
         ELSE
            CALL SRCOL1(8,255,255,255)
            CALL SRCOL1(128,255,255,255)
         END IF
C
C      This is an adaption to try to double buffer the output.
C      The concept is to make one row visible and then clear
C      the other row and then write the new data to it.
C      That is then made visible whilst the next row is being
C      written to another plane.
C
         IF (MOD(J,2).EQ.1) THEN
C
C         On ODD rows one set of planes is used
C
C
C         First make one set visible
C
            CALL ARGS_PUT1('2B01'X)
            CALL ARGS_PUT1('8282'X)
C
C         Then allow data to be written to the other set
C
            CALL ARGS_PUT1('2D01'X)
            CALL ARGS_PUT1('0808'X)
C
C         and then clear that set of planes.
C
            CALL ARGS_PUT1('3501'X)
            CALL ARGS_PUT1('0808'X)
         ELSE
C
C         The same process is repeated (with the planes reversed)
C         for the even rows.
C
            CALL ARGS_PUT1('2B01'X)
            CALL ARGS_PUT1('0A0A'X)
            CALL ARGS_PUT1('2D01'X)
            CALL ARGS_PUT1('8080'X)
            CALL ARGS_PUT1('3501'X)
            CALL ARGS_PUT1('8080'X)
         END IF
C
C      For each plot display the frame number at top right
C
         WRITE (FRLAB,'(I4)') J
         CALL ARGS_S1('XMA',475)
         CALL ARGS_S1('YMA',490)
         DO ITCH=1,4
           CALL ARGS_S1('JSI',ICHAR(FRLAB(ITCH:ITCH)))
         END DO
         CALL ARGS_FLUSH(-1)
C
C      We are now ready to do some real plotting!
C
C
C      This does a move to X=0, Y=first value
C
C      Here and below JY is the Y co-ordinate of the point
C      being plotted.
C
         JY=TEMP(1,J)
         CALL ARGS_PUT1('C000'X)
         CALL ARGS_PUT1('A000'X+JY)
C
C      Now loop through the points in this row
C
         DO IEL=IFL,IROW
C
C         IEL steps through each element of the row
C         IPOS is the x-position on the ARGS
C
            IPOS=(IEL-IFL)*ISTEP
            JY=TEMP(IEL,J)
            CALL LINE(IPOS,JY)
         END DO
C
C       This ensures that a whole row is written at once
C
         CALL ARGS_FLUSH(-1)
      END DO
C
C   This may be a redundant check to ensure that an exit occurs
C   if the RED button is pressed.
C
      IF (IB4.EQ.0) GO TO 100
  200 CONTINUE
C
C   Switch off the trackerball lamps
C
      CALL ARGS_LAMPS(0,0,0,0)
      END
      SUBROUTINE LINE(I,J)
C
C   Assuming that the ARGS 'pen' is positioned at the
C   previous point, this draws a line to the next point.
C
      CALL ARGS_PUT1('C000'X+I)
      CALL ARGS_PUT1('E000'X+J)
      END
      INTEGER FUNCTION IWAIT (DTSEC)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         FUNCTION IWAIT
C
C
C         It requests a  delay  of  a  specified  number  of  seconds  from
C         (nominally)  100*10E-9  up  to 5 minutes. It has the advantage of
C         not using up processor time looping around.
C
C         It returns a value of zero on successful completion.
C
C         DTSEC       Real  In    The number of seconds. Because it is real
C                                 fractions of a second can be requested.
C
C
C         C D Pike                 RGO                            10-AUG-82
C
C--------------------------------------------------------------------------




*
*
*  REQUEST DELAY
*
*
*  GIVEN:
*
*     DTSEC      DELAY (REAL, SECONDS)
*
*
*  RETURNED:
*
*     IWAIT      0 = SUCCESSFUL
*
*
*  THE MINIMUM DELAY IS (NOMINALLY) 100NS;
*  THE MAXIMUM IS 5 MINUTES.
*
*
*

      REAL DTSEC

      INTEGER LIB$GETEF,SYS$SETIMR,SYS$WAITFR,LIB$FREE_EF

      INTEGER IDT(2)
      DATA IDT(2)/-1/


*  PRESET STATUS
      JSTAT=-1

*  ENCODE DELTA TIME
      IDT(1)=-NINT(MAX(1.0,1E7*MIN(300.0,DTSEC)))


*  ASK SYSTEM TO ALLOCATE EVENT FLAG
      JSTAT=LIB$GET_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000

*  ASK FOR EVENT FLAG TO BE SET AFTER DELAY
      JSTAT=SYS$SETIMR(%VAL(NEF),IDT,,)
      IF (.NOT.JSTAT) GO TO 9000

*  WAIT FOR EVENT FLAG
      JSTAT=SYS$WAITFR(%VAL(NEF))
      IF (.NOT.JSTAT) GO TO 9000

*  FREE THE EVENT FLAG
      JSTAT=LIB$FREE_EF(NEF)
      IF (.NOT.JSTAT) GO TO 9000

*  RETURN STATUS
      JSTAT=0

 9000 CONTINUE
      IWAIT=JSTAT

      END
