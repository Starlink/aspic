	SUBROUTINE LUTREAD2(LUT,LOW,HIGH,LLOG)
C
C	ROUTINE TO SEND LOOKUP TABLE TO THE ARGS
C
C	ARGUMENTS
C		LUT	INTEGER ARRAY	LOOKUP TABLE
C		LOW	INTEGER		LOW THRESHHOLD VALUE
C		HIGH	INTEGER		HIGH THRESHHOLD VALUE
C		LLOG	LOGICAL		TRUE IFF LOG SCALE REQUIRED
C
C	WFL RGO FEB 1981
C
	INTEGER LUT(3,0:255),TABLE(3,0:255),LOW,HIGH,STATUS
	LOGICAL LLOG
C
C	GET ARGS
C
	CALL SRINIT(0,.FALSE.,STATUS)
	IF (STATUS.NE.0) THEN
		CALL WRUSER('NO ARGS',STATUS)
		GOTO 999
	ENDIF
C
C	SET UP TABLE (CHECK FOR THE CASE HIGH=LOW)
C
	IF (HIGH.EQ.LOW) THEN
		HIGH=HIGH+1
	ENDIF
	RATIO=255.0/(HIGH-LOW)
	DO I=0,255
		K=MAX(0.0,MIN(RATIO*(I-LOW),255.0))
		IF (LLOG) THEN
			K=45.986*LOG(REAL(K)+1.0)
		ENDIF
		DO J=1,3
			TABLE(J,I)=LUT(J,K)
		ENDDO
	ENDDO
C
C	SEND TO ARGS
C
	CALL SRCOLS(0,256,TABLE)
C
C	RETURN
C
999	RETURN
	END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C           ********************
C           *                  *
C           * Program  INSPECT *
C           *                  *
C           ********************
C
C
C
C          CALLING SEQUENCE:-
C              INSPECT
C
C
C          FUNCTION:-
C            This program can be used for inspecting areas of an image.
C            It is designed for Integer*2 images.
C
C            You choose an image and are then asked to define an
C            area by Cursor or Keyboard definition.
C            You can then choose an option from this list
C        Command            Function                    Short Command
C
C     1) HISTOGRAM  Display its histogram                       HI
C     2) MEAN       Calculate its mean and std dev              M
C     3) 3MEAN      Do 2) for points within 3sigma of the mean  3
C     4) VALUES     Type out a 5x5 grid of values in the area   V
C     5) SOLID      Display the area as a solid body plot       S
C     6) AREA       Choose a new area                           A
C     7) HELP       List the options                            HE
C     8) EXIT       Exit from the program                       E
C
C            After an option is completed the choice is repeated
C
C            The program can deal with the ICDISP displayed images
C            which can be compressed and/or windowed.
C
C
C
C
C
C          USER PARAMETERS:-
C
C
C          IMAGE                              The input image.
C
C          CURSOR        Yes                  Flag for defining area by
C                                             cursor or keyboard. Choice
C                                             are YES,NO.
C
C          XAREA         All                  X limits of area to be cho
C
C          YAREA         All                  Y limits of area to be cho
C
C          OPTION        Histogram            Flag for next course of
C                                             action. Choices are
C                                             as described above.
C
C          HIGH          Top                  Generally top of range to
C                                             be treated.
C
C          LOW           Bottom               Generally bottom of range
C                                             be treated
C
C          STEP          Appropriate          Size of grouping of points
C                                             in histogram. Default give
C                                             1000 across output device.
C
C          DEVICE        ARGS                 If the data are plotted ou
C                                             this is
C                                             the flag for which device
C                                             plot out on.
C                                             Choices are ARGS,TEKTRONIX
C                                             GOC,VERSATEC,CALCOMP,CC81.
C
C          DEVSIZE       Various                The size of the output p
C
C          DEVLIM        Min,Max              The Min and Max values to
C                                             plotted out.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     This causes the screen magnification to revert to
C                     and the cursor to go to the centre of the image.
C
C         WHITE 2     This causes the screen magnification to be divided
C                     by two. If this would cause it to be less than 1,
C                     is no effect.
C
C         WHITE 3     This causes the screen magnification to be multipl
C                     by two. If this would make it too large, there is
C                     effect.
C
C         RED   4     This is used to define a corner of the area, and t
C                     define the opposing corner. A rectangle is then
C                     painted round the area
C
C
C
C
C
C        AJ PENNY                  RGO         82-SEP-10
C
C
C-----------------------------------------------------------------
 
 
 
