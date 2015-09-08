      SUBROUTINE POLPRE (X,Y,MAXPTS,NPTS,MAXLNE,YLINE,LINEP,
     :                   XMINP,XMAXP,YVAL)
C+
C     POLPRE.
C
C     Subroutine to prepare the input arrays required by the
C     NAG least squares routine E02CAF from an observed set 
C     of X and Y positions.
C
C  Given;
C   X      (DA) X coords. of points to be fitted.
C   Y      (DA) Y   "   . "    "    "  "    "   .
C   MAXPTS (I)  Size of arrays X and Y.
C   NPTS   (I)  no. of data points.
C   MAXLNE (I)  Size of arrays LINEP, XMINP, XMAXP & YVAL below.
C
C  Returned;
C   YLINE  (I)  No. of distinct Y lanes in the image.
C   LANEP  (IA) No. of X values in each lane.
C   XMINP  (DA) Min. X value in each lane.
C   XMAXP  (DA) Max. X   "   "   "    "  .
C   YVAL   (DA) Y value for each lane.
C
C  Subroutines called;
C   None.
C
C  Structure:-
C   Y line counter = 0.
C   Set new line flag = .false.
C   Setup variables ready for analysing the first line.
C   Do for all pts.
C     If (not first point)
C       If significant change in Y value then
C         new line flag = .true.
C       else
C         new line flag = .false.
C       end if
C     end if
C     If (new line) then
C       increment the Y line counter
C        If not the first y line
C          Compute the end of position of the previous line.
C          Store; No. of points.
C                 Max X value for line.
C                 Min X value for line.
C                 Y value for the line.
C        end if
C        reset counter counting the number of pts. in this line.
C        Reset the max. and min. for this line.
C        record the y value for this line.
C      end if
C      increment the no. of pts. in this line by  +1.
C      check whether the X value for this point exceeds either
C        the current max or min for this line and if so reset them.
C   end do
C   store the values for the last line.
C
C  A C Davenhall./ROE/                                11/8/82.
C-
      INTEGER MAXPTS,NPTS,MAXLNE,YLINE
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS)
      DOUBLE PRECISION XMINP(MAXLNE),XMAXP(MAXLNE),YVAL(MAXLNE)
      INTEGER LINEP(MAXLNE)
C
      LOGICAL NEWLNE
      DOUBLE PRECISION CMAX,CMIN,VALUE
      INTEGER PTS,PCOUNT
C
C
C    Force the number of points to be sensible.
C
      PTS=MIN(NPTS,MAXPTS)
C
      YLINE=1
      NEWLNE=.FALSE.
      PCOUNT=0
      CMIN=X(1)
      CMAX=X(1)
      VALUE=Y(1)
C
      DO I=1,PTS
        IF (I.NE.1) THEN
C
C    Test for a significant change in Y; a new lane.
C
          IF (ABS(Y(I)).GT.1.0D-5.OR.ABS(Y(I-1)).GT.1.0D-5) THEN
C
C    One or more of the Y values are non-zero; the
C    test can be made on the ratio of the change to the
C    average value.
C      
            IF ((ABS(Y(I)-Y(I-1)))/ABS(Y(I)+Y(I-1)).GT.1.0D-5) THEN
              NEWLNE=.TRUE.
            ELSE
              NEWLNE=.FALSE.
            END IF
          ELSE
C
C    Both the Y values are essentially zero; the test is
C    redundant because both values are the same; ie. no
C    new line.
C
            NEWLNE=.FALSE.
          END IF
        END IF
C
C    Save the parameters found for the previous Y line if a 
C    new line has been found.
C
        IF (NEWLNE) THEN
          LINEP(YLINE)=PCOUNT
          XMINP(YLINE)=CMIN
          XMAXP(YLINE)=CMAX
          YVAL(YLINE)=VALUE
          PCOUNT=0
          CMIN=X(I)
          CMAX=X(I)      
          VALUE=Y(I)
          YLINE=YLINE+1
        END IF
        PCOUNT=PCOUNT+1
        CMIN=MIN(CMIN,X(I))
        CMAX=MAX(CMAX,X(I))
      END DO
C
C    Take care of the final set of values for the last line.
C
      LINEP(YLINE)=PCOUNT
      XMINP(YLINE)=CMIN
      XMAXP(YLINE)=CMAX
      YVAL(YLINE)=VALUE
C
      END
