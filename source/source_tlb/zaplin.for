      PROGRAM ZAPLIN
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   ZAPLIN *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               ZAPLIN
C
C
C          FUNCTION:-
C               It allows the user to define a horizontal or vertical  band
C               in an image displayed on the ARGS, by using the cursor. The
C               pixels within the band are replaced by values  interpolated
C               between those on either side of the band.
C
C
C          USE:-
C               It was developed to remove bad columns from CCD images, but
C               it  has also been used to reduce the imapact of tape errors
C               in PDS scans. A better way of removing localized defects is
C               to use PATCH.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This  is   the   input    2-D
C                                             Starlink  image,  which ought
C                                             to have been displayed on the
C                                             ARGS.
C
C         OUTPUT                              This  is  the  output   image
C                                             after   the   band  has  been
C                                             removed.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Pick two points; the difference  in  their  x  values
C                     defines a VERTICAL band.
C
C         WHITE 2     Same as 1
C
C         WHITE 3     Pick two points; the difference in their y  positions
C                     defines a HORIZONTAL band.
C
C         RED   4     Same as 3
C
C
C
C
C
C
C
C
C
C         K F Hartley              RGO                            14-JAN-82
C
C
C--------------------------------------------------------------------------



C      Written by K F Hartley at RGO on 30/11/81

C
C      Subroutines called
C         ASP_ZAPLIN	code attached.
C         RDIMAG	STARLINK
C         WRIMAG	STARLINK
C         WRERR		STARLINK
C         RDKEYC	STARLINK
C         RDKEYI	STARLINK
C         CNPAR		STARLINK
C         FRDATA	STARLINK
C         EXIT		FORTRAN
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),STATUS
C
C   Get the input and output images - must be 2-D.
C
      CALL RDIMAG('INPUT',FMT_R,2,AX,I,IPIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL.OR.I.NE.2) THEN
         CALL WRERR('HELLIN')
         GO TO 800
      END IF
      CALL WRIMAG('OUTPUT',FMT_R,AX,2,IPOUT,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('HELLOUT')
         GO TO 800
      END IF
C
C   Do the hard work.
C
      CALL ASP_ZAPLIN(%VAL(IPIN),AX(1),AX(2),%VAL(IPOUT))
C
C   Tidy up and go home.
C
  800 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE ASP_ZAPLIN(DIN,N,M,DOUT)
C
C      Subroutine ASP_ZAPLIN
C
C      Applies linear interpolation across a band in a 2-D array.
C
C      Given
C         DIN      RA      BDF input array (2-d)
C         N        I       The x-dimension of DIN
C         M        I       The y-dimension of DIN
C
C      Returned
C         DOUT      RA     BDF 2-D array of the same size as DIN.
C
C      Subroutines called
C         SRINIT	ARGSLIB
C         WRUSER	STARLINK
C         ARGS_CUROP	ARGS_CODE_TBC
C         ARGS_RDCUR	ARGS_CODE_TBC
C         ARGS_CURCL	ARGS_CODE_TBC
C
C      K F Hartley      RGO      23/11/81
C
      REAL DIN(0:(N-1),0:(M-1)),DOUT(0:(N-1),0:(M-1))
C
C   First initialize the ARGS and read the cursor twice.
C   It requires 2 positions : if buttons 1 or 2 are pressed
C   then the columns defined by the x-positions are replaced.
C   If buttons 3 or 4 are used then rows defined by the y-positions
C   are replaced. In either case linear interpolation is used.
C
      CALL SRINIT(0,.FALSE.,ISTAT1)
      IF (ISTAT1.NE.0) THEN
         CALL WRUSER('ARGS NOT AVAILABLE',ISTAT)
         GO TO 800
      END IF
      CALL ARGS_CUROP('1234','W')
      ID=0
      DO WHILE (ID.EQ.0)
         CALL ARGS_RDCUR(' ',ID,IB1,UX1,UY1)
      END DO
C
C   Now for the second point.
C   Change colour of cursor to stress this.
C
       CALL ARGS_CURC('G')
      ID=0
      DO WHILE (ID.EQ.0)
         CALL ARGS_RDCUR(' ',ID,IB2,UX2,UY2)
      END DO
      CALL ARGS_CURCL
C
C   Copy the whole of the input to the output
C
      DO J=0,M-1
         DO I=0,N-1
            DOUT(I,J)=DIN(I,J)
         END DO
      END DO
C
C   Do the rows or columns depending on the (second) button pressed.
C
      IF (IB2.EQ.1.OR.IB2.EQ.2) THEN
C
C      COLUMNS
C
         IL = INT ( MIN(UX1,UX2) )
         IH = INT ( MAX(UX1,UX2) )
         DO J=0,M-1
            DO I=IL,IH
               F = REAL(I-IL+1) / REAL(IH-IL+2)
               DOUT(I,J)=F*DIN(IH+1,J) + (1.0-F)*DIN(IL-1,J)
            END DO
         END DO

      ELSE

C
C      ROWS
C
         JL = INT (MIN(UY1,UY2))
         JH = INT (MAX(UY1,UY2))
         DO J=JL,JH
            F = REAL(J-JL+1) / REAL (JH-JL+2)
            DO I=0,N-1
               DOUT(I,J) = F*DIN(I,JH+1) + (1.0-F)*DIN(I,JL-1)
            END DO
         END DO

      END IF

C
C   Branch to here if there was an ARGS error.
C
  800 CONTINUE
      END
