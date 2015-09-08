C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   RDTOXY *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               RDTOXY
C
C
C          FUNCTION:-
C               It converts RA, DEC positions in an array to a  list  of  X
C               and Y's and displays the position on the ARGS screen using a
C               cross. The results are stored in the file RDTOXY.LIS.
C               If the ARGS are unavailable then just the output
C		to the terminal and file are obtained.
C
C
C          USE:-
C               It may be used to show the position of an  object  relative
C               to  the  stars  near  to  it, or to produce a list of X & Y
C               positions. The positions are stored in the file  RDTOXY.LIS
C               to  allow the user to obtain a hard copy of the output from
C               the printer. The parameter TRCOEFFS is used to  define  the
C               transformation   from   RA,  DEC  to  X,  Y.  Normally  the
C               coefficients created by CINVERT will be needed  -  this  is
C               set  up  by  the  command  procedure CCINRDX. Otherwise the
C               coefficients may be defined  on  the  command  line  or  in
C               response to a prompt in the usual way.
C
C
C
C         USER PARAMETERS:-
C
C         TRCOEFFS                            This is a  set  of  six  real
C                                             numbers  which  describes the
C                                             transformation       (Usually
C                                             obtained from CINVERT q.v.)
C
C         INPUT                               This is the name of the image
C                                             which  is  to be analysed. It
C                                             must be the one displayed  on
C                                             the ARGS.
C
C         RA                                  This is the  right  ascension
C                                             of the object.
C
C         DEC                                 This is  the  declination  of
C                                             the object.
C
C
C
C
C	WRITTEN BY
C
C         B Harrison               RGO         Version 1    July 1982
C
C
C--------------------------------------------------------------------------



C
C	Based on the program GTSTAR by K. F. Hartley
C

      INTEGER*4 PIN,STATUS,AXIN(2),POUT,AXOUT(2)
      REAL POSN(4,100)
      CHARACTER*1 MODE
      CHARACTER*1 MO
      CHARACTER*80 TEXT
      REAL COFS(6)
      LOGICAL FLAG
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

C
C	Open the file RDTOXY.LIS
C

      OPEN(UNIT=1,FILE='RDTOXY.LIS',STATUS='NEW')

C
C	Ask for TRCOEFFS if they have not already been defined.
C

      CALL RDKEYR('TRCOEFFS',.FALSE.,6,COFS,I,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL) THEN
 	CALL WRERR('TRERR')
	GOTO 800
      ENDIF
      FLAG=.TRUE.
C
C      FIRST PICK UP THE IMAGE FRAME
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIN,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF

C
C	Write the table headings into the file RDTOXY.LIS.
C

      WRITE(1,600)
      WRITE(1,601)
      WRITE(1,602)
      WRITE(1,603)
      WRITE(1,601)
      WRITE(1,604)
600   FORMAT(1H ,'      TITLE: RA,DEC TO X,Y')
601   FORMAT(1H ,' ')
602   FORMAT(1H ,4X,'RA',17X,'DEC',18X,'X',9X,'Y')
603   FORMAT(1H ,4X,'--',17X,'---',18X,'-',9X,'-')
604   FORMAT(1H ,2X,'H  M  S',13X,'D  M  S')
C
C   NOW INITIALIZE ARGS AND FIND OUT IF ANY IMAGES ARE KNOWN
C
      CALL SRINIT(0,.FALSE.,IASTAT)
      IF (STATUS.NE.0) THEN
        CALL WRERR('HELLARGS')
      END IF
      CALL ARGS_NUMIM(NPIC)
      CALL ARGS_OVOP(8,'G')
1     CONTINUE

C
C	Call subroutine RDXYKEY which take in the RA and DEC from
C	the keyboard.
C

      CALL RDXYKEY(RAS,DECS,IOERR)

C
C	If a null entry was made then stop.
C

      IF (IOERR.EQ.-1) GO TO 99

C
C	Apply the transformation RA, DEC to X, Y.
C

      X=COFS(1)+COFS(2)*RAS+COFS(3)*DECS
      Y=COFS(4)+COFS(5)*RAS+COFS(6)*DECS

C
C	Convert RA & DEC from radians to degrees using DECODE.
C

      CALL DECODE(RAS,DECS,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS,FLAG)

C
C  	Print the value of X and Y on the terminal.
C

      WRITE(TEXT,400)X,Y
      CALL WRUSER(TEXT,ISTAT)
400   FORMAT(1H ,'X=',F6.2,10X,'Y=',F6.2)

C
C	Print RA, DEC, X and Y into the file RDTOXY.LIS.
C

      IF (FLAG.EQ.0.0) THEN
        WRITE(1,401)IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS,X,Y
401  	FORMAT(1H ,1X,I2,1X,I2,1X,F5.2,9X,I2,1X,I2,1X,F5.2,9X,
     :	F6.2,4X,F6.2)
      END IF
      IF (FLAG.EQ.1.0) THEN
        WRITE(1,402)IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS,X,Y
402     FORMAT(1H ,2X,I2,1X,I2,1X,F5.2,8X,'-',I2,1X,I2,1X,F5.2,9X,
     :  F6.2,4X,F6.2)
      END IF

C
C 	Display a cross on the ARGS corresponding to X and Y.
C

      IF ((IASTAT.EQ.0).AND.(NPIC.GE.1)) THEN
      CALL ARGS_CROSS(NPIC,X,Y,5)
      END IF
      IOERR=0
      GOTO 1
800   CONTINUE
99    CLOSE(UNIT=1)
      CALL ARGS_OVCL(8,.FALSE.)
      END

      SUBROUTINE ARGS_CROSS(ID,XF,YF,IS)
      REAL VX(2),VY(2),HX(2),HY(2)
      D=REAL(IS)
C
C      Because the ARGS have their origin at (0,0) instead of (1,1)
C      we need to subtract 1 from the X and Y coordinates so the
C      cross is positioned correctly on the ARG's screen.
C
      VX(1)=XF-D-1.0
      VX(2)=XF+D-1.0
      VY(1)=YF-1.0
      VY(2)=YF-1.0
      HX(1)=XF-1.0
      HX(2)=XF-1.0
      HY(1)=YF-D-1.0
      HY(2)=YF+D-1.0
      CALL ARGS_POLYL(ID,2,HX,HY,ISTAT)
      CALL ARGS_POLYL(ID,2,VX,VY,ISTAT)
      END
