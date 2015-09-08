C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ******************
C                     *                *
C                     * Program   STAR *
C                     *                *
C                     ******************
C
C
C
C          CALLING SEQUENCE:-
C               STAR
C
C
C          FUNCTION:-
C               It may be used to locate the position and  intensity  of  a
C               star  profile which occupies most of a 2-D image. It writes
C               its results - x and y position , total  intensity  above  a
C               fitted  background  and  size (based on the assumption of a
C               Gaussian profile)  - to the terminal.
C
C
C          USE:-
C               It is really more use as a subroutine  to  be  incorporated
C               into  other  programs concerned with the location of stars,
C               eg GTSTAR.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               This   is   the   input   2-D
C                                             Starlink  image  containing a
C                                             single star.
C
C
C         P T Wallace              RAL                            13-JAN-82
C
C
C--------------------------------------------------------------------------



      INTEGER IDIMN(2)
      EQUIVALENCE (IDIMN(1),NX), (IDIMN(2),NY)

      INCLUDE 'INTERIM(FMTPAR)'


*  VALIDATE IMAGE
      CALL RDIMAG('IMAGE',FMT_R,2,IDIMN,NDIMS,IPIM,JSTAT)
      IF (NDIMS.NE.2) GO TO 9010
      IF (NX.LT.3.OR.NY.LT.3) GO TO 9020

*  GET WORK AREA FOR X & Y PROFILES
      CALL GETDYN('WK',FMT_R,MAX(NX,NY),IPW,JSTAT)

*  EVALUATE THE IMAGE
      CALL STAR(%VAL(IPIM),NX,NY,%VAL(IPW))

*  EXIT
      GO TO 9999

*  ERRORS
 9010 CONTINUE
      CALL WRUSER('ONLY 2-D IMAGE PERMITTED!',JSTAT)
      GO TO 9999

 9020 CONTINUE
      CALL WRUSER('IMAGE TOO SMALL!',JSTAT)

*  WRAP UP
 9999 CONTINUE
      CALL FRDATA(' ',JSTAT)

      END
      SUBROUTINE STAR(PIC,NX,NY,W)
*
*  DETERMINE BRIGHTNESS, POSITION & SIZE OF STAR
*
*  GIVEN:
*     PIC         2-D ARRAY CONTAINING STAR IMAGE
*     NX,NY       DIMENSIONS OF ARRAY
*     W           WORK ARRAY
*

      INTEGER NX,NY
      REAL PIC(NX,NY),W(*)

      CHARACTER*80 LINE


*  SAMPLE BACKGROUND
      BG=PIC(1,1)

*  COLLAPSE INTO X VERSUS Z
      DO IX=1,NX
         W(IX)=0.0
      END DO
      DO IY=1,NY
         DO IX=1,NX
            W(IX)=W(IX)+(PIC(IX,IY)-BG)
         END DO
      END DO

*  PROCESS X PROFILE
      CALL PP(W,NX,XINT,X0,XW)

*  COLLAPSE INTO Y VERSUS Z
      DO IY=1,NY
         S=0.0
         DO IX=1,NX
            S=S+(PIC(IX,IY)-BG)
         END DO
         W(IY)=S
      END DO

*  PROCESS Y PROFILE
      CALL PP(W,NY,YINT,Y0,YW)

*  ESTIMATE INTEGRATED INTENSITY AND FWHM
      TINT=(XINT+YINT)/2.0
      FWHM=(XW+YW)/2.0

*  REPORT
      WRITE (LINE,'(''INTENSITY ='',G12.4)') TINT
      CALL WRUSER(LINE,JSTAT)
      WRITE (LINE,'(''POSITION  =  ('',F6.1,'','',F6.1,'')'')') X0,Y0
      CALL WRUSER(LINE,JSTAT)
      WRITE (LINE,'(''FWHM      ='',F7.2)') FWHM
      CALL WRUSER(LINE,JSTAT)

      END
      SUBROUTINE PP(PRFL,N,TINT,C,W)
*
*  EVALUATE 1-D STAR PROFILE
*
*  GIVEN:
*     PRFL        1-D ARRAY CONTAINING STAR PROFILE
*     N           SIZE OF ARRAY
*
*  RETURNED:
*     TINT        TOTAL INTENSITY (ABOVE BACKGROUND)
*     C           CENTRE
*     W           FWHM
*

      INTEGER N
      REAL PRFL(N),TINT,C,W

      DOUBLE PRECISION SZ,SZP,SZP2


*  BASELINE PARAMETERS (STRAIGHT LINE THROUGH ENDPOINTS)
      Z0=PRFL(1)
      SLOPE=(PRFL(N)-Z0)/REAL(N)

*  PERFORM SUMMATIONS
      SZ=0D0
      SZP=0D0
      SZP2=0D0
      DO I=2,N-1
         P=REAL(I-1)
         Z=PRFL(I)-(Z0+SLOPE*P)
         SZ=SZ+DBLE(Z)
         ZP=Z*P
         SZP=SZP+DBLE(ZP)
         SZP2=SZP2+DBLE(ZP*P)
      END DO

*  FUDGE TOTAL INTENSITY IF VERY SMALL
      TINT=REAL(SZ)
      IF (ABS(TINT).LT.1E-10) SZ=1D0

*  DETERMINE CENTRE OF GRAVITY AND FWHM (ASSUMING GAUSSIAN)
      C=REAL(SZP/SZ)
      W=2.35482*SQRT(MAX(REAL((SZP2-SZP*SZP/SZ)/SZ),0.0))

      END
