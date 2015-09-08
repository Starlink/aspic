C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program  APERMAG *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               APERMAG   [CENAP=FALSE]
C
C
C          FUNCTION:-
C               It does aperture photometry on stars in a 2-D image. An
C               aperture of specified size is centered on a star at a
C               specified position and the flux in that aperture is
C               measured and the 'sky' flux in an annulus round it is
C               measured and the star magnitude is calculated as
C
C                          30 - 2.5*LOG  (star-sky.a1/a2)
C                                      10
C
C                                            where a1 = star aperture area
C                                                  a2 = sky aperture area
C
C                If the star is negative, the magnitude is set to 50,
C                and if it is too near the edge, it is set to 0.
C                   The results are typed on the terminal and sent to
C                an EDRS XYlist format file.
C
C
C          USE:-
C               The star positions are fed in by means of an EDRS XYlist
C               and the program goes to the positions and (optionally)
C               finds the centre of the star there,shifting the
C               aperture position to that centre, and then measures the
C               total values in the specified diameter aperture and in
C               an annulus of sqrt(2) times outer diameter.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               A 2-D image in the I*2 EDRS
C                                             format.
C
C         XYLIST                              This  is  a  list  of   (x,y)
C                                             co-ordinates in the EDRS
C                                             XYlist format.
C
C         OUTPUT                              This is the name of the EDRS
C                                             XYlist format file in which
C                                             the results are to be stored
C
C         TITLE      Output from APERMAG       The title to be written to the
C                                             output file.
C
C         APDIA          20.0                 The diameter of the aperture
C                                             in pixels.
C
C
C
C    NORMALLY DEFAULTED PARAMETERS
C
C         CENAP          TRUE                 If TRUE, centre on each star
C                                             before doing photometry. If
C                                             FALSE do photometry on the
C                                             unchanged input XY positions.
C
C
C         A J Penny       RGO                                  28-JUL-82
C
C
C--------------------------------------------------------------------------
C
C
C
C   CALLS
C     Starlink,Aspic,Edrs
C       GTXYLR,WRUSER,GTXYLW,CYDSCR,PTDSCR,RDKEYC,RDKEYR,RDKEYL
C       CNPAR,FRDATA
C
C     Grasp
C       GTIMAG,EXTLSA,AGAUSS
C
C     This file
C       GETXY,VOLUME,STORES
C
C   USES
C     %VAL facility
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    82-NOV
C
C --------------------------------------------------------


      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER NSIZE(2)
      INTEGER ISIDE(2)
      REAL RADIUS(2),APDIA(1)
      CHARACTER*72 TXT,TEXT
      LOGICAL*1 VALID,CENAP
      CHARACTER CVAL*1,TITLE(1)*30
C
C  Set continuation flag true
C
      VALID = .TRUE.
C
C
C   First get the data array
C
      CALL GTIMAG(IPIN,NSIZE,BSCALE,BZERO,INVAL,IERR)
      IF (IERR.NE.0) VALID = .FALSE.
C
C  Now seek a list of XY positions
C
      IF (VALID) THEN
         CALL GTXYLR('XYLIST',.FALSE.,LITEM,LSTLEN,IPXY,JSTAT)
         WRITE (TEXT,900) LSTLEN
  900    FORMAT ('NUMBER OF STARS IN THE LIST IS',I4)
         CALL WRUSER(TEXT,ISTAT)
         IF (JSTAT.NE.ERR_NORMAL.OR.LSTLEN.EQ.0) THEN
            VALID=.FALSE.
            CALL WRUSER('INVALID XYLIST',JSTAT)
         END IF
      ENDIF
C
C  Get the output list
C
      IF (VALID) THEN
         NITOUT = 16
         CALL GTXYLW('OUTPUT',.FALSE.,NITOUT,LSTLEN,IPOUT,JSTAT)
         IF (JSTAT.EQ.ERR_NORMAL) THEN
C
C  Copy the descriptor from the input XYlist.
C
            CALL CYDSCR('XYLIST','OUTPUT',ISTAT)
C
C  Copy the identifiers from the input XYlist
C
            CALL EXTLSA(%VAL(IPXY),LITEM,LSTLEN,1,LSTLEN,1,5,
     +                  %VAL(IPOUT),NITOUT,LSTLEN,1,1)
C   Store information to show how many values are stored for each star,
C   and how many star records are present.
C
            CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITOUT,RVAL,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +                  IERR)
C
C   Now pick up a new title
C
            TITLE(1) = 'Output from APERMAG'
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,IERR)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,
     +                  TITLE(1),IERR)
         ELSE
            VALID = .FALSE.
            CALL WRUSER('INVALID OUTPUT FILE',JSTAT)
         ENDIF
      ENDIF
C
C   NOW GET APERTURE DIAMETER
C
      IF (VALID) THEN
   51    CONTINUE
         APDIA(1) = 20.0
         CALL RDKEYR('APDIA',.TRUE.,1,APDIA(1),I,JSTAT)
         IF (JSTAT.NE.ERR_NORMAL.AND.JSTAT.NE.ERR_PARNUL)  THEN
            CALL WRUSER('BAD APERTURE, TRY AGAIN',JSTAT)
            CALL CNPAR('APDIA',JSTAT)
            GO TO 51
         ENDIF
         IF (APDIA(1).LT.0.01) THEN
            CALL WRUSER('APERTURE TOO SMALL, TRY AGAIN',JSTAT)
            CALL CNPAR('APDIA',JSTAT)
            GO TO 51
         ENDIF
C
C  Get the flag for centering on star before using aperture
C
         CALL RDKEYL('CENAP',.FALSE.,1,CENAP,NVAL,JSTAT)
C
C  Set size of Gaussian fitting box
C
         ISIDE(1) = APDIA(1)
         IF (ISIDE(1).GT.30) ISIDE(1) = 30
         ISIDE(2) = ISIDE(1)
C
C  Type header of terminal output
C
         WRITE(TEXT,910)
  910    FORMAT('   STAR     X        Y       MAG   RMS    ',
     +          'SKY      FLUX    DX   DY   INV')
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C
C ----------------------------------------------------------
C  Loop through the star list
C
      IF (VALID) THEN
         DO KSTAR = 1,LSTLEN
C
C      Get a pair of (X,Y) co-ordinates from the list
C
            CALL GETXY(%VAL(IPXY),LITEM,LSTLEN,KSTAR,X,Y)
C
C      Define aperture centre either by centering or straight list XY
C
            IF (CENAP) THEN
               CALL AGAUSS (%VAL(IPIN),NSIZE(1),NSIZE(2),X,Y,
     +                      ISIDE(1),ISIDE(2),
     +                      0,RADIUS(1),RADIUS(2),INVAL,20,
     +                      AMAG,HEIGHT,BASE,DXO,DYO,ANX,ANY,
     +                      RX,RY,RMS,ITER,NINVAL)
C
C      If found star centre, make new position, and note shift
C
               IF (AMAG.LT.49.0) THEN
                  AX = ANX
                  AY = ANY
                  DX = DXO
                  DY = DYO
               ELSE
                  AX = X
                  AY = Y
                  DX = 0.0
                  DY = 0.0
               ENDIF
            ELSE
               AX = X
               AY = Y
               DX = 0.0
               DY = 0.0
            ENDIF
C
C      Do the aperture photometry
C
             CALL VOLUME(%VAL(IPIN),NSIZE(1),NSIZE(2),INVAL,
     +                   BSCALE,BZERO,AX,AY,APDIA(1),AMAG,STAR,
     +                   KPIX,RMS,SKY,NINVAL)
C
C      Type the results on the terminal
C
            BMAG = AMAG
            IF (ABS(BMAG).GT.99.0) BMAG = SIGN(99.0,BMAG)
            BRMS = RMS
            IF (ABS(BRMS).GT.9.0) BRMS = SIGN(9.0,BRMS)
            BSKY = SKY
            IF (ABS(BSKY).GT.99999.0) BSKY = SIGN(99999.0,BSKY)
            BSTAR = STAR
            IF(ABS(BSTAR).GT.9.9E6) BSTAR = SIGN(9.9E6,BSTAR)
            WRITE (TEXT,920) KSTAR,AX,AY,BMAG,BRMS,BSKY,BSTAR,
     +                       DX,DY,NINVAL
  920       FORMAT (1H ,I5,2F9.2,F9.2,F6.2,F8.1,F10.1,2F5.1,I5)
            CALL WRUSER(TEXT,ISTAT)
C
C      Store the result
C
            CALL STORES(AX,AY,AMAG,STAR,DX,DY,NINVAL,KPIX,KSTAR,
     +                  RMS,SKY,%VAL(IPOUT),NITOUT,LSTLEN)
C
C
C
         ENDDO
      ENDIF
C
C -------------------------------------------------------
C
C   Finally tidy up and go home
C
      CALL FRDATA(' ',JSTAT)
      CALL EXIT
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R GETXY *
C      *           *
C      *************
C
C
C
C   This subroutine extracts the (x,y) co-ordinates from a
C   array obtained by using the EDRS routine GTXYLR
C
C   Parameters
C
C   (IN)
C      DATA	The array containing the parameters.
C      N   	The number of parameters in each record.
C      M	The number of stars in the list.
C      NPOS	This is the "star number", defining the record
C		which has to be searched.
C
C   (OUT)
C      X,Y	These are the required positions
C       	in X and Y respectively.
C
C   Written by K F Hartley at RGO on 3/3/82
C --------------------------------------------------------------
C
C
      SUBROUTINE GETXY(DATA,N,M,NPOS,X,Y)
C
C
C
      REAL DATA(N,M)
      X = DATA(6,NPOS)
      Y = DATA(7,NPOS)
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        **************
C        *            *
C        * S/R VOLUME *
C        *            *
C        **************
C
C
C
C
C   PURPOSE
C     This calculates the 'volume' in a circular area of the image
C     above a background, sky, level defined by the mean level in
C     an annulus around the circle (of the same area as the circle).
C
C   ARGUMENTS
C     IN
C       KP      Image
C       NPIX    Image X size
C       NLINES  Image Y size
C       INVAL   Pixel value flag for invalid
C       BSCALE,BZERO True pixel value = BS*value + BS
C       ANX,ANY Centre of circle
C       APDIA   Diameter of circle
C     OUT
C       AMAG    Star magnitude
C       STAR    Star flux
C       NSA     No of pixels inside circle
C       RMS     (Sqrt(star flux + sky flux))/star flux
C       SKY     Mean sky level
C       NINVAL  No of invalid points in circle
C
C   USES
C     I*2 arrays
C
C
C   A.J.PENNY                   RGO                    82-NOV
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE VOLUME(KP,NPIX,NLINES,INVAL,BSCALE,BZERO,ANX,ANY,
     +                  APDIA,AMAG,STAR,NSA,RMS,SKY,NINVAL)
C
C
C
      INTEGER*2 KP(NPIX,NLINES)
      DOUBLE PRECISION SA,SB,SC,ST
C
C  Set aperture and annulus size and positional coverage
C
      APRAD = APDIA/2.0
      SAPRAD = APRAD*SQRT(2.0)
      KBX = ANX - SAPRAD
      KEX = ANX + SAPRAD
      KBY = ANY - SAPRAD
      KEY = ANY + SAPRAD
C
C  If the total aperture + annulus is in the array, find the fluxes in
C  the aperture and the annulus
C
      IF(KBX.GE.1.AND.KEX.LE.NPIX.AND.KBY.GE.1.AND.KEY.LE.NLINES)THEN
         NSA = 0
         NSB = 0
         NINVAL = 0
         SA = 0.0
         SB = 0.0
         DO K = KBY,KEY
            Y = REAL(K)-ANY
            DO J = KBX,KEX
               X = REAL(J)-ANX
               D = SQRT(X*X+Y*Y)
               IF (D.LE.SAPRAD) THEN
                  IF (KP(J,K).EQ.INVAL) THEN
                     NINVAL = NINVAL + 1
                  ELSE
                     IF(D.LE.APRAD) THEN
                        SA = SA + DBLE(REAL(KP(J,K))*BSCALE+BZERO)
                        NSA = NSA + 1
                     ELSE
                        SB = SB + DBLE(REAL(KP(J,K))*BSCALE+BZERO)
                        NSB = NSB + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
C
C
         IF (NSB.GE.1) THEN
            SKY = SNGL(SB/DBLE(NSB))
            IF (NSA.GE.1) THEN
               SC = SB*DBLE(NSA)/DBLE(NSB)
               STAR = SNGL(SA-SC)
               IF (STAR.LT.1.0E-8) STAR = 1.0E-8
               AMAG = 30.0 - 2.5*ALOG10(STAR)
               ST = SA + SC
               IF (ST.LT.1.0E-8) ST = 1.0E-8
               RMS = SNGL(SQRT(ST))/STAR
               RMS = 2.5*ALOG10(1.0+RMS)
               IF (RMS.GT.999.99) RMS = 999.99
            ELSE
              STAR = 0.0
              AMAG = 50.0
              RMS = 0.0
            ENDIF
         ELSE
            SKY = 0.0
            STAR = 0.0
            AMAG = 50.0
            RMS = 0.0
         ENDIF
C
C  If aperture and annulus not entirely in array
C
      ELSE
         NSA = 0
         STAR = 0.0
         SKY = 0.0
         RMS = 0.0
         NINVAL = 0
         AMAG = 0.0
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STORES *
C      *            *
C      **************
C
C
C
C   PURPOSE
C      Store the results in the output array
C
C   ARGUMENTS
C    IN
C      X,Y     Position
C      AMAG,STAR  Star magnitude and flux
C      DX,DY   Posn - input position
C      NIN     No of invalid pixels
C      KPIX    No of pixels in circle round star
C      K       Star number
C      RMS     Star flux error
C      SKY     Sky level
C      NITEM,LSTLEN  Size of output array
C    OUT
C      DATA    Output array
C
C
C
C   A.J.PENNY                   RGO                    82-NOV
C ---------------------------------------------------------------
C
C
C
      SUBROUTINE STORES(X,Y,AMAG,STAR,DX,DY,NIN,KPIX,K,RMS,SKY,
     +                  DATA,NITEM,LSTLEN)
C
C
C
      REAL DATA(NITEM,LSTLEN)
C
C
C
      DATA(6,K) = X
      DATA(7,K) = Y
      DATA(8,K) = AMAG
      DATA(9,K) = DX
      DATA(10,K) = DY
      DATA(11,K) = 0.0
      DATA(12,K) = RMS
      DATA(13,K) = REAL(NIN)
      DATA(14,K) = STAR
      DATA(15,K) = SKY
      DATA(16,K) = REAL(KPIX)
C
C
C
      END



