C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYCHART *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYCHART
C
C
C          FUNCTION:-
C               This plots out a realistic looking star map, with filled
C               circles, bigger for brighter stars, at the positions of
C               an input list. The star identifications may be put by
C               the stars.
C               In a star identifier, a # as the 1st charcater is ignored,
C               and a star identifier consisting solely of # is not put
C               out.
C
C
C
C
C         USER PARAMETERS:-
C
C         INPUTS                              This is the name of the
C                                             input XY star file
C
C         INPUTL                              The input XYlist containing
C                                             the identifiers to be added
C                                             to the stars. Normally this
C                                             will be the same as INPUTS.
C                                             If you return blank, no
C                                             identifiers are put on.
C
C          DEVICE          ARGS               The device to write the chart
C                                             on.
C                                             Choice is ARGS,TEKTRONIX,GOC,
C                                             VERSATEC,CALCOMP,CALCOMP81,
C                                             PRINTRONIX,NONE.
C
C          PSIZE        Various               Physical size in metres of
C                                             chart
C
C          PENWIDTH                           If you are using a device
C                                             with an adjustable pen, you
C                                             input the width of the pen
C                                             you have installed in the
C                                             device (in mm).
C
C          XRANGE        Min,Max              The X limits of the chart.
C                                             Max must be bigger than Min
C
C          YRANGE        Min,Max              The Y limits of the chart.
C                                             Max must be bigger than Min
C
C          STMAG         Max + 1              The magnitude limit fainter
C                                             than which stars are not
C                                             plotted.
C
C          SCMAG         2.0                  The scale for the star plotted
C                                             diameters.
C                                             The diameters are (in XYlist
C                                             X,Y scale) -
C                                               Dia = (STMAG-MAG)*SCMAG
C
C          TSIZE         Chart Y size/75      The height of the plotted
C                                             identifiers (in XYlist scale)
C
C
C
C         A J Penny            RGO                                83-1-4
C
C
C--------------------------------------------------------------------------



*
*
*  CALLS
*      EDRS Package
*            GTXYLR,GTDSCR,GETCMD
*      STARLINK:
*            WRUSER,FRDATA,RDKEYR,RDKEYI,RDKEYC
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-11-4
* ------------------------------------------------------------



      PROGRAM XYCHART
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      REAL PSIZE(2),XR(2),YR(2)
      LOGICAL VALID
C
C  Set continuation

C
      VALID = .TRUE.
C
C  Obtain input XY position list
C
      CALL WRUSER('INPUT XY,MAG POSN LIST',ISTAT)
      CALL GTXYLR('INPUTS',.FALSE.,NITS,LSTS,IPINS,IERR)
      IF (IERR.NE.0) THEN
         VALID = .FALSE.
      ELSE
         IF (LSTS.EQ.0.OR.NITS.LT.8) THEN
            CALL WRUSER('ZERO FILE OR TOO FEW PARAMETERS',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get XY label file
C
      IF (VALID) THEN
         CALL WRUSER('INPUT LABEL LIST (BLANK=NONE)',ISTAT)
         CALL GTXYLR('INPUTL',.TRUE.,NITL,LSTL,IPINL,IERR)
         IF (IERR.EQ.0) THEN
            KLABEL = 1
            IF (LSTL.EQ.0.OR.NITL.LT.7) THEN
               CALL WRUSER('ZERO FILE OR TOO FEW PARAMETER',ISTAT)
               VALID = .FALSE.
            ENDIF
         ELSE
            KLABEL = 0
            IPINL = IPINS
            LSTL = LSTS
            NITL = NITS
            IF (IERR.GT.1) THEN
               VALID = .FALSE.
            ENDIF
         ENDIF
      ENDIF
C
C  Open chart
C
      IF (VALID) THEN
         CALL SGSOP(NUMWKS,PSIZE)
      ENDIF
C
C  Get chart parameters
C
      IF (VALID) THEN
         CALL GCPAR(%VAL(IPINS),NITS,LSTS,NUMWKS,KLABEL,XR,YR,
     +              TSIZE,PENW,STMAG,SCMAG)
         PENW = PENW*ABS(XR(2)-XR(1))/PSIZE(1)
      ENDIF
C
c  Get work space for modelling picture
C
      IF (VALID) THEN
         NX = (ABS(XR(2)-XR(1))*2.0)/TSIZE + 1.0
         NY = (ABS(YR(2)-YR(1))*2.0)/TSIZE + 1.0
         NXY = NX*NY
         CALL GETDYN('WORK',FMT_SB,NXY,IWK,ISTAT)
      ENDIF
C
C  Draw Chart
C
      IF (VALID) THEN
         CALL CHART(%VAL(IPINS),NITS,LSTS,%VAL(IPINL),NITL,LSTL,
     +              KLABEL,XR,YR,TSIZE,%VAL(IWK),NX,NY,PENW,
     +              STMAG,SCMAG)
      ENDIF
C
C  Close chart
C
      CALL SGSCL(NUMWKS)
C
C  Free data area
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GCPAR  *
C      *            *
C      **************
C
C
C    A J PENNY               RGO                      83-1-4
C ------------------------------------------------------------
C
      SUBROUTINE GCPAR(POSN,NS,MS,NUMWKS,KLABEL,XR,YR,TSIZE,
     +                 PENW,STMAG,SCMAG)
C
C
C
      REAL POSN(NS,MS),XR(2),YR(2)
C
C  Find out what width pen is
C
      IF (NUMWKS.EQ.1) THEN
         PENW = .0004
      ELSE IF (NUMWKS.EQ.5) THEN
         PENW = .0001
      ELSE IF (NUMWKS.EQ.3.OR.NUMWKS.EQ.8) THEN
         PENW = 0.0001
      ELSE
         CALL WRUSER('PHYSICAL WIDTH OF PEN CHOSEN (IN MM) ?',ISTAT)
         PENW = 0.5
         CALL RDKEYR('PENWIDTH',.TRUE.,1,PENW,NUM,ISTAT)
         PENW = PENW/1000.0
      ENDIF
C
C  Get plot size
C
    1 AMIN = POSN(6,1)
      AMAX = POSN(6,1)
      DO K = 2,MS
         IF (POSN(6,K).LT.AMIN) AMIN = POSN(6,K)
         IF (POSN(6,K).GT.AMAX) AMAX = POSN(6,K)
      ENDDO
      CALL WRUSER('X RANGE TO SHOW ?',ISTAT)
      XR(1) = AMIN - 0.05*(AMAX-AMIN)
      XR(2) = AMAX + 0.05*(AMAX-AMIN)
      CALL RDKEYR('XRANGE',.TRUE.,2,XR,NUM,ISTAT)
      IF (XR(2).LE.XR(1)) THEN
         CALL WRUSER('Max must be less than Min',ISTAT)
         CALL CNPAR('XRANGE',ISTAT)
         GO TO 1
      ENDIF
C
    2 AMIN = POSN(7,1)
      AMAX = POSN(7,1)
      DO K = 2,MS
         IF (POSN(7,K).LT.AMIN) AMIN = POSN(7,K)
         IF (POSN(7,K).GT.AMAX) AMAX = POSN(7,K)
      ENDDO
      CALL WRUSER('Y RANGE TO SHOW ?',ISTAT)
      YR(1) = AMIN - 0.05*(AMAX-AMIN)
      YR(2) = AMAX + 0.05*(AMAX-AMIN)
      CALL RDKEYR('YRANGE',.TRUE.,2,YR,NUM,ISTAT)
      IF (YR(2).LE.YR(1)) THEN
         CALL WRUSER('Max must be less than Min',ISTAT)
         CALL CNPAR('YRANGE',ISTAT)
         GO TO 2
      ENDIF
C
C  Get scale of star sizes
C
      CALL WRUSER('FAINTEST MAG = ?',ISTAT)
      STMAG = POSN(8,1)
      DO K = 1,MS
         IF(POSN(8,K).GT.STMAG) STMAG = POSN(8,K)
      ENDDO
      STMAG = STMAG + 1.0
      CALL RDKEYR('STMAG',.TRUE.,1,STMAG,NUM,ISTAT)
      CALL WRUSER('MAGNITUDE SCALE = ?',ISTAT)
      SCMAG = 2.0
      CALL RDKEYR('SCMAG',.TRUE.,1,SCMAG,NUM,ISTAT)
C
C  Get text size
C
      IF (KLABEL.EQ.1) THEN
         CALL WRUSER('TEXT SIZE = ? ',ISTAT)
         TSIZE = ABS(XR(1)-XR(2))/75.0
         CALL RDKEYR('TSIZE',.TRUE.,1,TSIZE,NUM,ISTAT)
      ELSE
         TSIZE = (ABS(XR(1)-XR(2)))/10.0
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CHART  *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
C
C
C
C   CALLS
C     Starlink
C     Aspic
C     Edrs
C     Grasp
C     This file
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Byte arrays
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-Z-Z
C
C -----------------------------------------------------------------
C
      SUBROUTINE CHART(POSN,NS,MS,ALABEL,NA,MA,KLABEL,XR,YR,TSIZE,
     +                 KDUM,KDX,KDY,PENW,STMAG,SCMAG)
C
C
C
      REAL POSN(NS,MS),ALABEL(NA,MA)
      CHARACTER*20 TEXT
      REAL XR(2),YR(2)
      BYTE KDUM(KDX,KDY)
      REAL XSH(8),YSH(8)
      DATA XSH/0.707,1,0.707,0.0,-.707,-1.0,-.707,0.0/
      DATA YSH/0.707,0.0,-0.707,-1.0,-0.707,0.0,0.707,1.0/
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      CALL SGS_SW(XR(1),XR(2),YR(1),YR(2),ISTAT)
      CALL SGS_SHTX(TSIZE)
      CALL SGS_STXJ('BL')
C
C  Draw box round area
C
      CALL SGS_BPOLY(XR(1),YR(1))
      CALL SGS_APOLY(XR(1),YR(2))
      CALL SGS_APOLY(XR(2),YR(2))
      CALL SGS_APOLY(XR(2),YR(1))
      CALL SGS_APOLY(XR(1),YR(1))
C
C  Plot stars
C
      DO K = 1,MS
         X = POSN(6,K)
         Y = POSN(7,K)
         AM = POSN(8,K)
         IF (AM.LT.STMAG) THEN
            CALL SGS_BPOLY(X,Y)
            RR = (STMAG-AM)*SCMAG/2.0
            RRR = RR*RR
            XEND = X + RR
            XA = X - RR
            DIR = 1.0
            DO WHILE (XA.LE.XEND)
               DX = XA - X
               YY = RRR - DX*DX
               IF (YY.LT.1.0E-20) YY= 1.0E-20
               YD = SQRT(YY)
               YA = Y + DIR*YD
               CALL SGS_APOLY(XA,YA)
               DIR = -1.0*DIR
               YA = Y + DIR*YD
               CALL SGS_APOLY(XA,YA)
               XA = XA + PENW
            ENDDO
         ENDIF
      ENDDO
C
C  Clear dummy array and then fill stars into it
C
      TTSIZE = TSIZE/2.0
      DO K = 1,KDY
         DO J = 1,KDX
            KDUM(J,K) = 0
         ENDDO
      ENDDO
      DO K = 1,MS
         X = POSN(6,K) - XR(1) + 1.0
         Y = POSN(7,K) - YR(1) + 1.0
         AM = POSN(8,K)
         IF (AM.LT.STMAG) THEN
            R = (STMAG-AM)*SCMAG/2.0 + TSIZE
            KXA = (X-R)/TTSIZE
            KXB = (X+R)/TTSIZE
            KYA = (Y-R)/TTSIZE
            KYB = (Y+R)/TTSIZE
            IF (KXA.LT.1)KXA = 1
            IF (KXA.GT.KDX) KXA = KDX
            IF (KXB.LT.1) KXB = 1
            IF (KXB.GT.KDX) KXB = KDX
            IF (KYA.LT.1) KYA = 1
            IF (KYA.GT.KDY) KYA = KDY
            IF (KYB.LT.1) KYB = 1
            IF (KYB.GT.KDY) KYB = KDY
            RR = R*R
            DO JY = KYA,KYB
               DO JX = KXA,KXB
                  DX = X - REAL(JX)*TTSIZE
                  DY = Y - REAL(JY)*TTSIZE
                  DD = DX*DX + DY*DY
                  IF (DD.LE.RR) KDUM(JX,JY) = 1
               ENDDO
            ENDDO
         ENDIF
      ENDDO
C
C  Add labels, if doing so
C
      IF (KLABEL.EQ.1) THEN
         DO K = 1,MA
C           Get star radius and label
            RADS = ((STMAG-POSN(8,K))*SCMAG/2.0)
            CALL XYID(ALABEL,NA,MA,K,TEXT)
            CALL CHARLN(TEXT,LEN)
C           Is label just '#',' ', or was this star too faint to plot?
            IF ((POSN(8,K).LT.STMAG).AND.
     +          (LEN.NE.0).AND.
     +          (.NOT.(LEN.EQ.1.AND.(TEXT(1:1).EQ.'#')))) THEN
               IF (TEXT(1:1).EQ.'#') TEXT(1:1)= ' '
               CALL LBGONE(TEXT)
               CALL CHARLN(TEXT,LEN)
               PLEN = REAL(LEN)*TSIZE*5.0/7.0
               CLEN = ANINT((REAL(LEN)*2.0*5.0/7.0) + 0.5)
C                 Set for 1st position
               KTURN = -1
               KDONE = 0
               DO WHILE (KDONE.EQ.0)
C                    Find next position in dummy array coords
                  KTURN = KTURN + 1
                  KANGLE = MOD(KTURN,8) + 1
                  KNUM = 1 + (KTURN/8)
                  RADL = RADS + TTSIZE + TSIZE*REAL(KNUM)
                  XD = POSN(6,K)-XR(1)+1.0+RADL*XSH(KANGLE)
                  YD = POSN(7,K)-YR(1)+1.0+RADL*YSH(KANGLE)
                  XD = ANINT(XD/TTSIZE)
                  YD = ANINT(YD/TTSIZE)
C                    See if tried all box
                  XA = POSN(6,K)
                  YA = POSN(7,K)
                  DDA = (XA-XR(1))**2.0 + (YA-YR(1))**2.0
                  DDB = (XA-XR(1))**2.0 + (YA-YR(2))**2.0
                  DDC = (XA-XR(2))**2.0 + (YA-YR(1))**2.0
                  DDD = (XA-XR(2))**2.0 + (YA-YR(2))**2.0
                  DD = MAX(DDA,DDB,DDC,DDD)
                  IF ((RADL*RADL).GT.DD) KDONE = 1
C                    See if this star actually is in box
                     XMAX = POSN(6,K) + RADS
                     XMIN = POSN(6,K) - RADS
                     YMAX = POSN(7,K) + RADS
                     YMIN = POSN(7,K) - RADS
                     IF (XMIN.GT.XR(2).OR.XMAX.LT.XR(1).OR.
     +                   YMIN.GT.YR(2).OR.YMAX.LT.YR(1)) KDONE = 1
C                    Then see if any of this position already occupied
                  JXA = XD
                  JXB = XD + CLEN + 1.0
                  JYA = YD
                  JYB = YD + 2.0
                  KOK = 1
                  DO JY = JYA,JYB
                     DO JX = JXA,JXB
                        IF (JX.GE.2.AND.JX.LE.(KDX-1).AND.
     +                      JY.GE.2.AND.JY.LE.(KDY-1)) THEN
                           IF (KDUM(JX,JY).EQ.1) KOK = 0
                        ELSE
                           KOK = 0
                        ENDIF
                     ENDDO
                  ENDDO
C                    If not and not stopped trying, put label
                  IF (KOK.EQ.1.AND.KDONE.NE.1) THEN
C                       Put label
                     XS = XD*TTSIZE + XR(1) - 1.0
                     YS = YD*TTSIZE + YR(1) - 1.0
                     CALL SGS_BTEXT(XS,YS)
                     CALL SGS_ATXL(TEXT)
C                       Draw line if not in 1st posn
                     IF (KTURN.NE.0) THEN
                     DF = XS - POSN(6,K)
                     DE = XS + PLEN - POSN(6,K)
                     IF (ABS(DF).GT.ABS(DE)) XS = XS + PLEN
                     IF (YS.LT.POSN(7,K)) YS = YS + TSIZE
                     XE = POSN(6,K)
                     YE = POSN(7,K)
                     ANG = ATAN2((YS-YE),(XS-XE))
                     R = (STMAG-POSN(8,K))*SCMAG/2.0 + TSIZE/3.0
                     XS = XS - COS(ANG)*TSIZE/3.0
                     YS = YS - SIN(ANG)*TSIZE/3.0
                     XE = XE + R*COS(ANG)
                     YE = YE + R*SIN(ANG)
                     CALL SGS_LINE(XS,YS,XE,YE)
                     ENDIF
C                       Fill in dummy array area
                     DO JY = JYA,JYB
                        DO JX = JXA,JXB
                           KDUM(JX,JY) = 1
                        ENDDO
                     ENDDO
C
                     KDONE = 1
                  ENDIF
C
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SGSCL  *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
C
C
C
C   CALLS
C     Starlink
C     Aspic
C     Edrs
C     Grasp
C     This file
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Byte arrays
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-Z-Z
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE SGSCL(NUMWKS)
C
C
C
      CALL SGS_CLOSE
      IF (NUMWKS.EQ.5)THEN
         CALL DO_DCL('PRINT/DELETE/QUEUE=GKS_RASTER/PARAM=(VERSATEC)
     +  FOR047.DAT',ISTAT) 
         IF (MOD(ISTAT,2).EQ.0) CALL GETMSG(ISTAT)
      ENDIF
      IF (NUMWKS.EQ.7) THEN
         CALL DO_DCL('PRINT/DELETE/QUEUE=GKS_RASTER/PARAM=(PRINTRONIX)
     +  FOR047.DAT',ISTAT)
         IF (MOD(ISTAT,2).EQ.0) CALL GETMSG(ISTAT)
      ENDIF
      IF (NUMWKS.EQ.10) THEN
         CALL DO_DCL('@LSTARDISK:[STARLOCAL.UTILITY.CALCOMP]CALPLOT
     + FOR047',ISTAT)
         IF (MOD(ISTAT,2).EQ.0) CALL GETMSG(ISTAT)
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYID   *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  OUT
C
C
C
C   CALLS
C     None
C
C   USES
C     Byte arrays
C
C
C   A.J.PENNY                   RGO                    83-3-6
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE XYID(KDATA,NC,NR,KR,TEXT)
C
C
C
      CHARACTER*20 TEXT
      BYTE KDATA(NC*4,NR)
C
C
C
      DO K = 1,20
         TEXT(K:K) = CHAR(KDATA(K,KR))
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R SGSOP  *
C      *            *
C      **************
C
C
C   PURPOSE
C
C   ARGUMENTS
C  IN
C  IN/OUT
C  OUT
C
C   STARLINK PARAMETERS
C
C
C
C   CALLS
C     Starlink
C     Aspic
C     Edrs
C     Grasp
C     This file
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Byte arrays
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-Z-Z
C
C -----------------------------------------------------------------
C
      SUBROUTINE SGSOP(NUMWKS,PSIZE)
C
C
C
      CHARACTER*20 WKSTN
      REAL PSIZE(2)
C
C  Get Device
C
      NUMWKS = 0
      DO WHILE (NUMWKS.EQ.0)
         IDEV = 1
         CALL GETCMD('DEVICE','ARGS,TEKTRONIX,GOC,VERSATEC,CALCOMP,X
     +                X,CALCOMP81,PRINTRONIX,NONE,HELP,?.',1,IDEV,ADEV,
     +                NDEV,ISTAT)
C           Translate into SGS Workstation number
         IF (IDEV.EQ.1) NUMWKS = 1
         IF (IDEV.EQ.2) NUMWKS = 2
         IF (IDEV.EQ.3) NUMWKS = 3
         IF (IDEV.EQ.4) NUMWKS = 5
         IF (IDEV.EQ.5) NUMWKS = 10
         IF (IDEV.EQ.6) NUMWKS = 0
         IF (IDEV.EQ.7) NUMWKS = 9
         IF (IDEV.EQ.8) NUMWKS = 7
         IF (IDEV.EQ.9) NUMWKS = -1
         IF (IDEV.EQ.10.OR.IDEV.EQ.11) THEN
            CALL WRUSER(' Choice is ARGS,TEKTRONIX,GOC,VERSATEC',ISTAT)
            CALL WRUSER('           CALCOMP,CALCOMP81,PRINTRONIX,NONE',
     +                  ISTAT)
         NUMWKS = 0
         ENDIF
      ENDDO
C
C
C
      IF (NUMWKS.EQ.5.OR.NUMWKS.EQ.7.OR.NUMWKS.EQ.10) THEN
         WRITE(WKSTN,901)NUMWKS
  901    FORMAT(I2,',047')
      ELSE
         WRITE(WKSTN,902)NUMWKS
  902    FORMAT(I2,',0')
      ENDIF
C
C  Get physical size
C
      CALL WRUSER('PHYSICAL SIZE IN METRES OF PLOT ?',ISTAT)
      PSIZE(1) = 0.2
      PSIZE(2) = 0.2
      CALL RDKEYR('PSIZE',.TRUE.,2,PSIZE,NUM,ISTAT)
C
C
C
      CALL SGS_OPEN(WKSTN,IZONID,ISTAT)
      CALL SGS_ZSIZE(PSIZE(1),PSIZE(2),'O',IZ,ISTAT)
C
C
C
      END



