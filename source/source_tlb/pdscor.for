C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      ******************
C      *                *
C      * Program PDSCOR *
C      *                *
C      ******************
C
C
C--------------------------------------------------------
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      CHARACTER TEXT*72,CVAL*1
      LOGICAL VALID
      CHARACTER TITLE*72,ITITLE*72
      INTEGER NIN(2)
C
C
      VALID = .TRUE.
C
C  Get Main image
C
      CALL GTIMG(IPIN,NIN,BS,BZ,INVAL,TITLE,IERR)
      IF (IERR.NE.0) VALID = .FALSE.
C
C  Get ITF table
C
      IF (VALID) THEN
         CALL GETITF(IPITF,NITF,BOTLIM,TOPLIM,DENVAL,ITITLE,IERR)
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C Get Clear levels
C
      IF (VALID) THEN
         CALL GETDYN('CLEAR',FMT_R,NIN(2),IPCL,ISTAT)
         IF (ISTAT.EQ.0) THEN
            CALL GETCLR(%VAL(IPIN),NIN(1),NIN(2),INVAL,%VAL(IPCL),
     +                  TITLE)
         ELSE
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get saturation density, if allowance to be made.
C
      IF (VALID) THEN
         KSAT = 0
         DO WHILE (KSAT.NE.1.AND.KSAT.NE.2)
            KSAT = 1
            CALL GETCMD('CHOICE','YES,NO,?,HELP.',1,
     +                  KSAT,TEXT,KTEXT,ISTAT)
            IF (KWHAT.EQ.3.OR.KWHAT.EQ.4) THEN
               CALL WRUSER('OPTIONS ARE :-',ISTAT)
               CALL WRUSER('YES,NO',ISTAT)
            ENDIF
         ENDDO
         IF (KSAT.EQ.1) THEN
            SATDEN = 10.0
            CALL RDKEYR('SATDEN',.TRUE.,1,SATDEN,NVAL,ISTAT)
            IF ((ISTAT.EQ.4).OR.SATDEN.LT.0.0) VALID = .FALSE.
            IF (VALID.EQ.FALSE) THEN
               CALL WRUSER('BAD VALUE',ISTAT)
            ENDIF
         ENDIF
      ENDIF
C
C  Open Output image
C
      IF (VALID) THEN
         CALL GT2DIW('OUTPUT',102,.FALSE.,NIN(1),NIN(2),IPOUT,IERR)
         IF(IERR.EQ.0) THEN
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +                 IERR)
            CALL PTDSCR('OUTPUT',' INVAL','INTEGER',INVAL,RVAL,
     +                  CVAL,IERR)
            CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BS,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZ,CVAL,IERR)
         ELSE
            VALID = .FALSE.
            CALL WRUSER('CANT OPEN THAT FILE',ISTAT)
         ENDIF
      ENDIF
C
C  Do the correction
C
      IF (VALID) THEN
         CALL DOCOR(%VAL(IPIN),NIN(1),NIN(2),INVAL,BS,BZ,%VAL(IPCL),
     +              %VAL(IPITF),NITF,BOTLIM,TOPLIM,KSAT,SATDEN,
     +              DENVAL,%VAL(IPOUT))
      ENDIF
C
C
C
      CALL FRDATA(' ',ISTAT)
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GETITF *
C      *            *
C      **************
C
C
C   PURPOSE
C     To open up an ITF correction table for use
C
C   ARGUMENTS
C  OUT
C   IPITF   Integer    Pointer to ITF
C   LEN     Integer    legth of ITF
C   BOTLIM  Real       Minimum valid input value
C   TOPLIM  Real       Maximum valid input value
C   DENVAL  Real       Reading for density change of unity
C   TITLE   Char*72    ITF Title
C   IERR    Integer    Error flag (=0 for success
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
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-6-2
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GETITF(IPTAB,LEN,BOTLIM,TOPLIM,DENVAL,TITLE,IERR)
C
C
C
      CHARACTER TITLE*72,CVAL*1
C
C  Get ITF table
C
      NGO = 1
    1 CALL GT2DIR('ITFTABLE',204,.FALSE.,LEN,NUP,IPTAB,IERRG)
C
C ITF TABLE SUCCESSFULLY OBTAINED... EXTRACT TITLE AND TABLE LIMITS
C FROM DESCRIPTOR
      IF (IERRG.EQ.0) THEN
         IERR = 0
C
         TITLE=' '
         CALL GTDSCR('ITFTABLE','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	      IERRT)
         BOTLIM=0.0
         CALL GTDSCR('ITFTABLE','LOLIM','REAL',IVAL,BOTLIM,CVAL,IERRL)
         TOPLIM=0.0
         CALL GTDSCR('ITFTABLE','UPLIM','REAL',IVAL,TOPLIM,CVAL,IERRU)
         DENVAL = 754.0
         CALL GTDSCR('ITFTABLE','DENVAL','REAL',IVAL,DENVAL,CVAL,
     +               IERRD)
C
C CHECK VALIDITY OF ITF TABLE... ALL DESCRIPTOR ITEMS PRESENT,
C ONLY 1 LINE IN THE INPUT IMAGE AND TOPLIM.GE.BOTLIM
C
         IF((IERRL.NE.0).OR.(IERRU.NE.0).OR.(IERRD.NE.0).OR.
     +      (NUP.NE.1).OR.(BOTLIM.GT.TOPLIM)) THEN
C
C IF INVALID, GIVE ERROR MESSAGE AND RETURN TO GET NEW ITF TABLE
C
            CALL CNPAR('ITFTABLE',ISTAT)
            CALL WRUSER('ITF Table faulty',ISTAT)
            NGO = NGO + 1
            IF (NGO.LT.3) THEN
               CALL WRUSER('Try again',ISTAT)
               GO TO 1
            ELSE
               CALL WRUSER('No more goes',ISTAT)
               IERR = 1
            ENDIF
	   ENDIF
      ELSE
         IERR = 1
      ENDIF
      CALL CNPAR('ITFTABLE',ISTAT)
C
C
C
      END



C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C      ****************
C      *              *
C      * S/R   GETCLR *
C      *              *
C      ****************
C
C
C
C          FUNCTION:-
C               IT GETS THE CLEAR AREAS MEASURED BY A SEGMENTED PDS RASTER
C               AND FROM THESE WORKS OUT THE PDS DRIFT BY FITTING A
C               POLYNOMIAL.
C               It fills a 1-D array with a 'clear' value for each
C               line of the main raster.
C                  IT DISPLAYS THE LEVELS OF THE REFERENCE SCANS AND
C               THE FIT AND THE RUN OF THE MODE OF THE MAIN SCAN
C               VALUES WHICH ARE NEAR THE CENTRAL SCAN VALUES
C               IMAGE
C
C
C
C        INPUT/OUTPUT
C
C    IN
C      MAIN   I*2(NMX,NMY)     The main raster
C      NMX    Int              X length of main raster
C      NMY    Int              Y length of main raster
C      INVALM Int              Invalid pixel value flag of main raster
C      TEXT   Character*72     Default text to write below display
C  OUT
C      ALINE  Real(NMX)        The run of 'clear' values for main
C
C         STARLINK PARAMETERS:-
C         IMAGER               STARLINK IMAGE OF THE CLEAR MEASURES
C
C         NUMSEG    Takes sq   NO OF SEGMENTS MAIN IMAGE WAS SEGMENTED
C                   Ref area   INTO
C                   and calcs
C
C         NTERMS     6         NUMBER OF TERMS (1-10) OF POLYNOMIAL
C                              TO BE USED IN FITTING REFERENCE
C                              SCANS DRIFT
C
C         DEVICE     ARGS      WHAT DEVICE THE GRAPHS ARE TO BE
C                              OUTPUT ON.
C                              CHOICES ARE NONE,GOC,ARGS,TEK,CALCOMP,
C                              CC81,VERSATEC.
C
C         DEVSIZE    20,10     SIZE OF GRAPHS IN CM ON DISPLAY DEVICE
C
C         TEXT                 TEXT TO BE TYPED OUT BELOW DIAGS IF
C                              WRITING TO HARDCOPY DEVICE
C
C         RANGE1,2             LIMITS ON RANGES OF VALUES DISPLAYED
C                    whole     ON THE GRAPHS
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C
*
*
*CALLS
*	THIS PACKAGE:
*	STARLINK:
*
*NOTES
*	USES VAX %VAL FACILITY
*
C         A J PENNY                RGO             8-JUN-83
C
* ----------------------------------------------------------------------
C
C
C
      SUBROUTINE GETCLR(MAIN,NMX,NMY,INVALM,ALINE,TEXT)
C
C
C
      INTEGER*2 MAIN(NMX,NMY)
      REAL ALINE(NMY)
      CHARACTER CVAL*1,PRBUF*80
      REAL AV(201),STD(201),AVX(201),VALM(100),VALMX(100)
      DOUBLE PRECISION BFIT(10),CHI,RMS,DAVX(201),DAV(201),DSTD(201)
      INTEGER NPT(201),NVT(1)
      CHARACTER TEXT*72
C
C  Get reference image
C
      CALL GT2DIR('IMAGER',102,.FALSE.,NPIXR,NLINER,IPINR,IERRB)
      CALL RDDSCR('IMAGER','INVAL','INTEGER',INVALR,RVAL,IERR)
C
C  Get the number of segments in the Main image
C
      NSEG = (NLINER/NPIXR) - 1
      CALL GETPAR('NUMSEG','INTEGER',1,1.0,200.0,.TRUE.,NSEG,VAL,IERR)
C
C  Get the Reference area values
C
      NSEGA = NSEG + 1
      NYS = NLINER/NSEGA
      DO K = 1,NSEGA
         NYA = 1 + (K-1)*NYS
         NYB = K*NYS
         CALL IMASPA(%VAL(IPINR),NPIXR,NLINER,1,NPIXR,NYA,NYB,
     +               INVALR,AV(K),STD(K),NPT(K))
         IF(NPT(K).GE.3) STD(K) = STD(K)/SQRT(FLOAT(NPT(K)-2))
         AVX(K) = 1.0 + REAL((K-1)*(NMY/(NSEGA-1)))
         IF (AVX(K).GT.REAL(NMY)) AVX(K) = REAL(NMY)
      ENDDO
C
C  Fit a mean line through the Ref values
C
      NVT(1) = 6 + (NSEGA-20)/5
      IF (NVT(1).GT.10) NVT(1) = 10
      IF (NVT(1).LT.6) NVT(1) = 6
      CALL RDKEYI('NTERMS',.TRUE.,1,NVT,NVAL,IERR)
      NTERMS = NVT(1)
      IF (NTERMS.LT.1) NTERMS = 1
      IF (NTERMS.GT.10) NTERMS = 10
      CALL CNPAR('NTERMS',ISTAT)
      DO K = 1,NSEGA
         DAVX(K) = AVX(K)/1000.0
         DAV(K) = AV(K)/1000.0
         DSTD(K) = STD(K)/1000.0
      ENDDO
      LM = 0
      CALL POLFIT(DAVX,DAV,DSTD,NSEGA,NTERMS,LM,BFIT,CHI,RMS)
      DO K = 1,NMY
         ALINE(K) = SNGL(BFIT(1))
         DO J = 2,NTERMS
            AK = REAL(K)/1000.0
            ALINE(K) = ALINE(K) + SNGL(BFIT(J))*AK**REAL(J-1)
         ENDDO
         ALINE(K) = ALINE(K)*1000.0
      ENDDO
C
C  Get the Main area mode values
C
      CALL DOMODE(MAIN,NMX,NMY,INVALM,VALM,VALMX,NVALM)
C
C  Display the Ref values and fit and the run of mode values in Main
C
      CALL MRDIS(AV,STD,AVX,201,NSEGA,ALINE,NMY,
     +           VALM,VALMX,NVALM,TEXT)
C
C
C
      CALL CNPAR('NUMSEG',ISTAT)
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R MRDIS *
C      *            *
C      **************
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE MRDIS(REFVAL,REFSTD,REFX,KREF,NSEGA,ALINE,NX,
     +                  VALM,VALMX,NVALM,ATITLE)
C
C
C
      REAL REFVAL(KREF),REFSTD(KREF),REFX(KREF)
      REAL ALINE(NX)
      REAL SIZE(2),AX(2),AY(2)
      REAL VALM(100),VALMX(100)
      CHARACTER TEXT*72,ATITLE*72
      INTEGER TEXTA(18)
      EQUIVALENCE (TEXTA(1),TEXT)
C
C
C
      CALL DEVOPN(IDEV,SIZE)
      IF (IDEV.EQ.1) GOTO 99
C
C  If hardcopy, get text to caption graph
C
      IF (IDEV.EQ.5.OR.IDEV.EQ.6.OR.IDEV.EQ.8) THEN
         TEXT = ATITLE
         CALL RDKEYC('TEXT',.TRUE.,1,TEXT,I,ISTAT)
         CALL CNPAR('TEXT',ISTAT)
         DO K = 1,67
            J = 73 - K
            JA = J - 5
            TEXT(J:J) = TEXT(JA:JA)
         ENDDO
      ELSE
         TEXT = '   '
      ENDIF
      TEXT(1:1) = 'F'
      TEXT(2:2) = 'I'
      TEXT(3:3) = 'G'
      TEXT(5:5) = ' '
C
C
C
      CALL PACK IN(SIZE(1),SIZE(2))
C
C
C
      REFMAX = REFVAL(1) + REFSTD(1)
      REFMIN = REFVAL(1) - REFSTD(1)
      DO K = 1,NSEGA
         RMAX = REFVAL(K) + REFSTD(K)
         RMIN = REFVAL(K) - REFSTD(K)
         IF (RMIN.LT.REFMIN) REFMIN = RMIN
         IF (RMAX.GT.REFMAX) REFMAX = RMAX
      ENDDO
      AX(1) = 1.0
      AX(2) = REFX(NSEGA)
      AY(1) = REFMIN
      AY(2) = REFMAX
      CALL WRUSER('INPUT DISPLAY LIMITS FOR REF LEVELS',ISTAT)
      CALL RDKEYR('RANGE1',.TRUE.,2,AY,NVAL,IERR)
      CALL CNPAR('RANGE1',ISTAT)
      CALL JBAXES(AX,2,SIZE(1),' ',1,AY,2,SIZE(2),' ',1)
      DO K = 1,NSEGA
         X = REFX(K)
         Y = REFVAL(K)
         CALL MARK PT (X,Y,3)
         YA = Y - REFSTD(K)
         CALL BREAK
         CALL JOIN PT(X,YA)
         YB = Y + REFSTD(K)
         CALL JOIN PT(X,YB)
      ENDDO
      CALL BREAK
      DO K = 1,NSEGA
         X = REFX(K)
         Y = REFVAL(K)
         CALL JOIN PT(X,Y)
      ENDDO
      CALL BREAK
      DO K = 1,NX
         X = REAL(K)
         Y = ALINE(K)
         CALL JOIN PT(X,Y)
      ENDDO
      TEXT(4:4) = '1'
      CALL TITLE(2,2,TEXTA,72)
C
C  Do Main image points with Ref fit subtracted
C
      CALL DEVCLS(IDEV)
      CALL DEVOPA(IDEV)
      CALL PACK IN(SIZE(1),SIZE(2))
      VALMAX = -32767.0
      VALMIN = 32767.0
      DO K = 1,NVALM
         KV = VALMX(K) + 0.5
         AVAL = VALM(K) - ALINE(KV)
         IF (AVAL.GT.-100000.0) THEN
            IF (AVAL.GT.VALMAX) VALMAX = AVAL
            IF (AVAL.LT.VALMIN) VALMIN = AVAL
         ENDIF
      ENDDO
      AX(1) = 1.0
      AX(2) = VALMX(NVALM)
      AY(1) = VALMIN
      AY(2) = VALMAX
      CALL WRUSER('INPUT RANGE FOR MAIN CORRECTED LEVELS',ISTAT)
      CALL RDKEYR('RANGE2',.TRUE.,2,AY,NVAL,IERR)
      CALL CNPAR('RANGE2',ISTAT)
      CALL JBAXES(AX,2,SIZE(1),' ',1,AY,2,SIZE(2),' ',1)
      DO K = 1,NVALM
         X = VALMX(K)
         KV = X + 0.5
         Y = VALM(K) - ALINE(KV)
         IF (VALM(K).GT.-100000.0) CALL MARK PT(X,Y,3)
      ENDDO
      TEXT(4:4) = '2'
      CALL TITLE(2,2,TEXTA,72)
C
C
C
      CALL DEVCLS(IDEV)
C
C
C
   99 CONTINUE
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOMODE *
C      *            *
C      **************
C
C -------------------------------------------------------------
C
C
C
      SUBROUTINE DOMODE(MAIN,NX,NY,INVAL,VALM,VALMX,NA)
C
C
C
      INTEGER*2 MAIN(NX,NY)
      REAL VALM(100),VALMX(100)
C
C
C
      KYA = NY/2 - NY/20
      KYB = KYA + NY/10
      CALL IMASPA(MAIN,NX,NY,1,NX,KYA,KYB,INVAL,AV,STD,NPT)
      KRANGE = 4.0*STD
      NRANGE = 10.0*STD
      CALL GETDYN('NUM',104,NRANGE,IPNUM,IERR)
      CALL GETDYN('XPOL',208,KRANGE,IPXPOL,IERR)
      CALL GETDYN('YPOL',208,KRANGE,IPYPOL,IERR)
      CALL GETDYN('SIGPOL',208,KRANGE,IPSIG,IERR)
C
C
C
      NR = NY/100
      IF (NR.LT.1) NR = 1
      NA = NY/NR
      NAH = NA/2
      AVA = AV
      DO K = NAH,NA
         KYA = 1 + (K-1)*NR
         KYB = KYA + NR - 1
         LIML = AVA - 5.0*STD
         LIMH = LIML + NRANGE - 1
         IF (LIML.LT.1) LIML= 1
         IF (LIML.GT.64534) LIML = 64534
         IF (LIMH.LT.1) LIMH = 1
         IF (LIMH.GT.64534) LIMH = 64534
         CALL CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,1,NX,KYA,KYB,AVH,
     +               KRANGE,NRANGE,%VAL(IPNUM),%VAL(IPXPOL),
     +               %VAL(IPYPOL),%VAL(IPSIG),IERR)
         IF (IERR.EQ.0) THEN
            AVA = AVH
            VALM(K) = AVH
         ELSE
            VALM(K) = -32767.0
         ENDIF
      ENDDO
      AVA = AV
      DO K = NAH-1,1,-1
         KYA = 1 + (K-1)*NR
         KYB = KYA + NR - 1
         LIML = AVA - 5.0*STD
         LIMH = LIML + NRANGE - 1
         IF (LIML.LT.1) LIML= 1
         IF (LIML.GT.64534) LIML = 64534
         IF (LIMH.LT.1) LIMH = 1
         IF (LIMH.GT.64534) LIMH = 64534
         CALL CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,1,NX,KYA,KYB,AVH,
     +               KRANGE,NRANGE,%VAL(IPNUM),%VAL(IPXPOL),
     +               %VAL(IPYPOL),%VAL(IPSIG),IERR)
         IF (IERR.EQ.0) THEN
            AVA = AVH
            VALM(K) = AVH
         ELSE
            VALM(K) = -100001.0
         ENDIF
      ENDDO
C
C
C
      DO K = 1,NA
         VALMX(K) = NR/2 + (K-1)*NR
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
C      * S/R DOCOR  *
C      *            *
C      **************
C
C
C   PURPOSE
C     To apply PDS corrections, subtract 'clear' levels, and
C     optionally apply saturation correction.
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
C   A.J.PENNY                   RGO                    83-6-2
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE DOCOR(MAIN,NX,NY,INVAL,BS,BZ,ALINE,TABLE,NTAB,
     +                 BOTLIM,TOPLIM,KSAT,SATDEN,DENVAL,MOUT)
C
C
C
      INTEGER*2 MAIN(NX,NY),MOUT(NX,NY)
      REAL ALINE(NY),TABLE(1,NTAB)
C
C
C
      RDINT = 1.0/(MAX((TOPLIM-BOTLIM),1.0E-20)/REAL(NTAB-1))
      IF (KSAT.EQ.1) THEN
         DF = -1.0*DENVAL*SATDEN
         DFF = 1.0/SATDEN
      ENDIF
      DO K = 1,NY
         VAL = BS*ALINE(K) + BZ
         CALL TABCOR(VAL,TABLE,NTAB,BOTLIM,TOPLIM,RDINT,CLEAR,IERR1)
         DO J = 1,NX
            VAL = BS*REAL(MAIN(J,K)) + BZ
            CALL TABCOR(VAL,TABLE,NTAB,BOTLIM,TOPLIM,RDINT,VALA,IERR2)
            IF (IERR1.NE.0.OR.IERR2.NE.0) THEN
               MOUT(J,K) = INVAL
            ELSE
               VAL = VALA - CLEAR
               IF (KSAT.EQ.1) THEN
                  IF (VAL.LT.1.0E-6) THEN
                     IF (VAL.GT.-1.0E-6) THEN
                        VAL = 0.0
                     ELSE
                        VAL = -1.0*DF*ALOG(1.0-ABS(VAL)*DFF)
                     ENDIF
                  ELSE
                     VAL = DF*ALOG(1.0-VAL*DFF)
                  ENDIF
               ELSE
                  VAL = VAL*DENVAL
               ENDIF
               MOUT(J,K) = (VAL-BZ)/BS
            ENDIF
         ENDDO
      ENDDO
C
C
C
      END
C
C
C
      SUBROUTINE TABCOR(VAL,TABLE,NTAB,BOTLIM,TOPLIM,RDINT,VALA,IERR)
C
C
C
      REAL TABLE(1,NTAB)
C
C
C
      IF (VAL.LT.BOTLIM.OR.VAL.GT.TOPLIM) THEN
         VALA = 0.0
         IERR = 1
      ELSE
         IERR = 0
         TABLOC = (VAL-BOTLIM)*RDINT + 1.0
         NTAB1 = INT(TABLOC)
         NTAB2 = MIN(NTAB1+1,NTAB)
         DTAB = TABLOC - REAL(NTAB1)
         VALA = TABLE(1,NTAB1) + DTAB*(TABLE(1,NTAB2)-TABLE(1,NTAB1))
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CALMOD *
C      *            *
C      **************
C
C -----------------------------------------------------------
C
C
C
      SUBROUTINE CALMOD(MAIN,NX,NY,INVAL,LIML,LIMH,NXA,NXB,NYA,NYB,
     +                  AV,KRANGE,NRANGE,NUM,XPOL,YPOL,SIGPOL,IERR)
C
C
C
      INTEGER*2 MAIN(NX,NY)
      INTEGER NUM(NRANGE)
      DOUBLE PRECISION XPOL(KRANGE),YPOL(KRANGE),SIGPOL(KRANGE)
      DOUBLE PRECISION ARRPOL(10),CHI,RMS
      REAL AX(2),AY(2),SIZE(2)
C
C
C
      IERR = 0
C
C
C
      DO K = 1,NRANGE
         NUM(K) = 0
      ENDDO
C
C
C
      DO K = NYA,NYB
         DO J = NXA,NXB
            KV = MAIN(J,K) - LIML + 1
            IF (KV.GE.1.AND.KV.LE.NRANGE) THEN
               IF (KV.NE.INVAL) NUM(KV) = NUM(KV) + 1
            ENDIF
         ENDDO
      ENDDO
C
C
C
      MAX = NUM(1)
      KMODE = 1
      DO K = 1,NRANGE
         IF (NUM(K).GT.MAX) THEN
            MAX = NUM(K)
            KMODE = K
         ENDIF
      ENDDO
C
C
C
      IF (NUM(KMODE).EQ.0) THEN
         IERR = 1
         AV = 0.0
      ELSE
         KA = KMODE - KRANGE/2
         KB = KA + KRANGE - 1
         IF (KA.LT.1) KA = 1
         IF (KB.GT.KRANGE) KB = KRANGE
         TOP = DBLE(NUM(KMODE))
         DO K = 1,KRANGE
            J = KA + K - 1
            YPOL(K) = DBLE(NUM(J))/TOP
            XPOL(K) = DBLE(K)
         ENDDO
         CALL POLFIT(XPOL,YPOL,SIGPOL,KRANGE,3,0,ARRPOL,CHI,RMS)
         BV = -1.0*ARRPOL(2)/(2.0*ARRPOL(3))
         IF (BV.LT.1.0.OR.BV.GT.REAL(KRANGE)) BV = REAL(KRANGE)/2.0
         AV = REAL(LIML+KA) + BV - 1.0
C
C
      ENDIF
C
C
C
      END



