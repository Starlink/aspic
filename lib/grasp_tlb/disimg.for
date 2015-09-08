C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        **************
C        *            *
C        * S/R DISIMG *
C        *            *
C        **************
C
C
C     PURPOSE
C        To display an image on the ARGS
C
C
C
*  STARLINK PARAMETERS
*
*    IMAGE
*         The input image
*    XRANGE
*         The X range of the image to be displayed
*    YRANGE
*         The Y range of the image to be displayed
*   PVLO,PVHI
*         The limits of the display range 
*    XC
*         The X position on the ARGS of the centre of the displayed image
*    YC
*         The Y position on the ARGS of the centre of the displayed image
*    MDIS
*         Flag for auto getting of +/- 3sigma lims as display range
*    DISINV
*         The value 'invalid' values are to be displayed as
*    TRIM
*         TRIM=FALSE will wrap the image values round a range of 256.
*         WRAP,LLOG,MDIS are disabled
*    WRAP
*         WRAP=FALSE means that values outside the display range will
*         be set to the limits of the range, =TRUE means they will
*         wrap round cyclically.
*    LOG
*         LOG=TRUE will perform a log transformation of the data between
*         PVLO and PVHI
*    AVERAG
*         AVERAG=TRUE will perform taking the mean of the values in
*         a compression box, otherwise the first value alone will give
*         the output value.
*    COMFAC
*         On exit, will be given the compression factor used
*    DX
*         On exit, will be given the X coord in the input image of the
*         first pixel in the display image
*    DY
*         On exit, will be given the Y coord in the input image of the
*         first pixel in the display image.
*
*  ARGUMENTS
*       None
*
*  CALLS
*       This file
*           DISPMM,DISPK,DISPL,DISPSS
*       Starlink
*           SRINIT,WRUSER,RDIMAGE,RDDSCR,CTOR,CTOI,RDKEYI,RDKEYR,RDKEYL,
*           WRKEYI,FRDATA
*
*  NOTES
*     Uses VAX %VAL facility.  Uses I*2 images
*
*  WRITTEN BY
*     A.J. PENNY                                   82-12-21
* -----------------------------------------------------------------



      SUBROUTINE DISIMG(IPIN,NSIZE,BSCALE,BZERO,INVAL,TITLE,IERRZ)
C
C
C
      INTEGER IDIMN(99),KDIM(2),KXR(2),KYR(2),KFAC(1),NSIZE(2)
      LOGICAL TRIM,LLOG,VALID,AVERAG,MDIS,WRAP
      CHARACTER TXT*70
      CHARACTER TITLE*72
      REAL DISPV(2)
C
C
C
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
C
C  Set continuation flag
C
      VALID = .TRUE.
C
C  Aquire ARGS
C
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
         VALID = .FALSE.
      ENDIF
C
C  Aquire image
C
      IF (VALID) THEN
         CALL GTIMG(IPIN,NSIZE,BSCALE,BZERO,INVAL,TITLE,IERR)
         IF (IERR.EQ.0) THEN
            IDIMN(1) = NSIZE(1)
            IDIMN(2) = NSIZE(2)
            WRITE (TXT,911)TITLE
  911       FORMAT(' ','TITLE IS  ',A50)
            CALL WRUSER(TXT,ISTAT)
         ELSE
            VALID = .FALSE.
         ENDIF
         IF (IERR.NE.0) VALID = .FALSE.
      ENDIF
C
C
C
      IF (VALID) THEN
C
C  Get window and consequent compression factor needed
C
         KXR(1) = 1
         KXR(2) = IDIMN(1)
         CALL RDKEYI('XRANGE',.TRUE.,2,KXR,NVALS,JSTAT)
         KYR(1) = 1
         KYR(2) = IDIMN(2)
         CALL RDKEYI('YRANGE',.TRUE.,2,KYR,NVALS,JSTAT)
         DO K = 1,2
            IF(KXR(K).LT.1)KXR(K) = 1
            IF(KXR(K).GT.IDIMN(1))KXR(K) = IDIMN(1)
            IF(KYR(K).LT.1)KYR(K) = 1
            IF(KYR(K).GT.IDIMN(2))KYR(K) = IDIMN(2)
         ENDDO
         IF (KXR(2).LT.KXR(1)) THEN
            KXRA = KXR(1)
            KXR(1) = KXR(2)
            KXR(2) = KXRA
         ENDIF
         IF (KYR(2).LT.KYR(1)) THEN
            KYRA = KYR(1)
            KYR(1) = KYR(2)
            KYR(2) = KYRA
         ENDIF
         LX = KXR(2) - KXR(1) + 1
         LY = KYR(2) - KYR(1) + 1
         LARGE = MAX(LX,LY)
         KFACT = 1 + (LARGE-1)/512
         KDIM(1) = LX/KFACT
         KDIM(2) = LY/KFACT
C
C  Put out window start and compression factor
C
         CALL WRKEYI('DX',KXR,1,JSTAT)
         CALL WRKEYI('DY',KYR,1,JSTAT)
         KFAC(1) = KFACT
         CALL WRKEYI('COMFAC',KFAC,1,JSTAT)
         IF (KFACT.GT.1) THEN
            WRITE(TXT,900)KFACT
  900       FORMAT('  COMPRESSION FACTOR = ',I5)
            CALL WRUSER(TXT,J)
         ENDIF
C
C  Open working space
C
         NELS = KDIM(1)*KDIM(2)
         CALL GETDYN('IWK',FMT_SW,NELS,IWKP,J)
C
C  Read input display parameters
C
         IXC = 256
         CALL RDKEYI('XC',.FALSE.,1,IXC,NVALS,JSTAT)
         IYC = 256
         CALL RDKEYI('YC',.FALSE.,1,IYC,NVALS,JSTAT)
         CALL RDKEYL('TRIM',.FALSE.,1,TRIM,NVALS,JSTAT)
         CALL RDKEYL('WRAP',.FALSE.,1,WRAP,NVALS,JSTAT)
         CALL RDKEYL('LOG ',.FALSE.,1,LLOG,NVALS,JSTAT)
         CALL RDKEYL('AVERAG',.FALSE.,1,AVERAG,NVALS,JSTAT)
         CALL RDKEYL('MDIS',.FALSE.,1,MDIS,NVALS,JSTAT)
C
C  Get display range
C
         IF (TRIM) THEN
            IF (MDIS) THEN
                CALL DISPSS(%VAL(IPIN),IDIMN(1),IDIMN(2),KXR,KYR,
     +                      INVAL,AM,STD,IERR)
                IF (AVERAG) THEN
                   SMOOTH = REAL(KFACT)
                ELSE
                   SMOOTH = 1.0
                ENDIF
                KVLO = AM + 3.0*STD/SMOOTH
                KVHI = AM - 2.0*STD/SMOOTH
                IF (KVLO.EQ.KVHI) KVLO = KVHI - 1
                PVLO = REAL(KVLO)*BSCALE + BZERO
                PVHI = REAL(KVHI)*BSCALE + BZERO
                WRITE(TXT,902)PVLO,PVHI
  902           FORMAT(' ','DISPLAY LOW = ',F12.4,'   HIGH = ',F12.4)
                CALL WRUSER(TXT,ISTAT)
            ELSE
               CALL DISPMM(%VAL(IPIN),IDIMN(1),IDIMN(2),KVLO,KVHI,
     +                     INVAL,KXR,KYR)
               PVLO = REAL(KVLO)*BSCALE + BZERO
               PVHI = REAL(KVHI)*BSCALE + BZERO
               CALL RDKEYR('PVLO',.TRUE.,1,PVLO,NVALS,JSTATA)
               CALL RDKEYR('PVHI',.TRUE.,1,PVHI,NVALS,JSTATB)
               IF (JSTATA.EQ.1.OR.JSTATB.EQ.1) THEN
                  WRITE(TXT,902)PVLO,PVHI
                  CALL WRUSER(TXT,ISTAT)
               ENDIF
            ENDIF
            KVLO = INT((PVLO-BZERO)/BSCALE)
            KVHI = INT((PVHI-BZERO)/BSCALE)
         ELSE
            KVLO = 0
            KVHI = 255
         ENDIF
C
C  Get level to put invalid pixels at
C
         DISINV = PVHI
         CALL RDKEYR('DISINV',.TRUE.,1,DISINV,NVALS,JSTAT)
         KDSINV = (DISINV-BZERO)/BSCALE
C
C  Take the main image array and make an array of the area to be output,
C  scaled, 0 to 255 from the lower and upper limits, as defined.
C
         CALL DISPK(%VAL(IPIN),IDIMN(1),IDIMN(2),KFACT,TRIM,WRAP,
     +              LLOG,AVERAG,KVLO,KVHI,%VAL(IWKP),KDIM(1),
     +              KDIM(2),INVAL,KDSINV,KXR,KYR)
C
C  Display the array on the ARGS
C
         CALL DISPL(%VAL(IWKP),KDIM(1),KDIM(2),IXC,IYC,TRIM,LLOG,PVLO,
     +              PVHI,KXR,KYR,KFAC(1))
C
C
C
      ENDIF
C
C
C
      IF (VALID) THEN
         IERRZ = 0
      ELSE
         IERRZ = 1
      ENDIF
C
C
C
      CALL FRDATA('IWK',ISTAT)
C
C
C
      END



