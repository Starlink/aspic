C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program ICFLASH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ICFLASH
C
C
C          FUNCTION:-
C             It displays an image on the ARGS. It scales it around
C             the sky level to show faint features. It does it as
C             fast as possible.
C             If the sample has a side greater than 512, it compresses
C             it down in display, either by sampling or averaging in
C             boxes. It assumes I*2 images. It displays the images with
C             white = low, black = high values.
C
C
C          USE:-
C               Looking at images.
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image (I*2)
C                                             to  be displayed.
C
C         OPTION     AVERAGE                  Choice of averaging or
C                                             sampling pixels if image
C                                             is > 512 on a side. Choice
C                                             is AVERAGE,SAMPLE.
C
C
C
C      STARLINK OUTPUT PARAMTERS
C
C         MIN                                 Output as ICFLASH_MIN, the
C                                             minimum display value.
C
C         MAX                                 Output as ICFLASH_MAX, the
C                                             maximum display value.
C
C         COMFAC                              Output as ICDISP_COMFAC, the
C                                             compression factor used for
C                                             the display
C
C         DX                                  Output as ICDISP_DX, the X
C                                             coordinate in the input image
C                                             of the first pixel in the 
C                                             displayed image.
C
C         DY                                  Output as ICDISP_DY, the Y
C                                             coordinate in the input image
C                                             of the first pixel in the
C                                             displayed image.
C          A J PENNY  RGO   83 MAY 26
C          BASED ON W PENCE   AAO  30 SEPT 82
C          BASED ON THE ADISP PROGRAM WRITTEN BY PTW/WFL/KFH        RGO
C
C
C--------------------------------------------------------------------------
C
C
C
      INTEGER IDIMN(2)
      CHARACTER*72 TEXT,TITLE
      LOGICAL VALID
      INTEGER KXR(2),KYR(2)
C
C
C
      VALID = .TRUE.
      CALL SRINIT(0,.TRUE.,JSTAT)
      CALL ARGS_CLRIM(ISTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
         VALID = .FALSE.
      ENDIF
C
C
C
      IF (VALID) THEN
         CALL GTIMG(IPIN,IDIMN,BS,BZ,INVAL,TITLE,ISTAT)
         IF (ISTAT.NE.0) VALID = .FALSE.
      ENDIF
C
C
C
      IF (VALID) THEN
         IF (IDIMN(1).GT.512.OR.IDIMN(2).GT.512) THEN
    1       KOPT = 1
            CALL GETCMD('OPTION','AVERAGE,SAMPLE,HELP,?.',1,KOPT,
     +                  TEXT,KTEXT,ISTAT)
            IF (KOPT.EQ.3.OR.KOPT.EQ.4) THEN
               CALL WRUSER('Choices are ',ISTAT)
               CALL WRUSER('AVERAGE  Pixels averaged in boxes',ISTAT)
               CALL WRUSER('SAMPLE   First pixel in box taken',ISTAT)
               CALL CNPAR('OPTION',ISTAT)
               GO TO 1
            ENDIF
         ELSE
           KOPT = 0
         ENDIF
      ENDIF
C
C
C
      IF (VALID) THEN
         IXC=256
         IYC=256
         LARGE = MAX(IDIMN(1),IDIMN(2))
         KCOM = 1 + (LARGE-1)/512
         KXS = IDIMN(1)/KCOM
         KYS = IDIMN(2)/KCOM
         NXY = KXS*KYS
         CALL GETDYN('WORK',102,NXY,IPK,ISTAT)
         CALL ASP_SWSP(500,8)
         CALL IFLASH3(%VAL(IPIN),IDIMN(1),IDIMN(2),IXC,IYC,
     +                   %VAL(IPK),KXS,KYS,KCOM,KOPT,INVAL,
     +                   VLO,VHI,BS,BZ)
         KXR(1) = 1
         KYR(2) = IDIMN(1)
         KYR(1) = 1
         KYR(2) = IDIMN(2)
         CALL DISPJ(KXS,KYS,IXC,IYC,.FALSE.,.FALSE.,VLO,VHI,KXR,
     +              KYR,KCOM)
      ENDIF
C
C
C
      CALL FRDATA(' ',ISTAT)
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
      SUBROUTINE IFLASH3(IPIC,NX,NY,IXC,IYC,KPIC,NXA,NYA,KCOM,
     +                   KOPT,INVAL,VLO,VHI,BS,BZ)
C
C
C
      DOUBLE PRECISION S,SS,TOT,AM,VAL
      INTEGER NX,NY,IXC,IYC,KXC(2),KYC(2)
      INTEGER*2 IPIC(NX,NY),KPIC(NXA,NYA)
      CHARACTER*72 TEXT
C
C  Make compressed array
C
      IF (KOPT.EQ.2) THEN
         KA = 1 - KCOM
         DO K = 1,NYA
            KA = KA + KCOM
            JA = 1 - KCOM
            DO J = 1,NXA
               JA = JA + KCOM
               KPIC(J,K) = IPIC(JA,KA)
            ENDDO
         ENDDO
      ENDIF
      IF (KOPT.EQ.1) THEN
         ATOT = KCOM*KCOM
         KA = 1 - KCOM
         DO K = 1,NYA
            KA = KA + KCOM
            JA = 1 - KCOM
            DO J = 1,NXA
               JA = JA + KCOM
               AV = 0
               DO LY = 1,KCOM
                  DO LX = 1,KCOM
                     AV = AV + REAL(IPIC(JA-1+LX,KA-1+LY))
                  ENDDO
               ENDDO
               KPIC(J,K) = AV/ATOT
            ENDDO
         ENDDO
      ENDIF
      IF (KOPT.EQ.0) THEN
         DO K = 1,NYA
            DO J = 1,NXA
               KPIC(J,K) = IPIC(J,K)
            ENDDO
         ENDDO
      ENDIF
C
C  Scale display image
C
      KXC(1) = 1
      KXC(2) = NXA
      KYC(1) = 1
      KYC(2) = NYA
      CALL DISPSS(KPIC,NXA,NYA,KXC,KYC,INVAL,AAM,ASTD,IERR)
      AMAX = AAM + 3.0*ASTD
      AMIN = AAM - 3.0*ASTD
      BBMAX = BS*AMAX + BZ
      BBMIN = BS*AMIN + BZ
      WRITE(TEXT,900)BBMIN,BBMAX
  900 FORMAT(' ','Min = ',G16.6,'  Max = ',G16.6)
      CALL WRUSER(TEXT,ISTAT)
      VLO = BBMAX
      VHI = BBMIN
      ARA = AMAX - AMIN
      IF (ABS(ARA).GT.1.0E-10) THEN
         AR = 255.0/(AMAX-AMIN)
      ELSE
         AR = 1.0
      ENDIF
      DO K = 1,NYA
         DO J = 1,NXA
            AV = REAL(KPIC(J,K))
            AV = AR*(AMAX-AV)
            IF (AV.LT.0.0) AV = 0
            IF (AV.GT.255.0) AV = 255.0
            KPIC(J,K) = AV
         ENDDO
      ENDDO
C
C  Put out image
C
      KXCA = 256 - NXA/2
      KYCA = 256 - NYA/2 - 1
      NLINES = INT(32767.0/2.0)/NXA
      DO JY = 1,NYA,NLINES
         NY2 = MIN(NLINES,NYA-JY+1)
         CALL ARGS_WPDI2(KPIC(1,JY),NY2*NXA,KXCA,JY+KYCA,NXA,
     +                   NY2,16,0,.FALSE.)
C
      ENDDO
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DISPJ  *
C      *            *
C      **************
C
C



      SUBROUTINE DISPJ(KX,KY,IXC,IYC,TRIM,LLOG,VLO,VHI,
     +                 KXR,KYR,KCOMP)
C
C
C
      LOGICAL TRIM,LLOG
      CHARACTER VALUE*80
      INTEGER KXR(2),KYR(2)
      CHARACTER TEXTAR*80
C
C
C
      CALL WRKEYI('DX',KXR(1),1,ISTAT)
      CALL WRKEYI('DY',KYR(1),1,ISTAT)
      CALL WRKEYI('COMFAC',KCOMP,1,ISTAT)
      CALL WRKEYR('MIN',VLO,1,ISTAT)
      CALL WRKEYR('MAX',VHI,1,ISTAT)
C
C
C
      CALL ASP_RWSP
      CALL ARGS_NUMIM(IDMAX)
      IF (IDMAX.GE.1) THEN
         CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
         CALL ASP_DZTOI('ZXC',VALUE,IZXC,JSTAT)
         CALL ASP_DZTOI('ZYC',VALUE,IZYC,JSTAT)
         CALL ASP_DZTOI('ZXF',VALUE,IXF,JSTAT)
         CALL ASP_DZTOI('ZYF',VALUE,IYF,JSTAT)
         CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
      ELSE
         IZXC=256
         IZYC=256
         IXF=1
         IYF=1
      ENDIF
      CALL ARGS_WRIM (IXC,IYC,KX,KY,KX,KY,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)
      ELSE
         CALL ARGS_RDPAR ('DISPZOOM',1,VALUE,NVALS,JSTAT)
         CALL ASP_LTODZ ('TRIM',TRIM,VALUE,JSTAT)
         CALL ASP_LTODZ ('LOG',LLOG,VALUE,JSTAT)
         CALL ASP_FTODZ ('PVLO',VLO,VALUE,JSTAT)
         CALL ASP_FTODZ ('PVHI',VHI,VALUE,JSTAT)
         CALL ASP_ITODZ ('ZXC',IZXC,VALUE,JSTAT)
         CALL ASP_ITODZ ('ZYC',IZYC,VALUE,JSTAT)
         CALL ASP_ITODZ ('ZXF',IXF,VALUE,JSTAT)
         CALL ASP_ITODZ ('ZYF',IYF,VALUE,JSTAT)
         CALL ARGS_WRPAR ('DISPZOOM',VALUE,1,JSTAT)
         WRITE(TEXTAR,903)KXR(1),KXR(2),KYR(1),KYR(2),KCOMP
  903    FORMAT(5I10)
         CALL ARGS_WRPAR('COMPRE',TEXTAR,1,ISTAT)
      ENDIF
C
C
C
      END



