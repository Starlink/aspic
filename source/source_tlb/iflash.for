C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  IFLASH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               IFLASH
C
C
C          FUNCTION:-
C               It displays an image on the  ARGS by simply copying
C               a 2-D starlink image to the ARGS memory.  IFLASH does
C               not do any intensity scaling, thus it is much faster
C               than the similar program ADISP.
C               If the image is bigger than 512 it is sampled down to
C               less.
C
C
C          USE:-
C               Mainly useful for displaying integer*2 arrays which have
C               pixels in the range 0 < N < 255, which is the extent of
C               the ARGS look up table.  Values outside this range will
C               be displayed with a LUT value of MOD(N,256)
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image (I*2)
C                                             to  be displayed.
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
      INTEGER IDIMN(99)
      INTEGER KXR(2),KYR(2)
C
C
C
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
      ELSE
         CALL RDIMAG('IMAGE',102,99,IDIMN,NDIMS,IPIN,JSTAT)
         IF (NDIMS.NE.2) THEN
            CALL WRUSER('MUST BE 2D IMAGE!',J)
         ELSE
            IXC=256
            IYC=256
            LX = IDIMN(1) - 1 + 1
            LY = IDIMN(2) - 1 + 1
            LARGE = MAX(LX,LY)
            KCOM = 1 + (LARGE-1)/512
            KXS = LX/KCOM
            KYS = LY/KCOM
            CALL ASP_SWSP(500,8)
            CALL IFLASH2(%VAL(IPIN),IDIMN(1),IDIMN(2),IXC,IYC,
     +                   KXS,KYS,KCOM)
C
C  Update ARGS database
C
            KXR(1) = 1
            KXR(2) = IDIMN(1)
            KYR(1) = 1
            KYR(2) = IDIMN(2)
            CALL DISPJ(KXS,KYS,IXC,IYC,.FALSE.,.FALSE.,0.0,
     +                  255.0,KXR,KYR,KCOM)
         ENDIF
      ENDIF
C
C
C
      CALL FRDATA(' ',ISTAT)
      END



C
C
C



      SUBROUTINE IFLASH2(IPIC,NX,NY,IXC,IYC,NXA,NYA,KCOM)
C
C
C
      INTEGER*2 IPIC(NX,NY),KPIC(10240)
C
C
C
      KXCA = 256 - NXA/2
      KYCA = 256 - NYA/2 - 1
      NLINES = 10240/NXA
      DO JY = 1,NYA,NLINES
         NY2 = MIN(NLINES,NYA-JY+1)
         LY = KCOM*(JY-1)
         LP = 0
         DO K = 1,NY2
            LY = LY + KCOM
            LX = 1 - KCOM
            DO KB = 1,NXA
               LX = LX + KCOM
               LP = LP + 1
               KPIC(LP) = IPIC(LX,LY)
            ENDDO
         ENDDO
         CALL ARGS_WPDI2(KPIC(1),NY2*NXA,KXCA,JY+KYCA,NXA,
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



