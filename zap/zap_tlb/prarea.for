      SUBROUTINE PRAREA(KDATA,KX,KY,LX,LY)
C
C
      INTEGER*2 KDATA(KX,KY)
      INTEGER LX(2),LY(2),KA(10)
      CHARACTER*72 TEXT
      CHARACTER YFLAG,TXT*69
C
C  Calculate steps between values to be typed
C
      NX = 1 + (LX(2)-LX(1))/10
      NY = 1 + (LY(2)-LY(1))/10
      NY = -1.0*NY
C
C  Calc where to put 'Y' marker
C
      KFLAG = LY(2) + NY*(((LY(1)-LY(2))/NY)/2)
C
C  Put out array
C
      DO K = LY(2),LY(1),NY
         IF (K.EQ.KFLAG) THEN
            YFLAG = 'Y'
         ELSE
            YFLAG = ' '
         ENDIF
         WRITE(TEXT,900)YFLAG,K,(KDATA(J,K),J=LX(1),LX(2),NX)
  900    FORMAT(' ',A1,I5,3X,10I6)
         CALL WRUSER(TEXT,ISTAT)
         CALL WRUSER(' ',ISTAT)
      ENDDO
C
C
C  Put out 'X' axis markers
      NXOUT = (LX(2)-LX(1))/NX + 1
      DO K = 1,NXOUT
         KA(K) = LX(1) + NX*(K-1)
      ENDDO
      WRITE(TEXT,901)(KA(K),K=1,NXOUT)
  901 FORMAT(' ',8X,10I6)
      CALL WRUSER(TEXT,ISTAT)
      DO K = 1,69
         TXT(K:K) = ' '
      ENDDO
      K = 11 + 6*(NXOUT-1)/2
      TXT(K:K) = 'X'
      WRITE(TEXT,902)(TXT(K:K),K=1,69)
  902 FORMAT(' ',69A1)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END
 
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R MSTDEV *
C      *            *
C      **************
C
C THIS S/R FINDS THE MEAN AND STD DEV FROM A HISTOGRAM WHICH
C IS STORED IN LINE AND WANTED FROM THE DATA IN RANGE NRAN(1)-NRAN(2)
C IF PARAMETER 'JN' IS NOT ONE, THE S/R WILL ITERATE THROWING OUT
C DATA MORE THAN THREE STD DEV FROM THE MEAN, DOING THIS 'JN' TIMES.
C
C -----------------------------------------------------
C
C
C
