CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C           Start routines for maximum entropy package MEM
C           for simple deblurring problems
C
C           written by John Fielden   November 1981
C             last modified            21-SEP-1982
C
C           Copyright (C) MEDC Ltd. 1982
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE MEMST(NOPT,LEVEL,DEF,ERR,W)
      DIMENSION W(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
C
C NOPT = M4*10000 + M3*1000 + M2*100 + M1*10 + M0
C
C M0 controls the intensity of the initial flat image, and what value
C (if any) is returned in DEF for use by MEM:
C   M0 = 0 is intensity=DEF, which is supplied by the calling routine.
C   M0 = 1 is intensity=mean of image. This value is returned in DEF.
C   M0 = 2 is intensity=0.5 * mean of image. This value is returned
C          in DEF.
C   M0 = 3 is no flat image set up. File 1 must be set up by the user.
C          DEF must be set up by the user (i.e. as for M0 = 0).
C   M0 = 4 is same as 3, but DEF is returned as for M0 = 1.
C   M0 = 5 is same as 3, but DEF is returned as for M0 = 2.
C
C M1 controls the setting up of the data file (21):
C   M1 = 0 is data file is set up from the Fourier transform of file 3.
C   M1 = 1 is the same as M1 = 0, except that file 3 is edged first.
C   M1 = 2 is the data file is just a copy of file 3. The Fourier
C          transform of the PSF is saved in file 39.
C   M1 = 3 is the same as M1 = 2, except that file 3 is edged first.
C   M1 = 4 is the data file is not set up. This file must be set up
C          by the user.
C
C M2 controls the k-space filter (only relevant for M1 = 0,1):
C   M2 = 0 is no k-space filter.
C   M2 = 1 is k-space filter supplied in file 38.
C
C M3 controls the setting up of the error file (22):
C   M3 = 0 is ERR=error.
C   M3 = 1 is ERR=signal/noise ratio.
C   M3 = 2 is "user defined errors", i.e. file 22 is set up from the
C          the inverse variances supplied in file 21.
C   M3 = 3 is no error file set up. File 22 must be set up by the user.
C   For M3 = 0,1 and M1 = 0,1,4 the data are assumed to be Fourier data
C   For M3 = 0,1 and M1 = 2,3 the data are assumed to be image data
C   For M3 = 0,1,2 it is assumed that C0 = 1.0 in the call to MEM
C
C M4 specifies the format of the Fourier transforms
C   M4 = 0 FOUR2 format. For this format MK must be a multiple of the
C          row size of the Fourier transform (N(1)+2).
C   M4 = 1 packed (transposed or not). For this format all the reals
C          must be in the first block (see below).
C
C LEVEL = L1*10 + L0
C
C L0 = progress diagnostics
C   L0 = 0 switches these off
C   L0 = 1 produces names of main routines
C   L0 = 2 produces additionally a read/write flowchart
C
C L1 = numerical diagnostics
C   L1 = 0 switches these off
C   L1 = 1 produces low level diagnostics (i.e. the scaling of the PSF,
C          the default level set and the noise level set).
C
C
C W is a workspace array used by MEMST if the edging option is
C specified. W is a REAL array:
C if NDIM = 1 then size of W = 2*ND(1)+1,
C if NDIM = 2 then size of W =  ND(1) + ND(2) + MAX( ND(1),ND(2) ).
C
C NDIM is the number of dimensions (if edging then NDIM.LE.2 otherwise
C   NDIM.LE.3).
C ND(1)..ND(NDIM) are the dimensions of the data.
C NC(1)..NC(2**(NDIM-1)) are the indices of the COMPLEX Fourier
C   transform array (regarded as 1D) where the real numbers are packed.
C   These are only relevant for M4 = 1. The reals must all be in the
C   first block, i.e. NC(2**(NDIM-1)).LE.MK
C
      CALL MEINIT
      DEFA=DEF
      ERRA=ERR
      M0=NOPT
      M1=M0/10
      M2=M1/10
      M3=M2/10
      M4=M3/10
      M0=M0 - 10*M1
      M1=M1 - 10*M2
      M2=M2 - 10*M3
      M3=M3 - 10*M4
      L0=LEVEL
      L1=L0/10
      L0=L0 - 10*L1
      IF(  (M0.EQ.0.OR.M0.EQ.1.OR.M0.EQ.2.OR.M0.EQ.3.OR.
     *      M0.EQ.4.OR.M0.EQ.5)
     *.AND.(M1.EQ.0.OR.M1.EQ.1.OR.M1.EQ.2.OR.M1.EQ.3.OR.
     *      M1.EQ.4)
     *.AND.(M2.EQ.0.OR.M2.EQ.1)
     *.AND.(M3.EQ.0.OR.M3.EQ.1.OR.M3.EQ.2.OR.M3.EQ.3)
     *.AND.(M4.EQ.0.OR.M4.EQ.1)) GOTO 1
      WRITE(IOUT,100) NOPT
      STOP
    1 IF(  (L0.EQ.0.OR.L0.EQ.1.OR.L0.EQ.2)
     *.AND.(L1.EQ.0.OR.L1.EQ.1)) GOTO 2
      WRITE(IOUT,101) LEVEL
      STOP
    2 CONTINUE
      IF(NDIM.GE.1.AND.NDIM.LE.2) GOTO 3
      IF(NDIM.LE.3.AND.M1.EQ.0) GOTO 3
      WRITE(IOUT,102) NDIM,M1
      STOP
    3 CONTINUE
      DO 4 I=1,NDIM
      IF(ND(I).GE.1) GOTO 4
      WRITE(IOUT,103)
      WRITE(IOUT,104) ND
      WRITE(IOUT,105)
      STOP
    4 CONTINUE
      IF(L0.GT.0) WRITE(IOUT,112) M4,M3,M2,M1,M0
      LIM=2**(NDIM-1)+1
      IF(M4.EQ.1) NC(LIM)=0
      N1=1
      N2=1
      IF(M1.EQ.0 .OR. M1.EQ.2 .OR. M1.EQ.4) GOTO 5
      N1=ND(1)+1
      N2=N1+1
      IF(NDIM.GE.2) N2=N1+ND(2)
    5 CONTINUE
      CALL MEMSA(DEFA,SIGNAL,W,W(N1),W(N2))
      IF(M0.EQ.1 .OR. M0.EQ.2 .OR. M0.EQ.4 .OR. M0.EQ.5) DEF=DEFA
      IF(M1.EQ.1 .OR. M1.EQ.3) CALL MEFFT(3,27)
      CALL MEFFT(4,26)
      IF(M1.EQ.2 .OR. M1.EQ.3) CALL MEMK(26,39)
      IF(M3.EQ.1) ERRA=SIGNAL/ERRA
      IF(M3.EQ.3) GOTO 7
      IF(L1.GT.0 .AND. M3.NE.2) WRITE(IOUT,113) ERRA
      CALL MEMSB(ERRA)
    7 NMOD=3
      IF(M1.EQ.4) RETURN
      IF(M1.EQ.0 .OR. M1.EQ.2) GOTO 8
      NMOD=4
      CALL MEMSC
      CALL MEIFT(25,6)
      CALL MEMSD(W,W(N1))
    8 CONTINUE
      IF(M1.EQ.0 .OR. M1.EQ.1) GOTO 9
      CALL MEMJ(NMOD,21)
      RETURN
    9 CONTINUE
      CALL MEFFT(NMOD,28)
      CALL MEMSE
      RETURN
  100 FORMAT(7H OPTION,I11,25H   NOT IMPLEMENTED. STOP.)
  101 FORMAT(7H LEVEL ,I11,25H   NOT IMPLEMENTED. STOP.)
  102 FORMAT(7H NDIM  ,I11,23H out of range for M1 = ,I1)
  103 FORMAT(36H Dimensions invalid. Dimensions are:)
  104 FORMAT(1X,3I12)
  105 FORMAT(6H STOP.)
  112 FORMAT(27H MEMST 22.08.82  Options = ,5I1)
  113 FORMAT(19H Noise level set = ,1PE10.3)
      END
C
      SUBROUTINE MEMSA(DEF,SIGNAL,XF,YF,W)
      DIMENSION XF(1),YF(1),W(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      IF(L0.GT.0) WRITE(IOUT,10)
      SUM=0.
      DO 1 I=1,NJ
      CALL UREAD(7)
      CALL MEMSA1(ST(KC(7)),SUM)
    1 CONTINUE
      SUM=1./SUM
      CALL UINIT
      DO 2 I=1,NJ
      CALL UREAD(7)
      CALL MEMSA2(ST(KC(7)),ST(KC(4)),SUM)
      CALL UWRITE(4)
    2 CONTINUE
      CALL UINIT
      IF(L1.GT.0) WRITE(IOUT,11) SUM
      IF(M1.EQ.0.OR.M1.EQ.2.OR.M1.EQ.4) GOTO 6
      K=ND(1)
      DO 3 I=1,K
    3 XF(I)=0.
      K=1
      IF(NDIM.GE.2) K=ND(2)
      DO 4 I=1,K
    4 YF(I)=0.
      J=1
      K=1
      DO 5 I=1,NJ
      CALL UREAD(4)
      CALL MEMSA3(ST(KC(4)),XF,YF,J,K)
    5 CONTINUE
      CALL UINIT
      CALL MEMSA4(XF,ND(1),W)
      IF(NDIM.GT.1) CALL MEMSA4(YF,ND(2),W)
    6 SUM=0.
      DO 7 I=1,NJ
      CALL UREAD(3)
      CALL MEMSA1(ST(KC(3)),SUM)
    7 CONTINUE
      CALL UINIT
      SIGNAL=SUM/(FLOAT(NJ)*FLOAT(MJ))
      IF(M0.EQ.1 .OR. M0.EQ.4) DEF=SIGNAL
      IF(M0.EQ.2 .OR. M0.EQ.5) DEF=0.5*SIGNAL
      IF(M0.GE.3) RETURN
      DO 8 I=1,NJ
      CALL MEMSA5(ST(KC(1)),DEF)
      CALL UWRITE(1)
    8 CONTINUE
      CALL UINIT
      IF(L1.GT.0) WRITE(IOUT,12) DEF
      RETURN
   10 FORMAT(8H   MemSA)
   11 FORMAT(15H PSF scaled by ,1PE10.3)
   12 FORMAT(21H Default level set = ,1PE10.3)
      END
C
      SUBROUTINE MEMSA1(X,SUM)
      DIMENSION X(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MJ
      IF(X(I).GT.0.0) SUM=SUM+X(I)
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSA2(X,Y,SCL)
      DIMENSION X(1),Y(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MJ
    1 Y(I)=X(I)*SCL
      RETURN
      END
C
      SUBROUTINE MEMSA3(B,XF,YF,J,K)
      DIMENSION B(1),XF(1),YF(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      N1=ND(1)
      DO 1 I=1,MJ
      XF(J)=XF(J)+B(I)
      YF(K)=YF(K)+B(I)
      J=J+1
      IF(J.LE.N1) GOTO 1
      J=1
      K=K+1
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSA4(F,N,W)
      DIMENSION F(1),W(1)
      DO 1 I=1,N
      W(I)=0.
      J=I
      DO 1 K=1,N
      W(I)=W(I) + F(K)*F(J)
      J=J+1
      IF(J.GT.N) J=1
    1 CONTINUE
      DO 2 I=1,N
    2 F(I)=1. - W(I)/W(1)
      RETURN
      END
C
      SUBROUTINE MEMSA5(X,D)
      DIMENSION X(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MJ
    1 X(I)=D
      RETURN
      END
C
      SUBROUTINE MEMSB(ERR)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      IF(L0.GT.0) WRITE(IOUT,10)
      XN=FLOAT(NJ)*FLOAT(MJ)
      IF(M3.EQ.2) GOTO 6
      IF(M1.EQ.2.OR.M1.EQ.3) GOTO 4
      A=4.0/(XN*XN*ERR*ERR)
      IF(M2.NE.0) GOTO 2
      K=1
      DO 1 I=1,NK
      CALL UREAD(26)
      CALL MEMSB1(ST(KC(26)),ST(KC(22)),A,K)
      CALL UWRITE(22)
    1 CONTINUE
      CALL UINIT
      RETURN
    2 CONTINUE
      K=1
      DO 3 I=1,NK
      CALL UREAD(26)
      CALL UREAD(38)
      CALL MEMSB2(ST(KC(26)),ST(KC(38)),ST(KC(22)),A,K)
      CALL UWRITE(22)
    3 CONTINUE
      CALL UINIT
      RETURN
    4 CONTINUE
      A=2.0/(XN*ERR*ERR)
      DO 5 I=1,NK
      CALL MEMSB3(ST(KC(22)),A)
      CALL UWRITE(22)
    5 CONTINUE
      CALL UINIT
      RETURN
    6 CONTINUE
      XN=0.0
      DO 7 I=1,NK
      CALL UREAD(21)
      CALL MEMSB4(ST(KC(21)),XN)
    7 CONTINUE
      CALL UINIT
      IF(L1.GT.0) WRITE(IOUT,11) XN
      XN=2./XN
      DO 8 I=1,NK
      CALL UREAD(21)
      CALL MEMSB5(ST(KC(21)),ST(KC(22)),XN)
      CALL UWRITE(22)
    8 CONTINUE
      CALL UINIT
      RETURN
   10 FORMAT(8H   MemSB)
   11 FORMAT(38H Number of pixels with finite errors = ,F9.0)
      END
C
      SUBROUTINE MEMSB1(X,Y,A,KK)
      DIMENSION X(1),Y(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
C Fourier dependent
C For FOUR2 format (M4=0) MK must be a multiple of N(1)+2
C For packed format (i.e. M4=1) all the reals must be in the first block
      INCR=2
      NHALF=2
      IF(M4.EQ.1) NHALF=2*NC(KK)
      DO 2 I=1,MK,2
      J=I+1
      T1=A*(X(I)*X(I) + X(J)*X(J))
      T2=T1
      IF(J.NE.NHALF) GOTO 1
      T1=0.5*T1
      T2=T1
      INCR=ND(1)+2-INCR
      NHALF=NHALF+INCR
      IF(M4.EQ.0) GOTO 1
      KK=KK+1
      NHALF=2*NC(KK)
      T1=0.5*A*X(I)*X(I)
      T2=0.5*A*X(J)*X(J)
    1 Y(I)=T1
      Y(J)=T2
    2 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSB2(X,W,Y,A,KK)
      DIMENSION  X(1),W(1),Y(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
C Fourier dependent
C For FOUR2 format (M4=0) MK must be a multiple of N(1)+2
C For packed format (i.e. M4=1) all the reals must be in the first block
      INCR=2
      NHALF=2
      IF(M4.EQ.1) NHALF=2*NC(KK)
      DO 2 I=1,MK,2
      J=I+1
      T1=A*(X(I)*X(I) + X(J)*X(J))*W(I)
      T2=T1
      IF(J.NE.NHALF) GOTO 1
      T1=0.5*T1
      T2=T1
      INCR=ND(1)+2-INCR
      NHALF=NHALF+INCR
      IF(M4.EQ.0) GOTO 1
      KK=KK+1
      NHALF=2*NC(KK)
      T1=0.5*A*X(I)*X(I)*W(I)
      T2=0.5*A*X(J)*X(J)*W(J)
    1 Y(I)=T1
      Y(J)=T2
    2 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSB3(X,A)
      DIMENSION X(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MJ
    1 X(I)=A
      RETURN
      END
C
      SUBROUTINE MEMSB4(X,XN)
      DIMENSION X(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MK
      IF(X(I).GT.0.) XN=XN+1.
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSB5(X,Y,SCL)
      DIMENSION X(1),Y(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      DO 1 I=1,MK
      Y(I)=X(I)*SCL
      IF(Y(I).LT.0.) Y(I)=0.
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSC
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      IF(L0.GT.0) WRITE(IOUT,10)
      L=1
      DO 1 I=1,NK
      CALL UREAD(26)
      CALL UREAD(27)
      CALL MEMSC1(ST(KC(26)),ST(KC(27)),ST(KC(25)),L)
      CALL UWRITE(25)
    1 CONTINUE
      CALL UINIT
      RETURN
   10 FORMAT(8H   MemSC)
      END
C
      SUBROUTINE MEMSC1(X,Y,Z,L)
      DIMENSION X(1),Y(1),Z(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      A=1.0/(FLOAT(NJ)*FLOAT(MJ))
C Fourier dependent
C For packed format (i.e. M4=1) all the reals must be in the first block
      NREAL=0
      IF(M4.EQ.1) NREAL=2*NC(L)
      DO 2 I=1,MK,2
      J=I+1
      T1=A*(X(I)*Y(I) - X(J)*Y(J))
      T2=A*(X(J)*Y(I) + X(I)*Y(J))
      IF(J.NE.NREAL) GOTO 1
      L=L+1
      NREAL=2*NC(L)
      T1=A*X(I)*Y(I)
      T2=A*X(J)*Y(J)
    1 Z(I)=T1
      Z(J)=T2
    2 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSD(XF,YF)
      DIMENSION XF(1),YF(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      IF(L0.GT.0) WRITE(IOUT,10)
      J=1
      K=1
      DO 1 I=1,NJ
      CALL UREAD(3)
      CALL UREAD(6)
      CALL MEMSD1(ST(KC(3)),ST(KC(6)),ST(KC(4)),XF,YF,J,K)
      CALL UWRITE(4)
    1 CONTINUE
      CALL UINIT
      RETURN
   10 FORMAT(8H   MemSD)
      END
C
      SUBROUTINE MEMSD1(D,DB,DZ,XF,YF,JJ,KK)
      DIMENSION D(1),DB(1),DZ(1),XF(1),YF(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      N1=ND(1)
      DO 1 I=1,MJ
      DZ(I)=DB(I) + XF(JJ)*YF(KK)*(D(I)-DB(I))
      JJ=JJ+1
      IF(JJ.LE.N1) GOTO 1
      JJ=1
      KK=KK+1
    1 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEMSE
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      IF(L0.GT.0) WRITE(IOUT,10)
      L=1
      DO 1 I=1,NK
      CALL UREAD(26)
      CALL UREAD(28)
      CALL MEMSE1(ST(KC(28)),ST(KC(26)),ST(KC(21)),L)
      CALL UWRITE(21)
    1 CONTINUE
      CALL UINIT
      RETURN
   10 FORMAT(8H   MemSE)
      END
C
      SUBROUTINE MEMSE1(D,B,Z,L)
      DIMENSION D(1),B(1),Z(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFT,NDIM,N(3),NC(5)
      A=1.0E-12
C Fourier dependent
C For packed format (i.e. M4=1) all the reals must be in the first block
      NREAL=0
      IF(M4.EQ.1) NREAL=2*NC(L)
      DO 2 I=1,MK,2
      J=I+1
      T=1.0/(B(I)*B(I) + B(J)*B(J) + A)
      T1=(D(I)*B(I) + D(J)*B(J))*T
      T2=(D(J)*B(I) - D(I)*B(J))*T
      IF(J.NE.NREAL) GOTO 1
      L=L+1
      NREAL=2*NC(L)
      T1=D(I)/(B(I)+A)
      T2=D(J)/(B(J)+A)
    1 Z(I)=T1
      Z(J)=T2
    2 CONTINUE
      RETURN
      END
C
      SUBROUTINE MEFFT(K,L)
      COMMON /MECOM/ C,TEST,CNEW,ALF,C0,RATE,DEFLOG,FLOOR,SUMF,SUMFLF,
     * SS,SC,CC,SBS,SBC,CBC,SBBS,SBBC,CBBC,SBBBS,SBBBC,CBBBC,
     * S4S,S4C,C4C,S5S,S5C,C5C,SBQ,QBQ,SBBQ,QBBQ,QBBBQ,
     * O1,O2,Z1,Z2,Z3,Z4,W1,W2,W3,W4,W5,W6,PR(40)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      DATA RP,WP/1HR,1HW/
      IF(L0.GE.1) WRITE(IOUT,5)
      PR(K)=RP
      PR(L)=WP
      IFACT=1
      KK=K
      LL=L
      CALL UFFT(KK,LL,IFACT)
      CALL UINIT
      RETURN
    5 FORMAT(14H   Forward FFT)
      END
C
      SUBROUTINE MEIFT(K,L)
      COMMON /MECOM/ C,TEST,CNEW,ALF,C0,RATE,DEFLOG,FLOOR,SUMF,SUMFLF,
     * SS,SC,CC,SBS,SBC,CBC,SBBS,SBBC,CBBC,SBBBS,SBBBC,CBBBC,
     * S4S,S4C,C4C,S5S,S5C,C5C,SBQ,QBQ,SBBQ,QBBQ,QBBBQ,
     * O1,O2,Z1,Z2,Z3,Z4,W1,W2,W3,W4,W5,W6,PR(40)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      DATA RP,WP/1HR,1HW/
      IF(L0.GE.1) WRITE(IOUT,5)
      PR(K)=RP
      PR(L)=WP
      IFACT=-1
      KK=K
      LL=L
      CALL UFFT(KK,LL,IFACT)
      CALL UINIT
      RETURN
    5 FORMAT(14H   Inverse FFT)
      END
