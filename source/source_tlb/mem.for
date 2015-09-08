C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                      *****************
C                      *               *
C                      * Program   MEM *
C                      *               *
C                      *****************
C
C
C
C           CALLING SEQUENCE:-
C                MEM [/ARGS=YES]
C
C
C           FUNCTION:-
C                A maximum entropy deconvolution program for 2D images.
C
C
C           USE:-
C                Given a blurred image, the  blurring  function  (which  may
C                have  been  estimated  from  the  image,  e.g. from stellar
C                profiles) and the noise levels of the  image,  the  program
C                reconstructs  the  most  uniform  image consistent with the
C                data. See Starlink User Note 34 for more details.
C
C
C
C          USER PARAMETERS:-
C
C          ARGS            FALSE               Controls whether any  display
C                                              is   produced   on  the  ARGS
C                                              during program execution.  If
C                                              this  parameter  is set, then
C                                              each iteration  is  displayed
C                                              on the ARGS.
C
C          DATA                                This specifies  the  Starlink
C                                              BDF  file containing the data
C                                              (i.e. the blurred image).
C
C          DEFAULT         0.5*mean level      This is the  level  to  which
C                                              the   image   tends   in  the
C                                              absence  of  any  data.   Its
C                                              value should be roughly equal
C                                              to the total amount of signal
C                                              on  the image spread over all
C                                              the pixels. The value of this
C                                              parameter  is  not  critical:
C                                              any  sensible  value   should
C                                              give  a  good reconstruction.
C                                              By default the  program  uses
C                                              half  the  mean  intensity of
C                                              the blurred image. See SUN 34
C                                              for more details.
C
C
C
C          ERRFILE                             This is the name of  the  BDF
C                                              file  containing a map of the
C                                              noise levels  in  the  image.
C                                              Each   pixel   in  this  file
C                                              should  contain  the  inverse
C                                              variance on the signal in the
C                                              corresponding  pixel  of  the
C                                              input data. This file is only
C                                              needed if  the  data  do  not
C                                              have  a  uniform  noise level
C                                              across the whole  image  (see
C                                              parameter NOISE below).
C
C          NITER           1                   This specifies the number  of
C                                              iterations  that  the program
C                                              is to run for. Values between
C                                              10   and   20   are   usually
C                                              adequate for most images.
C
C          NOISE                               This  specifies   a   uniform
C                                              noise  level across the whole
C                                              image.  This  value  is   the
C                                              standard   deviation  of  the
C                                              signal in each pixel  of  the
C                                              input    image.    If    this
C                                              parameter is unset  then  the
C                                              user  is  prompted for a file
C                                              containing a map of the noise
C                                              levels (see ERRFILE above).
C
C          OUTPUT                              This is the name of  the  BDF
C                                              file   to  which  the  output
C                                              image  is  written.   It   is
C                                              updated each iteration.
C
C          PSF                                 This is the name of  the  BDF
C                                              file      containing      the
C                                              point-spread function of  the
C                                              observations.  This file must
C                                              be the same size as the  data
C                                              file,   and   the  psf  wraps
C                                              around from the  top  of  the
C                                              image  to the bottom and from
C                                              the left to the right. It  is
C                                              not  important where the peak
C                                              of the psf is placed  as  the
C                                              program  will  move it to the
C                                              appropriate position.
C
C          START                               This specifies the name of  a
C                                              BDF  file containing an image
C                                              to  be  used  as  the   first
C                                              iteration   of  the  program.
C                                              This  file  should   be   the
C                                              output from an earlier run of
C                                              the program, and  allows  the
C                                              user   to   run  the  program
C                                              several   times   (e.g.    as
C                                              several   batch   jobs)    to
C                                              accumulate the desired number
C                                              of iterations.
C
C          WKFILE                              This specifies the name of  a
C                                              disc   file  to  be  used  as
C                                              workspace. This file is large
C                                              (10  images)  and  so  should
C                                              reside on a disc without disc
C                                              quotas in force. This file is
C                                              automatically deleted at  the
C                                              end of the program.
C
C
C
C John Fielden & Steve Gull         Cambridge                      28-AUG-83
C
C
C---------------------------------------------------------------------------
C
      BLOCK DATA
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMF/ IFACT,NDIM,NN(3),NC(5)
      DATA NJ,MJ,NK,MK / 0,4096,0,4096 /
      DATA KA / 1,1,2,3,4,4,4,13*-10,
     * 5,6,7,7,7,8,7,7,10*-10,9,10 /
      DATA KB / 1,1,2,3,4,4,4,13*-10,
     * 1,2,4,4,4,3,4,1,10*-10,2,2 /
      DATA IOUT/6/
      DATA L0,L1,M0,M1,M2,M3,M4 / 7*0 /
      DATA IFACT,NDIM,NN,NC / 1,2,0,0,1,5*0 /
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Starlink maximum-entropy deconvolution program
C
C     Written by John Fielden   April 1982, Starlink, Cambridge
C     Modified and installed in ASPIC
C                John Fielden & Steve Gull July 1983
C
C     Copyright (C) MEDC Ltd. 1982,1983
C
C     Uses MEDC Ltd. MEM package
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
C This program works only on disc
C Space is reserved for 4 buffers
      COMMON /MECOMS/ ST(16384)
      COMMON /MECOMF/ IFT,NDIM,NN(3),NC(5)
      COMMON /OPCOMP/ ISW
      DIMENSION W(4097)
      LOGICAL DISP,FLAT,VARERR,ONLINE
C Fourier format, and edging on
      NOPT=10010
      CALL COPY('Copyright (C) MEDC Ltd. 1982')
      CALL WRUSER('0Maximum entropy deconvolution program',ISTAT)
      ONLINE=.FALSE.
      CALL ONTEST(ONLINE)
      LEVEL=10
      IF(ONLINE) LEVEL=11
      CALL UINPUT(DISP,NITER,FLAT,ERR,VARERR,DEF,SCALE,NUM)
      IF(DEF.LE.0.0) NOPT=NOPT+2
      IF(.NOT.FLAT) NOPT=NOPT+3
      IF(VARERR) NOPT=NOPT+2020
      ISW=0
      CALL MEMST(NOPT,LEVEL,DEF,ERR,W)
      IF(VARERR) ISW=1
      CALL WRUSER(' Start routines completed',ISTAT)
      L0=0
      L1=0
      IF(M1.EQ.1 .AND. DISP) THEN
        NUM1=MAX(NUM-2,1)
        CALL UMAP(4,NUM1,SCALE)
        CALL WRUSER(' Edged data displayed',ISTAT)
      ENDIF
C      IF(ONLINE) CALL DIAG
      ITER=0
      CALL UOUT
      IF(DISP) THEN
        CALL UMAP(1,1,SCALE)
        WRITE(IOUT,2001) ITER
      ENDIF
      METHOD=3
C The MEM loop
      IF(NITER.EQ.0) GOTO 200
      DO 100 ITER=1,NITER
      CALL XMEM(METHOD,LEVEL,1.,0.2,DEF)
      CALL UOUT
      WRITE(IOUT,2000) ITER
      IF(DISP) THEN
        NUM1=MIN(ITER+1,NUM)
        CALL UMAP(1,NUM1,SCALE)
        WRITE(IOUT,2001) ITER
C        IF(ONLINE) CALL DIAG
      ENDIF
  100 CONTINUE
C
  200 CONTINUE
      CALL FRDATA(' ',ISTAT)
      STOP
 2000 FORMAT(' Iteration ',I3,' completed')
 2001 FORMAT(' Maximum entropy iterate ',I3,' displayed')
      END
C
      SUBROUTINE OPUS(J,K)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      COMMON /OPCOMP/ ISW
      IF(ISW.NE.0) GOTO 1
C Fourier transform OPUS
      CALL UFFT(J,K,1)
      RETURN
    1 CONTINUE
C Convolution
      CALL UFFT(J,K,1)
      II=1
      DO 2 I=1,NK
      CALL UREAD(K)
      CALL UREAD(39)
      CALL OP1(ST(KC(K)),ST(KC(39)),ST(KC(40)),II)
      CALL UWRITE(40)
    2 CONTINUE
      CALL UINIT
      CALL UFFT(40,K,-1)
      RETURN
      END
C
      SUBROUTINE OP1(X,Y,Z,L)
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
      SUBROUTINE TROPUS(K,J)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      COMMON /OPCOMP/ ISW
      IF(ISW.NE.0) GOTO 1
C Transpose Fourier transform
      CALL UFFT(K,J,-2)
      RETURN
    1 CONTINUE
C Correlation
      CALL UFFT(K,J,1)
      II=1
      DO 2 I=1,NK
      CALL UREAD(J)
      CALL UREAD(39)
      CALL TROP1(ST(KC(J)),ST(KC(39)),ST(KC(40)),II)
      CALL UWRITE(40)
    2 CONTINUE
      CALL UINIT
      CALL UFFT(40,J,-1)
      RETURN
      END
C
      SUBROUTINE TROP1(X,Y,Z,L)
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
      T1=A*(X(I)*Y(I) + X(J)*Y(J))
      T2=A*(X(J)*Y(I) - X(I)*Y(J))
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
      SUBROUTINE UFFT(J,K,IFT)
      COMMON /MECOMF/ IFACT,NDIM,ND(3),NC(5)
      COMMON /OPCOMS/ WORK(16384),CORE(4096),BUFF(1024)
      DIMENSION M(3)
      DATA M/16384,4096,1024/
      CALL FFTV(J,K,K,WORK,CORE,BUFF,ND,M,IFT)
      RETURN
      END
C
      SUBROUTINE UOUT
      COMMON /UCOMP/ IFRAME
      CALL UOUT1(%val(IFRAME))
      RETURN
      END
C
      SUBROUTINE UOUT1(FRAME)
      DIMENSION FRAME(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      K=1
      DO 1 I=1,NJ
      CALL UREAD(1)
      CALL UOUT2(ST(KC(1)),FRAME(K),MJ)
    1 K=K+MJ
      CALL UINIT
      RETURN
      END
C
      SUBROUTINE UOUT2(X,Y,M)
      DIMENSION X(1),Y(1)
      DO 1 I=1,M
    1 Y(I)=X(I)
      RETURN
      END
C
      SUBROUTINE ONTEST(ONLINE)
C Simple routine to determine whether a program is running online or not.
      LOGICAL ONLINE
      INTEGER SYS$GETJPI
      DIMENSION ITLIST(4),IBUFF(2)
      INTEGER*2 LEN
      DATA ITLIST(1) /'031D0008'X/
      DATA ITLIST(4) /0/
      ITLIST(2)=%loc(IBUFF)
      ITLIST(3)=%loc(LEN)
      II=SYS$GETJPI(,,,ITLIST,,,)
      ONLINE=LEN.GT.0
      RETURN
      END
C
      SUBROUTINE COPY(ITEXT)
      DIMENSION ITEXT(1)
      RETURN
      END
C
      SUBROUTINE FTURD(I,M,L,C)
      DIMENSION C(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      MR=2*M
      IF(KA(I).EQ.0) GOTO 1
C Disc
      IVBN=KD(I) + (L-1)*(M/64)
      CALL UGET(IVBN,C,MR)
      RETURN
C Core
    1 CONTINUE
      J=KB(I) + (L-1)*MR
      CALL FTU1(ST(J),C,M)
      RETURN
      END
C
      SUBROUTINE FTUWR(I,M,L,C)
      DIMENSION  C(1)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      MR=2*M
      IF(KA(I).EQ.0) GOTO 1
C Disc
      IVBN=KD(I) + (L-1)*(M/64)
      CALL UPUT(IVBN,C,MR)
      RETURN
C Core
    1 CONTINUE
      J=KB(I) + (L-1)*MR
      CALL FTU1(C,ST(J),M)
      RETURN
      END
C
      SUBROUTINE FTU1(X,Y,M)
      COMPLEX X(1),Y(1)
      DO 1 I=1,M
    1 Y(I)=X(I)
      RETURN
      END
C
      SUBROUTINE UREAD(I)
      COMMON /MECOM/ C,TEST,CNEW,ALF,C0,RATE,DEFLOG,FLOOR,SUMF,SUMFLF,
     * SS,SC,CC,SBS,SBC,CBC,SBBS,SBBC,CBBC,SBBBS,SBBBC,CBBBC,
     * S4S,S4C,C4C,S5S,S5C,C5C,SBQ,QBQ,SBBQ,QBBQ,QBBBQ,
     * O1,O2,Z1,Z2,Z3,Z4,W1,W2,W3,W4,W5,W6,PR(40)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      DATA RP/1HR/
      PR(I)=RP
      M=MJ
      IF(I.GT.20) M=MK
      IF(KA(I).EQ.0) GOTO 1
C Disc
      CALL UGET(KD(I),ST(KC(I)),M)
      KD(I)=KD(I)+M/128
      RETURN
C Core
    1 CONTINUE
      KC(I)=KD(I)
      KD(I)=KD(I)+M
      RETURN
      END
C
      SUBROUTINE UWRITE(I)
      COMMON /MECOM/ C,TEST,CNEW,ALF,C0,RATE,DEFLOG,FLOOR,SUMF,SUMFLF,
     * SS,SC,CC,SBS,SBC,CBC,SBBS,SBBC,CBBC,SBBBS,SBBBC,CBBBC,
     * S4S,S4C,C4C,S5S,S5C,C5C,SBQ,QBQ,SBBQ,QBBQ,QBBBQ,
     * O1,O2,Z1,Z2,Z3,Z4,W1,W2,W3,W4,W5,W6,PR(40)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      COMMON /MECOMS/ ST(1)
      DATA RP/1HW/
      PR(I)=RP
      M=MJ
      IF(I.GT.20) M=MK
      IF(KA(I).EQ.0) GOTO 1
C Disc
      CALL UPUT(KD(I),ST(KC(I)),M)
      KD(I)=KD(I)+M/128
      RETURN
C Core
    1 CONTINUE
      KD(I)=KD(I)+M
      KC(I)=KD(I)
      RETURN
      END
C
      SUBROUTINE UWIND(I)
      COMMON /MECOMP/ NJ,MJ,NK,MK,KA(40),KB(40),KC(40),KD(40),
     * IOUT,L0,L1,M0,M1,M2,M3,M4
      KD(I)=(KA(I)-1)*NK*(MK/128) + 1
      RETURN
      END

