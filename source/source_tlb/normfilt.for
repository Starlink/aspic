C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     **********************
C                     *                    *
C                     * Program   NORMFILT *
C                     *                    *
C                     **********************
C
C
C
C          CALLING SEQUENCE:-
C               NORMFILT
C
C
C          FUNCTION:-
C               This program applies  the  Normalised  Tuned  Filter  to  a
C               Starlink  image.  It  is  the  same as CONV except that the
C               result of the convolution is divided by the sum of the data
C               values in the region being convolved.
C
C
C          USE:-
C               It was designed to be used with a  point  spread  function,
C               such  as a star profile, as a means of using the concept of
C               a "tuned" filter to find the positions of all eg. stars  in
C               an  image.  The  technique  is  described in more detail in
C               RGO/LGP 5.1.
C
C
C
C         USER PARAMETERS:-
C
C         IN                                  The input 2-d Starlink image.
C
C         CNVFN                               This is the  name  of  a  2-d
C                                             Starlink    image    of   ODD
C                                             dimensions which is  used  as
C                                             the convolving function.
C
C         OUT                                 The   output   2-d   Starlink
C                                             image.
C
C
C         K F Hartley              RGO                             7-JAN-82
C
C
C--------------------------------------------------------------------------



*  IMAGEIN IS CONVOLVED WITH IMAGEPOINT AND THE RESULT
*  RETURNED IN IMAGEOUT
*
C      THIS VERSION HAS BEEN MODIFIED BY K F HARTLEY AT RGO
C      TO DIVIDE THE CONVOLVED IMAGE BY THE SQUARE ROOT
C      OF THE SUMS OF THE SQUARES OF THE IMAGE VALUES IN
C      THE REGION WHICH HAS BEEN CONVOLVED.
C
C      THIS CONVERTS THE PROGRAM INTO A "NORMALIZED TUNED FILTER"
C      IF THE CONVOLVING FUNCTION IS A TEMPLATE , SUCH AS
C      THE MEAN PROFILE OF A STAR.
C
*  IMAGEPOINT MUST HAVE ODD DIMENSIONS.

      INTEGER IDIMNA(2),IDIMNB(2),IDIMNC(2)
      EQUIVALENCE (IDIMNA(1),NXA), (IDIMNA(2),NYA)
      EQUIVALENCE (IDIMNB(1),NXB), (IDIMNB(2),NYB)
      EQUIVALENCE (IDIMNC(1),NXC), (IDIMNC(2),NYC)

      INCLUDE 'INTERIM(FMTPAR)'

      CALL RDIMAG('IN',FMT_R,2,IDIMNA,NDIMSA,IPA,JSTAT)
      IF (NDIMSA.NE.2) GO TO 9010

      CALL RDIMAG('CNVFN',FMT_R,2,IDIMNB,NDIMSB,IPB,JSTAT)
      IF (NDIMSB.NE.2) GO TO 9010

      NW=1
      DO I=1,2
         NA=IDIMNA(I)
         IF (NA.LT.1) GO TO 9020
         NB=IDIMNB(I)
         IF (NB.LT.1) GO TO 9030
         IF (MOD(NB,2).NE.1) GO TO 9040
         M=NB-1
         NW=NW*(NA+M)
         NC=NA-M
         IF (NC.LE.1) GO TO 9020
         IDIMNC(I)=NC
      END DO

      CALL GETDYN('WK',FMT_R,NW,IPW,JSTAT)
      CALL GETDYN('WK2',FMT_R,NW,IPS,JSTAT)

      CALL WRIMAG('OUT',FMT_R,IDIMNC,2,IPC,JSTAT)

      CALL CONV(%VAL(IPA),NXA,NYA,
     &           %VAL(IPB),NXB,NYB,
     &           %VAL(IPW),
     $           %VAL(IPS),
     &           %VAL(IPC))
      GO TO 9999

 9010 CONTINUE
      CALL WRUSER('ONLY 2-D IMAGES PERMITTED!',JSTAT)
      GO TO 9999

 9020 CONTINUE
      CALL WRUSER('IN TOO SMALL!',JSTAT)
      GO TO 9999

 9030 CONTINUE
      CALL WRUSER('CNVFN TOO SMALL!',JSTAT)
      GO TO 9999

 9040 CONTINUE
      CALL WRUSER('CNVFN DIMNS MUST BE ODD!',JSTAT)

 9999 CONTINUE
      CALL FRDATA(' ',JSTAT)
      END
      SUBROUTINE CONV(A,NXA,NYA,B,NXB,NYB,W,S,C)

      REAL A(NXA,NYA),B(NXB,NYB),
     &     W(NXA+NXB-1,NYA+NYB-1),C(NXA-NXB+1,NYA-NYB+1),
     &     S(NXA+NXB-1,NYA+NYB-1)

*  ZERO WORK ARRAY
      DO IYW=1,NYA+NYB-1
         DO IXW=1,NXA+NXB-1
            W(IXW,IYW)=0.0
            S(IXW,IYW)=0.0
         END DO
      END DO

*  CONVOLVE
      DO IYA=1,NYA
         MY=IYA-1
         DO IXA=1,NXA
            MX=IXA-1
            V=A(IXA,IYA)
            DO IYB=1,NYB
               IYW=IYB+MY
               DO IXB=1,NXB
                  IXW=IXB+MX
                  W(IXW,IYW)=W(IXW,IYW)+V*B(IXB,IYB)
                  S(IXW,IYW)=S(IXW,IYW)+V*V
               END DO
            END DO
         END DO
      END DO

*  COPY OUT VALID REGION
*   AND NORMALIZE IT WITH S
*   AND RESCALE TO REASONABLE RANGE FOR POSSIBLE INTEGERIZATION
      DO IYC=1,NYA-NYB+1
         IYW=IYC+NYB-1
         DO IXC=1,NXA-NXB+1
            IXW=IXC+NXB-1
            IF (S(IXW,IYW).NE.0.0) THEN
               C(IXC,IYC)=W(IXW,IYW)*255.0/SQRT(S(IXW,IYW))
            ELSE
               C(IXC,IYC)=0.0
            END IF
         END DO
      END DO

      END
