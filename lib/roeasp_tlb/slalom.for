      SUBROUTINE SLALOM (XV,YV,WTS,NP,XIN,NIN,YOUT)
*+
*   SLALOM
*
*   Curve fitting which attempts to give an 'eye' fit to a fairly
*   small number of points on a fairly smooth curve.  Developed for
*   fitting to the flux sensitivity curve in ESP.
*
*   Given (arguments):
*     XV    RA     array of X values for fitting (may be trampled on)
*     YV    RA       "      Y    "    "     "    (may be trampled on)
*     WTS   IA     array of 'weights' - 0 or 1   (may be trampled on)
*     NP    I      size of XV,YV,WTS
*     XIN   RA     array of X values for which Y is required
*     NIN   I      size of XIN,YOUT
*
*   Returned (argument):
*     YOUT  RA     array of Y values calculated for each XIN
*
*   Calls:
*     GPPFIT
*
*   Author:
*     J A Cooke/UOE/27Aug82
*-
      PARAMETER PI=3.1415927

      INTEGER NP,NIN
      REAL XV(NP),YV(NP),XIN(NIN),YOUT(NIN)
      INTEGER WTS(NP)
      INTEGER K,J,I,NV
      REAL XMAX,X,Y1,Y2,THETA,WEIGHT
      REAL A,B,C,G,A1,B1,C1,G1,A2,B2,C2,G2

*   delete unwanted points and rearrange data.....
      K=0
      DO J=1,NP
         IF (WTS(J).NE.0) THEN
            K=K+1
            XV(K)=XV(J)
            YV(K)=YV(J)
            WTS(K)=WTS(J)
         ENDIF
      ENDDO
*   K is now the number of wanted points

*   obtain YOUT for each XIN.....
      NV=0
      XMAX=XIN(1)-1.
      DO I=1,NIN
         X=XIN(I)
         IF (X.GT.XMAX) THEN
*         obtain new fit parameters.....
            NV=NV+1
            IF (NV.EQ.1) THEN
*            front end.....
               G=(YV(3)-YV(1))/(XV(3)-XV(1))
               CALL GPPFIT(XV(1),YV(1),XV(2),YV(2),G,A,B,C)
               XMAX=XV(2)
            ELSE IF (NV.EQ.(K-1)) THEN
*            back end.....
               G=(YV(K)-YV(K-2))/(XV(K)-XV(K-2))
               CALL GPPFIT(XV(K),YV(K),XV(K-1),YV(K-1),G,A,B,C)
               XMAX=XIN(NIN)
            ELSE
*            middle.....
               G1=(YV(NV+1)-YV(NV-1))/(XV(NV+1)-XV(NV-1))
               CALL GPPFIT(XV(NV+1),YV(NV+1),XV(NV),YV(NV),
     -          G1,A1,B1,C1)
               G2=(YV(NV+2)-YV(NV))/(XV(NV+2)-XV(NV))
               CALL GPPFIT(XV(NV),YV(NV),XV(NV+1),YV(NV+1),
     -          G2,A2,B2,C2)
               XMAX=XV(NV+1)
            ENDIF
         ENDIF

*      have the required fit parameters.....
         IF ((NV.EQ.1) .OR. (NV.EQ.K-1)) THEN
*         front end or back end.....
            YOUT(I)=A+B*X+C*X*X
         ELSE
*         middle, need cosine weighting.....
            Y1=A1+B1*X+C1*X*X
            Y2=A2+B2*X+C2*X*X
            THETA=((X-XV(NV))/(XV(NV+1)-XV(NV)))*PI
            WEIGHT=(COS(THETA)+1.)/2.
            YOUT(I)=WEIGHT*Y1+(1.-WEIGHT)*Y2
         ENDIF
      ENDDO

      END
