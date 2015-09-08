C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   XYSTAT  *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               XYSTAT
C
C
C          FUNCTION:-
C              Finds statistics for 1 or 2 sets of numbers taken from
C              XYlists.
C              The numbers are taken from file A, parameter no A1 and, if
C              wanted from file
C              B, parameter no B1, where A,A1,B,B1 are input by the user.
C
C              The statistics worked out are mean, standard deviation,
C              standard deviation of mean for each file and the
C              linear regression equation Y = A + B.X factors A and B
C              and their errors and the mean Y error from the line. This
C              assumes that the X errors are negligible.
C
C
C          USE:-
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              The 1st input XY list
C
C         PARNOA                              The paramter number in the
C                                             first file to take the
C                                             X values from.
C
C         INPUTB                              The 2nd input file.
C
C         PARNOB                              The parameter number in the
C                                             second file to take the Y
C                                             values from.
C
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C
C
C
C
C         A.J.Penny                RGO                            82-11-1
C
C
C--------------------------------------------------------------------------



C
C
C
      LOGICAL VALID,LOOP
      CHARACTER*72 TEXT
      INTEGER TEXTA(15)
      EQUIVALENCE (TEXTA(1),TEXT)
      REAL SIZE(2)
      CHARACTER CVAL*1
C
C
C
      VALID = .TRUE.
C
C Open input files
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEMA,LSTLA,IPINA,IERRA)
      IF (IERRA.NE.0) THEN
         CALL WRUSER('BAD FILE',ISTAT)
         VALID = .FALSE.
      ENDIF
      IF (VALID) THEN
         ALIMIT = NITEMA - 5
         CALL GETPAR('PARNOA','INTEGER',1,1.0,ALIMIT,.FALSE.,KPARA,
     +               RVAL,IERRB)
          IF (IERRB.NE.0) THEN
             CALL WRUSER('BAD PARAMETER',ISTAT)
             VALID = .FALSE.
          ENDIF
      ENDIF
      IF (VALID) THEN
         CALL GTXYLR('INPUTB',.TRUE.,NITEMB,LSTLB,IPINB,IERRC)
         IF (IERRC.NE.0) THEN
            NPAR = 1
         ELSE
            NPAR = 2
            IF (LSTLB.NE.LSTLA) THEN
               CALL WRUSER('FILES DIFFERENT LENGTHS',ISTAT)
               VALID = .FALSE.
            ELSE
               ALIMIT = NITEMB - 5
               CALL GETPAR('PARNOB','INTEGER',1,1.0,ALIMIT,.FALSE.,
     +                     KPARB,RVAL,IERRD)
               IF (IERRD.NE.0) THEN
                  CALL WRUSER('BAD PARAMETER',ISTAT)
                  VALID = .FALSE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C  Extract data
C
      IF (VALID) THEN
         CALL GETDYN('IWX',204,LSTLA,IPX,IERR)
         IF (IERR.NE.0) THEN
             CALL WRUSER('CANT GET WORKING SPACE',ISTAT)
             VALID = .FALSE.
         ELSE
            CALL GETDAT(%VAL(IPINA),NITEMA,LSTLA,KPARA,%VAL(IPX))
            IF (NPAR.EQ.2) THEN
               CALL GETDYN('IWY',204,LSTLA,IPY,IERRA)
               IF (IERRA.NE.0) THEN
                  CALL WRUSER('CANT GET WORKING SPACE',ISTAT)
                  VALID = .FALSE.
               ELSE
                  CALL GETDAT(%VAL(IPINB),NITEMB,LSTLA,KPARB,
     +                        %VAL(IPY))
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C  Do statistics
C
      IF (VALID) THEN
         IF (NPAR.EQ.1) THEN
            CALL WRUSER('FOR FILE A, USED AS X',ISTAT)
            CALL DOSTA(%VAL(IPX),LSTLA)
         ELSE
            CALL WRUSER('FOR FILE A,USED AS X',ISTAT)
            CALL DOSTA(%VAL(IPX),LSTLA)
            CALL WRUSER('FOR FILE B,USED AS Y',ISTAT)
            CALL DOSTA(%VAL(IPY),LSTLA)
            CALL WRUSER('FOR JOINT DATA',ISTAT)
            CALL DOSTB(%VAL(IPX),%VAL(IPY),LSTLA)
         ENDIF
      ENDIF
C
C  Free data areas
C
      CALL FRDATA(' ',JSTAT)
C
C
C
	END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOSTA  *
C      *            *
C      **************
C
C
C AJPENNY             RGO                    83-1-11
C -------------------------------------------------------------
C
C
C
      SUBROUTINE DOSTA(X,NX)
C
C
C
      REAL X(NX)
      DOUBLE PRECISION ANX,S,SS,SD,ASD
      CHARACTER*72 TEXT
C
C
C
      S = 0.0
      SS = 0.0
      DO K = 1,NX
         S = S + X(K)
         SS = SS + X(K)*X(K)
      ENDDO
C
C
C
      AM = 0.0
      SD = 0.0
      ASD = 0.0
      ANX = NX
      IF (NX.GE.1) THEN
         AM = S/ANX
         IF (NX.GE.2) THEN
            C = SS - ANX*AM*AM
            IF (C.GT.0.0) THEN
               SD = SQRT(C/(ANX-1.0))
            ELSE
               SD = 0.0
            ENDIF
            IF (NX.GE.3) THEN
               ASD = SD/SQRT(ANX-2.0)
            ENDIF
         ENDIF
      ENDIF
C
C
C
      WRITE(TEXT,900)NX
  900 FORMAT(' ','NO OF POINTS=        ',I7)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,901)AM
  901 FORMAT(' ','MEAN =               ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,902)SD
  902 FORMAT(' ','STD DEV OF A POINT = ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,903)ASD
  903 FORMAT(' ','STD DEV OF MEAN=     ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DOSTB  *
C      *            *
C      **************
C
C
C AJPENNY             RGO                    83-1-11
C -------------------------------------------------------------
C
C
C
      SUBROUTINE DOSTB(X,Y,NX)
C
C
C
      REAL X(NX),Y(NX)
      DOUBLE PRECISION ANX,SX,SY,SSX,SSY,SXY,TX,TY
      CHARACTER*72 TEXT
C
C
C
C
      SX = 0.0
      SY = 0.0
      SSX = 0.0
      SSY = 0.0
      SXY = 0.0
      TX = 0.0
      TY = 0.0
      ANX = NX
      IF (NX.GE.1) THEN
         DO K = 1,NX
            TX = TX + X(K)
            TY = TY + Y(K)
         ENDDO
         AX = TX/ANX
         AY = TY/ANX
          DO K = 1,NX
            SX = SX + (X(K)-AX)
            SY = SY + (Y(K)-AY)
            SSX = SSX + (X(K)-AX)*(X(K)-AX)
            SSY = SSY + (Y(K)-AY)*(Y(K)-AY)
            SXY = SXY + (X(K)-AX)*(Y(K)-AY)
         ENDDO
         B = SXY/SSX
         A = AY - B*AX
      ELSE
         A = 0.0
         B = 0.0
      ENDIF
C
C
C
      CORRL = 0.0
      IF (NX.GE.2) THEN
         IF (SSX.GT.0.0.AND.SSY.GT.0.0) THEN
            CORRL = SXY/SQRT(SSX*SSY)
         ENDIF
      ENDIF
C
C
C
      SD = 0.0
      SDA = 0.0
      SDB = 0.0
      IF (NX.GE.3) THEN
         C =(SSY-((SXY*SXY)/SSX))/(ANX-2.0)
         IF (C.GT.0.0) THEN
            SD = SQRT(C)
         ELSE
            SD = 0.0
         ENDIF
         SDB = SD/SQRT(SSX)
         SDA = SD*((1/ANX)+((AX*AX)/SSX))
      ENDIF
C
C
C
      WRITE(TEXT,900)
  900 FORMAT(' ','Y = A + BX  with X errors=0')
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,901)NX
  901 FORMAT(' ','No of points = ',I7)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,902)A
  902 FORMAT(' ','A =            ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,903)SDA
  903 FORMAT(' ','s.d. A =       ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,904)B
  904 FORMAT(' ','B =            ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,905)SDB
  905 FORMAT(' ','s.d. B =       ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,906)SD
  906 FORMAT(' ','s.d. of a Y value from line = ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(TEXT,907)CORRL
  907 FORMAT(' ','Correlation coefficent= ',G13.6)
      CALL WRUSER(TEXT,ISTAT)
C
C
C
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GETDAT *
C      *            *
C      **************
C
C
C  This s/r extracts a column from an XY list and puts it into an array
C
C  AJPENNY               RGO                         83-1-11
C --------------------------------------------------------------
C
C
C
      SUBROUTINE GETDAT(DATA,NITEM,LSTLEN,KPAR,OUT)
C
C
C
      REAL DATA(NITEM,LSTLEN),OUT(LSTLEN)
C
C
C
      DO K = 1,LSTLEN
         KOUT = 5 + KPAR
         OUT(K) = DATA(KOUT,K)
      ENDDO
C
C
C
      END



