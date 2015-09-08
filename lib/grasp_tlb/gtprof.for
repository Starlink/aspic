C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GTPROF *
C      *            *
C      **************
C
C
C   PURPOSE
C      This s/r aquires from the user the parameters of the Lorentzian
C      profile RX,RY,P,PRX,PRY where the profile is :-
C
C                 I   =      1
C                        ----------------
C                                 P(1+d2)
C                           1 + d1
C
C                                                             2       2
C                                         where d1=sqrt((x/RX) +(y/RY) )
C
C                                                            2        2
C                                             d2=sqrt((x/PRY) +(y/PRY) )
C
C            The default values are picked up from LORFIT_ or set
C            to 2,RX,2.3,5*RX,5*RY.
C
C   ARGUMENTS
C  OUT
C     PROF(5)  Real      The profile parameters as RX,RY,P,PRX,PRY
C     PARFIX   Logical   Flag for fixed (TRUE) or variable (FALSE) parameters
C                        in fit
C     VOLCAL   Logical   Flag for calculation of volume of fit (TRUE/FALSE)
C     IERR     Integer   Success flag (=0for success)
C
C   STARLINK PARAMETERS
C     RX,RY,P,PRX,PRY,PARFIX,VOLCAL
C     LORFIT_RX,LORFIT_RY,LORFIT_P,LORFIT_PRX,LORFIT_PRY
C
C
C   CALLS
C     Starlink
C        CNPAR,WRUSER
C     Grasp
C        GETSYM
C     Edrs
C        GETCMD,GETPAR
C
C
C
C   A.J.PENNY                   RGO                    83-2-28
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GTPROF(PROF,PARFIX,VOLCAL,IERR)
C
C
C
      REAL PROF(5)
      CHARACTER TEXT*72
      LOGICAL PARFIX,VOLCAL
      CHARACTER SYMB*20
C
C
C
      IERR = 0
C
C
C
      K = 1
      CALL GETCMD('PARFIX','NO,YES.',1,K,
     +            TEXT,KTEXT,ISTAT)
      IF (K.LT.1.OR.K.GT.2.OR.ISTAT.NE.0) THEN
         IERR = 1
      ELSE
         IF(K.EQ.1) THEN
            PARFIX = .FALSE.
            CALL WRUSER('Input the iteration starting profile'//
     +                  ' parameters',ISTAT)
         ELSE
            PARFIX = .TRUE.
            CALL WRUSER('Input the profile parameters',ISTAT)
         ENDIF
      ENDIF
C
C  Get profile
C
C
      SYMB = 'LORFIT_RX'
      CALL GETSYM(SYMB,PROF(1),ISTAT)
      IF (ISTAT.NE.0) PROF(1) = 2.0
      CALL GETPAR('RX','REAL',1,1.0E-5,1.0E5,.TRUE.,IVAL,
     +            PROF(1),ISTAT1)
      SYMB = 'LORFIT_RY'
      CALL GETSYM(SYMB,PROF(2),ISTAT)
      IF (ISTAT.NE.0) PROF(2) = PROF(1)
      CALL GETPAR('RY','REAL',1,1.0E-5,1.0E5,.TRUE.,IVAL,
     +            PROF(2),ISTAT2)
      SYMB = 'LORFIT_P'
      CALL GETSYM(SYMB,PROF(3),ISTAT)
      IF (ISTAT.NE.0) PROF(3) = 2.3
      CALL GETPAR('P','REAL',1,1.0E-5,1.0E5,.TRUE.,IVAL,
     +            PROF(3),ISTAT3)
      SYMB = 'LORFIT_PRX'
      CALL GETSYM(SYMB,PROF(4),ISTAT)
      IF (ISTAT.NE.0) PROF(4) = 5.0*PROF(1)
      CALL GETPAR('PRX','REAL',1,1.0E-5,1.0E5,.TRUE.,IVAL,
     +            PROF(4),ISTAT4)
      SYMB = 'LORFIT_PRX'
      CALL GETSYM(SYMB,PROF(5),ISTAT)
      IF (ISTAT.NE.0) PROF(5) = 5.0*PROF(2)
      CALL GETPAR('PRY','REAL',1,1.0E-5,1.0E5,.TRUE.,IVAL,
     +            PROF(5),ISTAT5)
C
C  If any of the profile parameters are out of range, set the continuation
C  flag to no
C
      IF (ISTAT1.NE.0.OR.ISTAT2.NE.0.OR.ISTAT3.NE.0.OR.
     +    ISTAT4.NE.0.OR.ISTAT5.NE.0) THEN
         IERR = 1
         CALL WRUSER('BAD ANSWER',ISTAT)
      ENDIF
C
C  Get flag for calculating star image volume
C
      IF (IERR.EQ.0) THEN
         K = 2
         CALL GETCMD('VOLCAL','NO,YES.',1,K,TEXT,KTEXT,ISTAT)
         IF (ISTAT.NE.0) THEN
            IERR = 1
         ELSE
            IF (K.EQ.1) THEN
               VOLCAL = .FALSE.
            ELSE
               VOLCAL = .TRUE.
            ENDIF
         ENDIF
      ENDIF
C
C
C
      CALL CNPAR('PARFIX',ISTAT)
      CALL CNPAR('VOLCAL',ISTAT)
      CALL CNPAR('RX',ISTAT)
      CALL CNPAR('RY',ISTAT)
      CALL CNPAR('P',ISTAT)
      CALL CNPAR('PRX',ISTAT)
      CALL CNPAR('PRY',ISTAT)
C
C
C
      END



