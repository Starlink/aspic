      SUBROUTINE TOPEQ
C+
C     TOPEQ.
C
C     Menu driven subroutine for computing equivalent profiles.
C
C     Structure:-
C
C     Attempt to obtain image frame.
C     if image obtained Ok
C       Create file of scan parameters.
C       Setup equivalent profile file.
C       Do while (not satisfied)
C         - Extract equivalent profile from current image.
C         - Compute integration table from profile.
C         - Obtain the profile from a VAX VMS file.
C         - Change scan parameters.
C         - Save eq. prof. as a Starlink image.
C       end do
C     end if
C     Tidy up image frame.
C
C     A C Davenhall. /ROE/                        10/12/81.
C     A C Davenhall. /ROE/  {Modified}            23/7/82.
C     A C Davenhall. /ROE/  {   "    }            25/8/82.
C-
      INTEGER NAXIS(2),OUTAXS(2),IXEXT,IYEXT
      INTEGER ISTAT,IST,STAT1,IMSTAT,CIPTR,SAPTR
      INTEGER ICST,IRST,DISP,CORTEST,NPTS
      INTEGER EQPSTT
      REAL PLTSCL,SKYCON,LOGMIN,DELOGI,CORR
C
      REAL PARMFIL(50)
      INTEGER PARMSIZ
      PARAMETER (PARMSIZ=50)
C
      LOGICAL MORE,PROFIL
      CHARACTER BUFF*80,TITLE*20
C
      REAL  EQPRO(2,100)
      INTEGER MAXEQP
      PARAMETER (MAXEQP=100)
C
C
C      Initialise the Args.
C
      ISTAT=0
      CALL SRINIT (0,.FALSE.,ISTAT)
C
C       Obtain the current image.
C
      IMSTAT=0
      CALL INPICR ('INPIC1',
     :    ' Enter filename for the image to be analysed;',
     :            2,NAXIS,CIPTR,IMSTAT)
      IF (IMSTAT.EQ.0) THEN
        IXEXT=NAXIS(1)
        IYEXT=NAXIS(2)
C        Setup scan parameters.
C
        CALL CRPRFL (PARMSIZ,PARMFIL)
C
C        Print out introductory message.
C
        CALL OUTPUT ('  ',ISTAT)
        CALL OUTPUT (
     :   ' Equivalent Profile Extraction Routines.',ISTAT)
        CALL OUTPUT ('  ',ISTAT)
        CALL HTOPEQ
C
C         Setup for handling menu driven commands.
C
        MORE=.TRUE.
        PROFIL=.FALSE.
        DO WHILE (MORE)
          BUFF='  '
          CALL READC ('COMMAND',' Enter command:-','  ',
     :                '  ','~',BUFF,ISTAT)
          CALL UPPCAS (BUFF)
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Extract equivalent profile from current image.
C
          IF (BUFF.EQ.'EXTRACT'.OR.BUFF.EQ.'EX') THEN
            ICST=IFIX(PARMFIL(1)+0.4)
            IRST=IFIX(PARMFIL(2)+0.4)
            PLTSCL=PARMFIL(3)
            LOGMIN=PARMFIL(5)
            DELOGI=PARMFIL(6)
            NPTS=IFIX(PARMFIL(7)+0.4)
            CALL EQPROF (%VAL(CIPTR),IXEXT,IYEXT,ICST,IRST,PLTSCL,
     :                   LOGMIN,DELOGI,NPTS,2,MAXEQP,EQPRO)
            PROFIL=.TRUE.
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Compute the equivalent profile using the sky
C    correction technique due to C. P. Blackman.
C
          IF (BUFF.EQ.'EXTRACTC'.OR.BUFF.EQ.'EC') THEN
            ICST=IFIX(PARMFIL(1)+4.0E-1)
            IRST=IFIX(PARMFIL(2)+4.0E-1)
            PLTSCL=PARMFIL(3)
            LOGMIN=PARMFIL(5)
            DELOGI=PARMFIL(6)
            NPTS=IFIX(PARMFIL(7)+4.0E-1)
            EQPSTT=0
            CALL EQPC (%VAL(CIPTR),IXEXT,IYEXT,ICST,IRST,
     :                 PLTSCL,LOGMIN,DELOGI,NPTS,2,MAXEQP,
     :                 EQPRO,EQPSTT)
            IF (EQPSTT.EQ.0) THEN
              PROFIL=.TRUE.
            ELSE
              CALL OUTPUT (
     :   ' ***ERROR in extracting profile.',ISTAT)
            END IF
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Compute integration table on current profile.
C
          IF (BUFF.EQ.'INTABLE'.OR.BUFF.EQ.'I') THEN
            IF (PROFIL) THEN
                ISTAT=0
                CALL READC ('TITLE',' Enter title for this profile;',
     :                    'Anonymous','A','Z',TITLE,ISTAT)
                SKYCON=PARMFIL(4)
                CORTEST=IFIX(PARMFIL(8)+0.4)
                CORR=PARMFIL(9)
                DISP=IFIX(PARMFIL(10)+0.4)
                CALL INTGTBL (TITLE,EQPRO,2,MAXEQP,SKYCON,CORTEST,CORR,
     :                DISP)
            ELSE
                CALL OUTPUT (' Profile not available.',ISTAT)
            END IF
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Obtain profile from a VAX VMS file.
C
          IF (BUFF.EQ.'INFILE'.OR.BUFF.EQ.'INF') THEN
            CALL INFILE (2,MAXEQP,NPTS,EQPRO)
            PROFIL=.TRUE.
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Change the scan parameter file.
C
          IF (BUFF.EQ.'CHANGE'.OR.BUFF.EQ.'C') THEN
            CALL EDPRFL (PARMSIZ,PARMFIL)
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Save the equivalent profile as a Starlink image.
C
          IF (BUFF.EQ.'SAVE'.OR.BUFF.EQ.'S') THEN
            IF (PROFIL) THEN
C
C         Obtain a pointer to the Starlink image to be output.
C
                OUTAXS(1)=NPTS
                OUTAXS(2)=2
                STAT1=0
                CALL OUTPICR ('EQPROF',
     :         ' Enter filename for the equivalent profile;',
     :           2,OUTAXS,SAPTR,STAT1)
C
C         Copy across the profile if the pointer has been obtained OK.
C
                IF (STAT1.EQ.0) THEN
                CALL KOPROF (MAXEQP,2,EQPRO,NPTS,%VAL(SAPTR))
                CALL OUTPUT (' Profile saved.',ISTAT)
                ELSE
                CALL OUTPUT (
     :  ' ***ERROR Unable to obtain a file for the profile.',ISTAT)
                END IF
                CALL CLEARIM ('EQPROF')
            ELSE
                CALL OUTPUT (' Profile not available.',ISTAT)
            END IF
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         List commands on the terminal.
C
          IF (BUFF.EQ.'HELP'.OR.BUFF.EQ.'H') THEN
            CALL HTOPEQ
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C         Exit.
C
          IF (BUFF.EQ.'EXIT'.OR.BUFF.EQ.'E') MORE=.FALSE.
        END DO
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain Starlink image successfully.',
     :     ISTAT)
      END IF
C
C       Tidy up the image frame input.
C
      CALL CLEARIM ('INPIC1')
      END
