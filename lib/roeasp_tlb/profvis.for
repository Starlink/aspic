      SUBROUTINE PROFVIS
C+
C     PROFVIS.
C
C     Subroutine to visually compare an input profile with
C     an artificial profile generated from the sum of an
C     r**1/4 law and an exponential disk law using
C     user input parameters.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C 
C  Subroutines called;
C   Interfaces:- INPICR, READC, UPPCAS, MULREP, OUTPUT, CLEARIM.
C   Args:-      ARGS_OVCLR, PICWRT.
C   E2D:-        KOPY, PROFVIH, INPARM, GENPRF, CMPLOT.
C
C  Structure:-
C
C   Attempt to obtain a pointer to the profile file.
C   If pointer obtained Ok.
C     Copy profile into arrays for plotting.
C     Do while (not happy with fit)
C       - Select graphics device.
C       - Compare profiles
C           enter parameters
C           generate profile
C           plot profilse
C       - List commands avaialble.
C       - Exit.
C     end do
C   else
C     print message saying can't input profile.
C   end if
C   tidy up image
C
C
C  A C Davenhall./ROE/                            2/8/82.
C-
      INTEGER PROPTR,IMSTAT,IOSTAT,STATUS
      INTEGER AXIS(2)
      INTEGER NPTS,NOBS
C
      INTEGER MAXPTS
      PARAMETER (MAXPTS=1024)
      REAL RADIUS(MAXPTS),LOGINT(MAXPTS),OBSINT(MAXPTS),
     :     RES(MAXPTS)
C
      REAL IOBULG,ROBULG,IODISK,ALPHA
      LOGICAL MORE,GRAFDV,CLEAR
      INTEGER DEVICE
      CHARACTER BUFFER*20,REPLY*15,TITLE*20
C
C
C    Attempt to obtain a pointer to the profile.
C
      IMSTAT=0
      CALL INPICR ('PROFILE',
     :  ' Enter filename for the profile;',
     :    2,AXIS,PROPTR,IMSTAT)
C
C    Proceed if the pointer has been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
C
C    Copy the input profile into arrays suitable for further
C    processing.
C
        NOBS=AXIS(1)
        NOBS=MIN(NOBS,MAXPTS)
        CALL KOPY (%VAL(PROPTR),NOBS,MAXPTS,RADIUS,OBSINT)
C
C    Obtain a title for the graphs.
C
        IOSTAT=0
        CALL READC ('TITLE',
     :   ' Enter title to appear on the graphs;',
     :   ' ',' ','~',TITLE,IOSTAT)
        CALL UPPCAS (TITLE)
C
C    Set up for interactive running.
C
        CALL PROFVIH
        GRAFDV=.FALSE.
        MORE=.TRUE.
C
        DO WHILE (MORE)
          IOSTAT=0
          CALL READC ('COMMAND',' Enter command:-',
     :                ' ',' ','~',BUFFER,IOSTAT)
          CALL UPPCAS (BUFFER)
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Select graphics device.
C
          IF (BUFFER.EQ.'DEVICE'.OR.BUFFER.EQ.'D') THEN
            CALL MULREP (' Enter graphics device required;',
     :                   'T4010,ARGS,VERSATEC$',REPLY,IOSTAT)
            IF (REPLY.EQ.'T4010')        DEVICE=1
            IF (REPLY.EQ.'ARGS')         DEVICE=2
            IF (REPLY.EQ.'VERSATEC')     DEVICE=3
            GRAFDV=.TRUE.
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Obtain model parameters, generate a profile and compare
C    with the observed profile.
C
          IF (BUFFER.EQ.'COMPARE'.OR.BUFFER.EQ.'C') THEN
            IF (GRAFDV) THEN
              STATUS=0
              CALL INPARM (IOBULG,ROBULG,IODISK,ALPHA,STATUS)
              IF (STATUS.EQ.0) THEN
                CALL GENPRF (IOBULG,ROBULG,IODISK,ALPHA,
     :                       MAXPTS,NOBS,RADIUS,LOGINT)
                NPTS=NOBS
                CALL READC ('COMMAND',
     : ' Adjust screen if necessary & hit return to continue.',
     :            ' ',' ','~',REPLY,IOSTAT)
                CALL CMPLOT (MAXPTS,NPTS,RADIUS,LOGINT,
     :                      MAXPTS,NOBS,RADIUS,OBSINT,
     :                      DEVICE,TITLE,'RADIUS',
     :                      'LOG I')
              ELSE
                CALL OUTPUT (
     :           '***ERROR Unable to obtain parameters.',
     :            IOSTAT)
              END IF
            ELSE
              CALL OUTPUT (' Graphics device not yet selected.',
     :                       IOSTAT)
            END IF
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Plot the residuals.
C
          IF (BUFFER.EQ.'RESID'.OR.BUFFER.EQ.'R') THEN
            IF (GRAFDV) THEN
              CALL RESID (MAXPTS,NPTS,OBSINT,LOGINT,RES)
              IF (DEVICE.EQ.1) THEN 
                CLEAR=.FALSE.
              ELSE
                CLEAR=.TRUE.
              END IF
              CALL AGRAPH (RADIUS,RES,MAXPTS,NPTS,DEVICE,CLEAR,
     :                     1,6,'RADIUS','RESIDUAL',TITLE)
            ELSE
              CALL OUTPUT (' Graphics device not yet selected.',
     :                      IOSTAT)
            END IF
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    List the commands available.
C
          IF (BUFFER.EQ.'HELP'.OR.BUFFER.EQ.'H') 
     :      CALL PROFVIH
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Exit.
C
          IF (BUFFER.EQ.'EXIT'.OR.BUFFER.EQ.'E')
     :      MORE=.FALSE.
        END DO
      ELSE
C
C    Unable to obtain a pointer to the profile.
C
        CALL OUTPUT (
     :   ' ***ERROR Unable to access profile.',IOSTAT)
      END IF
C
C    Tidy up the input image.
C
      CALL CLEARIM ('PROFILE')
      END
