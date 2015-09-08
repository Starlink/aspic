        SUBROUTINE INTGTBL (TITLE,KEEP,N1,N2,SKYCON,CORTEST,CORR
     :                      DISP)
C+
C         Subroutine to compute an integration table & some of the
C         parameters in the system of de Vaucouleurs given an
C         equivalent profile.
C
C         Note; The integration table & parameters are required to
C               be listed on the line printer & hence they are
C               written to a file using standard Fortran I/O.
C               All Fortran I/O statements appear below the
C               line of '+' characters inserted in the code.
C
C         Structure:-
C
C         For all the pts. in the profile;
C           Copy from "Starlink image" array.
C           Compute I from Log I.
C           Compute isophotal areas in square arcmin.
C         end do
C         Compute increments in luminosity between the isophotal
C         levels.
C         Sum these to give the total luminosity.
C         Compute the extrapolation correction for the total
C         luminosity.
C           the de Vaucouleurs way.
C         or 
C           the Okamura way for ellipticals.
C         or
C           the Okamura way for spirals.
C         or
C           obtain the value from the argument list.
C         Compute the total luminosity using Simpson's rule.
C         Add the extrapolation correction.
C         Compute the quartiles of K(r).
C         Compute the concentration indeces.
C         Compute mu(r), Log J & Rho.
C         Calculate the 25th Mag. Dia.
C         Output the results.
C
C        Given;
C         TITLE - Title of the profile being plotted (Character).
C         KEEP - 2D array holding profile as a "Starlink"
c                image (real).
C         N1 - X size of KEEP (integer).
C         N2 - Y size of KEEP (integer).
C         SKYCON - Sky Brightness in magnitudes/square arcsec (real).
C         CORTEST - Determines the type of extrapolation correction
C                   that is to be made (integer).
C                   = 1 - de Vaucouleurs.
C                   = 2 - Okamura, ellipticals.
C                   = 3 - Okamura, spirals.
C                        = 4 - Input with argument list.
C         CORR - The extrapolation correction if CORTEST = 4 (real).
C         DISP - Disposition of the integration table file (integer).
C                = 1 - Print/Delete.
C                = 2 - Print/Keep.
C                = 3 - Noprint /Keep.
C
C        Returned;
C         None.
C
C         Subroutines called;
C         LINLSQ, LAGIN, SIMP.
C         Interfaces; OUTPUT.
C
C         A C Davenhall. /St Andrews/                        Spring 81.
C         A C Davenhall. /ROE/                                24/11/81.
C         A C Davenhall. /ROE/   {Modified}                   15/7/82.
C         A C Davenhall./ROE/    {   "    }                   6/12/82.
C-
        REAL KEEP(N1,N2)
        INTEGER N1,N2,CORTEST,DISP
        REAL SKYCON,CORR
        CHARACTER TITLE*20
C
C         Arrays to hold parameters for the integration table.
C
        REAL LOGI(100),AREA(100),REQ(100),SUMDEL(100),DELTAL(100),
     :             K(100),RHO(100),LOGJ(100),INT(100),MU(100)
        INTEGER NISO,MAXISO
C
C         Arrays for interpolated pts. & Simpson's rule.
C
        REAL REQSIM(500),INTSIM(500)
        INTEGER NSIM,MAXSIM
C
C         Arrays to hold parameters at quartiles of K(r).
C
        REAL QUART(3),REQQU(3),LOGIQU(3),MUQU(3)
C
C         Arrays for de Vaucouleurs extrapolation.
C
        REAL X(20),F(20)
C
C         Arrays for Okamura's spiral extrapolation correction.
C
        REAL RSP(20),LOGISP(20)
C
C         Coeffs. for Okamura's elliptical extrapolation correction.
C
        REAL COEFF(7)
        REAL RQUART(100)
        REAL AD25(1),AMU25(1)
C
        INTEGER UNIT,STATUS,ISTAT
        REAL TOTLUMA,TOTLUMS,DIFF,TOTLUMAC,TOLUMSC,MUTOT,WORKTOT
        REAL UNITAIR,TEN,DUMMY
        REAL GRAD,ERRGRAD,C,ERRC
        REAL WORK1,WORK2
        INTEGER II,NOUTER,NEXTR,NN,IDISP
        PARAMETER (NEXTR=6)
        REAL BETA,GAMMA,BETAL,BETAL8,GAMMAL,CONS,DR,DRD,RESULT,
     :       POGSON,LOG10E,ALPHA
        REAL C21,C32,C31,D25
        REAL XMIN,XMAX,YMIN,YMAX,XMIN1,XMAX1,YMIN1,YMAX1,RANGE,BIT
        CHARACTER DATEB*9,TIMEB*8
        INTEGER LINE,PTHRO
        REAL ONE
        INTEGER LOST,NEWPTS
        CHARACTER BUFFER*60
C
        DATA QUART(1),QUART(2),QUART(3)/2.5E-1,5.0E-1,7.5E-1/
        DATA COEFF(1),COEFF(2),COEFF(3),COEFF(4),COEFF(5),COEFF(6),
     :     COEFF(7)/1.0E0,7.0E0,4.2E1,2.10E2,8.40E2,2.520E3,5.040E3/
      PARAMETER (PI=3.1415927E0)
      PARAMETER (TEN=1.0E1)
      PARAMETER (MAXISO=100)
      PARAMETER (MAXSIM=500)
      PARAMETER (DRD=1.0E0)
      PARAMETER (POGSON=2.5E0)
      PARAMETER (PTHRO=70)
      PARAMETER (UNIT=27)
      PARAMETER (ONE=1.0E0)
      PARAMETER (LOG10E=4.342945E-1)
      PARAMETER (BIT=2.0E-1)
C
C         Find the number of pts. in the profile.
C
        DO I=1,MAXISO
          IF (KEEP(1,I).GT.1.0E-3) NISO=I
        END DO
C
C         Check that the radii in the equivalent profile are
C         monotonically increasing and if not discard the
C         aberrant points.
C
        CALL EQPRCK (N2,N1,NISO,KEEP,NEWPTS)
        IF (NEWPTS.LT.NISO) THEN
          CALL OUTPUT (
     : 'A turnover has been detected in the equivalent profile,',
     :                 STATUS)
          CALL OUTPUT (
     : 'Points below the turnover will be discarded.',STATUS)
          LOST=NISO-NEWPTS
          WRITE(BUFFER,3000) LOST
 3000     FORMAT(1X,'No. of points discarded = ',I3)
          CALL OUTPUT (BUFFER,STATUS)
          NISO=NEWPTS
        END IF
C
C         Copy from the "Starlink image" array & compute the intensity
C         from the Log I values & the isophotal area in square arcmin.
C         from the equivalent radius.
C
        UNITAIR=PI/3.6E3
        DO I=1,NISO
          REQ(I)=KEEP(1,I)
          LOGI(I)=KEEP(2,I)
          INT(I)=TEN**LOGI(I)
          AREA(I)=UNITAIR*(REQ(I)**2)
        END DO
C
C         Compute increments in luminosity between these & the
C         total luminosity by adding them up.
C
        DELTAL(1)=0.0E0
        SUMDEL(1)=0.0E0
        DO I=2,NISO
          II=I-1
          DUMMY=(AREA(I)-AREA(II))*(INT(I)+INT(II))/2.0E0
          DELTAL(I)=DUMMY
          SUMDEL(I)=SUMDEL(II)+DUMMY
        END DO
        TOTLUMA=SUMDEL(NISO)*3.6E3
C
C         Compute the extrapolation correction for the total luminosity.
C
C         Trap illegal values of CORTEST.
C
        IF (CORTEST.LT.1.OR.CORTEST.GT.4) CORTEST=1
        IF (CORTEST.EQ.1) THEN
C
C         de Vaucouleurs correction. ApJ 131, p583 (1960).
C
C         NOUTER - No. of outer levels used to make a least squares fit.
C
          NOUTER=0
          DO J=1,NEXTR
            I=NISO+1-J
            II=I-1
            WORK1=(INT(I)+INT(II))*5.0E-1
            WORK2=DELTAL(I)/(INT(II)-INT(I))
            IF (WORK1.GE.1.0E-10.AND.WORK2.GE.1.0E-10) THEN
              NOUTER=NOUTER+1
              X(NOUTER)=ALOG10(WORK1)
              F(NOUTER)=ALOG10(WORK2)
            END IF
          END DO
          IF (NOUTER.GE.3) THEN
            CALL LINLSQ (X,F,NOUTER,20,.FALSE.,GRAD,C,ERRGRAD,ERRC,
     :                 CHISQ,UNIT)
            CORR=INT(NISO)*(TEN**((GRAD*ALOG10(INT(NISO)*5.0E-1))+C))
            CORR=CORR*3.6E3
          ELSE
            CORR=0.0E0
          END IF
        END IF
        IF (CORTEST.EQ.2) THEN
C
C         Okamura's correction for elliptical galaxies.
C         Annal. Tokyo Astron. Obs. (1977) XVI No.3 p.136.
C
C         Compute the quantity Beta. First compute r**1/4.
C
          DO I=1,NISO
            RQUART(I)=REQ(I)**(0.25)
          END DO
C
C         Least squares fit.
C
          CALL LINLSQ (RQUART,LOGI,NISO,MAXISO,.FALSE.,GRAD,C,
     :                       ERRGRAD,ERRC,CHISQ,UNIT)
C
C         Compute Beta.
C
          BETA=-GRAD/LOG10E
          GAMMA=BETA*((REQ(NISO))**2.5E-1)
          BETAL=ALOG10(BETA)
          BETAL8=BETAL*8.0E0
          GAMMAL=ALOG10(GAMMA)
C
C         CONS = Log10 ( 8.Pi.{intensity at outermost isophote})
C
          CONS=ALOG10(2.513284E1*INT(NISO))
          CORR=0.0E0
          DO I=1,7
            C=GAMMAL*FLOAT(8-I)
            C=C+CONS-BETAL8+(ALOG10(COEFF(I)))
            CORR=CORR+(TEN**C)
          END DO
          C=CONS+ALOG10(COEFF(7))-BETAL8
          CORR=CORR+(TEN**C)
        END IF
        IF (CORTEST.EQ.3) THEN
C
C         Okamura's correction for disk galaxies.
C         Annal. Tokyo Astron. Obs. (1977) XVI No.3 p.136.
C
C         Pick of the 10 faintest isophotes to avoid contamination
C         by the bulge.
C
          NOUTER=MIN(10,NISO)
          DO I=1,NOUTER
            II=NISO+1-I
            RSP(I)=REQ(II)
            LOGISP(I)=LOGI(II)
          END DO
          CALL LINLSQ (RSP,LOGISP,NOUTER,20,.FALSE.,GRAD,C,ERRGRAD,
     :                       ERRC,CHISQ,UNIT)
C
C         Compute alpha.
C
          ALPHA=-GRAD/LOG10E
C
C         Compute extrapolation correction.
C
          CORR=(2.0E0*PI*INT(NISO))*((ALPHA*REQ(NISO))+1.0E0)/ALPHA
        END IF
C
C         Compute the total luminosity using Simpson's Rule.
C
C         First obtain a suitable stepsize.
C
        DR=DRD
        NSIM=(REQ(NISO)/DR)+1
        IF (NSIM.GE.MAXSIM) THEN
          DO WHILE (NSIM.GE.MAXSIM) 
            DR=DR*1.2E0
            NSIM=(REQ(NISO)/DR)+1
          END DO
        END IF
        IF (MOD(NSIM,2).EQ.0) NSIM=NSIM+1
        DR=REQ(NISO)/FLOAT(NSIM-1)
        DO I=1,NSIM
          REQSIM(I)=DR*FLOAT(I-1)
        END DO
C
C         Interpolate intensities.
C
        CALL LAGIN (REQ,LOGI,REQSIM,INTSIM,MAXSIM,MAXISO,NISO,NSIM)
C
C         Compute r.I(r)
C
        DO I=1,NSIM
          INTSIM(I)=REQSIM(I)*(TEN**INTSIM(I))
        END DO
C
C         Integrate to get the total luminosity using Simpson's rule.
C
        CALL SIMP (INTSIM,DR,NSIM,MAXSIM,RESULT)
        TOTLUMS=RESULT*PI*2.0E0
C
C         Calculate the percentage difference between the total 
C         luminosity derived by adding the levels & that from
C         Simpson's rule.
C
        DIFF=((TOTLUMA-TOTLUMS)*2.0E2)/(TOTLUMA+TOTLUMS)
C
C         Add on the extrapolation corrections.
C
        TOTLUMAC=TOTLUMA+CORR
        TOTLUMSC=TOTLUMS+CORR
        MUTOT=SKYCON-(POGSON*ALOG10(TOTLUMSC))
C
C         Compute K(r) & its quartiles.
C
        WORKTOT=TOTLUMAC/3.6E3
        DO I=1,NISO
          K(I)=SUMDEL(I)/WORKTOT
        END DO
        CALL LAGIN (K,REQ,QUART,REQQU,3,MAXISO,NISO,3)
        CALL LAGIN (REQ,LOGI,REQQU,LOGIQU,3,MAXISO,NISO,3)
        DO I=1,3
          MUQU(I)=SKYCON-(POGSON*LOGIQU(I))
        END DO
C
C         Compute the concentration indeces.
C
        C21=REQQU(2)/REQQU(1)
        C32=REQQU(3)/REQQU(2)
        C31=REQQU(3)/REQQU(1)
C
C         Compute the remaining parameters for the integration table;
C         Mu, Log J & Rho.
C
        DO I=1,NISO
          MU(I)=SKYCON-(POGSON*LOGI(I))
          LOGJ(I)=LOGI(I)-LOGIQU(2)
          RHO(I)=REQ(I)/REQQU(2)
        END DO
C
C         Compute the 25th Mag. dia. if the profile goes faint
C         enough.
C
        IF (MU(NISO).LT.2.5E1) THEN
          AMU25(1)=2.5E1
          CALL LAGIN (MU,REQ,AMU25,AD25,1,MAXISO,NISO,1)
          D25=AD25(1)*2.0E0
        END IF
C
C::::::::::::::::::::::::
C
C         Plot the computed profiles on the Versatec.
C
C         First the equivalent profile.
C
        CALL AGRAPH (REQ,LOGI,MAXISO,NISO,3,.TRUE.,1,8,
     :        'R*(ARCSEC)','LOG I',TITLE)
C
C         Then the relative integrated luminosity curve.
C
        CALL AGRAPH (RHO,K,MAXISO,NISO,3,.TRUE.,2,0,
     :        'RHO','K(RHO)',TITLE)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         All Fortran I/O is done below this line.
C
 2000   FORMAT('1',56X,A20,30X,A8,1X,A9//
     :      47X,'Computation of Photometric Parameters.'
     :   4(/))
 2001   FORMAT(50X,'Number of isophotal levels = ',I3//)
 2002   FORMAT(35X,'Extrapolation correction calculated by the ',
     :   'method of de Vaucouleurs.'//)
 2003   FORMAT(57X,'X',15X,'F(X)'/)
 2004   FORMAT(50X,1PE12.3,5X,1PE12.3)
 2005   FORMAT(//35X,'Linear Least Squares Solution:-'/35X,
     :   'Gradient = ',1PE12.3,5X,'Error on Gradient = ',1PE12.3/
     :   35X,'Intercept = ',1PE12.3,4X,'Error on intercept = ',
     :   1PE12.3//)
 2006   FORMAT(//26X,'Extrapolation correction calculated by ',
     :   'Okamura''s method for elliptical galaxies.'//
     :    59X,'Beta = ',0PF7.4//)
 2007   FORMAT(41X,'Extrapolation correction read from parameter ',
     :    'file.'//)
 2008   FORMAT(35X,'Extrapolation correction = ',1PE12.3,2X,
     :    'Magnitudes/square arcsec.'//)
 2009   FORMAT(/35X,'Simpson''s Rule Integration:-'/35X,
     :    'Adjusted step size = ',F10.2,' Arcsec.',1X,
     :    'No. of interpolated steps = ',I3//)
 2010   FORMAT(/35X,'Total luminosities before adding extrapolation ',
     :    'correction;')
 2011   FORMAT(/35X,'Total luminosity by adding = ',1PE10.3/
     :    35X,'Total luminosity by Simpson''s rule = ',1PE10.3)
 2012   FORMAT(///35X,'Total luminosities after adding extrapolation ',
     :    'correction;')
 2013   FORMAT(///35X,'Total magnitude by Simpson''s rule = ',F10.2//)
 2014   FORMAT(35X,I5,8X,0PF3.2,6X,F5.1,4X,1PE10.2,9X,0PF6.2)
 2015   FORMAT(///35X,'Concentration indices:-',//35X,
     :   'C21 = ',0PF5.2,5X,'C32 = ',F5.2,5X,'C31 = ',F5.2)
 2016   FORMAT(///35X,'Equivalent Diameter of the 25 mag./sq.arcsec.',
     :          ' isophote = ',F6.1,' arcsec.'///)
 2017   FORMAT('1',6(/)49X,'Integration table for ',A20///)
 2018   FORMAT(37X,'Log I',3X,'A',6X,'P',4X,'Sum P',4X,'K',6X,
     :    'R*',4X,'Rho',3X,'Log J',3X,'Mu'/)
 2019   FORMAT(34X,F7.3,F7.3,7X,F7.3,F7.3,F7.1,F7.3,F7.3,F7.2)
 2020   FORMAT(48X,F7.3)
 2021   FORMAT('1',6(/),45X,'Integration table for ',A20,1X,
     :    'Continued.'//)
 2022   FORMAT(54X,0PF8.3,F8.3)
 2023   FORMAT(/40x,'**NOTE:- Mu computed for a sky constant of ',
     :    'zero. **'//)
 2024   FORMAT(/55X,'Sky Constant = ',F6.2//)
 2025   FORMAT(35X,'Difference between the two values = ',0PF5.2,'%')
 2026   FORMAT(35X,'Quartile',5X,'K(r)',5X,'Radius',5X,'Log I(r)',
     :     5X,'Surface Brightness'/56X,'(Arcsec)',18X,
     :     '(Mag./sq.arcsec)')
 2027   FORMAT(//28X,'Extrapolation correction calculated by ',
     :         'Okamura''s method for spiral galaxies.'//
     :          58X,'Alpha = ',0PF7.4//)
C
C         Obtain time & date from the system.
C
        CALL DATE (DATEB)
        CALL TIME (TIMEB)
C
C         Open file for output.
C
        OPEN (UNIT=UNIT,FILE='EQPROF.TMP',STATUS='NEW')
C
C         Write title.
C
        WRITE(UNIT,2000) TITLE,TIMEB,DATEB
        WRITE(UNIT,2001) NISO
C
C         Write out extrapolation correction.
C
        IF (CORTEST.EQ.1) THEN
          WRITE(UNIT,2002)
          WRITE(UNIT,2003)
          WRITE(UNIT,2004) (X(I),F(I),I=1,NOUTER)
          WRITE(UNIT,2005) GRAD,ERRGRAD,C,ERRC
        END IF
C
        IF (CORTEST.EQ.2) THEN
          WRITE(UNIT,2006) BETA
        END IF
        IF (CORTEST.EQ.3) THEN
          WRITE(UNIT,2027) ALPHA
        END IF
        IF (CORTEST.EQ.4) THEN
          WRITE(UNIT,2007)
        END IF
        WRITE(UNIT,2008) CORR
C
C         Write out total luminosities.
C
        WRITE(UNIT,2009) DR,NSIM
        WRITE(UNIT,2010)
        WRITE(UNIT,2011) TOTLUMA,TOTLUMS
        WRITE(UNIT,2025) DIFF
        WRITE(UNIT,2012)
        WRITE(UNIT,2011) TOTLUMAC,TOTLUMSC
        WRITE(UNIT,2013) MUTOT
C
C         Write out Quartiles of K(r).
C
        WRITE(UNIT,2026)
        WRITE(UNIT,2014) (I,QUART(I),REQQU(I),LOGIQU(I),MUQU(I),I=1,3)
C
C         Write out the concentration indeces.
C
        WRITE(UNIT,2015) C21,C32,C31
C
C         Write out the 25th mag. dia. if it has been calculated.
C
        IF (MU(NISO).LT.2.5E1) WRITE(UNIT,2016) D25
C
C         Write out the integration table.
C
        WRITE(UNIT,2017) TITLE
        LINE=14
        WRITE(UNIT,2018)
        WRITE(UNIT,2019) LOGI(1),AREA(1),SUMDEL(1),K(1),REQ(1),RHO(1),
     :    LOGJ(1),MU(1)
        DO I=2,NISO
          WRITE(UNIT,2020) DELTAL(I)
          WRITE(UNIT,2019) LOGI(I),AREA(I),SUMDEL(I),K(I),REQ(I),RHO(I),
     :    LOGJ(I),MU(I)
          LINE=LINE+2
          IF (LINE.GT.PTHRO) THEN
            WRITE(UNIT,2021) TITLE
            WRITE(UNIT,2018)
            LINE=7
          END IF
        END DO
        CORR=CORR/3.6E3
        WRITE(UNIT,2020) CORR
        WRITE(UNIT,2022) WORKTOT,ONE
        IF (SKYCON.EQ.0.0E0) THEN
          WRITE(UNIT,2023)
        ELSE
          WRITE (UNIT,2024) SKYCON
        END IF
C
C         Close file containing integration table. Disposal
C         of the file depends on the value of "DISP".
C
C         Take care of the case of an illegal value of "DISP".
C
        IF (DISP.LT.1.OR.DISP.GT.3) DISP=1
C
        IF (DISP.EQ.1) CLOSE(UNIT=UNIT,DISPOSE='PRINT/DELETE')
        IF (DISP.EQ.2) CLOSE(UNIT=UNIT,DISPOSE='PRINT')
        IF (DISP.EQ.3) CLOSE(UNIT=UNIT,DISPOSE='KEEP')
        END
