        SUBROUTINE APDISP (DIA,LOGDIA,SKYBRI,MAXAP,NAP,DELMARK,SKYCON)
C+
C       APDISP.
C
C         Menu driven subroutine displaying the values for the sky
C         brightness computed from various photoelectric measures
C         allowing deletion of inaccurate measures & the computation
C         of a final value for the sky brightness.
C
C  Given;
C   DIA      (RA)  Array containing the aperture diameters (arcsec).
C   LOGDIA   (RA)  Array with log10 of diameters.
C   SKYBRI   (RA)  Array containing sky brightness computed for each
C                  aperture.
C   MAXAP    (I)   Size of arrays DIA etc.
C   NAP      (I)   No. of apertures.
C
C  Returned;
C   DELMARK  (LA)  Array containing deletion marker for each
C                  aperture;
C                      = .TRUE. - Aperture deleted.
C                      = .FALSE. - Aperture not deleted.
C   SKYCON   (R)   Final value for sky constant.
C
C  Subroutines called;
C                   HAPDISP, APRINT.
C         Graphics; GDEVI1,TYPLO1,WINDO,QLOT,PICLR,DRAXIS,STOPLOT.
C         I/O;           OUTPUT,READC,UPPCAS,READI,MULREP.
C
C         A C Davenhall. /ROE/                                18/12/81.
C         A C Davenhall./ROE/     {Modified}                  3/11/82.
C-
        REAL DIA(MAXAP),LOGDIA(MAXAP),SKYBRI(MAXAP)
        LOGICAL DELMARK(MAXAP)
        INTEGER MAXAP,NAP
        REAL SKYCON
C
        CHARACTER BUFF*80,OUTBUFF*80,TITLE*20
        INTEGER ISTAT,IAPER,PRSTAT
        REAL LOGDIAO(50),SKYBRIO(50)
C
        INTEGER NAPOK,MAXAPOK
        PARAMETER (MAXAPOK=50)
C
        REAL XMIN,XMAX,YMIN,YMAX
        INTEGER DEVICE
C
C         Initialise the deletion markers so that no apertures
C         are deleted.
C
        DO I=1,NAP
          DELMARK(I)=.FALSE.
        END DO
C
C         Select a graphics device.
C
        ISTAT=0
        CALL MULREP (' Select required graphics device;',
     :               'ARGS,T4010$',BUFF,ISTAT)
        IF (BUFF.EQ.'ARGS')    DEVICE=2
        IF (BUFF.EQ.'T4010')   DEVICE=1
C
C         Print the help subroutine & set up for menu driven running.
C

        CALL OUTPUT ('  ',ISTAT)
        CALL OUTPUT (' Photoelectric calibration; Display the Results.',
     :                ISTAT)
        CALL OUTPUT ('  ',ISTAT)
        CALL HAPDISP
        MORE=.TRUE.
        DO WHILE (MORE)
          BUFF='  '
          ISTAT=0
          CALL READC ('COMMAND',' Enter display command:-',
     :     '  ','  ','~',BUFF,ISTAT)
          CALL UPPCAS (BUFF)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Plot all the apertures that have not been deleted.
C
          IF (BUFF.EQ.'PLOT'.OR.BUFF.EQ.'P') THEN
            CALL GDEVI1 (DEVICE,.TRUE.)
            CALL TYPLO1 (1,8)
C
C         Extract the points that have not been deleted.
C
            NAPOK=0
            DO I=1,NAP
              IF (.NOT.DELMARK(I)) THEN
                NAPOK=NAPOK+1
                LOGDIAO(NAPOK)=LOGDIA(I)
                SKYBRIO(NAPOK)=SKYBRI(I)
              END IF
            END DO
C
C         Setup plotting space & plot.
C
            CALL WINDO (LOGDIAO,SKYBRIO,NAPOK,NAPOK,
     :                XMIN,XMAX,YMIN,YMAX,.FALSE.)
            CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'LOG DIAMETER','SKY')
            CALL QLOT (LOGDIAO,SKYBRIO,MAXAPOK,NAPOK)
            CALL STOPLOT
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Plot all points, regardless of whether they are deleted or not,
C         but use a different plotting symbol for deleted & non-deleted
C         points.
C
          IF (BUFF.EQ.'PLOTALL'.OR.BUFF.EQ.'PA') THEN
            CALL GDEVI1 (DEVICE,.TRUE.)
            CALL WINDO (LOGDIA,SKYBRI,MAXAP,NAP,
     :                        XMIN,XMAX,YMIN,YMAX,.FALSE.)
            CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,'LOG DIAMETER','SKY')
C
C         Extract & plot the non-deleted points 
C         (plotting symbol = asterisk).
C
            NAPOK=0
            DO I=1,NAP
              IF (.NOT.DELMARK(I)) THEN
                NAPOK=NAPOK+1
                LOGDIAO(NAPOK)=LOGDIA(I)
                SKYBRIO(NAPOK)=SKYBRI(I)
              END IF
            END DO
            CALL TYPLO1 (1,8)
            CALL QLOT (LOGDIAO,SKYBRIO,MAXAPOK,NAPOK)
C
C         Extract and plot the deleted points.
C         (plotting symbol = inverted triangle).
C
            NAPOK=0
            DO I=1,NAP
              IF (DELMARK(I)) THEN
                NAPOK=NAPOK+1
                LOGDIAO(NAPOK)=LOGDIA(I)
                SKYBRIO(NAPOK)=SKYBRI(I)
              END IF
            END DO
            CALL TYPLO1 (1,2)
            CALL QLOT (LOGDIAO,SKYBRIO,MAXAPOK,NAPOK)
            CALL STOPLOT
            CALL OUTPUT (' Asterisk - Active points.',ISTAT)
            CALL OUTPUT (' Inverted triangle - Deleted points.',ISTAT)
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Delete a specfied entry.
C
          IF (BUFF.EQ.'DELETE'.OR.BUFF.EQ.'D') THEN
            ISTAT=0
            CALL READI ('IAPER',' No. of aperture to be deleted;',
     :                        0,1,NAP,IAPER,ISTAT)
            DELMARK(IAPER)=.TRUE.
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Reinstate a specified entry.
C
          IF (BUFF.EQ.'REINSTATE'.OR.BUFF.EQ.'R') THEN
            ISTAT=0
            CALL READI ('IAPER',' No. of aperture to be reinstated;',
     :                        0,1,NAP,IAPER,ISTAT)
            DELMARK(IAPER)=.FALSE.
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         List the apertures on the terminal.
C
          IF (BUFF.EQ.'LIST'.OR.BUFF.EQ.'L') THEN
            WRITE(OUTBUFF,2003)
 2003        FORMAT(11X,'Diameter',3X,'Log Dia.',1X,'Sky Bright.')
            CALL OUTPUT (OUTBUFF,ISTAT)
            DO I=1,NAP
              IF (.NOT.DELMARK(I)) THEN
                WRITE(OUTBUFF,2000) I,DIA(I),LOGDIA(I),SKYBRI(I)
 2000        FORMAT(1X,I5,0PF10.1,2X,F10.3,F10.2)
              ELSE
                WRITE(OUTBUFF,2001) I,DIA(I),LOGDIA(I),SKYBRI(I)
 2001        FORMAT(1X,I5,0PF10.1,2X,F10.3,F10.2,2X,'<-Deleted-<<-')
              END IF
              CALL OUTPUT (OUTBUFF,ISTAT)
            END DO
            CALL OUTPUT ('  ',ISTAT)
          END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Compute the mean sky brightness from
C         the non-deleted apertures.
C
          IF (BUFF.EQ.'MEAN'.OR.BUFF.EQ.'M') THEN
            SKYCON=0.0E0
            NAPOK=0
            DO I=1,NAP
              IF (.NOT.DELMARK(I)) THEN
                NAPOK=NAPOK+1
                SKYCON=SKYCON+SKYBRI(I)
              END IF
            END DO
            SKYCON=SKYCON/FLOAT(NAPOK)
            WRITE(OUTBUFF,2002) SKYCON
 2002        FORMAT(1X,'Mean Sky Brightness = ',0PF7.2)
            CALL OUTPUT (OUTBUFF,ISTAT)
          END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         List the apertures and brightnessess on the lineprinter.
C
          IF (BUFF.EQ.'PRINT'.OR.BUFF.EQ.'PR') THEN
            ISTAT=0
            CALL READC ('TITLE',' Enter title;','ALBATROSS',' ','~',
     :                TITLE,ISTAT)
            CALL APRINT (TITLE,MAXAP,NAP,DIA,LOGDIA,SKYBRI,DELMARK,
     :                 PRSTAT)
            IF (PRSTAT.NE.0) CALL OUTPUT (
     : ' ***ERROR Unable to send list of apertures to the printer.',
     :        ISTAT)
          END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Print list of commands available.
C
          IF (BUFF.EQ.'HELP'.OR.BUFF.EQ.'H') CALL HAPDISP
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
          IF (BUFF.EQ.'EXIT'.OR.BUFF.EQ.'E') MORE=.FALSE.
        END DO
        END
