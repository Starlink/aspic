      SUBROUTINE COLDF1
C+
C     COLDF1.
C
C     Subroutine to take two radial profiles in different colours
C     extracted from along the same axis of a nebular image and
C     compute the colour profile along that axis. The colour
C     profile is saved as a Starlink image. The input profiles
C     may be expressed as either sky normalised intensities
C     or as log intensity above sky.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPICR, RREADR, MULREP, OUTPUT, CLEARIM.
C   E2D:-         KOPY, DEFPTS, DIFPRF, KOPY3.
C
C  A C Davenhall./ROE/                                              2/4/83.
C-
      INTEGER CL1PTR,CL1PTS,CL1STT,
     :        CL2PTR,CL2PTS,CL2STT,
     :        DIFPTR,DIFPTS,OUTPTS,DIFSTT
C
      INTEGER AXIS2(2),AXIS1(1),IOSTAT
C
      INTEGER WRKSTT,C1RPTR,C1IPTR,C2RPTR,C2IPTR,
     :        DFRPTR,DFIPTR,DF2PTR
C
      LOGICAL IFLAG
      REAL CL1SKY,CL2SKY
      CHARACTER REPLY*10
C
C
C    Attempt to obtain a pointer to colour 1 profile.
C
      CL1STT=0
      CALL INPICR ('CL1PRF',
     :  ' Enter filename for the profile in the 1st passband;',
     :    2,AXIS2,CL1PTR,CL1STT)
C
C    Proceed if status ok.
C
      IF (CL1STT.EQ.0) THEN
        CL1PTS=AXIS2(1)
C
C    Attempt to obtain a pointer to colour 2 profile.
C
        CL2STT=0
        CALL INPICR ('CL2PRF',
     :   ' Enter filename for the profile in the 2nd passband;',
     :     2,AXIS2,CL2PTR,CL2STT)
C
C    Proceed if ok.
C
        IF (CL2STT.EQ.0) THEN
          CL2PTS=AXIS2(1)
C
C    Attempt to obtain pointers to the various work arrays needed to
C    hold the radii and intensities as separate arrays.
C
C    ...colour 1, radius.
C
          WRKSTT=0
          AXIS1(1)=CL1PTS
          CALL OUTPICR ('WORK1',' WORK ARRAY.',
     :                   1,AXIS1,C1RPTR,WRKSTT)
C
C    ...colour 1, intensity.
C
          CALL OUTPICR ('WORK2',' WORK ARRAY.',
     :                   1,AXIS1,C1IPTR,WRKSTT)
C
C    ...colour 2, radius.
C
          AXIS1(1)=CL2PTS
          CALL OUTPICR ('WORK3',' WORK ARRAY.',
     :                   1,AXIS1,C2RPTR,WRKSTT)
C
C    ...colour 2, intensity.
C
          CALL OUTPICR ('WORK4',' WORK ARRAY.',
     :                   1,AXIS1,C2IPTR,WRKSTT)
C
C    Proceed if pointers obtained ok.
C
          IF (WRKSTT.EQ.0) THEN
C
C    Copy the input profiles into the work arrays.
C
C    ...colour 1.
C
            CALL KOPY (%VAL(CL1PTR),CL1PTS,CL1PTS,
     :                 %VAL(C1RPTR),%VAL(C1IPTR))
C
C    ...colour 2.
C
            CALL KOPY (%VAL(CL2PTR),CL2PTS,CL2PTS,
     :                 %VAL(C2RPTR),%VAL(C2IPTR))
C
C    Determine the number of points in the output profile.
C
            CALL DEFPTS (CL1PTS,%VAL(C1RPTR),CL2PTS,%VAL(C2RPTR),
     :                   DIFPTS)
C
C    Attempt to obtain pointers to work arrays to hold the computed
C    colour difference profile.
C
            AXIS1(1)=DIFPTS
            CALL OUTPICR ('WORK5',' WORK ARRAY.',
     :                     1,AXIS1,DFRPTR,WRKSTT)
            CALL OUTPICR ('WORK6',' WORK ARRAY.',
     :                     1,AXIS1,DFIPTR,WRKSTT)
            CALL OUTPICR ('WORK7',' WORK ARRAY.',
     :                     1,AXIS1,DF2PTR,WRKSTT)
C
C    Proceed if these pointers have been obtained Ok.
C
            IF (WRKSTT.EQ.0) THEN
C
C    Obtain the sky brigthness in both colour bands.
C
              IOSTAT=0
              CALL READR ('CL1SKY',
     :         ' Enter sky brightness for 1st band (mag./sq.arcsec.);',
     :           2.2E1,1.0E1,3.0E1,CL1SKY,IOSTAT)
              CALL READR ('CL2SKY',
     :         ' Enter sky brightness for 2nd band (mag./sq.arcsec.);',
     :           2.2E1,1.0E1,3.0E1,CL2SKY,IOSTAT)
C
C    Determine whether the profiles are in LogI or I.
C
              CALL MULREP (
     :    ' Are the profiles input held as intensity or logI?',
     :         'LOGI,I$',REPLY,IOSTAT)
              IF (REPLY.EQ.'LOGI') THEN
                IFLAG=.TRUE.
              ELSE
                IFLAG=.FALSE.
              END IF
C
C    Finally compute the colour difference profile !!
C
              CALL DIFPRF (CL1PTS,%VAL(C1RPTR),%VAL(C1IPTR),
     :                     CL2PTS,%VAL(C2RPTR),%VAL(C2IPTR),
     :                     CL1SKY,CL2SKY,IFLAG,DIFPTS,
     :                     %VAL(DF2PTR),%VAL(DFRPTR),%VAL(DFIPTR),
     :                     OUTPTS)
C
C    Attempt to obtain a pointer to the file to hold the output
C    profile.
C
              DIFSTT=0
              AXIS2(1)=OUTPTS
              AXIS2(2)=2
              CALL OUTPICR ('OUTPRF',
     :         ' Enter filename for colour difference profile;',
     :           2,AXIS2,DIFPTR,DIFSTT)
C
C    Proceed to copy the colour difference profile if the pointer
C    has been obtained Ok.
C
              IF (DIFSTT.EQ.0) THEN
                CALL KOPY3 (OUTPTS,%VAL(DFRPTR),%VAL(DFIPTR),
     :                      %VAL(DIFPTR))
              ELSE
                CALL OUTPUT (
     :   ' ***ERROR Unable to obtain output file successfully.',IOSTAT)
              END IF
            ELSE
              CALL OUTPUT (
     :         ' ***ERROR obtaining pointers to work arrays.',
     :           IOSTAT)
            END IF
          ELSE
            CALL OUTPUT (
     :       ' ***ERROR unable to obtain pointers to work arrays.',
     :         IOSTAT)
          END IF
        ELSE
          CALL OUTPUT (
     :     ' ***ERROR Unable to obtain 2nd profile successfully.',
     :       IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain 1st profile successfully.',
     :     IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('CL1PRF')
      CALL CLEARIM ('CL2PRF')
      CALL CLEARIM ('OUTPRF')
      CALL CLEARIM ('WORK1')
      CALL CLEARIM ('WORK2')
      CALL CLEARIM ('WORK3')
      CALL CLEARIM ('WORK4')
      CALL CLEARIM ('WORK5')
      CALL CLEARIM ('WORK6')
      CALL CLEARIM ('WORK7')
C
      END
