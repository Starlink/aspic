      SUBROUTINE SIMPARS
C+
C     SIMPARS.
C
C     Subroutine to create a parameter file to control the
C     Starfield simulation generation program. The user is
C     interogated in order to obtain the parameters
C     necessary  to drive the simulation program and the
C     results, converted to a suitable form, are written to 
C     the parameter file.
C
C  Parameters obtained; Standard or Random image flag.
C                       Image size (X and Y dimensions)
C                       Pixel size (arcseconds).
C                       Dia. of telescope aperture (cm).
C                       Exposure time (sec).
C                       Sky brightness (mag./sq.arcsec.)
C                       Star density/galactic latitude factor.
C                       Moffat's parameter beta.
C                          "        "      R (arcsec).
C                       Atmospheric absorption.
C                       Telescope and detector efficiency.
C                       Magnitude threshold for plotting.
C                       If (globular cluster to be included in field)
C                         No. of stars in cluster.
C                         Distance to the cluster (pc).
C                         Total interstellar absorption to the cluster.
C                         Core radius for the cluster.
C                         Tidal radius for the cluster.
C                       end if.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- OUTPUT, MULREP, READI, READR, YESNO, OUTPICR,
C                CLEARIM.
C   Simim:-      ENCPAR.
C
C  A C Davenhall./ROE/                                 17/3/82.
C  A C Davenhall./ROE/    {Modified}                   30/8/82.
C  A C Davenhall./ROE/    {   "    }                   30/12/82.
C-
      CHARACTER ANSWR*10, REPLY*1
      INTEGER ISTAT
      REAL PLTSCL
C
C      Quantities written to the parameter file.
C
      INTEGER STNDRD,IXEXT,IYEXT,NGLOB
      REAL PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,BETA,R,
     :     ATMABS,EFFIC,THRESH,DIST,GABSOR,RCORE,RTIDAL
      LOGICAL GFLAG
C
C      Variables associated with parameter file.
C
      INTEGER AXIS(1),PARPTR,PARSTT
C
      CALL OUTPUT ('   ',ISTAT)
      CALL OUTPUT (' Parameters for Star field simulation.',ISTAT)
      CALL OUTPUT ('   ',ISTAT)
C
C      Determine whether a standard or random image is to be
C      produced.
C
      ISTAT=0
      CALL MULREP (' Is a Standard or Random image to be produced?',
     :             'STANDARD,S,RANDOM,R$',ANSWR,ISTAT)
      STNDRD=0
      IF (ANSWR.EQ.'RANDOM'.OR.ANSWR.EQ.'R') STNDRD=1
C
C      Obtain the X & Y sizes of the image.
C
      CALL READI ('IXEXT',' Enter X size of image;',512,1,2048,
     :             IXEXT,ISTAT)
      CALL READI ('IYEXT',' Enter Y size of image;',IXEXT,1,2048,
     :             IYEXT,ISTAT)
C
C      Obtain the size of a (square) pixel side in arcsec.
C
      CALL READR ('PLTSCL',' Enter the plate scale (arcsec/mm);',
     :             67.13,0.05,5.0E3,PLTSCL,ISTAT)
      CALL READR ('PIXSIZ',
     :  ' Enter size of (square) pixel size (micron);',
     :    15.0,0.5,2000.0,PIXSIZ,ISTAT)
      PIXSIZ=PIXSIZ*PLTSCL/1.0E3
C
C      Obtain the telescope aperture diameter (cm).
C
      CALL READR ('APERTR',
     :  ' Enter the diameter of the telescope aperture (cm);',
     :    115.0,1.0,6.0E3,APERTR,ISTAT)
C
C      Obtain the exposure time.
C
      CALL READR ('EXPOSE','Enter exposure time (sec);',
     :             3.6E3,1.0,1.8E4,EXPOSE,ISTAT)
C
C      Obtain the sky brightness in mag./sq.arcsec.
C
      CALL READR ('SKYBRI',
     :  ' Enter the sky brightness (mag./sq.arcsec.);',
     :    22.5,20.0,24.0,SKYBRI,ISTAT)
C
C      Obtain star density / galactic latitude factor.
C
      CALL READR ('STMULT',
     :  ' Enter galactic latitude/star density factor;',
     :    1.0,0.1,20.0,STMULT,ISTAT)
C
C      Obtain Moffat's quantities Beta and R.
C
      CALL READR ('BETA',' Enter Moffat''s quantity beta;',
     :             3.0,0.0,10.0,BETA,ISTAT)
      CALL READR ('R',' Enter Moffat''s quantity R (arcsec);',
     :             5.0,0.01,50.0,R,ISTAT)
C
C      Obtain values for the atmospheric absorption and
C      the instrumantal efficiency.
C
      CALL READR ('ATMABS',
     :   ' Enter value for the total atmosperic absorption;',
     :     2.0E-1,0.0E0,5.0E0,ATMABS,ISTAT)
      CALL READR ('EFFIC',
     :   ' Enter fraction of light lost in telescope & detector;',
     :     9.5E-1,0.0E0,1.0E0,EFFIC,ISTAT)
      EFFIC=1.0E0-EFFIC
C
C      Obtain a value for the plotting threshold.
C
      CALL READR ('THRESH',
     :  ' Enter value for the magnitude plotting threshold;',
     :    20.0,10.0,30.0,THRESH,ISTAT)
C
C      Inquire if a globular cluster is to be included in the field.
C
      CALL YESNO (' Is a globular cluster to be included in the field?',
     :            'Y',REPLY,ISTAT)
      IF (REPLY.EQ.'Y') THEN
        GFLAG=.TRUE.
      ELSE
        GFLAG=.FALSE.
      END IF
C
C      Proceed to obtain parameters for the cluster if it is required.
C
      IF (GFLAG) THEN
C
C      Obtain the no. of stars in the cluster.
C
        CALL READI ('NGLOB',' Enter no. of stars in the cluster;',
     :               100,1,5000,NGLOB,ISTAT)
C
C      Obtain distance to the cluster.
C
        CALL READR ('DIST',
     :   ' Enter distance to the cluster (pc);',
     :     5.1E3,1.0E1,1.0E7,DIST,ISTAT)
C
C      Obtain the total interstellar absorption along the l-o-s
C      to the cluster.
C
        CALL READR ('GABSOR',
     :   ' Enter total interstellar absorption to the cluster;',
     :     2.0E-1,0.0E0,5.0E0,GABSOR,ISTAT)
C
C      Obtain the core and tidal radii.
C
        CALL READR ('RCORE',' Enter core radius (pc);',
     :     7.0E-1,1.0E-2,1.0E3,RCORE,ISTAT)
        CALL READR ('RTIDAL',' Enter tidal radius (pc);',
     :     8.3E1,1.0E-2,1.0E5,RTIDAL,ISTAT)
      END IF
C
C      Attempt to obtain a pointer to the parameter file and encode
C      the given values into it if the pointer is obtained 
C      successfully.
C
      PARSTT=0
      IF (GFLAG) THEN
        AXIS(1)=19
      ELSE
        AXIS(1)=14
      END IF
      CALL OUTPICR ('PARAM',' Enter name of the parameter file;',
     :              1,AXIS,PARPTR,PARSTT)
      IF (PARSTT.EQ.0) THEN
        CALL ENCPAR (STNDRD,IXEXT,IYEXT,PIXSIZ,APERTR,EXPOSE,SKYBRI,
     :               STMULT,BETA,R,ATMABS,EFFIC,THRESH,GFLAG,NGLOB,
     :               DIST,GABSOR,RCORE,RTIDAL,AXIS(1),%VAL(PARPTR))
        CALL OUTPUT (
     :   ' Parameter file generated successfully.',ISTAT)
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access parameter file successfully.',
     :     ISTAT)
      END IF
      CALL CLEARIM ('PARAM')
      END
