      SUBROUTINE SIMSTR
C+
C     SIMSTR.
C 
C     Subroutine to generate an artificial star field and output
C     it as a Starlink image. A file containing the positions
C     and apparent magnitudes of all the stars generated is
C     also produced. In addition a listing of the star parameters
C     is printed and a finding chart plotted.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPICR, OUTPUT, CLEARIM.
C   E2D:-         KOPY.
C   SIMIM:-       DECPAR, STRSIM, CATCOPY.
C
C  Structure:-
C   Attempt to obtain pointer to parameter file.
C   If pointer obtained Ok
C     Decode the parameter file.
C     Attempt to obtain a pointer to the field luminosity function
C     If (required) Attempt to obtain a pointer to the globular
C                   cluster luminosity function.
C     If (luminosity functions obtained OK)
C       Copy luminosity functions to work arrays.
C       Attempt to obtain a pointer to the output image
C       If pointer obtained OK
C         Generate star field etc.
C         Attempt to obtain a pointer to the star catalogue file
C         If pointer obtained Ok
C           copy star catlogue to output file.
C         else
C           print message.
C         end if
C       else
C         print message
C       end if
C     else
C       print messsage.
C     end if
C   else
C     print message.
C   end if
C   Tidy up images.
C
C  A C Davenhall./ROE/                                  22/12/82.
C-
C
C    Luminosity function arrays.
C
      INTEGER MXBIN,NBIN
      PARAMETER (MXBIN=30)
      REAL MGLMFN(MXBIN),NLMFN(MXBIN)
      INTEGER GMXBIN,GBIN
      PARAMETER (GMXBIN=30)
      REAL GLMFN(GMXBIN),GNLMFN(GMXBIN)
C
C    Arrays to hold stellar parameters.
C
      INTEGER MAXSTR,NSTAR
      PARAMETER (MAXSTR=1000)
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR),
     :     WORK1(MAXSTR),WORK2(MAXSTR),WORK3(MAXSTR)
C
C    Miscellaneous.
C
      INTEGER IOSTAT,PRSTAT,UNIT3
      PARAMETER (UNIT3=22)
C
C    Pointers, sizes and statuses of Starlink images.
C
      INTEGER AXIS(2),AXIS1(1),
     :        IMPTR,IMSTAT,
     :        CATPTR,CATSTT,
     :        LUMPTR,LUMSTT,
     :        GLBPTR,GLBSTT,
     :        PARPTR,PARSTT
C
C    Quantities decoded from the parameter file.
C
      INTEGER STNDRD,XEXT,YEXT,NGLOB
      REAL PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,BETA,R,ATMABS,
     :     EFFIC,THRESH,DIST,RCORE,RTIDAL
      LOGICAL GFLAG
C
C
      IOSTAT=0
C
C    Attempt to obtain a pointer to the parameter file.
C
      PARSTT=0
      CALL INPICR ('PARAM',' Enter name of the parameter file;',
     :             1,AXIS1,PARPTR,PARSTT)

C
C    Proceed if the pointer has been obtained Ok.
C
      IF (PARSTT.EQ.0) THEN
C
C    Decode the parameter file.
C
        CALL DECPAR (AXIS1(1),%VAL(PARPTR),
     :               STNDRD,XEXT,YEXT,
     :               PIXSIZ,APERTR,EXPOSE,SKYBRI,STMULT,BETA,R,
     :               ATMABS,EFFIC,THRESH,GFLAG,NGLOB,DIST,
     :               GABSOR,RCORE,RTIDAL)
C
C    Attempt to obtain a pointer to the field luminosity function
C    file.
C
        LUMSTT=0
        CALL INPICR ('LUMFUN',
     :   ' Enter name of the field luminosity function file;',
     :     2,AXIS,LUMPTR,LUMSTT)
        NBIN=AXIS(1)
C
C    Attempt to obtain a pointer to the globular cluster luminosity
C    function file.
C
        GLBSTT=0
        CALL INPICR ('GLBFUN',
     :     ' Enter name of globular cluster luminsity function file;',
     :       2,AXIS,GLBPTR,GLBSTT)
        GBIN=AXIS(1)
C
        IF (LUMSTT.EQ.0.AND.GLBSTT.EQ.0) THEN
C
C    Copy the luminosity functions to the work arrays.
C
          CALL KOPY (%VAL(LUMPTR),NBIN,MXBIN,MGLMFN,NLMFN)
          IF (GFLAG) CALL KOPY (%VAL(GLBPTR),GBIN,GMXBIN,
     :                          GLMFN,GNLMFN)
C
C    Attempt to obtain a pointer to the image to hold the
C    generated star field.
C
          AXIS(1)=XEXT
          AXIS(2)=YEXT
          IMSTAT=0
          CALL OUTPICR ('FIELD',' Enter file name for star field;',
     :                  2,AXIS,IMPTR,IMSTAT)
C
          IF (IMSTAT.EQ.0) THEN
C
C    Generate the star field etc.
C
            PRSTAT=0
            CALL STRSIM (STNDRD,XEXT,YEXT,PIXSIZ,APERTR,EXPOSE,
     :                   SKYBRI,STMULT,BETA,R,ATMABS,EFFIC,
     :                   THRESH,MAXSTR,MXBIN,NBIN,MGLMFN,NLMFN,
     :                   GFLAG,GMXBIN,GBIN,GLMFN,GNLMFN,
     :                   NGLOB,DIST,GABSOR,RCORE,RTIDAL,UNIT3,
     :                   WORK1,WORK2,WORK3,NSTAR,XCORD,YCORD,
     :                   APMAG,%VAL(IMPTR),PRSTAT)
            IF (PRSTAT.NE.0) CALL OUTPUT (
     :        ' ***ERROR Unable to print listing correctly.',IOSTAT)
C
C    Attempt to obtain a pointer to the Starlink image to hold
C    the star catlogue.
C
            CATSTT=0
            AXIS(1)=3
            AXIS(2)=NSTAR
            CALL OUTPICR ('CATAL',
     :       ' Enter file name for star catlogue;',
     :         2,AXIS,CATPTR,CATSTT)
C
C    Copy the catologue if the pointer has been obtained Ok.
C
            IF (CATSTT.EQ.0) THEN
              CALL CATCOPY (MAXSTR,NSTAR,XCORD,YCORD,APMAG,
     :                      %VAL(CATPTR))
            ELSE
              CALL OUTPUT (
     :  ' ***ERROR Unable to obtain star catalogue file correctly.',
     :                     IOSTAT)
            END IF
          ELSE
            CALL OUTPUT (
     :  ' ***ERROR Unable to obtain image file correctly.',IOSTAT)
          END IF
        ELSE
          CALL OUTPUT (
     :  ' ***ERROR Unable to input luminosity functions successfully.',
     :                 IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :' ***ERROR Unable to access control parameter file successfully.',
     :               IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('FIELD')
      CALL CLEARIM ('CATAL')
      CALL CLEARIM ('LUMFUN')
      CALL CLEARIM ('GLBFUN')
      CALL CLEARIM ('PARAM')
C
      END
