      PROGRAM IAMANAL
*+
*   IAMANAL
*
*   COSMOS ANALYSER
*   part of IAM suite
*    given an input image and various thresholds, analyses image
*    and finds all objects and their parameters.
*
*   Given      (program parameters)
*
*    INPUTIMG	(RA)	input image to be analysed - 2d picture array
*    SKY	(R)	sky background level
*    THRLD	(R)	threshold setting
*    AREA	(I)	area cut - smallest number of pixels accepted
*			in an image
*    SKYMAG	(R)	sky background magnitude in one pixel (default 0)
*
*   Returned   (program parameters)
*
*    PARAMS	(RA)	list of parameters for each object - iyext=no.
*			of objects found
*
*   Returned (descriptors in PARAMS)
*
*    SKY	(R)	.................
*    THRLD	(R)	.................
*    AREA	(I)	.................
*    SKYMAG	(R)	.................
*    MAXDIM	(R)	the larger dimension of INPUTIMG, (IGJOB needs
*			it later for versatek)
*
*  Work Arrays (not returned)
*
*    WORK0	(RA)		temporary array for PARAMS - later
*				copied to PARAMS
*    WORK1	(DA)		store space
*    WORK2	(IA)		address space
*    WORK3	(IA)		marker store
*    WORK4	(RA)		single scan
*
*   Subroutines called :
*   IAMANALS,INITSTK,GETSTK,PUTSTK,POP,NEWPORT,PROMAR,ADDPIX : ROEASP
*
*     THE 14 PARAMETERS CALCULATED IN THIS VERSION ARE :-
*     1  -  X-CENTROID UNWEIGHTED
*     2  -  Y-CENTROID UNWEIGHTED
*     3  -  SEMI-MAJOR AXIS UNWEIGHTED
*     4  -  SEMI-MINOR AXIS UNWEIGHTED
*     5  -  ORIENTATION UNWEIGHTED
*     6  -  X-CENTROID WEIGHTED
*     7  -  Y-CENTROID WEIGHTED
*     8  -  SEMI-MAJOR AXIS WEIGHTED
*     9  -  SEMI-MINOR AXIS WEIGHTED
*    10  -  ORIENTATION WEIGHTED
*    11  -  AREA
*    12  -  TOTAL INTENSITY
*    13  -  MAXIMUM INTENSITY
*    18  -  ELLIPTICITY UNWEIGHTED
*    19  -  ELLIPTICITY WEIGHTED
*
*   B.D.Kelly/ROE/1981
*   D. Tudhope/ROE/Sept 1982
*   A C Davenhall./ROE/18.1.84:
*        {Modified to check against exceeding the Max. no. of objects.}
*   A C Davenhall./ROE/5.4.87:
*        {Area cut was "out by 1" causing objects with an area equal
*         to the area cut to be rejected instead of accepted. This bug
*         was fixed so that the software conforms to the documentation
*         and behaves like the COSMOS software.}
*
*   Algorithm by R.K.Lutz.
*-

      INTEGER NPIN,NPTMP,NPSTR,NPADD,NPMAR,NPSCN,NPPAR
      INTEGER NAXIN(2),NAXTMP(2),NAXSTR(2),NAXADD,NAXMAR,NAXSCN,
     :        NAXPAR(2)
      INTEGER AREA,AREA1,ISTATUS,FSTATUS
      REAL    SKY,THRLD,SKYMAG,MAXDIM
      CHARACTER*70 BUFFER
C*  char vars for writing descriptors
      CHARACTER*20 CSKY,CTHRLD,CSKYMAG,CMAXDIM
      CHARACTER*5 CAREA

      ISTATUS=0
C*  input program parameters
      CALL INPICR('INPUTIMG',' ',2,NAXIN,NPIN,ISTATUS)
      CALL READR('SKY',' ',0.0,-1.0E19,1.0E19,SKY,ISTATUS)
      CALL READR('THRLD',' ',0.0,-1.0E19,1.0E19,THRLD,ISTATUS)
      CALL READI('AREA',' ',10,1,10000,AREA,ISTATUS)
      CALL READR('SKYMAG',' ',0.0,0.0,1000000.0,SKYMAG,ISTATUS)
C*  Obtain the max. permitted no. of objects.
      CALL READI ('MAXOBJ',' ',1000,1,1000000,MAXOBJ,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
      IF ((SKY.EQ.-99999.0) .OR. (THRLD.EQ.-99999.0)) THEN
C*  check sky and thrld assigned
        CALL WRERR('NOST')
      ELSE
C*  Define the required size for temporary array to hold output parameters
C*  until at the end the number of parameters is known and they are copied into PARAMS
        NAXTMP(1)=40
        NAXTMP(2)=MAXOBJ
C*  others depend on INPUTIMG
        NAXSTR(1)=16
        NAXSTR(2)=NAXIN(1)+1
        NAXADD=NAXIN(1)
        NAXMAR=NAXIN(1)+1
        NAXSCN=NAXIN(1)
C*  get virtual mem for work arrays
        CALL OUTPICR('WORK0',' ',2,NAXTMP,NPTMP,ISTATUS)
        CALL OUTPICR('WORK1',' ',2,NAXSTR,NPSTR,ISTATUS)
        CALL OUTPICR('WORK2',' ',1,NAXADD,NPADD,ISTATUS)
        CALL OUTPICR('WORK3',' ',1,NAXMAR,NPMAR,ISTATUS)
        CALL OUTPICR('WORK4',' ',1,NAXSCN,NPSCN,ISTATUS)

C*  call IAMANALS to do the work
        AREA1=AREA-1
        AREA1=MAX(AREA1,0)
        CALL IAMANALS(%VAL(NPIN),NAXIN(1),NAXIN(2),SKY,THRLD,AREA1,
     :               SKYMAG,NAXTMP(1),NAXTMP(2),%VAL(NPTMP),NUMIMAG,
     :               %VAL(NPSTR),%VAL(NPADD),%VAL(NPMAR),%VAL(NPSCN),
     :               FSTATUS)
        IF (NUMIMAG.EQ.0) THEN
C*  check analyser actually found some images !
          CALL WRERR('NOIM')
        ELSE
C*  check that there were not more than the max. permitted no. of
C*  images.
          IF (FSTATUS.NE.0) CALL WRERR ('FULL')
C*  copy only the exact number of parameters from temporary array into PARAMS
          NAXPAR(1)=NAXTMP(1)
          NAXPAR(2)=NUMIMAG
          CALL OUTPICR('PARAMS','Enter file for parameters;',
     :                  2,NAXPAR,NPPAR,ISTATUS)
C*  calculate larger dimension of INPUTIMG for use by plotting programs
          MAXDIM=REAL(MAX(NAXIN(1),NAXIN(2)))
C*  output descriptor items for params, describing these results
C*  first convert nos-->chars and then use wrdscr
          WRITE(CSKY,'(F20.5)') SKY
          WRITE(CTHRLD,'(F20.5)') THRLD
          WRITE(CSKYMAG,'(F20.5)') SKYMAG
          WRITE(CMAXDIM,'(F20.5)') MAXDIM
          WRITE(CAREA,'(I5)') AREA
          CALL WRDSCR('PARAMS','SKY',CSKY,1,ISTATUS)
          CALL WRDSCR('PARAMS','THRLD',CTHRLD,1,ISTATUS)
          CALL WRDSCR('PARAMS','SKYMAG',CSKYMAG,1,ISTATUS)
          CALL WRDSCR('PARAMS','MAXDIM',CMAXDIM,1,ISTATUS)
          CALL WRDSCR('PARAMS','AREA',CAREA,1,ISTATUS)
          IF (ISTATUS.NE.0) THEN
            CALL WRERR('MISWRITE')
          ELSE
            CALL COPY(NAXTMP(1),NAXTMP(2),%VAL(NPTMP),
     :                NAXPAR(1),NUMIMAG,%VAL(NPPAR))
C*  print number of objects on terminal
            WRITE(BUFFER,'('' Number of objects = '',I4)'),NUMIMAG
            CALL WRUSER(BUFFER,ISTATUS)
          ENDIF
        ENDIF
      ENDIF
      ENDIF
      END
