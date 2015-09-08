      SUBROUTINE CVPLOTS
*+
*
*     CVPLOTS
*
*     Contour and Vector PLOT Subroutine
*     this routine produces an anotated contour and vector plot
*     of polarization data . three input images are needed .
*     one image which is to be contoured and the corresponding
*     total polarization and polarization angle images .
*     
*     Subroutines called :
*     INPICR , OUTPICR , YESNO , CLEARIM , WRUSER      : ASPFACE
*     CVINFO , CVCONT , CADD1 , COPY , RAPBLO , RAPGAU : E2DASP
*
*     original author C Aspin
*     D.W.T.Baines/ROE/Feb/1983
*
*-
      INTEGER NAXIS(2) , NAXIS2(2) , NAXISP(2) , NAXIST(2)
      INTEGER NAXW1(2) ,  NAXW2(2) ,  NAXW3(2) ,  NAXW4(2)
      INTEGER NPTR , NPTR2 , NPTRP , NPTRT , NPTW1 , NPTW2
      INTEGER NPTW3 , NPTW4 , NPTW5 , NPTW6 , NPTW7
      INTEGER NAXW5 , NAXW6 , NAXW7 , NCON , NSIZE , NXA , NXB
      INTEGER IST , ISTAT
      REAL BASE , CINT , PI , SCAL , SIGMA , THADD
      CHARACTER*1 REPLY , LOOP
      CHARACTER*80 TITLE
      CHARACTER*8 DEVICE , SMOOTH
      PARAMETER ( PI = 3.141593 )
*
*     the loop control variable is initially 'Y'
*
      LOOP = 'Y'
*
*     initialize the staus return parameters to 0
*
      IST = 0
      ISTAT = 0
*
*     initialize size of smoothing box 
*
      NSIZE = 1
*
*     request the image to be contoured
*
      CALL INPICR('INFILE','Data file for contour plot',
     :            2,NAXIS,NPTR,IST)
*
*     request the polarization image
*
      CALL INPICR('INFILP','Total Polarization data file',
     :            2,NAXISP,NPTRP,IST)
*
*     request the angle image
*
      CALL INPICR('INFILT','Polarization angle data file',
     :            2,NAXIST,NPTRT,IST)
*
*     if all files have been accessed o.k. carry on
*
      IF ( IST .EQ. 0 ) THEN
*
*        check that all the images are the same size
*
         IF ( NAXIS(1).EQ.NAXISP(1) .AND. NAXIS(1).EQ.NAXIST(1) .AND. 
     :        NAXIS(2).EQ.NAXISP(2) .AND. NAXIS(2).EQ.NAXIST(2) ) THEN
*
*           perform the plotting loop while LOOP = 'Y'
*
            DO WHILE ( LOOP .EQ. 'Y' )
*
*              get all the neccessary information for the plot
*
               CALL CVINFO (SCAL,THADD,TITLE,NCON,BASE,CINT,SMOOTH,
     :                      NSIZE,SIGMA,DEVICE)
*
*              set up all the neccessary workspace
*
               NAXIS2(1)=NAXIS(1)
               NAXIS2(2)=NAXIS(2)
               CALL OUTPICR('WORK','SMOOTHED CONTOUR DATA',
     :                      2,NAXIS2,NPTR2,ISTAT)
               NAXW1(1) = NAXIS(1)
               NAXW1(2) = NAXIS(2)
               CALL OUTPICR ('WORK1','ANGLE + OFFSET',
     :                       2,NAXW1,NPTW1,ISTAT)
               NAXW2(1) = NAXIS(1)
               NAXW2(2) = NAXIS(2)
               CALL OUTPICR ('WORK2','VECTOR WORKSPACE 1',
     :                       2,NAXW2,NPTW2,ISTAT)
               NAXW3(1) = NAXIS(1)
               NAXW3(2) = NAXIS(2)
               CALL OUTPICR ('WORK3','VECTOR WORKSPACE 2',
     :                       2,NAXW3,NPTW3,ISTAT)
               NAXW4(1) = NAXIS(1)
               NAXW4(2) = NAXIS(2)
               CALL OUTPICR ('WORK4','2-D ROLLING BUFFER',
     :                       2,NAXW4,NPTW4,ISTAT)
               NAXW5 = NAXIS(1)
               CALL OUTPICR ('WORK5','MARGINAL SUM',
     :                       1,NAXW5,NPTW5,ISTAT)
               NAXW6 = NSIZE
               CALL OUTPICR ('WORK6','1-D ROLLING BUFFER',
     :                       1,NAXW6,NPTW6,ISTAT)
               NAXW7 = NSIZE
               CALL OUTPICR ('WORK7','WEIGHTS WORKSPACE',
     :                       1,NAXW7,NPTW7,ISTAT)
*
*              add in the offset angle THADD to the angle frame
*
               CALL CADD1 (NAXIST(1),NAXIST(2),%VAL(NPTRT),THADD,
     :                     NAXW1(1),NAXW1(2),%VAL(NPTW1))
*
*              copy the data to be contoured into a work array ready for any
*              smoothing
*
               NXA = NAXIS(1)
               NXB = NAXIS(2)
               CALL COPY (NAXIS(1),NAXIS(2),%VAL(NPTR),
     :                    NXA,NXB,%VAL(NPTR2))
*
*              sort out what smoothing was required
*
               IF ( SMOOTH .EQ. 'BOX') THEN
                  CALL RAPBLO (NSIZE,NAXIS(1),NAXIS(2),%VAL(NPTR),
     :                         %VAL(NPTR2),%VAL(NPTW4),%VAL(NPTW5),
     :                         ISTAT)
               ELSEIF ( SMOOTH .EQ. 'GAUSSIAN' ) THEN
                  CALL RAPGAU (SIGMA,NSIZE,NAXIS(1),NAXIS(2),
     :                         %VAL(NPTR2),%VAL(NPTW4),%VAL(NPTW6),
     :                         %VAL(NPTW7),%VAL(NPTW5))
               ENDIF
               CALL CVCONT (NAXIS(1),NAXIS(2),%VAL(NPTRP),
     :                      %VAL(NPTW1),SCAL,NCON,BASE,CINT,NSIZE,
     :                      SIGMA,TITLE,SMOOTH,DEVICE,%VAL(NPTR2),
     :                      %VAL(NPTW2),%VAL(NPTW3))
*
*              if the graphics device being used is not already the versatek
*              ask if a copy of the plot is to be produced on the versatek
*
               IF ( DEVICE .NE. 'VERSATEK' ) THEN
                  CALL YESNO ('Produce copy of plot on the versatek ?',
     :                        'N',REPLY,ISTAT)
                  IF ( REPLY.EQ.'Y' ) THEN
                     DEVICE = 'VERSATEK'
                     CALL CVCONT (NAXIS(1),NAXIS(2),%VAL(NPTRP),
     :                            %VAL(NPTW1),SCAL,NCON,BASE,CINT,NSIZE,
     :                            SIGMA,TITLE,SMOOTH,DEVICE,%VAL(NPTR2),
     :                            %VAL(NPTW2),%VAL(NPTW3))
                  END IF
               END IF
*
*              clear the workspace
*
               CALL CLEARIM ('WORK')
               CALL CLEARIM ('WORK1')
               CALL CLEARIM ('WORK2')
               CALL CLEARIM ('WORK3')
               CALL CLEARIM ('WORK4')
               CALL CLEARIM ('WORK5')
               CALL CLEARIM ('WORK6')
               CALL CLEARIM ('WORK7')
*
*              find out if another plot is to be performed
*
               CALL YESNO ('Do you want to do another plot ?',
     :                     'N',REPLY,ISTAT)
*
*              set the loop control variable to the reply value
*
               LOOP = REPLY
            END DO
*
*           clear the input images
*
	      CALL CLEARIM('INFILE')
            CALL CLEARIM('INFILP')
            CALL CLEARIM('INFILT')
         ELSE
*
*           here if the sizes of the input files are different
*
            CALL WRUSER ('Error : Input mages are not same size',
     :                   ISTAT)
         ENDIF
      ELSE
*
*        here if an error accessing input data files
*
         CALL WRUSER ('Error accessing input data files',ISTAT)
      ENDIF
      END
