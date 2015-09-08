      SUBROUTINE STXY
*+
*     STXY
*
*     Calculates the X,Y coordinates of an image from Gaussian fit to data
*     at a point defined by a cursor.
*
*     Calls : SRINIT
*             INPICR,OUTPICR,READI,WRUSER,CLEARIM
*             BOXCUR,CURFITXY
*
*     B.D.Kelly/ROE/1981
*     D.W.T.Baines/ROE/JAN 1983
*-
      INTEGER IX,IY,ISIZE,NX,ISTAT,IFAIL
      INTEGER NAXIS(2),NPTR,IST,NXY(2)
      INTEGER NPAREA,NPXM,NPYM
*
*     set all status return variables to zero
*
      IST=0
      ISTAT=0
      IFAIL=0
*
*     set initial cursor position to 256,256
*
      IX=256
      IY=256
*
*     allocate the args without reseting it
*
      CALL SRINIT (0,.FALSE.,IFAIL)
*
*     if ifail is 0 then continue , otherwise report error and end
*
      IF(IFAIL.EQ.0) THEN
*
*        get the initial size of the fitting area
*
         CALL READI('IAREA','GIVE INITIAL SIZE OF FITTING AREA',40,20,
     :              512,NX,ISTAT)
         NXY(1)=NX
         NXY(2)=NX
         ISIZE=NX
*
*        get the Image frame
*
         CALL INPICR('INPIC1','GIVE INPUT IMAGE',2,NAXIS,NPTR,IST)
         IF(IST.EQ.0) THEN
*
*           open the results listing file
*
            OPEN(UNIT=32,NAME='OBJECTS.LIS',TYPE='NEW',
     :           ACCESS='SEQUENTIAL',FORM='FORMATTED')
*
*           call up the cursor to start off the loop
*
            CALL WRUSER ('CENTRE BOX ON OBJECT',ISTAT)
            CALL BOXCUR (IX,IY,ISIZE)
*
*           if IX and IY are both non-zero , go through the loop
*
            DO WHILE (IX.NE.0 .AND. IY.NE.0)
*
*              check size of cursor for sensible size of fitting area
*
               IF (ISIZE.LT.20 .OR. ISIZE.GT.512) THEN
*
*                 if not , inform user and take ISIZE equal to 40
*
                  CALL WRUSER ('SIZE OF FITTING AREA OUT OF RANGE',
     :                         ISTAT)
                  CALL WRUSER ('40 ASSUMED',ISTAT)
                  ISIZE = 40
               ENDIF
*
*              if NX is no longer equal to ISIZE then reset it 
*
               IF(NX.NE.ISIZE) THEN
                  NX=ISIZE
                  NXY(1)=NX
                  NXY(2)=NX
               ENDIF
*
*              get workspace
*
               CALL OUTPICR('WORK1','WORKSPACE',2,NXY,NPAREA,IST)
               CALL OUTPICR('WORK2','WORKSPACE',1,NX,NPXM,IST)
               CALL OUTPICR('WORK3','WORKSPACE',1,NX,NPYM,IST)
*
*              perform the fitting
*
               CALL CURFITXY(NAXIS(1),NAXIS(2),%VAL(NPTR),IX,IY,
     :                       NXY(1),NXY(2),%VAL(NPAREA),%VAL(NPXM),
     :                       %VAL(NPYM))
*
*              clear the workspace
*
               CALL CLEARIM('WORK1')
               CALL CLEARIM('WORK2')
               CALL CLEARIM('WORK3')
*
*              call the cursor
*
               CALL WRUSER ('CENTRE BOX ON OBJECT',ISTAT)
               CALL BOXCUR (IX,IY,ISIZE)
            END DO
*
*           close the results listing file
*
            CLOSE(UNIT=32)
         ELSE
*
*           here on error in INPICR
*
            CALL WRUSER('ERROR IN STARXY',ISTAT)
            CALL WRUSER('UNABLE TO ACCESS INPUT IMAGE SUCCESSFULLY',
     :                  ISTAT)
         ENDIF
*
*        clear the input image
*
         CALL CLEARIM('INPIC1')
      ELSE
*
*        here on error allocating the args
*
         CALL WRUSER ('ERROR IN ALLOCATING THE ARGS',ISTAT)
      ENDIF
      END
