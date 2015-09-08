      SUBROUTINE GAUFIT1
*+
*   GAUFIT1
*
*   Calculates image parameters from Gaussian fit to data
*   Aproximate coordinates of the images are taken from
*   elsewhere, and astrometric positions determined.
*
*   B.D.Kelly/ROE/1981
*-
 
      INTEGER NAXIS(2),NCORD(2),NPTR,NPCORD,IST,NXY(2)
      INTEGER NPAREA,NPXM,NPYM,NPSRS,NPSUM,NPNSUM
 
      IST=0

*
*   user-specified parameters
*
      CALL READI('MODE','Give No. of parameters in fit, 3<MODE<5',
     :           3,3,5,MODE,IST)
      CALL READI('IAREA','Give size of fitting area',40,20,512,NX,IST)
      CALL READR('EXP','Give saturation exponent',20.0,1.0,50.0,
     :           P,IST)
      CALL READR('DSAT','Give saturation density',1.0E5,1.0,
     :           1.0E10,DS,IST)
      NXY(1)=NX
      NXY(2)=NX
*
*   Get workspace
*
      CALL OUTPICR('WORK1','WORKSPACE',2,NXY,NPAREA,IST)
      CALL OUTPICR('WORK2','WORKSPACE',1,NX,NPXM,IST)
      CALL OUTPICR('WORK3','WORKSPACE',1,NX,NPYM,IST)
      CALL OUTPICR('WORK4','WORKSPACE',1,NX,NPSRS,IST)
      CALL OUTPICR('WORK5','WORKSPACE',1,NX,NPSUM,IST)
      CALL OUTPICI('WORK6','WORKSPACE',1,NX,NPNSUM,IST)
*
*   Image frame and list of approximate star positions
*
      CALL INPICR('INPIC1','Give input image',2,NAXIS,NPTR,IST)
      CALL INPICR('COORDS','Give source of coordinates',
     :            2,NCORD,NPCORD,IST)
 
      IF(IST.EQ.0) THEN
        CALL GAUFIT2(NAXIS(1),NAXIS(2),%VAL(NPTR),
     :               NCORD(1),NCORD(2),%VAL(NPCORD),
     :               MODE,NXY(1),NXY(2),P,DS,%VAL(NPAREA),
     :               %VAL(NPXM),%VAL(NPYM),%VAL(NPSRS),%VAL(NPSUM),
     :               %VAL(NPNSUM))
      ELSE
        CALL WRUSER('Error accessing the input images',ISTAT)
      ENDIF
 
      CALL CLEARIM('INPIC1')
      CALL CLEARIM('COORDS')
      CALL CLEARIM('WORK1')
      CALL CLEARIM('WORK2')
      CALL CLEARIM('WORK3')
      CALL CLEARIM('WORK4')
      CALL CLEARIM('WORK5')
      CALL CLEARIM('WORK6')
 
      END
