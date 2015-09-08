      SUBROUTINE COORDS1
*+
*   COORDS1
*
*   Allow a user to specify up to 500 positions in the current
*   image using the ARGS cursor.
*
*   Subroutines called :
*   COORDS2            : E2DLIB
*
*   B.D.Kelly/ROE/1.12.1981
*   D.W.T.Baines/ROE/April 1983
*-
      INTEGER NCORD(2),IST,ISTAT,NPTR,NTEMP(2),NPTRT
*
*    initialise status variables
*
      IST=0
      ISTAT=0
*
*    set up workspace to hold 500 coordinates
*
      NTEMP(1) = 3
      NTEMP(2) = 500
      CALL OUTPICR('WORK1','WORKSPACE FOR COORDS',2,NTEMP,NPTRT,IST)
      IF(IST.EQ.0) THEN
         CALL COORDS2(NTEMP(1),NTEMP(2),%VAL(NPTRT),NPTS)
         NCORD(1) = 3
         NCORD(2) = NPTS
         CALL OUTPICR('COORDS','Give file for coordinates',
     :                2,NCORD,NPTR,IST)
         IF(IST.EQ.0)THEN
            NXST = 1
            NXFN = NCORD(1)
            NYST = 1
            NYFN = NCORD(2)
            CALL SUBIM1(NTEMP(1),NTEMP(2),%VAL(NPTRT),NXST,NXFN,
     :                  NYST,NYFN,NCORD(1),NCORD(2),%VAL(NPTR))
         ELSE
            CALL WRUSER('Error accessing coordinate output file',
     :                  ISTAT)
         ENDIF
      ELSE
         CALL WRUSER('Error allocating workspace',ISTAT)
      ENDIF

      CALL CLEARIM('WORK1')
      CALL CLEARIM('COORDS')

      END
