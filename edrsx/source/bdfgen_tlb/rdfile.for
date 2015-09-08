      SUBROUTINE RDFILE(UNIT,DATA,NPT,NFIELD,LOLIM,HILIM,NRD,IERR)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO READ IN DATA FROM LOGICAL UNIT 'UNIT' INTO ARRAY DATA,
*       AND FIND THEIR LIMITS
*
*SOURCE:
*       RDFILE.FOR in BDFGEN.TLB
*
*ARGUMENTS
*       UNIT (IN)
*       INTEGER
*              THE LOGICAL UNIT NUMBER TO READ DATA IN FROM
*       DATA (IN)
*       REAL(NPT,NFIELD)
*              THE ARRAY INTO WHICH THE DATA WILL BE PUT
*       NPT (IN)
*       INTEGER
*              THE NUMBER OF RECORDS TO BE READ IN
*       NFIELD (IN)
*       INTEGER
*              THE NUMBER OF FIELDS IN EACH RECORD TO BE READ
*       LOLIM (OUT)
*       REAL (NFIELD)
*              THE LOWER LIMIT OF EACH FIELD
*       HILIM (OUT)
*       REAL (NFIELD)
*              THE UPPER LIMIT OF EACH FIELD
*       NRD (OUT)
*       INTEGER
*              GIVES THE ACTUAL NUMBER OF VALUES IN THE ARRAYS
*       IERR (OUT)
*       INTEGER
*              ERROR FLAG. ZERO FOR SUCCESS.
*
*CALLS
*       DSB PROGS:
*              RDFILR
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
C
C
      PARAMETER (MAXVAL=20)

      INTEGER UNIT
      LOGICAL OK
      REAL VAL(MAXVAL),DATA(NPT,NFIELD),HILIM(NFIELD),LOLIM(NFIELD)
      CHARACTER PRBUF*80
C
C INITIALIZE ERROR FLAG AND ERROR COUNTERS
C
      IERR=0
      NRE=0
C
C INITIALIZE LIMITS
C
      DO I=1,NFIELD
         HILIM(I)=-1.0E32
         LOLIM(I)=1.0E32
      ENDDO
C
C READ THROUGH RECORDS
C
      NRD=0
      DO I=1,NPT
         NRD=NRD+1
C
C CALL RDFILR TO READ IN THE NFIELD VALUES
C
   5     CALL RDFILR(UNIT,VAL,NFIELD,NVAL,1,IERR)
C
C IF READ PERFORMED SUCCESSFULLY STORE THE DATA
C
         IF(IERR.EQ.0) THEN
            DO J=1,NVAL
               DATA(I,J)=VAL(J)
            ENDDO
         ELSE
C
C DEAL WITH READING ERRORS
C
            IF(IERR.EQ.-1.OR.IERR.GT.0) THEN
               NRD=NRD-1
               GOTO 999
            ENDIF
            IF(IERR.EQ.-3) THEN
               WRITE(PRBUF,34) NRD
   34          FORMAT('*** AT OR NEAR RECORD ',I7)
               CALL LBGONE(PRBUF(23:))
               CALL WRUSER(PRBUF,ISTAT)
               GOTO 5
            ELSE
               IF(NVAL.LE.1) GOTO 5
            ENDIF
         ENDIF
C
C UPDATE LIMITS
C
         DO J=1,NVAL
            HILIM(J)=MAX(HILIM(J),VAL(J))
            LOLIM(J)=MIN(LOLIM(J),VAL(J))
         ENDDO

  10  ENDDO

C
C FINISH
C
 999  RETURN
      END
