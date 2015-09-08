C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYARITH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYARITH
C
C
C          FUNCTION:-
C               This allows you to make a new XY list from one or two old
C               XY lists with the output parameters being of the form
C
C                     oparam(n) = a  *aparam(1) + a  *aparam(2) + ....
C                                  n1              n2
C
C                                 + b  *bparam(1) + b  *bparam(2) + ....
C                                    n1              n2
C
C                                 + c
C                                    n
C
C                Thus you can add,subtract,multiply by consts any
C                combination of parameters in either input file.
C
C                The XY positions in a file are the 1st two params
C
C
C
C
C          USE:-
C               Input one or two input files, input how many parameters
C               the output file has and the combining factors. The
C               program does the transfer.
C               The inputting of the factors is done as :-
C                 They are all put to zero and the default asking is
C                 for the corresponding params no with a factor of 1, so
C                 it would make a sum of the two input files.
C               Remember that the 1st two parameters (usually the XY posns)
C               are treated as ordinary parameters
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              This is the name of the  .BDF
C                                             1st input file
C
C         INPUTB                              This is the name of the
C                                             optional 2nd file
C
C         NUMXYP      That of 1st file        The no of params in the output
C                                             file
C
C         APARNO          See above           The no of the param in the
C                                             1st file that the following
C                                             factor is to be applied to.
C
C         FACTOR          See above           The factor in the transfer
C                                             equations.
C
C         BPARNO          See above           The no of the param in the
C                                             1st file that the following
C                                             factor is to be applied to.
C
C         CONST           0.0                 The constant to be added to
C                                             this number
C
C         OUTPUT                              The name of the .BDF output
C                                             output file.
C
C         TITLE    Output from XYARITH       The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                21-SEP-82
C
C
C--------------------------------------------------------------------------



*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUTA
*     INPUTB
*            The input EDRS XY file
*     NUMXYP
*     APARNO
*     BPARNO
*     FACTOR
*     OUTPUT
*     TITLE
*
*  CALLS
*      EDRS Package
*            GTXYLR,GTDSCR
*      STARLINK:
*            WRUSER,FRDATA
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-9-21
* ------------------------------------------------------------



      PROGRAM XYARITH
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,PRBUF*72
      REAL AFACT(30,30),BFACT(30,30),ACONST(30)
      LOGICAL*1 VALID
C
C  Set continuation

C
      VALID = .TRUE.
C
C  Obtain input XY list
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEMA,LSTLEN,IPINA,IERR)
      NPARMA = NITEMA - 5
      IF(IERR.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         TITLE = 'Output from XYARITH'
         CALL GTDSCR('INPUTA','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
         WRITE(PRBUF,900)NPARMA
  900    FORMAT(' ','NO OF PARAMS = ',I5)
         CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C  Obtain optional 2nd input XY list
C
      CALL GTXYLR('INPUTB',.TRUE.,NITEMB,LSTLENB,IPINB,IERR)
      NPARMB = NITEMB - 5
      IF (IERR.EQ.1) THEN
         NITEMB = NITEMA
         LSTLENB = LSTLEN
         KTWO = 0
      ELSE
         IF (IERR.EQ.0) THEN
            KTWO = 1
         ELSE
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Extract  2nd Title
C
      IF (VALID) THEN
         IF (KTWO.EQ.1) THEN
            TITLE = ' '
            CALL GTDSCR('INPUTB','TITLE','CHARACTER',IVAL,RVAL,
     +                  TITLE,IERR)
            WRITE (PRBUF,902)TITLE
            CALL WRUSER(PRBUF,ISTAT)
            WRITE(PRBUF,900)NPARMB
            CALL WRUSER(PRBUF,ISTAT)
         ENDIF
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF ((LSTLEN.EQ.0).OR.(LSTLEN.NE.LSTLENB).OR.
     +       (NITEMA.EQ.0).OR.(NITEMB.EQ.0)) THEN
            CALL WRUSER('FILES DONT MATCH OR ZERO',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get the Output file no of parameters
C
      IF (VALID) THEN
         NPARMC = NPARMA
         CALL RDKEYI('NUMXYP',.TRUE.,1,NPARMC,NVAL,ISTAT)
         NITEMC = NPARMC + 5
      ENDIF
C
C  Get the equations factors
C
      IF (VALID) THEN
         DO K = 1,NPARMC
            CALL WRUSER(' ',ISTAT)
            WRITE(PRBUF,910)K
  910       FORMAT(' ','OUTPUT PARAM NO',I3,'  END THIS PARAM BY -1')
            CALL WRUSER(PRBUF,ISTAT)
            NPAR = K - 1
            CALL WRUSER('GET INPUTA FACTORS',ISTAT)
            DO J = 1,PARMA
               AFACT(K,J) = 0.0
            ENDDO
            KROUND = 0
            DO WHILE (NPAR.NE.-1)
               KROUND = KROUND + 1
               NPAR = NPAR + 1
               KNPAR = NPAR
               IF (KROUND.EQ.2) KNPAR = -1
               IF (KNPAR.GT.NPARMA) KNPAR = -1
               CALL CNPAR('APARNO',ISTAT)
               CALL RDKEYI('APARNO',.TRUE.,1,KNPAR,NVAL,ISTAT)
               NPAR = KNPAR
               IF (NPAR.GT.NPARMA) THEN
                  CALL WRUSER('TOO LARGE !',ISTAT)
               ELSE
                  IF (NPAR.GE.1) THEN
                     CALL CNPAR('FACTOR',ISTAT)
                     FACTOR = 1.0
                     CALL RDKEYR('FACTOR',.TRUE.,1,FACTOR,NVAL,IERR)
                     AFACT(K,NPAR) = FACTOR
                  ENDIF
               ENDIF
            ENDDO
C
C
C
            IF (KTWO.EQ.1) THEN
               CALL WRUSER(' ',ISTAT)
               CALL WRUSER('GET INPUTB FACTORS',ISTAT)
               DO J = 1,NPARMB
                  BFACT(K,J) = 0.0
               ENDDO
               NPAR = K - 1
               KROUND = 0
               DO WHILE (NPAR.NE.-1)
                  NPAR = NPAR + 1
                  KROUND = KROUND + 1
                  KNPAR = NPAR
                  IF (KROUND.EQ.2) KNPAR = -1
                  IF (KNPAR.GT.NPARMB) KNPAR = -1
                  CALL CNPAR('BPARNO',ISTAT)
                  CALL RDKEYI('BPARNO',.TRUE.,1,KNPAR,NVAL,ISTAT)
                  NPAR = KNPAR
                  IF (NPAR.GT.NPARMB) THEN
                     CALL WRUSER('TOO LARGE !',ISTAT)
                  ELSE
                     IF (NPAR.GE.1) THEN
                        CALL CNPAR('FACTOR',ISTAT)
                        FACTOR = 1.0
                        CALL RDKEYR('FACTOR',.TRUE.,1,FACTOR,NVAL,
     +                              IERR)
                        BFACT(K,NPAR) = FACTOR
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C
C  Get constant factor
C
         CALL WRUSER(' ',ISTAT)
         CALL WRUSER('CONSTANT FACTORS',ISTAT)
         DO K = 1,NPARMC
            ACONST(K) = 0.0
         ENDDO
         DO K = 1,NPARMC
            WRITE(PRBUF,940)K
  940       FORMAT(' ','OUTPUT PARAM NO ',I3)
            CALL WRUSER(PRBUF,ISTAT)
            CALL CNPAR('CONST',ISTAT)
            ACONST(K) = 0.0
            CALL RDKEYR('CONST',.TRUE.,1,ACONST(K),NVAL,ISTAT)
         ENDDO
C
C
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEMC,LSTLEN,IPOUT,IERR)
         IF (IERR.NE.0) THEN
            CALL WRUSER('CANT STORE OUTPUT LIST',IERR1)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C
      IF (VALID) THEN
C
C Get title to output list and store it and the descriptors
C
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEMC,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +               IERR)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
C
C  Do the loading of the Output from the Inputs
C
         CALL DOXYAR(%VAL(IPINA),NITEMA,LSTLEN,%VAL(IPINB),
     +               NITEMB,%VAL(IPOUT),NITEMC,AFACT,BFACT,ACONST,
     +               KTWO)
C
C  Copy the 1st input file identifiers over
C
         CALL EXTLSA(%VAL(IPINA),NITEMA,LSTLEN,1,LSTLEN,1,5,
     +               %VAL(IPOUT),NITEMC,LSTLEN,1,1)
C
C  Type no of entries
C
         IF (LSTLEN.NE.1) THEN
            WRITE(PRBUF,920)LSTLEN
  920       FORMAT(T22,I7,' LIST ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
  921       FORMAT (' ',22X,I6,' LIST ENTRIES')
         ELSE
            CALL WRUSER('                         1 LIST ENTRY',ISTAT)
  922       FORMAT (' ',22X,'   ONE LIST ENTRY')
         ENDIF
      ENDIF
C
C  Free data area
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      ************
C      *          *
C      * S/R DOXYAR *
C      *          *
C      ************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE DOXYAR(DATA,NITEMA,LSTLEN,DATB,NITEMB,RES,
     +                NITEMC,AFACT,BFACT,ACONST,KTWO)
C
C
C
      REAL DATA(NITEMA,LSTLEN),DATB(NITEMB,LSTLEN)
      REAL RES(NITEMC,LSTLEN)
      REAL AFACT(30,30),BFACT(30,30),ACONST(30)
C
C
C
      NPARMA = NITEMA - 5
      NPARMB = NITEMB - 5
      NPARMC = NITEMC - 5
C
C
C
      DO K = 1,LSTLEN
         DO J = 1,NPARMC
            JA = J + 5
            RES(JA,K) = 0.0
            DO L = 1,NPARMA
               LA = L + 5
               RES(JA,K) = RES(JA,K) + AFACT(J,L)*DATA(LA,K)
            ENDDO
            IF (KTWO.EQ.1) THEN
               DO L = 1,NPARMB
                  LA = L + 5
                  RES(JA,K) = RES(JA,K) + BFACT(J,L)*DATB(LA,K)
               ENDDO
            ENDIF
            RES(JA,K) = RES(JA,K) + ACONST(J)
         ENDDO
      ENDDO
C
C
C
      END



