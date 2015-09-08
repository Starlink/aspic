C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYMULTA *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYMULTA
C
C
C          FUNCTION:-
C              This takes two XYlists and makes a third by copying
C              over the first one, replacing the second parameter in it
C              with the multiple of the second parameters of the first and
C              second files.
C
C              The multiplication of the 2nd parameters is not parameter
C              by matching parameter. Rather for each value of the 2nd
C              param in the 1st file, the 1st param is taken and then an
C              the 1st param in the 2nd file that is equal to it found,
C              the matching 2nd param in the 2nd file taken and used to
C              multiply the 2nd param in the 1st file.
C
C              Thus in the two files the 1st parameter MUST be in
C              strict  ASCENDING order. They do not have to cover equal
C              ranges.
C
C              As there may not be an exactly equal values 1st param in
C              the 2nd file, the actual value taken for the 2nd param
C              in the 2nd file is found by cubic fitting over the 4
C              points having 1st params around the value of the 1st
C              param in the 1st file.
C
C              If the 2nd file doesnt have 1st params covering the range
C              of the 1st param in the 1st file, then the value of the
C              2nd param nearest (in 1st param terms) is taken.
C
C              This program was written to multiply two response functions
C              together.
C
C          USE:-
C               Input the files (remember it is the 2nd file which is only
C               used to find interpolated multiplying factors).
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              This is the name of the  .BDF
C                                             1st input file
C
C         INPUTB                              This is the name of the
C                                             2nd file
C
C         OUTPUT                              The name of the .BDF output
C                                             output file.
C
C         TITLE    Output from XYMULTA       The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                83-2-16
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
*       A.J. PENNY                                      83-2-16
* ------------------------------------------------------------



      PROGRAM XYMULTA
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,TEXT*72
      CHARACTER TITLEA*30
      LOGICAL*1 VALID
C
C  Set continuation

C
      VALID = .TRUE.
C
C  Obtain input XY list
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEM,LSTLEN,IPIN,IERR)
      IF(IERR.NE.0) VALID = .FALSE.
C
C  Extract Title
C
      IF (VALID) THEN
         CALL GTDSCR('INPUTA','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +               IERR)
         WRITE(TEXT,902)TITLE
  902    FORMAT(' ','TITLE IS  ',A30)
         CALL WRUSER(TEXT,ISTAT)
         CALL CHARLN(TITLE,KLEN)
         IF (KLEN.EQ.0) TITLE = 'OUTPUT FROM XYMULTA'
      ENDIF
C
C  Obtain 2nd input XY list
C
      IF (VALID) THEN
         CALL GTXYLR('INPUTB',.TRUE.,NITEMB,LSTLENB,IPINB,IERR)
         IF (IERR.NE.0) THEN
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Extract  2nd Title
C
      IF (VALID) THEN
         CALL GTDSCR('INPUTB','TITLE','CHARACTER',IVAL,RVAL,
     +                  TITLEA,IERR)
         WRITE (TEXT,902)TITLEA
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C  Check that there are some values to store
C
      IF (VALID) THEN
         IF ((LSTLEN.EQ.0).OR.(LSTLENB.EQ.0)) THEN
            CALL WRUSER('FILE ZERO',ISTAT)
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Open output list
C
      IF (VALID) THEN
         CALL GTXYLW('OUTPUT',.FALSE.,NITEM,LSTLEN,IPOUT,IERR)
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
         CALL CYDSCR('INPUTA','OUTPUT',IERR)
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
         CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
C
C  Do the loading of the Output from the 1st list
C
         CALL EXTLSA(%VAL(IPIN),NITEM,LSTLEN,1,LSTLEN,1,NITEM,
     +               %VAL(IPOUT),NITEM,LSTLEN,1,1)
C
C  Load the second parameter
C
         CALL DOMULT(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPINB),NITEMB,
     +               LSTLENB,%VAL(IPOUT),NITEM,LSTLEN,IERR)
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
C      * S/R DOMULT *
C      *          *
C      ************
C
C
C
C ---------------------------------------------------------
C
C
C
      SUBROUTINE DOMULT(DATA,NXA,NYA,DATB,NXB,NYB,OUT,NXC,NYC,
     +                  IERR)
C
C
C
      REAL DATA(NXA,NYA),DATB(NXB,NYB),OUT(NXC,NYC)
      REAL XA(4),YA(4)
C
C
C
      IERR = 0
      IF (NXA.LT.7.OR.NYB.LT.1) IERR = 1
      IF (NXB.LT.7.OR.NYB.LT.5) IERR = 1
      IF (NXC.LT.7.OR.NYC.LT.NYA) IERR = 1
      IF (IERR.EQ.0) THEN
C
C
C
        DO K = 1,NYA
C
C  Find the 4 points around the pos
C
           X = DATA(6,K)
           LOOP = .TRUE.
           J = 1
           DO WHILE ((LOOP).AND.(J.LE.NYB))
              IF (DATB(6,J).GT.X) THEN
                 LOOP = .FALSE.
              ELSE
                 J = J + 1
              ENDIF
           ENDDO
           NYBA = NYB - 1
           IF (J.GT.2.AND.J.LT.NYBA) THEN
              DO KK = 1,4
                 JK = J + KK - 3
                 XA(KK) = DATB(6,JK)
                 YA(KK) = DATB(7,JK)
              ENDDO
              CALL INTERP(XA,YA,X,YB)
           ELSE
             IF (J.LT.3) YB = DATB(7,1)
             IF (J.GT.NYBA) YB = DATB(7,NYBA)
           ENDIF
C
C  Do multiplication
C
           OUT(7,K) = DATA(7,K)*YB
         ENDDO
      ENDIF
C
C
C
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R INTERP *
C      *            *
C      **************
C
C
C   PURPOSE
C     This takes 4 X,Y points and fits a cubic to them and returns
C     the value of Y at a given X point
C
C   ARGUMENTS
C   IN
C       X eal(4)   The X values
C       Y   Real(4)   The Y values
C       XA  Real      The X place to get a Y value for
C   OUT
C       YA  Real      The Y value at the X place
C
C   CALLS
C     Grasp
C       POLFIT
C
C   USES
C     Double precision inside
C
C   A.J.PENNY                   RGO                    83-2-16
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE INTERP(X,Y,XA,YA)
C
C
C
      REAL X(4),Y(4),XC(4),YC(4)
      DOUBLE PRECISION DX(4),DY(4),SD(4),A(4),CHI,RMS
C
C
C
      XMAX = X(1)
      YMAX = Y(1)
      DO K = 2,4
         IF (X(K).GT.XMAX) XMAX = X(K)
         IF (Y(K).GT.YMAX) YMAX = Y(K)
      ENDDO
      XMIN = X(1)
      YMIN = Y(1)
      DO K = 2,4
         IF (X(K).LT.XMIN) XMIN = X(K)
         IF (Y(K).LT.YMIN) YMIN = Y(K)
      ENDDO
      DO K = 1,4
         XC(K) = X(K) - XMIN
         YC(K) = Y(K) - YMIN
      ENDDO
      KXF = 0
      IF (ABS(XMAX-XMIN).GT.1.0) THEN
         DO K = 1,4
            XC(K) = XC(K)/(XMAX-XMIN)
         ENDDO
         KXF = 1
      ENDIF
      KYF = 0
      IF (ABS(YMAX-YMIN).GT.1.0) THEN
         DO K = 1,4
            YC(K) = YC(K)/(YMAX-YMIN)
         ENDDO
         KYF = 1
      ENDIF
      DO K = 1,4
         DX(K) = DBLE(XC(K))
         DY(K) = DBLE(YC(K))
         SD(K) = 1.0
      ENDDO
C
C
C
      CALL POLFIT(DX,DY,SD,4,4,0,A,CHI,RMS)
C
C
C
      XB = XA - XMIN
      IF (KXF.EQ.1) XB = XB/(XMAX-XMIN)
      YA = (A(1)+(A(2)*XB)+(A(3)*XB*XB)+(A(4)*XB*XB*XB))
      IF (KYF.EQ.1) YA = YA*(YMAX-YMIN)
      YA = YA + YMIN
C
C
C
      END



