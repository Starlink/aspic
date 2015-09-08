C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   SATCOR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               SATCOR
C
C
C          FUNCTION:-
C              This applies the electronograph saturation law to an
C              image, or de-saturates an image. That is, the data in an
C              image are fed through
C
C                      Val = -K.Dsat.ln(1-K.Val/Dsat)
C
C               or
C
C                      Val = K.Dsat.(1-exp(-K.Val/Dsat)
C
C
C             If a density is less than 1.0E-5, it is copied
C             staright over.
C
C
C          USE:-
C               For correcting for saturation or a wrong correction or
C               mimicking saturation.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The name of the input  image.
C                                             It must be an I*2 image
C
C         SATDEN         10.0                 Saturation density
C
C         DENVAL         754                  Pixel value which corresponds
C                                             D = 1
C
C         CHOICE         DESAT                Apply desaturation (DESAT) or
C                                             saturation (SAT).
C
C         OUTPUT                              The Output image
C
C         TITLE                               The Title to be attached as a
C                                             descriptor   of   the  Output
C                                             image
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         A.J.Penny                RGO                             83-MAY-22
C
C
C--------------------------------------------------------------------------






C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         PROGRAM SATCOR
C
C
C CALLS
C             EDRS PACKAGE:
C                     GT2DIW,PTDSCR,RDDSCR,
C             STARLINK:
C                     FRDATA,RDKEYC,RDKEYI,WRUSER
C                     RDIMAG,CTOI,CTOR
C             THIS FILE:
C                     TRANS
C
C NOTES
C             USES VAX %VAL FACILITY
C
C
C
C
C -------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER NSIZE(2)
      LOGICAL*1 VALID
      CHARACTER TEXT*72,CVAL*1,TITLE(1)*30
C
C Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
C
      CALL GTIMAG(IPIN,NSIZE,BSCALE,BZERO,INVAL,IERR)
C
C  If cant pick up input frame, set the continuation flag to no
C
      IF (IERR.NE.0) THEN
         VALID = .FALSE.
         CALL WRUSER('NO VALID IMAGE',ISTAT)
      ENDIF
C
C -------------------------------------------------------------------
C
C  Get parameters
C
      IF (VALID) THEN
         SATDEN = 10.0
         CALL RDKEYR('SATDEN',.TRUE.,1,SATDEN,NVAL,ISTAT)
         IF ((ISTAT.EQ.4).OR.SATDEN.LT.0.0) VALID = .FALSE.
         IF (VALID.EQ.FALSE) THEN
            CALL WRUSER('BAD VALUE',ISTAT)
         ELSE
            DENVAL = 754.0
            CALL RDKEYR('DENVAL',.TRUE.,1,DENVAL,NVAL,ISTATA)
            IF ((ISTATA.EQ.4).OR.DENVAL.LT.0.0) VALID = .FALSE.
            IF (VALID.EQ.FALSE) THEN
               CALL WRUSER('BAD VALUE',ISTAT)
            ELSE
               KWHAT = 0
               DO WHILE (KWHAT.NE.1.AND.KWHAT.NE.2)
               KWHAT = 1
               CALL GETCMD('CHOICE','DESATURATE,SATURATE,HELP,?.',1,
     +                     KWHAT,TEXT,KTEXT,ISTAT)
               IF (KWHAT.EQ.3.OR.KWHAT.EQ.4) THEN
                  CALL WRUSER('OPTIONS ARE :-',ISTAT)
                  CALL WRUSER('DESATURATE,SATURATE',ISTAT)
               ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF
C
C ----------------------------------------------------------------
C
C  Obtain Output image
C
C
      IF (VALID) THEN
         CALL GT2DIW('OUTPUT',102,.FALSE.,NSIZE(1),NSIZE(2),IPOUT,IERR)
         IF(IERR.EQ.0) THEN
C
C  Get Title
C
            TITLE(1) = 'Output from SATCOR'
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C LOAD TITLE AND DESCRIPTORS
C
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +                 IERR)
            CALL PTDSCR('OUTPUT',' INVAL','INTEGER',INVAL,RVAL,
     +                  CVAL,IERR)
            CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BSCALE,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
         ELSE
            VALID = .FALSE.
            CALL WRUSER('CANT GET THAT FILE',ISTAT)
         ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C
C  Transfer area
C
      IF (VALID) THEN
         CALL TRANS(%VAL(IPIN),NSIZE(1),NSIZE(2),%VAL(IPOUT),
     +              SATDEN,DENVAL,KWHAT,BSCALE,BZERO,INVAL)
      ENDIF
C
C -------------------------------------------------------------------
C
C  Tidy up and exit
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          *************
C          *           *
C          * S/R TRANS *
C          *           *
C          *************
C
C   This does the correction
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(IM1,KX1,KY1,IM2,SATDEN,DENVAL,KWHAT,BS,BZ,INVAL)
C
C
C
      INTEGER*2 IM1(KX1,KY1),IM2(KX1,KY1)
C
C
C
      IF (KWHAT.EQ.1) THEN
         DO K = 1,KY1
            DO J = 1,KX1
               IF (IM1(J,K).NE.INVAL) THEN
                  DEN = (REAL(IM1(J,K))*BS+BZ)/DENVAL
                  IF (DEN.LT.1.0E-5) THEN
                     IM2(J,K) = IM1(J,K)
                  ELSE
                     IF (DEN.GE.(SATDEN-1.0E-5)) THEN
                        IM2(J,K) = INVAL
                     ELSE
                        DENA = -1.0*SATDEN*ALOG(1.0-(DEN/SATDEN))
                        VAL = (DENA*DENVAL-BZ)/BS
                        IF (VAL.GT.32767.9) VAL= 32767.0
                        IM2(J,K) = VAL
                     ENDIF
                  ENDIF
               ELSE
                  IM2(J,K) = INVAL
               ENDIF
            ENDDO
         ENDDO
      ELSE
         DO K = 1,KY1
            DO J = 1,KX1
               IF (IM1(J,K).NE.INVAL) THEN
                  DEN = (REAL(IM1(J,K))*BS+BZ)/DENVAL
                  IF (DEN.GT.1.0E-5) THEN
                     DENA = SATDEN*(1.0-EXP(-1.0*DEN/SATDEN))
                     IM2(J,K) = (DENA*DENVAL-BZ)/BS
                  ELSE
                     IM2(J,K) = IM1(J,K)
                  ENDIF
               ELSE
                  IM2(J,K) = INVAL
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C
C
C
      END



