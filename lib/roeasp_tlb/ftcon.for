      SUBROUTINE FTCON
C+
C     Complex conjugation routine for FTs produced by the double
C     FFT routines
C
C     B.V.McNally ROE/DEC-1983
C
C-

      INTEGER NAXIS(2),NOUT(2)
      INTEGER NPTR1,NPTR2,NWK1,NWK2,IST,ISTAT
      CHARACTER INVERS*10,REPORT*1

C   Get input and output images and workspace

      CALL INPICR('INPIC1','GIVE INPUT FRAME',2,NAXIS,NPTR1,IST)
      NOUT(1)=MAX(NAXIS(1),NAXIS(2))
      NOUT(2)=NOUT(1)
      CALL OUTPICR('OUTPIC1','GIVE OUTPUT FRAME',2,NOUT,NPTR2,IST)
      CALL OUTPICR('WORK1','WORK ARRAY',2,NOUT,NWK1,IST)
      NSIZE=4*NOUT(1)
      CALL OUTPICR('WORK2','WORK ARRAY',1,NSIZE,NWK2,IST)

C   If status is ok, copy to output array and pad non-square images.

      IF(IST.EQ.0) THEN
         CALL COPY(NAXIS(1),NAXIS(2),%VAL(NPTR1),NOUT(1),NOUT(2),
     :            %VAL(NPTR2))
         IF(NAXIS(1).NE.NAXIS(2)) THEN
            CALL PICSQ(NAXIS(1),NAXIS(2),NOUT(1),%VAL(NPTR2))
         ENDIF

C      Allow choice of whether progress reports are sent to terminal
C      during the FFT.

         CALL YESNO('PROGRESS REPORTS TO TERMINAL ? Y OR N','Y',
     :              REPORT,IST)


         CALL FTCONJ2D(REPORT,NOUT(1),NOUT(2),%VAL(NPTR2),%VAL(NWK1),
     :                 %VAL(NWK2))

      ELSE
         CALL WRUSER('ERROR IN FTCONJ',ISTAT)
      ENDIF

         CALL CLEARIM('INPIC1')
         CALL CLEARIM('OUTPIC1')
         CALL CLEARIM('WORK1')
         CALL CLEARIM('WORK2')

      END
