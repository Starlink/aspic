      SUBROUTINE THRESH1
*+
*   THRESH1
*
*   Sets values within a frame to zero if they lie below or
*   above specified values.
*
*   Subroutines called    :
*   INPICR,OUTPICR,READR  : ASPFACE
*   THRESH1               : E2DASP
*   WRUSER                : STARLINK
*
*   B.D.Kelly/ROE/29.1.1982
*-
      INTEGER NAXIS1(2),NAXIS2(2),NPTR,NPOUT,IST,ISTAT
      REAL THRHI,THRLO
*
*    set the status variable to 0
*
      IST=0
*
*    get the filename of the image to be thresholded
*
      CALL INPICR('INPIC1','Give image to be thresholded',
     :            2,NAXIS1,NPTR,IST)
*
*    get the low threshold value
*
      CALL READR('THRLO','Give low threshold value',0.0,-1.0E19,
     :           1.0E19,THRLO,IST)
*
*    and get the high threshold value
*
      CALL READR('THRHI','Give high threshold value',0.0,-1.0E19,
     :           1.0E19,THRHI,IST)
      NAXIS2(1)=NAXIS1(1)
      NAXIS2(2)=NAXIS1(2)
*
*    get the filename for the thresholded image
*
      CALL OUTPICR('OUTPIC1','GIVE OUTPUT FRAME',2,NAXIS2,NPOUT,IST)
*
*    check for any errors accessing the i/o frames
*
      IF(IST.EQ.0) THEN
         CALL THRESH2(NAXIS1(1),NAXIS1(2),%VAL(NPTR),THRLO,THRHI,
     :                NAXIS2(1),NAXIS2(2),%VAL(NPOUT))
      ELSE
*
*    here when error accessing the i/o frames
*
         CALL WRUSER('Error accessing the I/O frames',ISTAT)
      ENDIF
*
*    tidy up the input and output frames
*
      CALL CLEARIM('INPIC1')
      CALL CLEARIM('OUTPIC1')
      END
