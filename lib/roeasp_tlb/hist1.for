      SUBROUTINE HIST1(NX,NY,ARRAY,ISTART,IFIN,JSTART,JFIN)
*+
*   HIST1
*
*     Calculate histogram of current image, compute statistical
*     properties from it, and plot the histogram on the
*     graphics terminal.
*
*   Given      (arguments)
*   NX          X-dimension of image
*   NY          Y-dimension of image
*   ARRAY       image
*   ISTART      index of left edge of area to be analysed
*   IFIN        index of right edge of area
*   JSTART      index of bottom edge of area
*   JFIN        index of top edge of area
*
*   Subroutines called;
*    E2D:-       GENHIS, HISTPROP, GDEVI1, TYPLO1, HISTLIST,
*                HISTPLOT, CURVAL, STOPLOT.
*    Fings:-     DEVSPE.
*    I/Faces:-   READC, READR.
*    Starlink:-  WRUSER.
*
*	B.D KELLY/ROE/1981
*       A C Davenhall./ROE/  {Modified}                  2/12/82.
*-
      CHARACTER*10 TBUF
      CHARACTER*60 PROCH
      INTEGER NX,NY,ISTART,IFIN,JSTART,JFIN
      INTEGER JHIST(2048)
      REAL ARRAY(NX,NY)
      REAL XMIN,XMAX,YMIN,YMAX,YDUM1,YDUM2
*
*   Calculate the range of the data.
*
      VALMAX=ARRAY(ISTART,JSTART)
      VALMIN=VALMAX
      DO J=JSTART,JFIN
        DO I=ISTART,IFIN
          VALMAX=MAX(VALMAX,ARRAY(I,J))
          VALMIN=MIN(VALMIN,ARRAY(I,J))
        ENDDO
      ENDDO
*
*   Calculate the default histogram.
*
      CALL GENHIS(ARRAY,NX,NY,ISTART,IFIN,JSTART,JFIN,
     &            VALMAX,VALMIN,JHIST,2048)
      ARMAX=VALMAX
      ARMIN=VALMIN
      CALL HISTPROP(JHIST,2048,VALMAX,VALMIN,RSUM,RMEAN,RMED,RMODE)
*
*   Write out properties of the distribution
*
      WRITE(PROCH,'(''Maximum value = '',1PE12.3)') VALMAX
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''Minimum value = '',1PE12.3)') VALMIN
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''Sum = '',1PE12.3)') RSUM
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''Mean = '',1PE12.3)') RMEAN
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''Median = '',1PE12.3)') RMED
      CALL WRUSER(PROCH,ISTS)
      WRITE(PROCH,'(''Mode = '',1PE12.3)') RMODE
      CALL WRUSER(PROCH,ISTS)
      CALL WRUSER ('  ',ISTS)
*
*   Interactive node.
*
*   Define type of plotting to be joining pts. with straight lines.
*
      CALL TYPLO1 (2,1)
*
      CALL HISTLIST
      TBUF=' '
      DO WHILE(TBUF.NE.'E')
        TBUF=' '
        CALL READC ('REPLY','Give HIST command:-',' ','A','ZZZZ',
     &               TBUF,IST)
        IF(TBUF.EQ.'E') THEN
          CONTINUE
        ELSE IF(TBUF.EQ.'P') THEN
          CALL STOPLOT
          CALL GDEVI1 (1,.FALSE.)
          CALL DEVSPE (4800)
          CALL HISTPLOT(JHIST,ARMAX,ARMIN,XMIN,XMAX,YMIN,YMAX)
        ELSE IF(TBUF.EQ.'C') THEN
          CALL CURVAL (XMIN,XMAX,YMIN,YMAX,ARMIN,YDUM1,ARMAX,YDUM2)
          CALL WRUSER (' Please wait. Histogram being rebinned.',ISTS)
          CALL GENHIS(ARRAY,NX,NY,ISTART,IFIN,JSTART,JFIN,
     &                ARMAX,ARMIN,JHIST,2048)
        ELSE IF (TBUF.EQ.'S') THEN
          CALL READR ('ARMIN',' Enter minimum value to be plotted;',
     &                 VALMIN,VALMIN,VALMAX,ARMIN,ISTS)
          CALL READR ('ARMAX',' Enter maximum value to be plotted;',
     &                VALMAX,ARMIN,VALMAX,ARMAX,ISTS)
          CALL OUTPUT (' Please wait. Histogram being rebinned.',ISTS)
          CALL GENHIS (ARRAY,NX,NY,ISTART,IFIN,JSTART,JFIN,
     &                 ARMAX,ARMIN,JHIST,2048)
        ELSE
          CALL HISTLIST
        ENDIF
      ENDDO
*
      CALL STOPLOT
      END
