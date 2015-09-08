      SUBROUTINE IGJOBS(IXEXT,IYEXT,PARAMS,MAXDIM)
C+
C   IGJOBS
C
C      produces FINGS verstatek plot file and tells user to plot it
C    called from IGJOB
C
C   Given         (arguments)
C   IXEXT,IYEXT    (I)   dimensions of PARAMS
C   PARAMS         (RA)  array with images found by IAMANAL
C   MAXDIM    (R)    larger dimension of original image for versatek window
C
C      B.D KELLY/ROE/1981
C	D. Tudhope/ROE/Sept 1982
C-

      INTEGER IXEXT,IYEXT
      REAL PARAMS(IXEXT,IYEXT)
      REAL MAXDIM
      INTEGER K
      REAL XPOS,YPOS,AXMAJ,AXMIN,ECC,THETA

*   Plot ellipses matching IAM parameters.
*   Images of area less than 2 are not plotted.
*
      CALL VERSA
C*  assume TVANAL output is 1..maxdim, not 0..maxdim-1
      CALL WINDOL(1.0,MAXDIM,1.0,MAXDIM)
      CALL VUPORT(0.0,1.0,0.0,1.0)
      CALL CLICTL(1)

      DO K=1,IYEXT
        IF(PARAMS(11,K).GT.2.0) THEN
          XPOS=PARAMS(1,K)
          YPOS=PARAMS(2,K)
          AXMAJ=2.0*PARAMS(3,K)
          AXMIN=2.0*PARAMS(4,K)
          ECC=SQRT(AXMAJ**2-AXMIN**2)/AXMAJ
          THETA=PARAMS(10,K)/57.2958
          CALL ELLIPSE(XPOS,YPOS,AXMAJ,ECC,THETA)
        ENDIF
      ENDDO
      CALL DEVEND

C*  tell user what's happening
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER('Versatek plot file created by FINGS',ISTAT)
      CALL WRUSER('Now type Versatek plot command at your',ISTAT)
      CALL WRUSER(' node for FINGS (VPLOT at ROE)',ISTAT)

      END
