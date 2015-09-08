	SUBroutine crb_ghardcopy
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   GHARDCOPY *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C
C   GHARDCOPY  [WHITE=value] [BLACK=value] [XLIMITS=I1,I2] [YLIMITS=J1,J2]
C
C
C          FUNCTION:-
C               Copies the current ARGS display onto the Versatec printer-
C               plotter in grey-scale. By default the entire 512x512-pixel 
C               area is copied from the ARGS memory into an INTEGER*2 array,
C               which is plotted on the printer as a GKS cell  array with a
C               key of grey levels. BLACK and WHITE define the values which
C               will be black and white respectively on the final plot. 
C               Values outside the range will be black at the BLACK end of
C               the range and white at the WHITE end.
C
C               Optionally, any rectangular region  of  the  ARGS screen
C               may be plotted by specifying XLIMITS and YLIMITS  on the
C               command line. The plot is automatically sent to the Versa-
C               tec print queue, and then deleted once plotted.
C
C          USE:-
C               Useful for getting a hard copy of  any  graphics  currently
C               displayed  on  the  ARGS. BLACK and WHITE should be at 
C               least different by 16. If the background is mostly black on
C               the ARGS then make a negative (as are the defaults).
C
C
C
C         USER PARAMETERS:-
C
C         LABEL                               Optional   label   for    the
C                                             title of the plot.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         WHITE             0                 Value of pixel that will cor-
C                                             respond to white in the grey-
C                                             scale plot.
C
C         BLACK           255                 Value of pixel that will cor-
C                                             respond to black in the grey-
C                                             scale plot.
C
C         XLIMITS         0,511               Minimum and  maximum  columns
C                                             on the ARGS to be plotted.
C
C         YLIMITS         0,511               Minimum and maximum  rows  on
C                                             the ARGS to be plotted.
C
C
C         Malcolm J Currie (RL.STAR)   1986 July 17 based upon AHARDCOPY by   
C         W D Pence                AAO                            13-JAN-83
C
C
C
C
C
C--------------------------------------------------------------------------



C
C      Copies the ARGS image and prints it on the Versatec as a grey
C      scale.
C
      INTEGER IDIMN(2),IX(2),IY(2),LBLACK,LWHITE,LOOP

      CHARACTER*80 LABEL

      LOGICAL GRYRNG

      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('Unable to get ARGS.',ISTAT)
        STOP
      END IF
C
C      Get boundary of rectangular area to be output
C
      GO TO 12
10    CALL CNPAR('XLIMITS',ISTAT)
      CALL CNPAR('YLIMITS',ISTAT)
12    CALL RDKEYI('XLIMITS',.FALSE.,2,IX,NXVALS,ISTAT)
      IF (NXVALS .LT. 2 )THEN
        CALL WRUSER('Must input min and max limits',ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .GE. IX(2))THEN
        CALL WRUSER('First limit must be less than the second',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .LT. 0 .OR. IX(2) .GT. 511)THEN
        CALL WRUSER('Limits must be in the range 0 to 511',ISTAT)
        GO TO 10
      END IF
      CALL RDKEYI('YLIMITS',.FALSE.,2,IY,NYVALS,ISTAT)
      IF (NYVALS .LT. 2)THEN
        CALL WRUSER('Must input min and max limits',ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .GE. IY(2))THEN
        CALL WRUSER('First limit must be less than the second',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .LT. 0 .OR. IY(2) .GT. 511)THEN
        CALL WRUSER('Limits must be in the range 0 to 511',ISTAT)
        GO TO 10
      END IF
C
      CALL RDKEYC('LABEL',.FALSE.,1,LABEL,NVAL,IS)
C
      IDIMN(1)=IX(2)-IX(1)+1
      IDIMN(2)=IY(2)-IY(1)+1
      NSIZE=IDIMN(1)*IDIMN(2)
C
C      Get work space large enough to hold the ARGS image
C
      CALL GETDYN('IMAGE',102,NSIZE,IPIN,JSTAT)
C
C       Load the ARGS image into the work space
C
      CALL SRFPR(%VAL(IPIN),IDIMN(1),IDIMN(1),IDIMN(2),IX(1),IY(1))
C
C      Get work space large enough to hold the inverted ARGS image
C
      CALL GETDYN('IMAGE2',102,NSIZE,IPOUT,JSTAT)
C
C      Invert the image
C
      CALL FLVI2(%VAL(IPIN),IDIMN(1),IDIMN(2),%VAL(IPOUT))
      CALL FRDATA('IMAGE',ISTAT)
C
C      Get the values corresponding to black and white
C
      GRYRNG = .FALSE.
      LOOP = 0

      DO WHILE ( .NOT. GRYRNG )
         CALL RDKEYI('WHITEG',.FALSE.,1,LWHITE,IVALS,ISTAT)

         CALL RDKEYI('BLACKG',.FALSE.,1,LBLACK,IVALS,ISTAT)

         IF ( LBLACK .EQ. LWHITE ) THEN
            CALL WRUSER('Error - black and white are equal. Reselect',
     :                  ISTAT)
            LOOP = LOOP + 1
            IF ( LOOP .EQ. 3 ) THEN
               CALL WRUSER('ABORTING.',ISTAT)
               CALL EXIT
            END IF
         ELSE
            GRYRNG =.TRUE.
         END IF
      END DO
C
C      Use PHOTOGRAPH library to produce a grey-scale plot via a GKS
C      cell array.
C
      CALL PHO_FOTO2(%VAL(IPOUT),IDIMN(1),IDIMN(2),LWHITE,LBLACK,LABEL,
     :               ISTAT)
C
C      Use VMS command to print the file
C
      ISTATUS=LIB$SPAWN(
     : '$PRINT/NOFEED/DELETE/PASSALL/QUEUE=SYS_VERSATEC VERSATEC.BIT')
	call cnpar('BLACKG',istat)
	call cnpar('WHITEG',istat)
      END
C***********************************************************************
      SUBROUTINE FLVI2(IMAGIN,NX,NY,IMAOUT)

      INTEGER*2 IMAGIN(NX,NY),IMAOUT(NX,NY)

      DO J=1,NY
         DO I=1,NX
            IMAOUT(I,NY-J+1)=IMAGIN(I,J)
         END DO
      END DO

      END
C***********************************************************************
      SUBROUTINE SRFPR(IB2,JD1,NX,NY,X,Y)
C     --------------------------------------------------------
C READS RECTANGULAR PIXEL ARRAY FROM THE ARGS. EACH PIXEL VALUE
C IS CONTAINED IN ONE INTEGER*2 ARRAY ELEMENT.
C 1ST SUBSCRIPT INCREASES WITH INCREASING X.
C 2ND SUBSCRIPT CORRESPONDS TO Y
C ARGUMENTS:
C    IB2   (EXIT) 2-D ARRAY CONTAINING PIXEL VALUES.
C    JD1   (ENTRY) 1ST DIMENSION OF ACTUAL ARRAY CORRESPDG TO IB2
C    NX,NY  (ENTRY) NUMBER OF PIXELS IN X- AND Y- DIRECTIONS
C    X,Y    (ENTRY) BOTTOM LEFT OF RECTANGLE TO BE COPIED
C ----------------------------------------------------------------
      PARAMETER BITS=16
      INTEGER JD1
      INTEGER*2 IB2(JD1,NY)
      INTEGER NX,NY, X,Y
      INTEGER PPE,HLINES
      INTEGER*2 CHECK
      REAL PPB
C   CHECK ARGUMENTS
      CHECK=IB2(1,1)
      CHECK=IB2(JD1,NY)
      CHECK=IB2(NX,NY)
C
C               NUMBER OF (P)IXELS (P)ER ARRAY (E)LEMENT
C               & PER (B)YTE (LATTER IS REAL BECAUSE IT COULD BE <1).
C
      PPE=16/BITS
      PPB=8.0/FLOAT(BITS)
      HLINES=10000
C
C     ADJUST HLINES TO THE SPACE ALLOWED BY ARGS_FPRI2
C
      HLINES=MIN(HLINES,(32767*PPE)/NX, (INT(32767.0*PPB)/NX))
C
C     CAN READ CHUNKS OF POSSIBLY MORE THAN 1 ROW.
C
      DO  JY=1,NY,HLINES
C       (NY2 IS # OF ROWS TO BE PUT THIS ITERATION)
        NY2=MIN(HLINES,NY-JY+1)
        CALL ARGS_FPRI2(IB2(1,JY),NY2*NX,X,Y+JY-1,
     1          NX,NY2)
      END DO
      END
C -----------------------------------------------------------------
      SUBROUTINE ARGS_FPRI2(BUFF,N,X1,Y1,NX,NY)
C     -----------------------------------------------
C READS PACKED PIXEL ARRAY FROM ARGS.
C ARGUMENTS
C   BUFF   (ARRAY) ON OUTPUT CONTAINS PACKED DATA (INTEGER*2)
C   N      (ENTRY) NUMBER OF INTEGER*2 ELEMENTS IN BUFF
C   X1,Y1  (ENTRY) BOTTOM LEFT OF PIXEL IMAGE.
C   NX,NY  (ENTRY) NUMBER OF PIXELS TO BE OCCUPIED IN X & Y RESP.
C
C -------------------------------------------------------------------
      INTEGER*2 BUFF(N),IBUFF(32771)
      INTEGER X1,Y1,NX,NY
      INTEGER NW,AW,PPAW,NAME
      INTEGER*2 CHECK,IRDINS(10)
C
      INTEGER*2 IJUNK
      LOGICAL LPRT,LARGSD,LARGSS,INITD
      COMMON /CARGS1/IJUNK(520),L1,L2,L3,LPRT,LARGSD,LARGSS,
     1 CHAN,INITD,L4
      DATA NW/1/
      DATA NAME/3HFPR/
C
C   CHECK ARRAY EXTREMITIES
      CHECK=BUFF(1)
      CHECK=BUFF(N)
C   CHECK NUMBER OF WORDS
      IF( N*NW.GT.32767 ) CALL ARGS_ITF(12,.TRUE.,N,N,Z)
C   (P)IXELS (P)ER (A)RGS (W)ORD
      PPAW=1
C   NEED EXACT NUMBER OF ARGS DATA WORDS OR FEWER.
      AW=MIN( N*NW, (NX*NY+PPAW-1)/PPAW)
C
C   MOVE PEN, OUTPUT ARGS ORDER & THEN READ THE IMAGE.
C
      CALL ARGS_S1(3HXMA,X1)
      CALL ARGS_S1(3HYMA,Y1)
C
C     OPCODE FOR READING ARGS MEMORY = '6B04'
C
      IRDINS(5)='6B04'X
      IRDINS(6)=X1
      IRDINS(7)=Y1
      IRDINS(8)=NX
      IRDINS(9)=NY
      IRDINS(10)=AW
C
C     ARGS_IDCALLRD SENDS 'IRDINS' TO THE ARGS AND RECEIVES
C        THE REPLY IN IBUFF.  THERE ARE 4 HEADER WORDS IN THE
C        BEGINNING OF EACH BUFFER WHICH MUST BE IGNORRED.
C
      CALL ARGS_IDCALLRD(1,CHAN,IRDINS,10,IBUFF,AW+4,NOK)
C
      DO I=1,AW
        BUFF(I)=IBUFF(I+4)
      END DO
      END
C
