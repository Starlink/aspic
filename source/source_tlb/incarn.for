C++
C     COPY - Copy incarnation.
C
C     This program will copy an incarnation from an existing
C     bulk data frame into a new frame.
C
C     Program parameters:
C     ------------------
C     INPUT    Bulk data frame for input
C     OUTPUT   Bulk data frame for output
C     FORMAT   Character string denoting data format of
C              incarnation to be copied.
C	       'SB' = Signed Byte
C	       'SW' = Signed Word
C	       'SL' = Signed Longword
C	       'R ' = Real
C	       'DP' = Double Precision
C	       'UB' = Unsigned Byte
C	       'UW' = Unsigned Word
C
C     D.PEARCE  12-AUG-80  (Chilton)
C--
C
      IMPLICIT  INTEGER(A-Z)
C
      CHARACTER FORMAT*2
      CHARACTER TYPE*8
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C
C     .....get format type
      CALL RDKEYC('FORMAT',.FALSE.,1,FORMAT,I,STATUS)
C
C     .....determine format code
      IF (FORMAT.EQ.'SB') CODE=FMT_SB
      IF (FORMAT.EQ.'SW') CODE=FMT_SW
      IF (FORMAT.EQ.'SL') CODE=FMT_SL
      IF (FORMAT.EQ.'R ') CODE=FMT_R
      IF (FORMAT.EQ.'DP') CODE=FMT_DP
      IF (FORMAT.EQ.'UB') CODE=FMT_UB
      IF (FORMAT.EQ.'UW') CODE=FMT_UW
C
C     .....allocate input frame data
      CALL RDDATA('INPUT',CODE,TYPE,SIZE,INPTR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
	 CALL WRERR('INBAD')
	 GOTO 90
      ENDIF
C
C     .....calculate bytes per value
      BPV=CODE-(CODE/100)*100
C
C     .....copy all descriptor information
      CALL CYDSCR('INPUT','OUTPUT',STATUS)
C
C     .....allocate output frame data
      CALL WRDATA('OUTPUT',CODE,TYPE,SIZE,OUTPTR,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
	 CALL WRERR('OUTBAD')
	 GOTO 90
      ENDIF
C
C     .....copy input data to output frame
      CALL COPY(%val(INPTR),%val(OUTPTR),SIZE*BPV)
C
C     .....release all frame data
   90 CALL FRDATA(' ',STATUS)
C
      CALL EXIT
      END
      SUBROUTINE COPY(INPUT,OUTPUT,N)
C
C     Copies one array to another. Should really be implemented
C     using the MOVC instruction (allows 64k to be transferred
C     at a time). It's up to you if you want to do this.
C
      IMPLICIT  INTEGER(A-Z)
      LOGICAL*1 INPUT(N),OUTPUT(N)
C
      DO 20 I=1,N
         OUTPUT(I)=INPUT(I)
   20 CONTINUE
C
      RETURN
      END
