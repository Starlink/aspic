      SUBROUTINE OUTPICI(NAME,PROMPT,NDIMS,NAXIS,NPTR,ISTAT)
C+
C   OUTPICI
C
C   Return a pointer to an integer output array
C
C   ASPIC interface version
C
C   Given      (arguments)
C   NAME        image name
C   PROMPT      prompt to user if Starlink disk-file needed
C   NDIMS       dimensionality of required array
C   ISTAT       input status
C   NAXIS       dimensions of required array
C
C   Returned   (arguments)
C   NPTR        pointer to first element of array
C   NAXIS       dimensions of array
C   ISTAT       status return = MAX(input status,output status)
C
C   Subroutine calls :
C   WRUSER,WRIMAG    : STARLINK
C   BATCH            : ASPFACE
C
C   B.D.Kelly/ROE/21.9.1981
C-

      CHARACTER*(*) NAME,PROMPT
      CHARACTER*72 COMC,IDENT1,DEFAULT
      CHARACTER*7 STORE
      INTEGER NDIMS,NPTR,ISTAT,IST1,IST2,JDIMS,JCOM
      INTEGER NAXIS(NDIMS)
      LOGICAL BATFL
C
C
C
      DEFAULT=' '
      CALL BATCH(BATFL)

      IF(NAME(1:4).EQ.'WORK') THEN
        NTDIMS=1
        DO JDIMS=1,NDIMS
          NTDIMS=NTDIMS*NAXIS(JDIMS)
        ENDDO
        CALL GETDYN(NAME,104,NTDIMS,NPTR,IST2)
        ISTAT=MAX(ISTAT,IST2)
      ELSE
        IF(.NOT.BATFL) CALL WRUSER(PROMPT,IST1)
        CALL WRIMAG(NAME,104,NAXIS,NDIMS,NPTR,IST2)
        CALL READC('REPLY','GIVE FRAME TITLE',DEFAULT,
     &             ' ','}',IDENT1,IST2)
        CALL WRDSCR(NAME,'TITLE',IDENT1,1,IST1)
        ISTAT=MAX(ISTAT,IST2)
      ENDIF

      END
