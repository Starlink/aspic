      SUBROUTINE INPICI(NAME,PROMPT,NDIMS,NAXIS,NPTR,ISTAT)
C+
C   INPICI
C
C   Return a pointer to an integer input array
C
C   ASPIC interface version
C
C   Given      (arguments)
C   NAME        image name
C   PROMPT      prompt to user if Starlink disk-file needed
C   NDIMS       dimensionality of required array
C   ISTAT       input status
C
C   Returned   (arguments)
C   NPTR        pointer to first element of array
C   NAXIS       dimensions of array
C   ISTAT       status return = MAX(input status,output status)
C
C   Subroutine calls :
C   WRUSER,RDIMAG    : STARLINK
C   BATCH            : ASPFACE
C
C   B.D.Kelly/ROE/21.9.1981
C-

      CHARACTER*(*) NAME,PROMPT
      CHARACTER*72 COMC,IDENT1
      CHARACTER*7 STORE
      INTEGER NDIMS,NPTR,ISTAT,IST1,IST2,JDIMS,JCOM
      INTEGER NAXIS(NDIMS)
      LOGICAL BATFL
C
C
C
      CALL BATCH(BATFL)

      IF(.NOT.BATFL) CALL WRUSER(PROMPT,IST1)
      CALL RDIMAG(NAME,104,NDIMS,NAXIS,JDIMS,NPTR,IST2)
      IF(IST2.EQ.0) THEN
        CALL RDDSCR(NAME,'TITLE',1,IDENT1,JDUM,IST1)
        IF(IST1.EQ.0) THEN
          CALL WRUSER(IDENT1,IST1)
        ELSE
          IDENT1=' '
        ENDIF
      ENDIF
      ISTAT=MAX(ISTAT,IST2)

      END
