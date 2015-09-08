      PROGRAM RDKEY
C
C+++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   RDKEY *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               RDKEY   [ ILEVEL=n ]  [ INPUT=name ]
C
C
C          FUNCTION:-
C               It allows the user to enter a list of RA and DEC's  and
C               corresponding  character  identifiers  into a file from the
C               terminal.
C
C
C          USE:-
C               It is part of the EDRS sub-package.
C
C
C
C         USER PARAMETERS:-
C
C         XYPOSN                              This is  used  repeatedly  to
C                                             ask for an RA and DEC
C                                             and  (up  to   20   character)
C                                             identifier.  If no identifier
C                                             is input one is  created.  If
C                                             the identifier already exists
C                                             that entry is overwritten. Up
C                                             to     MAXENTRY     different
C                                             identifiers may be input.
C
C         OUTPUT                              This  is  the  name  of   the
C                                             output file.
C
C         TITLE                               This  is  an  optional  title
C                                             which   may  be  stored  with
C                                             OUTPUT.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   It  controls  the  level   of
C                                             printed  output. A value of 1
C                                             supresses all printed output.
C
C         INPUT                               If present it is  a  list  of
C                                             positions in the same format.
C                                             The program behaves as if the
C                                             contents  had just been input
C                                             from the keyboard.
C
C
C         MAXENTRY                            The default value is 100,  or
C                                             100   more  than  present  in
C                                             INPUT.
C
C
C
C	  B Harrision		 RGO	Version 1	July 1982
C
C
C--------------------------------------------------------------------------


C
C	Based on the program XYKEY by R. F. Warren-Smith     Durham
C
C
C
      CHARACTER CVAL*1,TITLE(1)*30,PRBUF*40
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C OBTAIN AN OPTIONAL INPUT DATA FRAME
C
      CALL GTXYLR('INPUT',.TRUE.,NITEM,LSTLEN,IPIN,IERRI)
C
C IF INPUT NOT OBTAINED, SET DEFAULT VALUES FOR THE LIST DIMENSIONS
C NITEM=NO OF 4-BYTE ITEMS PER LIST RECORD
C LSTLEN=NO OF LIST RECORDS
C
      IF(IERRI.NE.0) THEN
	NITEM=7
	LSTLEN=0
      ENDIF
C
C SET DEFAULT FOR MAX LENGTH OF OUTPUT LIST, THEN OBTAIN VALUE FROM
C ENVIRONMENT
C
      LENOUT=LSTLEN+100
      CALL GETPAR('MAXENTRY','INTEGER',1,REAL(LSTLEN+1),1.0E6,.TRUE.,
     +LENOUT,RVAL,IERR)
C
C OBTAIN VIRTUAL MEMORY WORKSPACE TO HOLD IDENTIFIERS AND RA,DEC
C VALUES
C
      CALL GETDYN('ID',104,5*LENOUT,IPID,ISTATI)
      CALL GETDYN('X',104,LENOUT,IPX,ISTATX)
      CALL GETDYN('Y',104,LENOUT,IPY,ISTATY)
C
C IF SPACE NOT AVAILABLE... GIVE ERROR MESSAGE AND ABORT
C
      IF((ISTATI.NE.0).OR.(ISTATX.NE.0).OR.(ISTATY.NE.0)) THEN
	CALL WRERR('NOSPACE')
        GO TO 99
      ENDIF
C
C IF INPUT WAS SUPPLIED, COPY INPUT LIST DATA TO WORKSPACE
C
      IF(IERRI.EQ.0) THEN
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPID),1,20)
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPX),21,24)
	CALL EXTLST(%VAL(IPIN),NITEM,LSTLEN,%VAL(IPY),25,28)
      ENDIF
C
C CALL RADINK TO ADD NEW KEYBOARD VALUES TO THE WORKSPACE
C
      CALL RADINK(%VAL(IPID),%VAL(IPX),%VAL(IPY),LENOUT,LSTLEN,IERR)
C
C IF NO LIST OBTAINED, GIVE ERROR MESSAGE AND ABORT... OTHERWISE
C OBTAIN OUTPUT DATA FRAME
C
      IF(LSTLEN.LE.0) THEN
        CALL WRERR('NOLIST')
	GO TO 99
      ELSE
	CALL GTXYLW('OUTPUT',.FALSE.,7,LSTLEN,IPOUT,IERR2)
	IF(IERR2.EQ.0) THEN
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS FROM WORKSPACE TO
C OUTPUT DATA FRAME
C
          CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPID),1,20)
	  CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPX),21,24)
	  CALL ADDLST(%VAL(IPOUT),7,LSTLEN,%VAL(IPY),25,28)
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
	  IF(ILEVEL.GE.2) THEN
            WRITE(PRBUF,104)LSTLEN
	    IF(LSTLEN.EQ.1) PRBUF(28:)='ENTRY'
  104       FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
            CALL LBGONE(PRBUF(20:))
	    CALL WRUSER(' ',ISTAT)
            CALL WRUSER(PRBUF,ISTAT)
	    CALL WRUSER(' ',ISTAT)
	  ENDIF
C
C DEFAULT OUTPUT TITLE IS EITHER THE INPUT TITLE, OR BLANK.
C THEN OBTAIN A NEW ONE FROM THE ENVIRONMENT
C
	  TITLE(1)=' '
	  IF(IERRI.EQ.0) THEN
	    CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +      IERR)
          ENDIF
          CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C COPY INPUT DESCRIPTOR TO OUTPUT (IF AVAILABLE) THEN UPDATE
C DESCRIPTOR ITEMS
C
	  IF(IERRI.EQ.0) CALL CYDSCR('INPUT','OUTPUT',ISTAT)
	  CALL PTDSCR('OUTPUT','NITEM','INTEGER',7,RVAL,CVAL,IERR)
	  CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLEN,RVAL,CVAL,
     +    IERR)
	  CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +	  IERR)
	ENDIF
      ENDIF
C
C FREE ALL DATA AREAS AND RETURN
C
   99 CALL FRDATA(' ',ISTAT)
      END
