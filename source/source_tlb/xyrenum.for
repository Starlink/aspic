C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program XYRENUM *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               XYRENUM
C
C
C          FUNCTION:-
C               It takes an EDRS format XY file and makes an identical
C               one, except the identinifers are labelled #1,#2,,,#n
C
C
C          USE:-
C               Mainly used when editing EDRS XY files
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the  .BDF
C                                             input file
C
C         XYWOUT                              The name of the .BDF output
C                                             output file.
C
C         XYWTIT   Output                     The Title to be added to the
C                                             output file
C
C
C         A J Penny            RGO                                12-SEP-82
C
C
C--------------------------------------------------------------------------



*  PURPOSE
*        To rename the identifiers of an XY file with sequential
*        numbers
*
*  METHOD
*
*
*  ARGUMENTS
*        None
*
*  STARLINK PARAMETERS
*     INPUT
*            The input EDRS XY file
*
*  CALLS
*      GRASP file
*          XYLWRI
*      EDRS Package
*            GTXYLR,GTDSCR
*      STARLINK:
*            WRUSER,FRDATA
*
*  NOTES
*       Uses VAX %VAL facility
*
*  WRITTEN BY:
*       A.J. PENNY                                      82-9-12
* ------------------------------------------------------------



      PROGRAM XYRENUM
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
      CHARACTER CVAL*1,TITLE*30,NAME*9,FILE*13,PRBUF*72
C
C  Obtain input XY list
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.EQ.0) THEN
C
C  Extract Title
C
         TITLE = 'Output from XYRENUM'
         CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,IERR)
         WRITE (PRBUF,902)TITLE
  902    FORMAT(' ','TITLE     ',A30)
         CALL WRUSER(PRBUF,ISTAT)
C
C  Write it to output file
C
         CALL XYLWRI(%VAL(IPIN),NITEM,LSTLEN,1,LSTLEN,6,NITEM,IERR)
C
C  Type no of entries
C
         IF (LSTLEN.NE.1) THEN
            WRITE(PRBUF,920)LSTLEN
  920       FORMAT(T22,I7,' LIST ENTRIES')
            CALL WRUSER(PRBUF,ISTAT)
  921       FORMAT (' ',22X,I6,' LIST ENTRIES')
         ELSE
            CALL WRUSER('                         1 LIST ENTRY',ISTAT)
  922       FORMAT (' ',22X,'   ONE LIST ENTRY')
         ENDIF
      ENDIF
C
C  Free data area
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END



