      SUBROUTINE OUT(BX,BY,TX,TY,MODE,COLTAB,LOGS,TAB)
C
C      THIS ROUTINE : FILLS A SET OF LUTS WITH VALUES BETWEEN 0 AND 255
C      AND OUTPUTS IT TO THE ARGS
C
C      THE TABLE (TAB) IS 3 BY 256 (3 COLOURS AND 256 ENTRIES)
C
      INTEGER TAB(3,0:255),COLTAB(3,0:255),LOGS(0:255)
      IX=BX
      IY=BY
      JX=TX
      JY=TY
C
C      SPECIAL CARE IS NEEDED IF THE LINE IS VERTICAL (IX=JX)
C
      IF (IX.NE.JX) THEN
         DO 100 I=0,255
          IF(IX.LT.JX) THEN
            X=BY+FLOAT(I-IX)*(TY-BY)/FLOAT(JX-IX)
          ELSE
            X=TY-FLOAT(I-JX)*(TY-BY)/FLOAT(IX-JX)
          END IF
            IF (X.LT.0.0) X=0.0
            IF (X.GT.255.0) X=255.0
            IVAL=IFIX(X)
            IF (MODE.EQ.1) IVAL=LOGS(IVAL)
            TAB(1,I)=COLTAB(1,IVAL)
            TAB(2,I)=COLTAB(2,IVAL)
            TAB(3,I)=COLTAB(3,IVAL)
  100    CONTINUE
      ELSE
      DO 200 I=0,IX
         TAB(1,I)=COLTAB(1,0)
         TAB(2,I)=COLTAB(2,0)
         TAB(3,I)=COLTAB(3,0)
  200 CONTINUE
      DO 300 I=IX+1,255
         TAB(3,I)=COLTAB(3,255)
         TAB(2,I)=COLTAB(2,255)
         TAB(1,I)=COLTAB(1,255)
  300 CONTINUE
      END IF
      TAB(1,0)=0
      TAB(2,0)=0
      TAB(3,0)=0
      CALL SRCOLS(0,256,TAB)
      IF (IFL.NE.0) THEN
         CALL WRUSER('ERROR IN SRCOLS',ISTAT)
      END IF
      END
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   XYCURB *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-   XYCURB  [ILEVEL= ]
C
C
C          FUNCTION:-
C               This program displays a section of an XY list of positio
C               the ARGS, numbered in their order in the list. By means
C               the cursor you can amend the positions of the stars in t
C               section.
C               You can then choose wether to store just the
C               amended section or the whole input list with the amended
C               section.The identifiers of the objects are preserved.
C
C               Optionally,  positions which lie outside the displayed a
C               can not be put up, with their positions left unchanged.
C
C               You can choose wether to keep points which lie
C               outside the displayed area.
C
C               A position can be removed from the list by putting the
C               refined position to the top l.h. corner of the image.
C
C
C          USE:-
C               When the section is put up, the cursor is set to the 1st
C               object. You move the cursor to an amended posn and press
C               red button. The position is stored and the cursor moves
C               the next object, until the section is finished.
C               A position to the top l.h. of the image
C               means that that position is to be removed from the list
C               To exit from amending positions, place the cursor
C               at the bottom l.h. corner of the image or below and to
C               left of that corner and press the button.  The remaining
C               positions will still be copied over.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input XY list
C
C         LIMITS          whole               The section of the list to
C                                             amended.
C
C         OVERCL          N                   Flag for clearing the cros
C                                             left  on  the  ARGS  from
C                                             previous program. Choices
C                                             N,Y
C
C         NUMINP          NO                  Flag for adding the number
C                                             the cross of the input lis
C
C         OUTPUT                              The  Output  file  contain
C                                             the positions
C
C         TITLE                               The Title to be  included
C                                             the Output file.
C
C         ALLINP        YES                   The output list to be the
C                                             input list with the amende
C                                             positions, or not, when on
C                                             the amended posns are stor
C
C         OPTION        INSIDE                Choice to display only tho
C                                             inside the area on the ARG
C                                             or all. Choices are INSIDE
C
C         SAVEOUT       NO                    Choice wether to save poin
C                                             outside displayed area. Ch
C                                             are YES,NO.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   Flag for  outputting  on
C                                             terminal  the  positions f
C                                             the cursor as yo  along.
C                                             default  (=2) makes it hap
C                                             automatically. Setting it
C                                             1   on  running  the  prog
C                                             supresses it.
C
C
C       USE OF TRACKER-BALL BUTTONS
C
C       GREEN 1     Reverts centre to image centre and magnification to
C
C       WHITE 2     Decreases magnification by times 2
C
C       WHITE 3     Increases magnification by times 2
C
C       RED   4     Inputs present position of cursor as star position.
C                   If this is to bottom lh of image blh corner, the
C                   program finishes.
C
C
C
C         A J Penny                RGO                             3-SEP
C
C
C---------------------------------------------------------------------
 
 
 
C
*CALLS
*       THIS FILE
*            XYINCB
*	THIS PACKAGE:
*		GETPAR,GTXYLR,EXTLST,GTXYLW,ADDLST,LBGONE,GTDSCR,
*		PTDSCR
*	STARLINK:
*		GETDYN,WRERR,WRUSER,RDKEYC,CYDSCR,FRDATA
*       ARGSLIB:
*               SRINIT
*	ASPIC:
*		ASP_PAN
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*ADAPTED BY
*     A.J. PENNY
* ----------------------------------------------------------------------
C
C
C
