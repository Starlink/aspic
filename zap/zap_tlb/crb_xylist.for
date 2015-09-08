      SUBROUTINE CRB_XYLIST
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO PRINT THE CONTENTS OF AN X,Y LIST DATASET
*
*METHOD
*	OBTAIN INPUT DATASET. PRINT TITLE IF PRESENT. CALL PRLIST TO
*	PRINT THE DATA
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	INPUT
*		THE INPUT X,Y LIST DATASET
*
*CALLS
*	THIS PACKAGE:
*		GTXYLR,GTDSCR,PRLIST
*	STARLINK:
*		WRUSER,FRDATA
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE*30
C
C OBTAIN INPUT XY LIST
C
      CALL GTXYLR('INPUT',.FALSE.,NITEM,LSTLEN,IPIN,IERRXY)
      IF(IERRXY.EQ.0) THEN
C
C INPUT SUCCESSFULLY OBTAINED... EXTRACT TITLE
C
	TITLE=' '
	CALL GTDSCR('INPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,IERR)
C
C IF TITLE PRESENT, PRINT IT
C
	CALL WRUSER(' ',ISTAT)
	CALL WRUSER(' ',ISTAT)
	IF(TITLE.NE.' ') THEN
	  CALL WRUSER('                      TITLE: '//TITLE,ISTAT)
	  CALL WRUSER(' ',ISTAT)
	ENDIF
C
C CALL PRLIST TO PRINT THE CONTENTS OF THE LIST
C
	CALL PRLIST(%VAL(IPIN),NITEM,LSTLEN)
      ENDIF
C
C FREE DATA AREA AND RETURN
C
      CALL FRDATA(' ',ISTAT)
	CALL CNPAR('INPUT',ISTAT)
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   PATCH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               PATCH  [ NOISE=f ]  [ ORDER=n ]  [ CURSOR=t ]  [ FILE=f
C
C
C          FUNCTION:-
C               It allows the user to replace several circular  patches
C               an image with a fitted noisy piece of synthetic data. It
C               derived from a program, by  W  D  Pence (at  University
C               Sussex).The ARGS cursor and trackerball are used to cont
C               the process.
C
C
C          USE:-
C               It may be used to remove large defects, bright galaxies
C               any other localised unwanted pixels. The result can be v
C               convincing. If  the  defects  are  more  like  strips  t
C               circular patches the program ZAPLIN should be used.
C
C
C
C         USER PARAMETERS:-
C
C         IN                                  The input 2-D image which
C                                             being  displayed on the AR
C
C         OUTPUT                              The  new   image   with
C                                             patched regions.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         NOISE           1                   This is a noise factor  wh
C                                             may  be  between 0 (no noi
C                                             and 1 (noise level calcula
C                                             from the whole image).
C
C         ORDER           3                   This is the the degree of
C                                             2-dimensional  surface  wh
C                                             is to be fitted to an annu
C                                             around the circular patch.
C                                             may  be  0  (constant)  to
C                                             (bi-cubic).
C
C
C         CURSOR          TRUE                This defines if the cusor
C                                             be used to define the patc
C                                             If FALSE then the position
C                                             size of the  patches  will
C                                             read from the file PATCHES
C
C
C         FILE            FALSE               This defines  if  the posi
C                                             and size of  the  patches
C                                             be   written   to   the
C                                             PATCHES.DAT
C
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept this size of patch  and  then  do  a  fit
C                     display  the results. AFTER THIS BUTTON has been u
C                     then GREEN means accept the result  and  move  on
C                     another  location,  whilst  RED  means  revert to
C                     previous state.
C
C         WHITE 2     Decrease the size of the patch.
C
C         WHITE 3     Increase the size of the patch.
C
C         RED   4     Exit from the program.
C
C
C
C
C
C
C         D J King-W D Pence       RGO-U. of Sussex                7-JAN
C
C
C-----------------------------------------------------------------------
 
 
 
