      SUBROUTINE ARGS_TBSTX(IX,IY,IB1,IB2,IB3,IB4)
*     -------------------------------------------

*+
*
*  ARGS_TBSTATX
*
*  EXECUTE ARGS TRACKERBALL/CURSOR STATUS READING ROUTINE
*
*
*
*
*  RETURNED:
*
*     IX,IY              CURSOR X,Y
*     IB1,IB2,IB3,IB4    STATUS OF BUTTONS 1-4 (1=PRESSED)
*
*
*-
      INTEGER IX,IY,IB1,IB2,IB3,IB4
*
*   DATA AREA FOR REPLY FROM ARGS PROGRAM.
      INTEGER*2 TD(6)
* 
*
*   INITIALISE TRACKERBALL AND LIGHT LAMPS.
      CALL ARGS_TBINIT
*   RUN ARGS PROGRAM
      CALL RUN_TBST
*   OBTAIN RESULTS AND DELIVER TO CALLER
      CALL READ_TBST(TD)
      IX=TD(1)
      IY=TD(2)
      IB1=TD(3)
      IB2=TD(4)
      IB3=TD(5)
      IB4=TD(6)

      END
