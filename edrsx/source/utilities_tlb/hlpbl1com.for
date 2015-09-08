*      module hlpbl1com
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Common block to communicate between the gthelp routine
*       and an output routine which controls the formatting of
*       help information on the user's screen
*
*SOURCE
*       HLPBL1COM.INC in UTILITIES.TLB
*
*USED BY
*       All programs which access Starlink parameters
*AUTHOR
*       R.F. Warren-Smith
*       (modified by D.S. Berry MAVAD::DSB 26/8/87)
*----------------------------------------------------------------------
*
* THE NUMBER OF NON-BLANK LINES OUTPUT SO FAR (INITIALLY SET NEGATIVE
* TO SUPRESS FIRST FEW LINES)
*
      INTEGER NLOUT
*
* FLAG TO INDICATE IF INITIAL BLANKS ON LINES ARE TRIMMED OFF
*
      LOGICAL TRIM
*
* SUCCESS STATUS: HELP FOUND OR NOT
*
      LOGICAL CANHLP
*
* NUMBER OF LINES OF TEXT IN A BLOCK. BLOCKS ARE SEPERATED BY A PROMPT
* FOR THE USER TO PRESS RETURN TO CONTINUE
*
      INTEGER BLOCK
*
* NUMBER OF LINES OF TEXT ACTUALLY DISPLAYED SINCE START OF THE CURRENT
* BLOCK
*
      INTEGER NWRITE
*
* NAME OF HELP LIBRARY. SET TO 'FIRST TIME' IF NO ATTEMPT HAS BEEN MADE
* TO FIND THE LIBRARY SO FAR
*
      CHARACTER LIBNAM*255
      DATA LIBNAM /'FIRST TIME'/


      COMMON /HLPBL1/NLOUT,TRIM,CANHLP,BLOCK,NWRITE,LIBNAM
