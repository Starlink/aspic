      PROGRAM MODHELP

C+
C    PROGRAM MODHELP
C	short help listing for MODPIX suite of programs
C
C  D. Tudhope/ROE/Jan 1983
C
C-

      CALL WRUSER('       MODPIX PROGRAMS FOR PICTURE MODIFICATION',I)
      CALL WRUSER(' All programs take input image from stack and',I)
      CALL WRUSER(' return modified output image to stack.',I)
      CALL WRUSER(' They can be run in any order.',I)
      CALL WRUSER('  ',I)
      CALL WRUSER('         general modifications',I)
      CALL WRUSER(' MODHELP -- THIS HELP INFORMATION',I)
      CALL WRUSER(' INDPIX  -- MODIFY INDIVIDUAL PIXEL',I)
      CALL WRUSER(' LINFIX  -- FILL HORIZONTAL LINE WITH GIVEN VALUE',I)
      CALL WRUSER(' COLFIX  -- FILL VERTICAL LINE WITH GIVEN VALUE',I)
      CALL WRUSER(' RECFIL  -- FILL RECTANGLE WITH GIVEN VALUE',I)
      CALL WRUSER(' OUTSET  -- SET PIXELS OUTSIDE CIRCLE TO VALUE',I)
      CALL WRUSER(' DISKFIL -- FILL CIRCLE WITH GIVEN VALUE',I)
      CALL WRUSER('         fourier transform modifications',I)
      CALL WRUSER(' LOFREQ  -- ELIMINATE LOW FREQUENCIES',I)
      CALL WRUSER(' HIFREQ  -- ELIMINATE HIGH FREQUENCIES',I)
      CALL WRUSER(' SYMDIS  -- FILL 4 SYMMETRIC DISCS WITH VALUE',I)
      CALL WRUSER(' SINCFIL -- MULTIPLY IMAGE BY SINC FUNCTION',I)
      CALL WRUSER('  ',I)
      CALL WRUSER('    see ROE LUN 14.1 for more details',I)
      CALL WRUSER(' ',I)
      END
