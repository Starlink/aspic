      SUBROUTINE RV_INITIALISE
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINR RV_ININTIALIZE
C
C
C         This routine initializes an array of  ICL-equivalent  characters, 
C         which are stored in a common array RVTRAN. 
C
C
C         J V Carey                RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                             
                                                                                
                                                                                
                                                                                
C This subroutine initialises the character*1 array CHARVAL
C with the character equivalents of the ICL code
C The array is stored in a common block RVTRAN
C

      CHARACTER*1 CHARVAL(0:63)



      COMMON/RVTRAN/ CHARVAL
C

C
      CHARVAL(0)  = '0'
      CHARVAL(1)  = '1'
      CHARVAL(2)  = '2'
      CHARVAL(3)  = '3'
      CHARVAL(4)  = '4'
      CHARVAL(5)  = '5'
      CHARVAL(6)  = '6'
      CHARVAL(7)  = '7'
      CHARVAL(8)  = '8'
      CHARVAL(9)  = '9'
      CHARVAL(10) = ':'
      CHARVAL(11) = ';'
      CHARVAL(12) = '<'
      CHARVAL(13) = '='
      CHARVAL(14) = '>'
      CHARVAL(15) = '?'
      CHARVAL(16) = ' '
      CHARVAL(17) = '!'
      CHARVAL(18) = '"'
      CHARVAL(19) = '#'
      CHARVAL(20) = '$'
      CHARVAL(21) = '%'
      CHARVAL(22) = '&'
      CHARVAL(23) = ''''
      CHARVAL(24) = '('
      CHARVAL(25) = ')'
      CHARVAL(26) = '*'
      CHARVAL(27) = '+'
      CHARVAL(28) = ','
      CHARVAL(29) = '-'
      CHARVAL(30) = '.'
      CHARVAL(31) = '/'
      CHARVAL(32) = '@'
      CHARVAL(33) = 'A'
      CHARVAL(34) = 'B'
      CHARVAL(35) = 'C'
      CHARVAL(36) = 'D'
      CHARVAL(37) = 'E'
      CHARVAL(38) = 'F'
      CHARVAL(39) = 'G'
      CHARVAL(40) = 'H'
      CHARVAL(41) = 'I'
      CHARVAL(42) = 'J'
      CHARVAL(43) = 'K'
      CHARVAL(44) = 'L'
      CHARVAL(45) = 'M'
      CHARVAL(46) = 'N'
      CHARVAL(47) = 'O'
      CHARVAL(48) = 'P'
      CHARVAL(49) = 'Q'
      CHARVAL(50) = 'R'
      CHARVAL(51) = 'S'
      CHARVAL(52) = 'T'
      CHARVAL(53) = 'U'
      CHARVAL(54) = 'V'
      CHARVAL(55) = 'W'
      CHARVAL(56) = 'X'
      CHARVAL(57) = 'Y'
      CHARVAL(58) = 'Z'
      CHARVAL(59) = '['
      CHARVAL(60) = '$'
      CHARVAL(61) = ']'
      CHARVAL(62) = '^'
      CHARVAL(63) = '_'

      END
