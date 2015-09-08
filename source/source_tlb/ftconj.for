      PROGRAM FTCONJ
C+
C   FTCONJ
C
C   Dummy main program to use FTCONJ (Complex conjugation)
C   subprogram on a FT produced by the DFFT routines
C   The program is designed for the complex conjugation of a
C   transform which is Hermitian, and so only half of the modes
C   have been retained for a complete description of the image.
C   The program proceeds as follows
C   (1) The data is unpacked into a work array and the data is
C       rotated 90 degrees anticlockwise to replace the rows in
C       row order
C   (2) The row conjugations are carried out
C   (3) The partially conjugated FT is then rotated pi/2
C       clockwise to convert the columns to rows for efficiency of
C       access by the next stage of analysis
C   (4) Process (1) is repeated for the columns (in row layout)
C   (5) Small number harmonics, usually of most interest, are
C       shifted to the centre as follows.
C       (a) For rows (actually columns) 1 to IYEXT/2 + 1, row number
C           is increased by (IYEXT/2 - 1)
C       (b) For rows (columns) IYEXT/2 + 2 to IYEXT, row number is
C           decreased by (IYEXT/2 + 1)
C       (c) For columns (actually rows) 1 to IXEXT/2 + 1, column
C           number is increased by (IXEXT/2 - 1)
C       (d) For columns (rows) IXEXT/2 + 2 to IXEXT, column number
C           is decreased by (IXEXT/2 + 1)
C       The combination of rotation and shifting leaves (e.g.)
C       cosine,cosine harmonics in the bottom, right quadrant and
C       places the (0,0) harmonic at position (IXEXT/2,IYEXT/2 + 1)
C       in the array as in the input data
C
C       B.V.McNally ROE/DEC-1983
C
C-
      CALL FTCON
      END
