C+	PROGRAM APLOTQ
C
C   A P L O T Q
C
C   Plots an image to a quadrant of the ARGS scales up or down to
C   fit whole image onto quadrant (or larger dimension if the image
C   is not square) thus images not 512 square are handled by calling
C   generalised plot routine, PLOTS if scaling is involved coords
C   returned from the cursor will only be approx. and should not be
C   used for accurate work - when a 1-1 window should be used.
C   When scaling down, averaging not done properly, - only roughly
C   over a 2*2 area (to be quick)
C
C    D. Tudhope/ROE/Aug 1982
C-

      CALL PLOTQ
      END
