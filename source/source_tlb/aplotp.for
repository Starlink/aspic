C+      PROGRAM APLOTP
C
C   A P L O T P
C
C   Plots an image to the ARGS
C    scales up or down to fit whole image onto args screen (or larger dim if not square)
C    thus images not 512 square are handled, by calling generalised plot routine, PLOTS
C    if scaling is involved coords returned from the cursor will only be approx.
C    and should not be used for accurate work - when a 1-1 window should be used.
C    when scaling down, averaging not done properly, - only roughly over a 2*2 area (to be quick)
C
C	D. Tudhope/ROE/Aug 1982
C_

      CALL PLOT
      END
