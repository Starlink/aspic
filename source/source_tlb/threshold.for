      PROGRAM THRESH
*+
*
*    THRESH
*
*    Purpose : To set to zero all values of the given image
*              which lie outside the specified lower and upper
*              limits.
*
*    Usage   : The input image is prompted for followed by the
*              low threshold limit and the high threshold limit.
*              A filename for the output image is then requested.
*              All values which are less than the low threshold
*              limit or greater then the high threshold limit
*              will be set to zero.
*
*    Subroutine called :
*    THRESH1           : E2DASP
*
*    D.W.T.Baines/ROE/March 1983
*
*-
      CALL THRESH1
      END
