*   Common block holding working-set limit and base priority
      INTEGER WSL,BP
      COMMON /CASP_WSP/WSL,BP
      SAVE /CASP_WSP/
      DATA WSL,BP/-1,-1/
