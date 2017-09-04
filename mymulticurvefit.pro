PRO mymulticurvefit, XYW, A, F, PDER
  X = XYW[*,0]
  Y = XYW[*,1]
  W = XYW[*,2]
  F = A[0] + A[1]*X + A[2]*Y + A[3]*W
  init=indgen(n_elements(X))
  init(*)=1
  PDER = [[init], [X], [Y], [W]]
END