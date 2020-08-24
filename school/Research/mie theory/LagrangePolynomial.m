function P = LagrangePolynomial(x,xs,fs)
# Synopsis
#
# This function implements the Lagrange Polynomial formula from section
# 4.2 of the leture notes
#
# Fits a polynomial to a certain set of {xs} between 0 and 4
# and their corresponding {fs}
# Compute Largrange Polynomial for the given samples
# The formula is a sum of ns terms, so we initialize it to the additive identity
# The formula:
# P = sum k = 1:n of (numerator/denominator)*fs(k)
# Numerator and demoninator are products
n = length(xs);
N = length(x);
P = zeros(1,N);
for(kx = 1:N) 
  for(k = 1:n)
    Numerator = 1;   # Can't initialize to zero because its a product
    Denominator = 1; # Same here
    for (j = 1:n)
      if (j != k)
	Numerator *= (x(kx)- xs(j));
        Denominator *= (xs(k) - xs(j));
      endif
    endfor
    P(kx) += (Numerator / Denominator)*fs(k);
  endfor
endfor
endfunction 

