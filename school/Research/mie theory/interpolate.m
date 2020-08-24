## Octave file to interpolate on given data:
file = "./e1.csv"
e1.f = csvread(file);
e1.x = e1.f(:,1);
e1.y = e1.f(:,2);
e1.n = 7;

xmin = ceil(min(e1.x));
xmax = floor(max(e1.x));
step = 0.01;
vals = xmin:step:xmax;

## plot(e1.x,e1.y,'.')
## hold on;

e1.p = polyfit(e1.x,e1.y,e1.n);
e1.i = polyval(e1.p,vals);

str = "";

## plot(vals,e1.i);
for i = 1:length(e1.p)
  str = [str num2str(e1.p(i)) " * (e ** " num2str(i-1) ") + "];
endfor
