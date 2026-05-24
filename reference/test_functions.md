# Test Functions for Global Optimisation

Test functions commonly used as benchmark for global optimisation
problems

## Usage

``` r
ackley(x)
griewank(x)
rastrigin(x)
rosenbrock(x)
schafferF6(x)
schwefel(x)
sphere(x)
sackley(x, o=-32+64*runif(length(x)), fbias=-140)
sgriewank(x, o=-600+1200*runif(length(x)), fbias=-180)
srastrigin(x, o=-5+10*runif(length(x)), fbias=-330)
srosenbrock(x, o=-100+200*runif(length(x)), fbias=390)
sschwefel1_2(x, o=-100+200*runif(length(x)), fbias=-450)
ssphere(x, o=-100+200*runif(length(x)), fbias=-450)
```

## Arguments

- x:

  numeric vector to be evaluated

- o:

  numeric shifting vector to be used, with the same length of `x`

- fbias:

  numeric with the bias to be imposed

## Details

The **Ackley** test function is multimodal and separable, with several
local optima that, for the search range \[-32, 32\], look more like
noise, although they are located at regular intervals. The Ackley
function only has one global optimum located at the point o=(0,...,0).
It is defined by: \$\$ ackley = 20+\exp(1)-20\exp\left(
-0.2\sqrt{\frac{1}{n}\sum\_{i=1}^{n}x\_{i}^2}
\right)-\exp\left(\frac{1}{n}\sum\_{i=1}^{n}\cos(2\pi x\_{i})\right) ;
-32 \leq x_i \leq 32 \\ ; \\ i=1,2,\ldots,n \$\$

The generalized **Rastrigin** test function is non-convex, multimodal
and additively separable. It has several local optima arranged in a
regular lattice, but it only has one global optimum located at the point
o=(0,...,0). The search range for the Rastrigin function is \[-5.12,
5.12\] in each variable. This function is a fairly difficult problem due
to its large search space and its large number of local minima. It is
defined by:

\$\$ rastrigin = 10n+\sum\_{i=1}^{n}\left\[x\_{i}^{2}-10\cos(2\pi
x\_{i})\right\] \\ ; \\ -5.12 \leq x_i \leq 5.12 \\ ; \\ i=1,2,\ldots,n
\$\$

The **Griewank** test function is multimodal and non-separable, with
several local optima within the search region defined by \[-600, 600\].
It is similar to the Rastrigin function, but the number of local optima
is larger in this case. It only has one global optimum located at the
point o=(0,...,0). The function interpretation changes with the scale;
the general overview suggests convex function, medium-scale view
suggests existence of local minima, and finally zoom on the details
indicates complex structure of numerous local minima. While this
function has an exponentially increasing number of local minima as its
dimension increases, it turns out that a simple multistart algorithm is
able to detect its global minimum more and more easily as the dimension
increases (Locatelli, 2003). It is defined by:

\$\$ griewank =
\frac{1}{4000}\sum\_{i=1}^{n}x\_{i}^{2}-\prod\_{i=1}^{n}\cos\left(\frac{x_i}{\sqrt{i}}\right)+1
\\ ; \\ -600 \leq x_i \leq 600 \\ ; \\ i=1,2,\ldots,n \$\$

The **Rosenbrock** function is non-convex, unimodal and non-separable.
It is also known as *Rosenbrock's valley* or *Rosenbrock's banana*
function. The global minimum is inside a long, narrow, parabolic shaped
flat valley. To find the valley is trivial. To converge to the global
minimum, however, is difficult. It only has one optimum located at the
point o=(1,...,1). It is a quadratic function, and its search range is
\[-30, 30\] for each variable. It is defined by:

\$\$ rosenbrock =
\sum\_{i=1}^{n-1}\left\[100(x\_{i+1}-x\_{i}^{2})^{2}+(1-x\_{i})^{2}\right\]
\\ ; \\ -30 \leq x_i \leq 30 \\ ; \\ i=1,2,\ldots,n \$\$

The main difficulty of the **Schaffer's F6** test function is that the
size of the potential maxima that need to be overcome to get to a
minimum increases the closer one gets to the global minimum. It is
defined by:

\$\$ schafferF6 =
0.5+\frac{\sin^{2}\sqrt{\sum\_{i=1}^{n}x\_{i}^{2}}-0.5}{(1+0.001\sum\_{i=1}^{n}x\_{i}^{2})^{2}}
\\ ; \\ -100 \leq x_i \leq 100 \\ ; \\ i=1,2,\ldots,n \$\$  

The *first function of De Jong's* or **Sphere** function is one of the
most simple test functions available in the specialized literature. This
continuous, convex, unimodal and additively separable test function can
be scaled up to any number of variables. It belongs to a family of
functions called quadratic functions and only has one optimum in the
point o=(0,...,0). The search range commonly used for the Sphere
function is \[-100, 100\] for each decision variable. It is defined by:

\$\$ sphere = \sum\_{i=1}^{n} x\_{i}^2 \\ ; \\ -100 \leq x_i \leq 100 \\
; \\ i=1,2,\ldots,n \$\$  

The **Schwefel's** function is non-convex, multimodal, and additively
separable. It is deceptive in that the global minimum is geometrically
distant, over the parameter space, from the next best local minima.
Therefore, the search algorithms are potentially prone to convergence in
the wrong direction. In addition, it is less symmetric than the
Rastrigin function and has the global minimum at the edge of the search
space \[-500, 500\] at position o=(420.9687,...,420.9687). Additionally,
there is no overall, guiding slope towards the global minimum like in
Ackley's, or less extreme, in Rastrigin's function. It is defined by:

\$\$ schwefel = 418.982887274338n + \sum\_{i=1}^{n} -x\_{i}
\sin(\sqrt{\|x\_{i}\|} \\ ; \\ -500 \leq x_i \leq 500 \\ ; \\
i=1,2,\ldots,n \$\$

The **Shifted Schwefel's Problem 1.2** function is unimodal,
non-separable, and scalable. It is defined by:

\$\$ sschwefel1\\2 = \sum\_{i=1}^{n} { \left(\sum\_{j=1}^{i} {x\_{j}}
\right)^2 } + f\\bias \\ ; \\ -500 \leq x_i \leq 500 \\ ; \\
i=1,2,\ldots,n \$\$  

Some optimisation algorithms take advantage of known properties of the
benchmark functions, such as local optima lying along the coordinate
axes, global optimum having the same values for many variables and so
on. In order to avoid the previous shortcomings, shifting vector and a
single bias is introduced for some benchmark functions, reported
afterwards.

The **Shifted Ackley** is defined by: \$\$ sackley =
20+\exp(1)-20\exp\left(-0.2\sqrt{\frac{1}{n}\sum\_{i=1}^{n}z\_{i}^2}\right)-\exp\left(\frac{1}{n}\sum\_{i=1}^{n}\cos(2\pi
z\_{i})\right) + f\\bias , z=x-o ; \\ i=1,2,\ldots,n \$\$  

The **Shifted Griewank** is defined by: \$\$ sgriewank =
\frac{1}{4000}\sum\_{i=1}^{n}z\_{i}^{2}-\prod\_{i=1}^{n}\cos\left(\frac{z_i}{\sqrt{i}}\right)+1 +
f\\bias \\ , \\ z=x-o ; \\ i=1,2,\ldots,n \$\$  

The **Shifted Sphere** is defined by: \$\$ ssphere = \sum\_{i=1}^{n}
z\_{i}^2 + f\\bias \\ , \\ z=x-o ; \\ i=1,2,\ldots,n \$\$

## Value

Each test function returns a single numeric value corresponding to the
function evaluated on the vector `x`

## References

Dieterich, J.M. and B.Hartke. 2012. Empirical review of standard
benchmark functions using evolutionary global optimization. Appl.Math.
3. 1552-1564, DOI:10.4236/am.2012.330215  

Barrera, J., and C. Coello Coello. 2010, Test function generators for
assessing the performance of PSO algorithms in multimodal optimization,
in Handbook of Swarm Intelligence, vol. 8, edited by B. Panigrahi, Y.
Shi, and M.-H. Lim, chap. Adaptation, Learning, and Optimization, pp.
89-117, Springer Berlin Heidelberg, doi:10.1007/978-3-642-17390-5 4  

Problem Definitions and Evaluation Criteria for the CEC 2005 Special
Session on Real-Parameter Optimization
<http://www.cmap.polytechnique.fr/~nikolaus.hansen/Tech-Report-May-30-05.pdf>  

Test functions for optimization needs:
<https://robertmarks.org/Classes/ENGR5358/Papers/functions.pdf>  

**Web pages**:

Test functions for optimization.
<https://en.wikipedia.org/wiki/Test_functions_for_optimization>

Benchmark Problems
[http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume24/ortizboyer05a-html/node6.html](http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume24/ortizboyer05a-html/node6.md)

Test Functions for Unconstrained Global Optimization
<http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page364.htm>

Rosenbrock: <https://www.sfu.ca/~ssurjano/rosen.html>,
<https://en.wikipedia.org/wiki/Rosenbrock_function>

Sphere: <https://www.sfu.ca/~ssurjano/spheref.html>

Rastrigin: <https://www.sfu.ca/~ssurjano/rastr.html>,
<https://en.wikipedia.org/wiki/Rastrigin_function>

Ackley: <https://www.sfu.ca/~ssurjano/ackley.html>

Griewank: Locatelli, M. 2003. A note on the griewank test function,
Journal of Global Optimization, 25 (2), 169-174,
doi:10.1023/A:1021956306041

Schaffer's F6 Xiaohong Qiu, Jun Liu. 2009. A Novel Adaptive PSO
Algorithm on Schaffer's F6 Function. Hybrid Intelligent Systems,
International Conference on, pp. 94-98, 2009 Ninth International
Conference on Hybrid Intelligent Systems

Schwefel: <https://www.sfu.ca/~ssurjano/schwef.html>

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md)
