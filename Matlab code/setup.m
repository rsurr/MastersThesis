% setup.m: setup global parameters for the solution to the dynamic duopoly problem
%
%         John Rust, University of Maryland, November, 2010


   global og c_og c1 c2 ic qa qw v0 v1 cmin cmax csp clb beta_a beta_b k1 k2 bet df sigma eta ngp nqp ctol satol;
   global pr intmeth debg maxit init1;
   global cgrid p1vec brp1vec brp2vec gameinfo pgridtype;

   og=0;                 % enter 0 for solution to problem with no outside good, otherwise enter 1
                         % to include an outside good

   c_og=5.0;             % the "cost" of the outside good, if one is present in the model

   c1=5.0;               % marginal cost of production for firm 1 under its initial production technology

   c2=5.0;               % marginal cost of production for firm 2 under its initial production technology

   df=0.95;              % per period discount factor for the firms' discounted profits
   bet=0.95;

   cmin=0.0;             % lower bound on the state space, the lowest possible value of the 
                         % state of teh art marginal cost of production  

   clb=0.8;              % fraction of the current production cost c representing the largest
                         % possible decrease in costs in any given period

   cmax=5.0;             % upper bound on the state space, the highest possible value of the 
                         % state of the art marginal cost of production  

   csp=1.0;              % spacing parameter for the cost grid point: csp=1 results in uniformly
                         % spaces points between [cmin,cmax] whereas csp > 1 results in points that
                         % are more closely spaced near cmin, and csp < 1 results in points that are
                         % more closely spaces near cmax

                         
   ic=0.1;               % coefficient on the current cost of production (representing state of
                         % current technology) in the probability of a further advancement in
			 % technology this period, represented by a further lowering of costs,
			 % as given by a draw from a beta distribution on the interval [c,cmin].

   beta_a=0.0;           % a parameter of the beta distribution over the interval [c,cmin] where
                         % c is the current best technology marginal cost of production and cmin
			 % is a lower bound on this marginal cost beyond which technological improvements
			 % can no further reduce costs, even in the limit as time goes to infinity.
			 % So the expected cost, given an innovation occurs, will be
			 % cmin+(c-cmin)*beta_a/(beta_a+beta_b)

   beta_b=0.0;           % a parameter of the beta distribution over the interval [c,cmin] where
                         % c is the current best technology marginal cost of production and cmin
			 % is a lower bound on this marginal cost beyond which technological improvements
			 % can no further reduce costs, even in the limit as time goes to infinity.
			 % So the expected cost, given an innovation occurs, will be
			 % cmin+(c-cmin)*beta_a/(beta_a+beta_b)

   k1=0.3;               % parameter in the numerator of the function k(c), representing the cost of
                         % acquiring plant capable of producing at the current state of the art marginal
			 % cost of production, c

   k2=1.0;               % parameter the denominator, the coefficient of c, in the function k(c)

   sigma=0.00000;        % extreme value scaling parameter for the effect of unobserved demand shocks affecting
                         % consumers' choices between the two goods produced the duopolists and the outside good,
			 % if present. As sigma goes to zero, each consumer simply chooses the good that is 
			 % the cheapest, or the outside good if the "cost" of the outside good is less than the
			 % Bertrand duopoly prices charged by the two firms

   eta=0.0;              % scaling parameter for unobserved extreme value investment cost shocks affecting the duopolists'
                         % decisions on whether to invest in an improved state of the art production facility
			 % or not. When eta is zero, there are no unobserved investment cost shocks, and each
			 % firm invests in the state of the art if the expected future discounted profits net of
			 % the cost of investing is greater than the discounted future profits of continuing to
			 % produce using the firm's existing production equipment

   ngp=11;               % number of grid points for the cost variables (same number of grids used for
                         % each firm)

   nqp=10;               % number of quadrature points used in the numerical integration

   np1=100;              % number of points on the [0,1] interval used for initial grid search to find fixed points in
                         % seg.m and regs.m.  Used to create p1vec vector below

   pgridtype='quad';     % type of grid to generate for initial grid search of probabilities, either 'unif' which generates
                         % a 'uniform' grid, (with possibility non-uniform spacing near 0 or  1 depending on p1sp below), or
                         % 'quad' which generates the points as quadrature abscissa on the (0,1) interval which have closer
                         % spacing in the points symmetrically near 0 and near 1.

   p1sp=1.0;             % grid spacing parameter for the p1vec

   ctol=1e-11;           % convergence tolerance for Newton algorithm

   satol=1e-6;           % convergence tolerance for successive approximations algorithm for (v10,v11,v20,v21)

   maxit=200;            % maximum number of successive approximations iterations allowed

   init1=1;              % sets the order of the backward induction calculations in bellman.c:
                         % init1=1, do the calculations for firm 1 first, then do firm 2
			 % init1=0, do the calculations for firm 2 first, then do firm 1

   intmeth=1;            % 1 for 3 dimensional multi-linear interpolation, 0 for simplicial interpolation
                         % of the value functions in the successive approximation iterations in dds_c.c

   pr=1;                 % 1 for detailed output on the iterations (useful in debugging)
                         % 0 for only summary output

   debg=0;               % 1 for additional output for use in debugging program
                         % 0 for no debugging output

   % set up the grids and value functions

   cgrid=zeros(ngp,1);

   cgrid(1)=cmin;
   for i=2:ngp;
     cgrid(i)=cgrid(i-1)+(cmax-cgrid(i-1))/(ngp-i+1)^csp;
   end;

   p1vec=zeros(np1,1);
   brp1vec=zeros(np1,1);
   brp2vec=zeros(np1,1);

   p1vec(1)=0;
   for i=2:np1;
     p1vec(i)=p1vec(i-1)+(1-p1vec(i-1))/(np1-i+1)^p1sp;
   end;
