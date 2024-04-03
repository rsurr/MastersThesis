
   function [eqinfo]=bne(c1,c2);

   global og c_og sigma maxit;

  if (sigma > 0);

   p=max([c1 c2]');
   p=[p p]';

   pp=zeros(2,1);

   tol=.00001;
   cp=2*tol;

   i=0;

   while (cp > tol & i < maxit);

     pp=p-inv(df(p,c1,c2))*f(p,c1,c2);
     cp=max(abs(f(pp,c1,c2)));
     i=i+1; 
     p=pp;

%     fprintf('%i %g\n',i,cp);

   end;

   s=zeros(2,1);

   if (og);

    s(1)=pr1(p);
    s(2)=pr2(p);

   else;

   s(1)=pr(p);
   s(2)=1-s(1);

   end;

   pf=zeros(2,1);
   pf(1)=(p(1)-c1)*s(1);
   pf(2)=(p(2)-c2)*s(2);

   eqinfo=[pf' p' s']';

 else;

   pf=zeros(2,1);
   p=max([c1 c2]');
   p=[p p]';

   if (og);

     if (c1 < c2);

        if (c1 > c_og);

          pf(1)=0;
          pf(2)=0;
          s=[0 0]';

        else;

          if (c2 < c_og);
    
            pf(1)=(c2-c1);
            pf(2)=0;
            s=[1 0]';

          else;

            pf(1)=(c_og-c1);
            pf(2)=0;
            s=[1 0]';

          end;

        end;

     else;

        if (c2 > c_og);

          pf(1)=0;
          pf(2)=0;
          s=[0 0]';

        else;

          if (c1 < c_og);
    
            pf(1)=0;
            pf(2)=(c1-c2);
            s=[0 1]';

          else;

            pf(1)=0;
            pf(2)=(c_og-c2);
            s=[0 1]';

          end;

        end;

      end;

   else;

      if (c1 < c2);

        pf(1)=(c2-c1);
        pf(2)=0;
        s=[1 0]';

      else;

        pf(1)=0;
        pf(2)=(c1-c2);
        s=[0 1]';

      end;

   end;

   eqinfo=[pf' p' s']';

 end; 

%   fprintf('sig                 %g\n',sigma);
%   fprintf('Costs               c1=%g c2=%g\n',c1,c2);
%   fprintf('Equilibrium prices: p1=%g p2=%g\n',p(1),p(2));
%   fprintf('Market Shares:      P1=%g P2=%g outside good: %g\n',s(1),s(2),1.0-s(1)-s(2));
%   fprintf('Profits             pf1=%g pf2=%g\n',pf(1),pf(2));
