% eqplot.m: plots equilibrium prices of two firms as a function of sigma in Bertrand duopoloy


setup;

x=(0:.0001:.1)';

x=[x' (.2:.1:1)]';

n=size(x,1);

c1=1.5;
c2=2;

profit=zeros(n,2);
price=zeros(n,2);
marketshare=zeros(n,2);

for i=1:n;

   sigma=x(i);
   eqinfo=bne(c1,c2);

   profit(i,:)=eqinfo(1:2)';
   price(i,:)=eqinfo(3:4)';
   marketshare(i,:)=eqinfo(5:6)';

end;

figure(1);
plot(x,profit(:,1),'-r',x,profit(:,2),'--b');
xlabel('\sigma');
ylabel('Profit');
legend(sprintf('Firm 1, c_1=%g',c1),sprintf('Firm 2, c_2=%g',c2),'Location','NorthWest');
title('Duopoly Profits as a Function of \sigma');

figure(2);
plot(x,price(:,1),'-r',x,price(:,2),'--b');
xlabel('\sigma');
ylabel('Price');
legend(sprintf('Firm 1, c_1=%g',c1),sprintf('Firm 2, c_2=%g',c2),'Location','NorthWest'),
title('Duopoly Prices as a Function of \sigma');

figure(3);
plot(x,marketshare(:,1),'-r',x,marketshare(:,2),'--b');
xlabel('\sigma');
ylabel('Market Share');
legend(sprintf('Firm 1, c_1=%g',c1),sprintf('Firm 2, c_2=%g',c2));
title('Duopoly Market Shares as a Function of \sigma');
