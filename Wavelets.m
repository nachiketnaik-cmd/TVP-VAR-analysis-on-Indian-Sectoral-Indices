clear all;
clc;
% r=Data;
load r.mat;

n=size(r,2);
m=7; %indicate the level of decomposition

W=zeros(size(r,1),n*m);
aux0=-7
for i=1:n
    aux0=aux0+8
    wavcoef = modwt(r(:,i),'db2',m)';
    W(:,aux0:aux0+6)=wavcoef(:,1:end-1);
end

d1=zeros(size(r,1),n); d2=zeros(size(r,1),n); d3=zeros(size(r,1),n); d4=zeros(size(r,1),n); d5=zeros(size(r,1),n); d6=zeros(size(r,1),n); d7=zeros(size(r,1),n);
aux1=-7; aux2=-6;aux3=-5;aux4=-4;aux5=-3;aux6=-2;aux7=-1;
for i=1:n
    aux1=aux1+8; aux2=aux2+8; aux3=aux3+8; aux4=aux4+8; aux5=aux5+8; aux6=aux6+8; aux7=aux7+8;
    d1(:,i)=W(:,aux1); d2(:,i)=W(:,aux2); d3(:,i)=W(:,aux3); d4(:,i)=W(:,aux4); d5(:,i)=W(:,aux5); d6(:,i)=W(:,aux6); d7(:,i)=W(:,aux7);
end

% save d1.mat d1;
% save d2.mat d2;
% save d3.mat d3;
% save d4.mat d4;
% save d5.mat d5;
% save d6.mat d6;
% save d7.mat d7;



