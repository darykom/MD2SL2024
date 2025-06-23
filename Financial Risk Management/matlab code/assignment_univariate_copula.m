clc; clear; close all

folderName = 'Figure'; 
if ~exist(folderName, 'dir')
    mkdir(folderName);
    disp(['Cartella "' folderName '" creata.']);
end


% function show_kdes_loss(X, stocknames)
%     figure
%     stocks = size(X,2);
%     for k=1:stocks
%         ksdensity(-X(:, k)); hold on;
%     end
%     legend(stocknames);
%     title('KDE of losses'); 
%     hold off
% end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% format long g

palette = common.get_pastel_colors; 
cols       = [palette.Purple;      palette.Orange;      palette.Pink;      palette.Green];
cols_dark  = [palette.DarkPurple;  palette.DarkOrange;  palette.DarkPink;  palette.DarkGreen];
cols_light = [palette.LightPurple; palette.LightOrange; palette.LightPink; palette.LightGreen];

data = readtable("new_assets.csv");
Y = data(2:end,2:5).Variables;
X = -diff(log(Y));

lr_range = common.find_common_range(X);
common.show_data('assets', data, cols_dark)
common.show_logReturns('log_returns', data, lr_range, cols_dark)

stocknames = {'BakerHughes', 'BlackRock', 'Nvidia', 'Pfizer'};

qq_ranges = [-0.301,0.301; -0.201,0.201; -0.251,0.251; -0.151,0.151];

VaR_cmp = zeros(length(stocknames), 3);
ES_cmp  = zeros(length(stocknames), 3);
for idstock = 1:length(stocknames)

    x = X(:,idstock);
    assetlabel = stocknames{idstock};
    col = cols(idstock,:); col_dk = cols_dark(idstock,:); col_lt = cols_light(idstock,:);
    qqrange = qq_ranges(idstock,:);

    common.show_logret([assetlabel '_logret'], x, assetlabel, lr_range, col_dk)
    
    %common.show_asset([assetlabel '_quot_logret'], data, idstock+1, assetlabel, lr_range)
    % show_kdes_loss(X, stocknames)
    
    common.check_stationarity([assetlabel '_statio'], x, assetlabel);
    
    alpha = 0.95;

    % metodo storico
    [VaR_hist, ES_hist] = common.historical_risk_measures(x, alpha);
    common.show_pdf_var_es([assetlabel '_VarEs_hist'], x, assetlabel, [], VaR_hist, ES_hist, col, col_dk)

    % metodo parametrico
    fit_n=fitdist(x,'Normal');
    common.show_qqplot([assetlabel '_gaus_qqplot'], x, fit_n, qqrange);
    [VaR_gau, ES_gau] = common.parametric_risk_measures(x, alpha, ExamDistr.gauss);
    
    common.show_pdf_var_es([assetlabel '_gaus_varcov'], x, assetlabel, fit_n, VaR_gau, ES_gau, col, col_dk);
    
    fit_t=fitdist(x,'tlocationscale');
    common.show_qqplot([assetlabel '_stud_qqplot'], x, fit_t, qqrange);
    [VaR_stud, ES_stud] = common.parametric_risk_measures(x, alpha, ExamDistr.student);
    common.show_pdf_var_es([assetlabel '_stud_varcov'], x, assetlabel, fit_t, VaR_stud, ES_stud, col, col_dk)

    mc_syms = 100000;
    % rng(1) % rng(seed) specifies the seed for the random number generator so that you get the same simulated data if you do not change seed
    % [VaR_MCgau, ES_MCgau, rgVaR_MCgau, rgES_MCgau] = common.montecarlo_risk_measures(x, alpha, mc_syms, ExamDistr.gauss);
    rng(1) 
    [VaR_MCstud, ES_MCstud, rgVaR_MCstud, rgES_MCstud] = common.montecarlo_risk_measures([assetlabel '_MCstud'], x, alpha, mc_syms, ExamDistr.student, assetlabel, col_lt);

    VaR_cmp(idstock,:) = [VaR_gau, VaR_stud, VaR_hist];
    ES_cmp(idstock,:)  = [ES_gau,  ES_stud,  ES_hist];

    common.show_distr_tail([assetlabel '_cmp_distrfit'], x, assetlabel, fit_n, fit_t, VaR_cmp(idstock,:), col, col_lt)
    
end

common.compare_varcov_distr('cmp_varcov_VaR', VaR_cmp(:,1:2), {'Normal', 'Student'}, stocknames, 'VaR comparison', cols_dark, [0.020,0.055]);
common.compare_varcov_distr('cmp_varcov_ES',  ES_cmp(:,1:2),  {'Normal', 'Student'}, stocknames, 'ES comparison',  cols_dark, []);
%
%common.compare_risk_methods('cmp_risk_VaR', VaR_cmp(:,2:3), {'Student', 'Historical'}, stocknames, cols, 'VaR comparison');
%common.compare_risk_methods('cmp_risk_ES', ES_cmp(:,2:3), {'Student', 'Historical'}, stocknames, cols, 'ES comparison');
%
common.compare_varcov_distr('cmp_risk_VaR', VaR_cmp(:,[3,2]), {'Historical', 'Student'}, stocknames, 'VaR comparison', cols_dark, []);
common.compare_varcov_distr('cmp_risk_ES',  ES_cmp(:, [3,2]), {'Historical', 'Student'}, stocknames, 'ES comparison',  cols_dark, []);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

X = diff(log(Y));

n=length(X); k = size(X,2);
rnk=zeros(n,k);
U=zeros(n,k);

for i = 1 : k
    for j = 1 : n
    
    rnk(j,i)=sum(X(:,i)<=X(j,i));
    U(j,i)=rnk(j,i)/(n+1);
    
    end
end
common.show_scatterplot('unif_marginals', U);

[hat_rho_ML_g]=copulafit('gaussian',U);
[hat_rho_ML_t, hat_nu_ML]=copulafit('t',U);  

LL_g=sum(log(copulapdf('gaussian', U, hat_rho_ML_g)))
LL_t=sum(log(copulapdf('t', U, hat_rho_ML_t, hat_nu_ML)))

BIC_g=log(n)*2-2*LL_g
BIC_t=log(n)*2-2*LL_t

%%%%%%%%%%%%% con le t stimate marginalmente

UStud = zeros(n,k);
for i = 1 : k
    fit_t=fitdist(X(:,i),'tlocationscale');
    % normalizzo i dati a media nulla e varianza unitaria
    xn = (X(:,i)-fit_t.mu)/fit_t.sigma;
    UStud(:,i) = tcdf(xn,fit_t.nu);
end
hat_rho_ML_gStud = copulafit('gaussian',UStud);
[hat_rho_ML_tStud, hat_nu_MLStud] = copulafit('t',UStud);  

LL_gStud=sum(log(copulapdf('gaussian', UStud, hat_rho_ML_gStud)))
LL_tStud=sum(log(copulapdf('t', UStud, hat_rho_ML_tStud, hat_nu_MLStud)))

BIC_gStud=log(n)*2-2*LL_gStud
BIC_tStud=log(n)*2-2*LL_tStud



common.show_scatterplot('unif_marginals_Stud', UStud)


% scatterplot dei log returns
common.show_scatterplot('Xpairplot', X)



%%%%% EXTRA: VaR e ES per un certo portafoglio usando Monte Carlo %%%%%%%%%

% beta = [10 10 10 10];
% omega = beta.*Y(1,:);
% 
% 
% rng(1) % rng(seed) specifies the seed for the random number generator so that you get the same simulated data if you do not change seed
% m=10^2; % take at least m=10^4  
% 
% C=zeros(n,4,m);
% S=zeros(n,4,m); 
% 
% for j = 1 : m
% 
% C(:,:,j)=  copularnd('t', hat_rho_ML_t, hat_nu_ML ,n);
% 
% end
% 
% Fit1=fitdist(X(:,1),'tlocationscale');
% Fit2=fitdist(X(:,2),'tlocationscale');
% Fit3=fitdist(X(:,3),'tlocationscale');
% Fit4=fitdist(X(:,4),'tlocationscale');
% 
% X_mc(:,1,:)=Fit1.mu+ Fit1.sigma*tinv(C(:,1,:),Fit1.nu); % quantile transform 
% X_mc(:,2,:)=Fit2.mu+ Fit2.sigma*tinv(C(:,2,:),Fit2.nu);
% X_mc(:,3,:)=Fit3.mu+ Fit3.sigma*tinv(C(:,3,:),Fit3.nu);
% X_mc(:,4,:)=Fit4.mu+ Fit4.sigma*tinv(C(:,4,:),Fit4.nu);
% 
% eX(:,1,:) = exp(X_mc(:,1,:));
% eX(:,2,:)=  exp(X_mc(:,2,:));
% eX(:,3,:)=  exp(X_mc(:,3,:));
% eX(:,4,:)=  exp(X_mc(:,4,:));
% 
% L_mc=zeros(n,m);
% 
% for j = 1 : m
% 
%     L_mc(:,j)= - omega(1)*(eX(:,1,j) -1) - omega(2)*(eX(:,2,j) -1) - omega(3)*(eX(:,3,j) -1) - omega(4)*(eX(:,4,j) -1);
% 
% end
% 
% VaRs = quantile(L_mc,0.99);
% VaRs_975 = quantile(L_mc,0.975);
% 
% VaR_MC=mean(VaRs);
% RCI_VaR_MC=quantile(VaRs,0.975); % 95% emp. conf. int.
% LCI_VaR_MC=quantile(VaRs,0.025);
% 
% exc=zeros(m,1);
% D=zeros(n,m);
% ESs=zeros(m,1);
% 
%  for j = 1 : m
% 
%    exc(j) = sum(L_mc(:,j) > VaRs_975(j));
%    D(:,j)=  (L_mc(:,j) > VaRs_975(j));
%    ESs(j) = sum(L_mc(:,j).*D(:,j))/exc(j);
% 
%  end
% 
% ES_MC = mean(ESs);
% RCI_ES_MC= quantile(ESs,0.975);
% LCI_ES_MC= quantile(ESs,0.025);

% figure 
% subplot(1,2,1)
% ksdensity(VaRs)
% title('Density of VaR estimates')
% 
% subplot(1,2,2)
% ksdensity(ESs)
% title('Density of ES estimates')
% 

