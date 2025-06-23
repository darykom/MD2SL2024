
classdef common
    methods(Static)
    
% Data visualization ------------------------------------------------------

        function colors = get_pastel_colors()
        
            pastel.LightGray   = [140, 140, 140]/255;
            pastel.LightBlue   = [136, 189, 230]/255;
            pastel.LightOrange = [251, 178,  88]/255;
            pastel.LightGreen  = [144, 205, 151]/255;
            pastel.LightPink   = [246, 170, 201]/255;
            pastel.LightBrown  = [191, 165,  84]/255;
            pastel.LightPurple = [188, 153, 199]/255;
            pastel.LightYellow = [237, 221,  70]/255;
            pastel.LightRed    = [240, 126, 110]/255;
            pastel.Gray        = [ 77,  77,  77]/255;
            pastel.Blue        = [ 93, 165, 218]/255;
            pastel.Orange      = [250, 164,  58]/255;
            pastel.Green       = [ 96, 189, 104]/255;
            pastel.Pink        = [241, 124, 176]/255;
            pastel.Brown       = [178, 145,  47]/255;
            pastel.Purple      = [178, 118, 178]/255;
            pastel.Yellow      = [222, 207,  63]/255;
            pastel.Red         = [241,  88,  84]/255;
            pastel.DarkGray    = [ 38,  38,  38]/255;
            pastel.DarkBlue    = [ 38,  93, 171]/255;
            pastel.DarkOrange  = [223,  92,  36]/255;
            pastel.DarkGreen   = [  5, 151,  72]/255;
            pastel.DarkPink    = [229,  18, 111]/255;
            pastel.DarkBrown   = [157, 114,  42]/255;
            pastel.DarkPurple  = [123,  58, 150]/255;
            pastel.DarkYellow  = [199, 180,  46]/255;
            pastel.DarkRed     = [203,  32,  39]/255;
        
            % pastel.blue = [158, 202, 225] / 255;
            % pastel.orange = [253, 192, 134] / 255;
            % pastel.green = [174, 213, 129] / 255;
            % pastel.red = [244, 159, 160] / 255;
            % pastel.purple = [197, 176, 213] / 255;
            % pastel.brown = [196, 156, 148] / 255;
            % pastel.pink = [247, 182, 210] / 255;
            % pastel.gray = [199, 199, 199] / 255;
            % pastel.yellow = [224, 221, 138] / 255;
            % pastel.cyan = [153, 216, 201] / 255;
            % pastel.dark_blue = [140, 182, 205] / 255;
            % pastel.dark_orange = [247, 163, 110] / 255;
            % pastel.dark_green = [142, 193, 120] / 255;
            % pastel.dark_red = [237, 137, 139] / 255;
            % pastel.dark_purple = [178, 159, 192] / 255;
            % pastel.dark_brown = [177, 139, 133] / 255;
            % pastel.dark_pink = [239, 164, 190] / 255;
            % pastel.dark_gray = [180, 180, 180] / 255;
            % pastel.dark_yellow = [216, 214, 123] / 255;
            % pastel.dark_cyan = [134, 199, 186] / 255;
            colors = pastel;
        end

        function show_data(fname, data, cols)
            t = data(2:end,1).Variables;
        
            figure()
            subplot(4,1,1)
            y = data(2:end,2).Variables;
            plot(t, y, 'Color', cols(1,:)); 
            box off; ylabel('USD'); grid on; set(gca, 'GridLineStyle', ':');
            title('BakerHughes closure price')
            
        
            subplot(4,1,2)
            y = data(2:end,3).Variables;
            plot(t, y, 'Color', cols(2,:)); 
            box off; ylabel('USD'); grid on; set(gca, 'GridLineStyle', ':');
            title('BlackRock closure price')
        
            subplot(4,1,3)
            y = data(2:end,4).Variables;
            plot(t, y, 'Color', cols(3,:)); 
            box off; ylabel('USD'); grid on; set(gca, 'GridLineStyle', ':');
            title('Nvidia closure price')
        
            subplot(4,1,4)
            y = data(2:end,5).Variables;
            plot(t, y, 'Color', cols(4,:)); 
            box off; ylabel('USD'); grid on; set(gca, 'GridLineStyle', ':');
            xlabel('year')
            title('Pfizer closure price')

            % Ottieni gli handle degli assi
            ax = findobj(gcf, 'type', 'axes');
            % Regola la posizione dei subplot
            for i = 1:length(ax)
                pos = ax(i).Position;
                pos(4) = 0.85 * pos(4);
                ax(i).Position = pos;
            end
        
            savepdf(gcf, fname, [16,16])
        end
    
        function c_range = find_common_range(values)
            c_range = [min(values); max(values)];
            c_range = [min(c_range(1,:)) max(c_range(2,:))];
            c_range = ceil(max(abs(c_range))*100)/100*[-1,1];
        end
        
        function show_logReturns(fname, data, lr_range, cols)
            t = data(2:end,1).Variables;
        
            figure()
            subplot(4,1,1)
            y = data(2:end,2).Variables;
            plot(t(1:end-1), diff(log(y)), 'Color', cols(1,:)); 
            box off; ylabel('x_1(t)'); grid on; set(gca, 'GridLineStyle', ':');
            ylim(lr_range); set(gca, 'YTick', -0.2:0.1:0.2);
            title('BakerHughes log-returns')
            
            subplot(4,1,2)
            y = data(2:end,3).Variables;
            plot(t(1:end-1), diff(log(y)), 'Color', cols(2,:)); 
            box off; ylabel('x_2(t)'); grid on;  set(gca, 'GridLineStyle', ':');
            ylim(lr_range); set(gca, 'YTick', -0.2:0.1:0.2);
            title('BlackRock log-returns')
        
            subplot(4,1,3)
            y = data(2:end,4).Variables;
            plot(t(1:end-1), diff(log(y)), 'Color', cols(3,:)); 
            box off; ylabel('x_3(t)'); grid on;  set(gca, 'GridLineStyle', ':');
            ylim(lr_range); set(gca, 'YTick', -0.2:0.1:0.2);
            title('Nvidia log-returns')
        
            subplot(4,1,4)
            y = data(2:end,5).Variables;
            plot(t(1:end-1), diff(log(y)), 'Color', cols(4,:)); 
            box off; ylabel('x_4(t)'); grid on;  set(gca, 'GridLineStyle', ':');
            ylim(lr_range); xlabel('year'); set(gca, 'YTick', -0.2:0.1:0.2);
            title('Pfizer log-returns')
        
            % Ottieni gli handle degli assi
            ax = findobj(gcf, 'type', 'axes');
            % Regola la posizione dei subplot
            for i = 1:length(ax)
                pos = ax(i).Position;
                pos(4) = 0.85 * pos(4);
                ax(i).Position = pos;
            end

            savepdf(gcf, fname, [16,16])
        end

        function show_asset(fname, data, col, asset, lr_range)
            t = data(2:end,1).Variables;
            
            y = data(2:end,col).Variables;
            figure,
            subplot(2,1,1)
            plot(t, y); box off; grid on; set(gca, 'GridLineStyle', ':');
            title([asset ' closure price'])
            xlabel('year'); ylabel('USD')
        
            x = diff(log(y));
            subplot(2,1,2) 
            plot(t(1:end-1), x); box off;  grid on; ylim(lr_range)
            title([asset ' log returns'])
            xlabel('year'); ylabel('x(t)')

            savepdf(gcf, fname, NaN)
        end

        function show_logret(fname, x, asset, lr_range, col)
            figure
            plot(1:length(x), x, 'Color', col); box off;  
            grid on; set(gca, 'GridLineStyle', ':'); ylim(lr_range)
            title([asset ' log returns']); set(gca, 'YTick', -0.2:0.1:0.2);
            xlabel('index'); ylabel('x(t)');
            set(gca, 'FontSize', 6);

            savepdf(gcf, fname, [64,8])
        end

        function show_pdf_var_es(fname, x, assetlabel, fit_d, VaR, ES, facecol, edgecol)
            pastel = common.get_pastel_colors();
            
            figure
            histogram(x,-0.305:0.01:0.305, 'Normalization', 'pdf', 'EdgeColor', edgecol, 'FaceColor', facecol); 
            title(assetlabel); ylabel('pdf'); xlabel('loss')
            hold on
            if ~isempty(fit_d)
                %[pdf_vals, x_vals] = ksdensity(x);
                %plot(x_vals, pdf_vals, '-', 'LineWidth', 1.5, 'Color', pastel.Gray, 'DisplayName', 'kde');
            %else
                x_vals = min(x) : (max(x)-min(x))/400 : max(x);
                pdf_vals = pdf(fit_d, x_vals);
                plot(x_vals, pdf_vals, '-', 'LineWidth', 1.5, 'Color', pastel.Gray, 'DisplayName', fit_d.DistributionName);
            end
            xlim([-0.25,0.25]); ylim([0,33]); 
            xline(VaR, '-',  'LineWidth', 1, 'Color', pastel.DarkBlue, 'DisplayName', 'VaR');
            xline(ES,  '--', 'LineWidth', 1, 'Color', pastel.DarkBlue, 'DisplayName', 'ES'); 
            
            h_var = findobj(gcf, 'DisplayName', 'VaR');
            h_es = findobj(gcf, 'DisplayName', 'ES');
            if isempty(fit_d)
                %h_pdf = findobj(gcf, 'DisplayName', 'kde');
                legend([h_var, h_es]);
            else
                h_pdf = findobj(gcf, 'DisplayName', fit_d.DistributionName);
                legend([h_pdf, h_var, h_es]);
            end
            

            common.set_white_hgrid(gca)
            
            ax.Box = 'off';
            ax.Layer = 'top';

            h_legend = findobj(gcf, 'Type', 'legend');
            for i = 1:numel(h_legend)
                h_legend(i).Box = 'off';
            end

        
            savepdf(gcf, fname, [16,10])
        end

        function set_white_hgrid(ax)
            grid on;
            %ax = gca;
            ax.XGrid = 'off'; 
            ax.GridColor = 'white';
            ax.GridLineStyle = "-";
            ax.GridLineWidthMode = "manual";
            ax.GridLineWidth = 0.1;
            ax.GridColorMode = "auto";
            ax.GridAlpha = 1;
            
            ax.Box = 'off';
            ax.Layer = 'top';
        end

        function show_distr_tail(fname, x, assetlabel, fit_n, fit_t, VaR, facecol, edgecol)
            pastel = common.get_pastel_colors();
            
            figure;
            %[pdf_vals, x_vals] = ksdensity(x);
            %plot(x_vals, pdf_vals, '-', 'LineWidth', 1.5, 'Color', edgecol, 'DisplayName', 'kde'); hold on
            plot(x, -0.25+0*x, '+', 'MarkerSize',2,'Color',edgecol); hold on
            histogram(x,-0.305:0.01:0.305, 'Normalization', 'pdf', 'EdgeColor', 'white', 'FaceColor', facecol, 'FaceAlpha', 0.66); 
            title(assetlabel); ylabel('pdf'); xlabel('loss'); hold on
            xlim([0,0.3]); ylim([-0.5,5.25]); 
            
            grid on; set(gca, 'GridLineStyle', '--');

            x_vals = 0:0.0025:0.3; %min(x) : (max(x)-min(x))/400 : max(x);
            pdf_n = pdf(fit_n, x_vals);
            pdf_t = pdf(fit_t, x_vals);
            plot(x_vals, pdf_n, '-', 'LineWidth', 1.5, 'Color', pastel.LightGray, 'DisplayName', fit_n.DistributionName);
            plot(x_vals, pdf_t, '-', 'LineWidth', 1.5, 'Color', 'black',          'DisplayName', fit_t.DistributionName);

            xline(VaR(1), '-', 'LineWidth', 1, 'Color', pastel.LightGray, 'DisplayName', ['VaR ' fit_n.DistributionName]);
            xline(VaR(2), '-', 'LineWidth', 1, 'Color', 'black',          'DisplayName', ['VaR ' fit_t.DistributionName]);
            xline(VaR(3), ':', 'LineWidth', 1, 'Color', pastel.DarkGray,  'DisplayName', 'VaR Historical');


            h_n  = findobj(gcf, 'DisplayName', fit_n.DistributionName);
            h_t  = findobj(gcf, 'DisplayName', fit_t.DistributionName);
            h_vn = findobj(gcf, 'DisplayName', ['VaR ' fit_n.DistributionName]);
            h_vt = findobj(gcf, 'DisplayName', ['VaR ' fit_t.DistributionName]);
            h_vh = findobj(gcf, 'DisplayName', 'VaR Historical');

            legend([h_n, h_t, h_vn, h_vt, h_vh]);
            h_legend = findobj(gcf, 'Type', 'legend');
            for i = 1:numel(h_legend)
                h_legend(i).Box = 'off';
            end
        
            savepdf(gcf, fname, [15,16])

        end

        function compare_varcov_distr(fname, risks, distr, assets, tit, cols, ylimits)
            pastel = common.get_pastel_colors();
            figure
            
            minr=min(risks(:));
            maxr=max(risks(:));
            rrange = [minr, maxr]; dr=(maxr-minr)/50;
            plot(rrange,rrange,'--', 'LineWidth',1, 'Color', pastel.LightGray); hold on;
            xlabel(distr(1)); ylabel(distr(2));
            title(tit);
            for a=1:numel(assets)
                plot(risks(a,1), risks(a,2), '.', 'Color', cols(a,:), 'MarkerSize',16); 
                text(risks(a,1)+dr, risks(a,2), assets{a}, AffectAutoLimits="on")
            end
            box off; grid on; set(gca, 'GridLineStyle', ':'); axis("square"); 
            if ~isempty(ylimits), ylim(ylimits); end
            savepdf(gcf, fname, NaN)
        end

        function compare_risk_methods(fname, risks, methods, assets, cols, titlabel)
            figure;
            m = 1:length(methods); dr=0.1;
            for a=1:length(assets)
                plot(m, risks(a,1:end), 'o-', 'Color', cols(a,:), 'LineWidth', 1); hold on
                text(m(end)+dr, risks(a,end), assets{a}, AffectAutoLimits="on");
            end
            xlim([0.5,length(methods)+0.5])
            title(titlabel)
            xticks(m); xticklabels(methods); box off; grid on; set(gca, 'GridLineStyle', ':');
            xlabel('method')

            savepdf(gcf, fname, NaN);
        end

        function show_qqplot(fname, x, fit_d, qrange)
            figure, 
            h = qqplot(x, fit_d); grid on; set(gca, 'GridLineStyle', ':');
            axis('equal'); xlim(qrange); ylim(qrange); 
            h_points = h(1); 
            %h_points.Marker = 'o';
            h_points.MarkerSize = 2;
            savepdf(gcf, fname, NaN)
        end

        function show_scatterplot(fname, U)
            figure; plotmatrix(U);
            %axis("square")
            %ax = gca; ax.Box = 'off';
            savepdf(gcf, fname, [16,10])
        end

% Stationarity ------------------------------------------------------------
        
        function statio = check_stationarity(fname, x, assetlabel)
            %h_adf = adftest(x(:,1),Model="TS",Lags=0:2);
        
            % The result h = 0 indicates that this test fails to reject 
            % the null hypothesis of a unit root against the autoregressive alternative.
            %h_adf = adftest(x) == 1;
            [h_adf,pValue_adf,stat_adf,cValue_adf] = adftest(x);
        
            % h = 1 suggests that the series is unit root nonstationary
            %h_kpss = kpsstest(x) == 0;
            [h_kpss,pValue_kpss,stat_kpss,cValue_kpss] = kpsstest(x);
        
            statio = [h_adf,pValue_adf,stat_adf,cValue_adf; ...
                      h_kpss,pValue_kpss,stat_kpss,cValue_kpss];
        
            figure; 
            subplot(2,1,1)
            [acf, lags, bounds, h_acf] = autocorr(x); 
            title(assetlabel); ylabel('ACF'); box off; set(gca, 'GridLineStyle', ':');
            xlim([0.01, 20.5]); ylim([-0.25,0.25])
            subplot(2,1,2); 
            [pacf, lags, bounds, h_pacf] = parcorr(x); 
            title(' '); ylabel('PACF'); box off; set(gca, 'GridLineStyle', ':');
            xlim([0.01, 20.5]); ylim([-0.25,0.25])
            
            h_acf(1).MarkerSize  = 3;
            h_pacf(1).MarkerSize = 3;

            h_legend = findobj(gcf, 'Type', 'legend');
            for i = 1:numel(h_legend)
                h_legend(i).Box = 'off';
            end
        
            savepdf(gcf, fname, [16,9])
        end

% "Parametric method" ---------------------------------------------------

        function [VaR, ES] = parametric_risk_measures(loss, alpha, dtype)
            if dtype == ExamDistr.gauss
                [VaR, ES] = common.normal_risk_measures(loss, alpha);
            else 
                [VaR, ES] = common.t_risk_measures(loss, alpha);
            end
        end
        
        
        function [VaR, ES] = normal_risk_measures(loss, alpha)
        
            fit_n=fitdist(loss,'Normal');
        
            z_score = norminv(alpha);
            VaR = fit_n.mu + fit_n.sigma*z_score;
        
            phi_value = normpdf(z_score);
            ES = fit_n.mu + fit_n.std * phi_value / (1 - alpha);
        end
        
        
        function [VaR, ES] = t_risk_measures(loss, alpha)
        
            fit_t=fitdist(loss,'tlocationscale');
        
            if (fit_t.nu<=1)
                VaR = NaN; ES=NaN;
            else
                t_val = tinv(alpha, fit_t.nu);
                VaR = fit_t.mu + fit_t.sigma * t_val;
            
                pdf_val = tpdf(t_val, fit_t.nu);
                es_std = (pdf_val / (1 - alpha)) * ((fit_t.nu + t_val^2) / (fit_t.nu - 1));
                ES = fit_t.mu + fit_t.sigma * es_std;
            end   
        end

% Historical Method -------------------------------------------------------

        function [VaR, ES] = historical_risk_measures(loss, alpha)
        
            % n = length(loss);
            % sorted_loss = sort(loss);
            % var_idx = ceil(n*alpha);
            % VaR = sorted_loss(var_idx);
            VaR = quantile(loss,alpha);
        
            idES = loss>VaR;
            ES = mean(loss(idES));
        
            % exc=sum(loss>VaR);
            % AA=(loss>VaR);
            % ES_hist_emp=sum(loss.*AA)/exc
        end

% Monte Carlo univariato --------------------------------------------------

        function [VaR, ES, rangeVaR, rangeES] = montecarlo_risk_measures(fname, loss, alpha, mc_syms, dtype, assetlabel, facecol)
        
            if dtype == ExamDistr.gauss
                fit_d = fitdist(loss,'Normal');
            else 
                fit_d = fitdist(loss,'tlocationscale');
            end
        
            n = length(loss);
            
            VaRs = zeros(mc_syms,1); ESs=zeros(mc_syms,1);
            for k=1:mc_syms
                sample_loss = random(fit_d, n, 1);
                [VaR_k, ES_k] = common.historical_risk_measures(sample_loss, alpha);
                VaRs(k) = VaR_k;
                ESs(k) = ES_k;
            end
        
            VaR = mean(VaRs);
            % 95% emp. conf. int.
            RCI_VaR_MC=quantile(VaRs,0.975); 
            LCI_VaR_MC=quantile(VaRs,0.025);
            rangeVaR = [LCI_VaR_MC, RCI_VaR_MC];
        
            ES = mean(ESs);
            % 95% emp. conf. int.
            RCI_ES_MC= quantile(ESs,0.975);
            LCI_ES_MC= quantile(ESs,0.025);
            rangeES = [LCI_ES_MC, RCI_ES_MC];
        
            figure;
            %subplot(1,2,1)
            %ksdensity(VaRs)
            histogram(VaRs, 'Normalization', 'pdf', 'EdgeColor', facecol, 'FaceColor', facecol); hold on
            title([assetlabel ' M.C. VaR']); ylabel('pdf'); xlabel('Value at Risk')
            common.set_white_hgrid(gca)
            xline(VaR, '-k', 'LineWidth', 1);
            xline(rangeVaR(1), '--k', 'LineWidth', 1);
            xline(rangeVaR(2), '--k', 'LineWidth', 1);
            xlim([0.02,0.09]); ylim([0,300]);
            %
            savepdf(gcf, [fname '_VaR'], [16,10])

            figure;
            %subplot(1,2,2)
            %ksdensity(ESs)
            histogram(ESs, 'Normalization', 'pdf', 'EdgeColor', facecol, 'FaceColor', facecol); hold on
            title([assetlabel ' M.C. ES']); ylabel('pdf'); xlabel('Expected Shortfall')
            common.set_white_hgrid(gca)
            xline(ES, '-k', 'LineWidth', 1);
            xline(rangeES(1), '--k', 'LineWidth', 1);
            xline(rangeES(2), '--k', 'LineWidth', 1);
            xlim([0.02,0.09]); ylim([0,300]);
            %
            savepdf(gcf, [fname '_ES'], [16,10])
        end

    end
end



function savepdf(fig, fname, papersize)

    if ~isnan(papersize) 
        dpi = get(0, 'ScreenPixelsPerInch');
        cm_width = papersize(1);
        cm_height = papersize(2);

        % Converti i pixel in centimetri
        fig.Position(3)  = cm_width * (dpi / 2.54);
        fig.Position(4)  = cm_height * (dpi / 2.54);
    end

    fig.PaperPositionMode = 'auto';    
    exportgraphics(fig, ['.\Figure\' fname '.pdf'], 'ContentType', 'vector');
end


