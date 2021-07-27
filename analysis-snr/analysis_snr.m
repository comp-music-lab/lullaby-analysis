function analysis_snr
    %%
    J = 1024;
    
    N = 8192;
    M = 3/4*N;
    al = 0.95;
    xi_db = 15;
    initialization = 'min';
    
    be = 0.0;
    delta = Inf;
    
    samplefig = true;
    
    %%
    audiodir = './data/';
    song_list = {...
        'NAIV-009', 'NAIV-018', 'NAIV-021', 'NAIV-023', 'NAIV-026', 'NAIV-043', 'NAIV-078',...
        'NAIV-081', 'NAIV-093', 'NAIV-094', 'NAIV-095', 'NAIV-097', 'NAIV-099', 'NAIV-101',...
        'NAIV-104', 'NAIV-111'
        };
    song_group = [1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1];
    pair_id = [2, 4, 1, 8, 1, 8, 3, 6, 4, 7, 7, 2, 3, 5, 5, 6];
    
    %% Estimate SNR
    diary IPL_snr.txt
    fprintf('\n%s\n', datestr(datetime));
    snr_group = zeros(length(song_list), 3);
    
    for i=1:length(song_list)
        %%
        [x, fs] = audioread(strcat(audiodir, song_list{i}, '.wav'));
        y = x;
        
        %% Initialization is randomly done so taking average of the results as final estimate
        sgm_N = 0;
        
        parfor j=1:J
            sgm_j = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);
            sgm_N = sgm_N + sgm_j;
        end
        
        sgm_N = sgm_N./J;

        %%
        P_n = sum(sgm_N, 1)./N;
        sgm_est = median(P_n(ceil(size(sgm_N, 2)/2):end));
        snr_est = (sum(y.^2)/length(y))/sgm_est - 1;
        
        fprintf('%s: %e | %3.3f\n', song_list{i}, sgm_est, 10*log10(snr_est));
        
        snr_group(i, :) = [10*log10(snr_est) song_group(i) pair_id(i)];
        
        if samplefig
            %%
            [~, snr_post, p_H1_hist, Y, F, T] = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);

            figure(1);
            h_plotpow(y, sgm_N, Y, T, N, M, fs);

            figure(2);
            h_plotsnr(snr_post, T);
            
            %%
            figure(3);
            subplot(2, 1, 1);
            surf(T, F, 10.*log10(Y), 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            ylim([F(1) 5000]);
            colorbar;
            title('Spectrogram of signal');

            subplot(2, 1, 2);
            surf(T, F, 10.*log10(Y), 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            colorbar;

            figure(4);
            subplot(2, 1, 1);
            surf(T, F, 10.*log10(snr_post), 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            ylim([F(1) 5000]);
            colorbar;
            title('Distribution of a posterior SNR');

            subplot(2, 1, 2);
            surf(T, F, 10.*log10(snr_post), 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            colorbar;

            figure(5);
            subplot(2, 1, 1);
            surf(T, F, p_H1_hist, 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            ylim([F(1) 5000]);
            colorbar;
            title('Posterior probability of voicing detection');

            subplot(2, 1, 2);
            surf(T, F, p_H1_hist, 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            colorbar;

            figure(6);
            subplot(2, 1, 1);
            surf(T, F, sgm_N, 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            ylim([F(1) 5000]);
            colorbar;
            title('Spectrogram of estimated noise');

            subplot(2, 1, 2);
            surf(T, F, sgm_N, 'edgecolor', 'none');
            view(0, 90);
            axis tight;
            colorbar;

            %%
            drawnow;
            
            f = figure(1);
            f.Position = [100 100 540 400];
            saveas(f, strcat('./figure/', song_list{i}, '.png'));
            
            pause(0.5);
        end
    end
    
    diary off
    
    %%
    figure(7);
    h = gscatter(snr_group(:, 2), snr_group(:, 1), snr_group(:, 3));
    set(h, 'LineStyle', ':');
    title('Comparison of SNR between song pairs');
end

function h_plotsnr(snr_post, T)
    %%
    snr_pri = snr_post - 1;
    snr_pri(snr_pri < 0) = 0;
    plot(T, 10.*log10(mean(snr_pri, 1)));
    title('Estimated a priori SNR per frame');    
end

function h_plotpow(y, sgm_N, Y, T, N, M, fs)
    %%
    P_y = zeros(length(T), 1);
    
    for i=1:length(T)
        n_start = 1 + (i - 1)*(N - M);
        n_end = n_start + N - 1;
        assert(abs(T(i)*fs - (n_start + n_end - 1)/2) < 1e-8, '');
        
        y_i = y(n_start:n_end);
        P_y(i) = sum(y_i.^2)/N;
    end
    
    %%
    P_Y = sum(Y, 1)./N;
    
    %%
    P_n = sum(sgm_N, 1)./N;
    sgm_med = median(P_n(ceil(T/2):end));
    
    %%
    plot(T, 10.*log10(P_y)); hold on;
    plot(T, 10.*log10(P_Y), '-.m');
    plot(T, 10.*log10(P_n), '--g');
    plot([T(1) T(end)], 10.*log10([sgm_med sgm_med]), '--c'); hold off;
    xlabel('Time (sec)');
    ylabel('Power (dB)');
    legend('Power of signal', 'Power of signal (winodwed)', 'Estimated power of noise', 'Estimated power of nosie (median)');
    title('Signal power and estimated noise power');
    ylim([-Inf, 0]);
end