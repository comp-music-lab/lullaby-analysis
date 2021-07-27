function experiment_vocalset
    %%
    N = 8192;
    M = 3/4*N;
    al = 0.95;
    xi_db = 15;
    initialization = 'min';
    
    be = 0.0;
    delta = Inf;
    
    snr_max = 70;
    
    %%
    audiodir = './data/';
    song_list = {...
        'f9_caro_straight', 'm4_dona_straight', 'f6_long_messa_e', 'm3_arpeggios_breathy_u',...
        'f2_row_straight', 'm1_caro_vibrato', 'f8_long_trill_i', 'm6_scales_c_slow_piano_e',...
        'f5_scales_f_slow_forte_o', 'm8_arpeggios_vocal_fry_a'...
        };
    
    %% demo - zero noise
    J = 1024;
    snr_ub = zeros(length(song_list), 1);
    
    for i=1:length(song_list)
        %%
        [x, fs] = audioread(strcat(audiodir, song_list{i}, '.wav'));
        sgm = 0;
        snr_true = Inf;
        y = x;
        
        sgm_j = 0;
        parfor j=1:J
            %%
            [sgm_N, ~, ~, ~, ~, ~] = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);
            
            %%
            P_n = sum(sgm_N, 1)./N;
            sgm_est = median(P_n(ceil(size(sgm_N, 2)/2):end));
            
            %%
            sgm_j = sgm_j + sgm_est;
        end
        
        sgm_est = sgm_j/J;
        snr_est = (sum(y.^2)/length(y))/sgm_est - 1;
        snr_ub(i) = 10*log10(snr_est);

        fprintf('%e, %e | %3.3f, %3.3f\n', sgm, sgm_est, 10*log10(snr_true), 10*log10(snr_est));
        
        %%
        [sgm_N, snr_post, ~, Y, ~, T] = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);
        
        figure(1);
        h_plotpow(y, sgm_N, snr_post, Y, T, N, M, fs);

        drawnow;
        pause(0.5);
    end
    
    %%
    J = 512;
    sgm_hist = zeros(J*length(song_list), 5);
    
    for i=1:length(song_list)
        %%
        tic;
        fprintf('%d / %d ... ', i, length(song_list));
            
        %%
        [x, fs] = audioread(strcat(audiodir, song_list{i}, '.wav'));
        
        P_x = sum(x.^2)/length(x);
        snr_db = rand(J, 1) .* snr_max;
        sgm = P_x./(10.^(snr_db./10));
        
        sgm_hist_i = zeros(J, 4);
        
        %%
        parfor j=1:J
            %%
            n = normrnd(0, sqrt(sgm(j)), [length(x) 1]);
            y = x + n;
            snr_true = sum(x.^2)/sum(n.^2);

            %%
            sgm_N = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);

            %%
            P_n = sum(sgm_N, 1)./N;
            sgm_est = median(P_n(ceil(length(T)/2):end));

            snr_est = (sum(y.^2)/length(y))/sgm_est - 1;

            %%
            sgm_hist_i(j, :) = [sgm(j), sgm_est, 10*log10(snr_true), 10*log10(snr_est)];
        end
        
        idx_start = 1 + (i - 1)*J;
        idx_end = i*J;
        sgm_hist(idx_start:idx_end, 1:4) = sgm_hist_i;
        sgm_hist(idx_start:idx_end, 5) = i;
        
        %%
        t = toc;
        fprintf('%3.3f sec. elapsed\n', t);
    end
    
    %%
    figure(6);
    gscatter(sgm_hist(:, 3), sgm_hist(:, 4), sgm_hist(:, 5)); hold on;
    idx = repmat((1:10), [2 1]);
    snr_plot = [snr_ub snr_ub]';
    h = gscatter(repmat([1 snr_max], [1 length(song_list)]), snr_plot(:), idx(:));
    set(h, 'LineStyle', '-.');
    set(h, 'Marker', 'none');
    hold off;
    xlabel('Ground-truth SNR');
    ylabel('Estimated SNR');
    title('Simulation with the VocalSet dataset');
    legend(song_list, 'Interpreter', 'none');
    xlim([0 snr_max*1.05]);
    ylim([0 snr_max*1.05]);
    
    f = figure(6);
    f.Position = [100 100 550 550];
    saveas(f, './figure/vocalset.png');
    
    %% demo
    [x, fs] = audioread(strcat(audiodir, song_list{randi(length(song_list))}, '.wav'));
    snr_db = rand*snr_max;
    P_x = sum(x.^2)/length(x);
    sgm = P_x/(10^(snr_db/10));
    n = normrnd(0, sqrt(sgm), [length(x) 1]);
    y = x + n;
    snr_true = sum(x.^2)/sum(n.^2);
    assert(abs(sum(n.^2)/length(n) - sgm)/sgm < 1e-2, 'Check noise power');

    %%
    [sgm_N, snr_post, p_H1_hist, Y, F, T] = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);

    %%
    P_n = sum(sgm_N, 1)./N;
    sgm_est = median(P_n);

    snr_est = (sum(y.^2)/length(y))/sgm_est - 1;
    fprintf('%e, %e | %3.3f, %3.3f\n', sgm, sgm_est, 10*log10(snr_true), 10*log10(snr_est));

    %%
    figure(1);
    h_plotpow(y, sgm_N, snr_post, Y, T, N, M, fs);

    drawnow;
    
    %%
    figure(2);
    subplot(2, 1, 1);
    surf(T, F, 10.*log10(Y), 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    ylim([F(1) 5000]);
    colorbar;
    
    subplot(2, 1, 2);
    surf(T, F, 10.*log10(Y), 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    colorbar;
    
    figure(3);
    subplot(2, 1, 1);
    surf(T, F, 10.*log10(snr_post), 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    ylim([F(1) 5000]);
    colorbar;
    
    subplot(2, 1, 2);
    surf(T, F, 10.*log10(snr_post), 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    colorbar;
    
    figure(4);
    subplot(2, 1, 1);
    surf(T, F, p_H1_hist, 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    ylim([F(1) 5000]);
    colorbar;
    
    subplot(2, 1, 2);
    surf(T, F, p_H1_hist, 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    colorbar;
    
    figure(5);
    subplot(2, 1, 1);
    surf(T, F, sgm_N, 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    ylim([F(1) 5000]);
    colorbar;
    
    subplot(2, 1, 2);
    surf(T, F, sgm_N, 'edgecolor', 'none');
    view(0, 90);
    axis tight;
    colorbar;
end

function h_plotpow(y, sgm_N, snr_post, Y, T, N, M, fs)
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
    subplot(2, 1, 1);
    plot(T, 10.*log10(P_y)); hold on;
    plot(T, 10.*log10(P_Y), '-.m');
    plot(T, 10.*log10(P_n), '--g');
    plot([T(1) T(end)], 10.*log10([sgm_med sgm_med]), '--c'); hold off;
    
    %%
    snr_pri = snr_post - 1;
    snr_pri(snr_pri < 0) = 0;
    subplot(2, 1, 2);
    plot(T, 10.*log10(mean(snr_pri, 1)));
    
    %subplot(3, 1, 3);
    %histogram(y.^2, 500, 'edgecolor', 'none', 'Normalization', 'pdf');
end