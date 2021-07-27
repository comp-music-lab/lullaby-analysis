function experiment_sinwaves
    %%
    N = 2048;
    M = 3/4*N;
    al = 0.95;
    xi_db = 30;
    initialization = 'min';
    
    be = 0.0;
    delta = Inf;
    
    snr_max = 70;
    
    %%
    J = 1000;
    sgm_hist = zeros(J, 4);
    
    parfor j=1:J
        %%
        [x, fs] = h_testsignal;
        snr_db = rand * snr_max;
        P_x = sum(x.^2)/length(x);
        sgm = P_x/(10^(snr_db/10));
        n = normrnd(0, sqrt(sgm), [length(x) 1]);
        y = x + n;
        snr_true = sum(x.^2)/sum(n.^2);
        
        %%
        sgm_N = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization);
        
        %%
        P_n = sum(sgm_N, 1)./N;
        sgm_est = median(P_n);

        snr_est = (sum(y.^2)/length(y))/sgm_est - 1;
        
        %%
        sgm_hist(j, :) = [sgm, sgm_est, 10*log10(snr_true), 10*log10(snr_est)];
    end
    
    %%
    figure(6);
    sgm_diff = sgm_hist(:, 1) - sgm_hist(:, 2);
    subplot(2, 1, 1);
    scatter(sgm_hist(:, 1), sgm_diff, 2);
    title(sprintf('Average diff. = %3.3f', mean(abs(sgm_diff))), 'interpreter', 'none');
    
    snr_diff = sgm_hist(:, 3) - sgm_hist(:, 4);
    subplot(2, 1, 2);
    scatter(sgm_hist(:, 3), snr_diff, 2);
    title(sprintf('Average diff. = %3.3f', mean(abs(snr_diff))), 'interpreter', 'none');
    
    %% demo
    [x, fs] = h_testsignal;
    snr_db = rand * snr_max;
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
    sgm_med = median(P_n);
    
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

function [x, fs] = h_testsignal
    fs = 16000;
    K_x = randi(10);
    x = 0;
    t_x = (0:(fs/2 - 1))./fs;
    freqset = max(100, rand*300);
    freqset = 2.^((0:16)./12) .* freqset;
    amp = max(0.1, rand(15, K_x));
    amp = [zeros(1, K_x); amp; zeros(1, K_x)];
    
    w = ones(1, length(t_x));
    w(1:800) = 1./(1 + exp(-linspace(-12, 12, 800)));
    w(end - 799:end) = 1./(1 + exp(-linspace(12, -12, 800)));
    w(1) = 0;
    
    for k=1:K_x
        x_k = [];
        
        for i=1:17
            x_i = amp(i, k).*sin(2*pi*k*freqset(i).*t_x);
            x_i = x_i .* w;
            x_k = [x_k; x_i']; %#ok<AGROW>
        end
        
        x = x + x_k;
    end
end