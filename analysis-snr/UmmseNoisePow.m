function [sgm_N, snr_post, p_H1_hist, Y, F, T] = UmmseNoisePow(y, fs, N, M, al, be, delta, xi_db, initialization)
    %% Model assumptions
    % Signal and noise are uncorrelated/independent.
    % Noise is zero mean Gaussian and variance is constant over time.
    % Power of signal is Gaussian. 
    % a priori SNR is constant.
    
    %% Parameters
    L = floor((length(y) - N)/(N - M)) + 1;
    w = hft90d(N);
    %w = hanning(N, 'periodic');
    p_H0_pri = 0.5;
    
    %% STFT
    ACF = sum(w)/N;
    ENBWCF = (sum(w.^2)/N)/(ACF^2);
    [Y, F, T] = spectrogram(y./ACF, w, M, N, fs);
    
    Y(2:(N/2), :) = Y(2:(N/2), :) .* sqrt(2);
    Y = Y./sqrt(N * ENBWCF);
    % figure(1); hist3([real(Y(:)) imag(Y(:))], [25 25], 'CdataMode', 'auto'); view(2);
    Y = Y.*conj(Y);
    
    %{
    Y = Y.*conj(Y);
    Y = Y./(N * ENBWCF);
    Y(2:(N/2), :) = Y(2:(N/2), :) .* 2;
    %}
    
    %{
    error_pow = zeros(length(T), 3);
    for i=1:length(T)
        n_start = 1 + (i - 1)*(N - M);
        n_end = n_start + N - 1;
        assert(abs(T(i)*fs - (n_start + n_end - 1)/2) < 1e-8, '');
        
        y_i = y(n_start:n_end);
        
        e_y = sum(y_i.^2)/N;
        e_Y = sum(Y(:, i))/N;
        e_yw = sum((y_i.*w./ACF).^2)/(ENBWCF*N);
        
        error_pow(i, 1) = e_y - e_Y;
        error_pow(i, 2) = e_yw - e_Y;
        error_pow(i, 3) = e_Y/e_y;
    end
    %}
    
    %% Main process
    y = y(:);
    
    sgm_N = zeros(N/2 + 1, L);
    assert(all(size(Y) == size(sgm_N)), 'Spectrogram matrix size is inconsistent');
    
    if strcmp('bartlett', initialization)
        sgm_N(:, 1) = bartlettpow(y, N);
    elseif strcmp('min', initialization)
        sgm_N(:, 1) = minpow(y, N);
    end
    
    snr_post = sgm_N .* 0;
    p_H1_hist = sgm_N .* 0;
    
    xi = 10^(xi_db/10);
    
    s_H1 = 1 - p_H0_pri;
    odds_prior = p_H0_pri/(1 - p_H0_pri);
    
    for l=2:L
        %% Voicing probability estimate
        lik_ratio = (1 + xi) .* exp(-Y(:, l)./sgm_N(:, l - 1) * xi/(1 + xi));
        p_H1_pos = 1./(odds_prior .* lik_ratio + 1);
        
        %% Avoiding stagnation 
        s_H1 = be.*s_H1 + (1 - be).*p_H1_pos;
        p_H1_pos(s_H1 > delta) = min(delta, p_H1_pos(s_H1 > delta));
        
        %% Posterior for unvoicing
        p_H0_pos = 1 - p_H1_pos;
        
        %% Noise estimate
        E_N = p_H0_pos.*Y(:, l) + p_H1_pos.*sgm_N(:, l - 1);
        
        %% Exponential moving average filter
        sgm_N(:, l) = al.*sgm_N(:, l - 1) + (1 - al).*E_N;
        
        %% Compute a posterior SNR
        snr_post(:, l) = Y(:, l)./sgm_N(:, l - 1);
    
        %% Historical P_H1
        p_H1_hist(:, l) = p_H1_pos;
    end
    
    p_H1_hist(:, 1) = 1 - p_H0_pri;
    snr_post(:, 1) = NaN;
end

function w = hft90d(N)
    j = (0:(N - 1))';
    z = 2*pi*j/N;
    w = 1 - 1.942604.*cos(z) + 1.340318.*cos(2.*z) - 0.440811.*cos(3.*z) + 0.043097.*cos(4.*z);
end

function Y = minpow(y, N)
    L = length(y);
    
    J = 1000;
    K = 32;
    P = zeros(J, 1);
    
    for j=1:J
        i = randi(L - N);
        P(j) = sum(y(i:(i + N - 1)).^2)/N;
    end
    
    P = sort(P, 'asc');
    sgm = mean(P(1:K));
    
    noise = normrnd(0, sqrt(sgm), [N 1]);
    Y = fft(noise);
    Y = Y.*conj(Y)./N;
    Y = Y(1:(N/2 + 1));
    Y(2:N/2) = Y(2:N/2) .* 2;
end

function Y = bartlettpow(y, N)
    Y = zeros(N, 1);
    L = length(y);
    flag = zeros(L, 1);
    portion = 0;
    K = 0;
    
    while portion <= 0.5
        i = randi(L - N);
        flag_i = flag(i:(i + N - 1));
        
        if sum(flag_i) == 0
            Y_i = fft(y(i:(i + N - 1)));
            Y_i = Y_i.*conj(Y_i);
            Y = Y + Y_i./N;
            
            flag(i:(i + N - 1)) = 1;
            K = K + 1;
            
            portion = sum(flag)/L;
        end
    end
    
    Y = Y./K;
    Y = Y(1:(N/2 + 1));
    Y(2:N/2) = Y(2:N/2) .* 2;
end

%{
Y_r = rand - 0.5;
Y_i = rand - 0.5;
Y = Y_r + Y_i*1i;
lmd = 3*rand;
xi = 35*rand;

f = @(x, lmd) 1/(pi*lmd) * exp(-abs(x)^2/lmd);

p_1 = f(Y, lmd);
p_2 = f(Y, lmd*(1 + xi));
r_1 = p_1/p_2;

r_2 = (1 + xi)*exp(-abs(Y)^2/lmd*(xi/(1 + xi)));

assert(abs(r_1 - r_2) < 1e-10, 'Check likelihood ratio');
disp([r_1 r_2]);
%}

%{
Y_r = rand - 0.5;
Y_i = rand - 0.5;
Y = Y_r + Y_i*1i;
lmd = 3*rand;
xi = 35*rand;
p_H0 = rand;

likratio = (1 + xi)*exp(-abs(Y)^2/lmd*(xi/(1 + xi)));
odds_prior = p_H0/(1 - p_H0);
p_H1 = 1/(1 + odds_prior*likratio);

A = abs(Y)^2/lmd;
B = log((1 + xi)/(p_H1^(-1) - 1)*odds_prior) * (1 + xi)/xi;

disp([A B]);
%}