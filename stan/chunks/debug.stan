
    for (i in 1:nobs) {
      int j = 0;
      for (k in 1:tmax) {
        j += is_nan(trunc_obs[k, i]) ? 1 : 0;
      }
      j += phi <= 1e-3 ? 1 : 0;
      if (j) {
        print("Issue with Dataset");
        print(i);
        print("Posterior prediction (no observation error)");
        print(trunc_obs[, i]);
        print("Truncation  distribution estimate");
        print(cdfs[, i]);
        print("Logmean and Logsd intercept");
        print(logmean_init);
        print(logsd_init);
        print("Logmean and Logsd for each dataset");
        print(logmean[i]);
        print(logsd[i]);
        print("Overdispersion");
        print(sqrt_phi);
      }
    }
