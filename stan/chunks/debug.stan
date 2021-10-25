    for (i in 1:ncmfs) {
      int j = 0;
      for (k in 1:dmax) {
        j += is_nan(cmfs[k, i]) ? 1 : 0;
      }
      j += phi <= 1e-3 ? 1 : 0;
      if (j) {
        print("Issue with CMF");
        print(i);
        print("Truncation  distribution estimate");
        print(cmfs[, i]);
        print("Logmean and Logsd intercept");
        print(logmean_int);
        print(logsd_int);
        print("Logmean and Logsd for CMF");
        print(logmean[i]);
        print(logsd[i]);
        print("Overdispersion");
        print(sqrt_phi);
      }
    }
