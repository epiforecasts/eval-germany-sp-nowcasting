tar_target(epinowcast_settings, {
  list(
    save_warmup = FALSE,
    output_loglik = FALSE,
    pp = FALSE,
    iter_warmup = 1000,
    iter_sampling = 2000,
    chains = 2,
    threads_per_chain = 2,
    adapt_delta = 0.95,
    show_messages = FALSE
  )
})
