# 2021-11-20

- Added a initial framework for reporting the nowcast results.
- Added a simple adjust for spuriously large or small quantiles when models have had issues with fitting.

# 2021-11-19

- Simplified the model with an age grouped random walk to have independent random walks by age rather than a shared global walk and a group specific random walk in the residuals. This was based on initial backcasting where the more complex model was unstable during fitting indicating issues with the model formulation.

# 2021-11-17

- Add targets workflow for processing new data.
- Add targets workflow for nowcasting using a range of models.
- Add targets workflow for formatting nowcasts and added nowcast metadata.

# 2021-10-26

- Initial version of semi-parametric nowcasting model with flexible date of reference and date of report effects implemented in stan with supporting R parsing code.

# 2021-10-17

- Set up project and imported key files from `{EpiNow2}`.
