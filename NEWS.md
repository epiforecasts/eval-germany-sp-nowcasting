# 2021-11-24

- Added README's for the data and bash script folders. 
- Added retrospective nowcasts back to the 15th of October 2021.

# 2021-11-23

- Added an introduction and methods write up. 
- Added a real-time evaluation report skeleton.
- Added additional repository and project details to the README.
- Added a home page to the published reports webpage linking to resources.

# 2021-11-20

- Added a initial framework for reporting the nowcast results.
- Added a simple adjustment for spuriously large or small quantiles when models have had issues with fitting. This is simply a rescaling of quantiles that are greater/smaller than 25% of the median.

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
