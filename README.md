# Maintenance, rebuild and deterioration of the School Estate

A collaboration between BW (MSc Operational Research @ LSE) and MG (DfE) to develop a model for modelling the effect of different spending policies on the Gross Internal Floor Area (GIFA) condition of the School Estate. BW contributed towards a MSc thesis.  

There is one principle function `rebmainder_all_in_one()` that has the following arguments:

The user needs a 6L vector starting state and a 6 by 6 transition matrix. The file `05_user_input_and_ggplot_output.R` guides an inexperienced user through the typical steps from input to output.

|Argument|Description|
|---|---|
|starting_state|A 6L vector of GIFA state condition c(n, a, b, c, d, e). Where n is new and e is decommisioned.|
|timesteps|The number of years or timesteps the model should forecast.|
|rebuild_investment|The total monies (£) invested in rebuilding the School estate per year. A vector with length equal to the timesteps.|
|maintenance_investment|The total monies (£) invested in maintaining the School estate per year. A vector with length equal to the timesteps.|
|r|rebuild rate or unit cost of bringing one GIFA back to new condition|
|transition_matrix|A 6 by 6 transition matrix created using the markovchain package. See tm_create_and_tidy.R for guidance.|
|discount_rate_rebuild|Discount factor for rebuild cost, rebuilding is inefficient.|
|discount_rate_maintain|Discount factor for rebuild cost, rebuilding is inefficient.|
|prop_maint_|Proportion of maintenance budget assigned to GIFA of condition... Must add to 1.|
|prop_rebuild_|Proportion of rebuild budget assigned to GIFA of condition... Must add to 1.|
|cost_d_to_a|Maintenance costs of a transition. Cost of converting one unit GIFA of D to one unit GIFA of A.|

## Installing the package

The package can be installed with the `devtools` package with `devtools::install_github('mammykins/maintenance')`.

If you cannot use this function (due to firewalls for instance) you can download the package as a `.zip` file from the main repository page, and run `devtools::install_local('path_to_zip_file')`.

## Using rebmainder_all_in_one()

### From the terminal

To assist with typical user-input open the `05_user_input_and_ggplot_output.R`, this has some defaults for the many arguments.

### Initial State

You will also need initial state data which is a six-length vector and a 6 by 6 transition matrix. These can be read in using `00_get_data`.

### From an R session

Launch an R session as normal and run the following (again setting the arguments as required):

```
library(maintenance)
rebmainder_all_in_one()
```
