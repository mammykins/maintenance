# Maintenance, rebuild and deterioration of the School Estate

A collaboration between BW (MSc Operational Research @ LSE) and MG (DfE) to develop a model for modelling the effect of different spending policies on the condition of the School Estate contributing towards a MSc thesis.  

There is one principle function `rebmainder_all_in_one()` that has the following arguments:

|Argument|Description|
|---|---|
|starting_state|A 6L vector of GIFA state condition c(n, a, b, c, d, e). Where n is new and e is decommisioned.|


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
