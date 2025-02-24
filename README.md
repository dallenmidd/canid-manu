# Data and code Van Horn & Allen (in submission)
Data and code for a manuscript in submission. 

- `data_spatial/nh_spatial` Data for the New Hampshire spatial regression
	- `WMU` Wildlife management unit 
	- `coy` Coyote catch per unit effort in that WMU
	- `gf` Grey fox catch per unit effort in that WMU
	- `rf` Red fox catch per unit effort in that WMU
	- `lyme` Lyme disease cases per 100,000 people in that WMU
- `data_spatial/ny_spatial` Data for the New York spatial regression
	- `fur_area` Name of the NY fur area
	- `coy` Coyote sighted per 1000 hunter hours
	- `gf` Grey fox sighted per 1000 hunter hours
	- `rf` Red fox sighted per 1000 hunter hours
	- `lyme` Lyme disease cases per 100,000 people in that WMU
- `data_temporal/` This directory has data for the temporal regressions. The columns are:
	- `year`
	- `state_lyme` number of Lyme disease cases reported by the state department of health
	- `cdc_lyme` number of Lyme disease cases reported by the CDC
	- `coy` Coyote catch per unit effort reported by the state wildlife agency
	- `gf` Grey fox catch per unit effort reported by the state wildlife agency
	- `rf` Red fox catch per unit effort reported by the state wildlife agency
	- `rain` Total rain in that year (inches)
	- `temp` Average temperature in that year (F)
- `figures/` This directory has manuscript and appendix figures.
- `pred-host-tick_model.R` R code for the predator-host-tick infection model
- `spatial_regressions.R` R code for the spatial regressions
- `temporal_regressions.R` R code for the temporal regressions