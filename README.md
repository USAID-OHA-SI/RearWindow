# RearWindow
[![Travis build status](https://travis-ci.org/USAID-OHA-SI/RearWindow.svg?branch=master)](https://travis-ci.org/USAID-OHA-SI/RearWindow)

Quarterly Reviews of MER Data

### Instructions

Install this package on your local machine.

```{r}
#install devtools for installing package from GitHub
  install.packages("devtools")
  library("devtools")
#install/update package
  install_github("USAID-OHA-SI/RearWindow")
#load package
  library("RearWindow")
```

Download the MSD dataset, saving it as an rds file. The OUxIM MSD can be downloaded from [PEPFAR Panorama](https://pepfar-panorama.org).

```{r}
#install/update ICPIutilitites
  install_github("ICPI/ICPIutilities")
#load package
  library("ICPIutilities")
#read in OUxIM MSD and convert it to an rds file 
  read_msd("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20180921_v2_2.rds")
```

Pick a country you would like to create quarterly comparison visuals and run the `rw_assemble_plots()` function.
```{r}
#identify your country of interest 
  ou <- "Tanzania"
#identify your file path
  filepath <- "~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20180921_v2_2.rds"
rw_assemble_plots(ou, filepath)
```

Copy the filepath from the R console. Open up a new PowerPoint presentation. Under the Insert tab on the ribbon, select Photo Album and then New Photo Album. Paste the file path into the window and select all the plots. Last steup is to change the Album layout. Under the Picture Layout drop down, select 1 picture with title.