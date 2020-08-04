# framinghamPrism

## Installation

You will need the `X11` package:
https://www.xquartz.org/

## Deployment

```
install.packages("opencpu")
install.packages("rredis")
opencpu::ocpu_start_server()
```

## Using `framinghamPrism` in R

```
install.packages("devtools") 
devtools::install_github("resplab/peermodels")
library("peermodels")
connect_to_model('framinghamPrism', api_key=API_KEY, local_server=TRUE)
```
