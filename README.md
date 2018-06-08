
## Setup
Clone this repo and enter the directory:
```
$ git clone https://github.com/dashaub/m4.git
$ cd m4
```

Install [docker](https://www.docker.com/). On Windows/OSX, you may need to modify preferences of the Docker daemon to utilize more than 2 CPU cores and allow up to 8GB RAM usage.

Build the docker image:
```
docker build .  --compress -t m4:5.0 --build-arg BUILD_DATE=2018-05-08
```

Note that the `BUILD_DATE` should _not_ be modified if a reproducible build is desired since this pins the versions of the packages installed to those available at this date.

## Forecasts

Creating the forecasts is computationally expensive and could take several weeks. Run the image:
```
docker run -it --name forecastHybrid m4:5.0 /root/m4/forecastM4.sh
```

Running the job like this will forecast each series serially and can take 2-3 weeks on a fast CPU. Instead you may wish to launch several individual forecasting processes simultaneously by launching a container for a single series.

To produce forecasts for only a single series (e.g. `Daily`), pass the series name as an argument. It can also be especially useful to change the container name to something descriptive if you wish to launch several containers simultaneously.
```
docker run -it --name forecastHybridDaily m4:5.0 /root/m4/forecastM4.sh Daily
```

If the process completes successfully, you will receive a message in the console:
```
Point forecasts and prediction intervals created successfully!
Pressing enter will exit and erase all results.
```
Before exiting, you will wish to copy the results from the Docker container to your filesystem. The forecasts will appear **inside** the docker container in the `mean/` directory; the upper/lower 95% prediction intervals will appear in `upper/` and `lower/`, respectively. In another terminal (_not_ inside the docker container), use `docker cp` to copy the forecasts from the container to your filesystem. The `mean/`, `upper/', and `lower/` directories of your local filesystem will now contain the forecasts and prediction intervals, and you can safely exit and destroy the container.

To copy all of these results back to the host filesystem, run the following commands (where `forecastHybrid` is the name of the docker continer):
```
$ docker cp forecastHybrid:/root/m4/mean .
$ docker cp forecastHybrid:/root/m4/upper .
$ docker cp forecastHybrid:/root/m4/lower .
```

The forecasts can be combined into a single file:
```
cat upper/*.csv > upper_submission.csv
cat lower/*.csv > lower_submission.csv
cat mean/*.csv > mean_submission.csv
```
To prevent exponential upper-bound growth in the prediction intervals, it can be useful to run `cleanUpper.R` to adjust series that exhibit this explosive growth.
