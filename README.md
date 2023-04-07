# movie-recommender

## Download MovieLens data

Please download MovieLens 25M Dataset from https://grouplens.org/datasets/movielens/ with the following R script.

```
temp <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-25m.zip", temp)
unzip(temp, exdir = "./")
```


