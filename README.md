# Baseball Data
Code to scrape, clean, organize, etc. Major League Baseball [Statcast data](https://baseballsavant.mlb.com/statcast_search). Also pretty plots and the sort-of-ugly code that produced them.

## Here are some plots:

### Home Run Rate per Batted Ball, 2008-2019

Is the ball still juiced?

![Home Run Rate per Batted Ball, 2008-2019](/plots/home_run_rate_bb_2008_2019_2019-08-09.png)
([Plot code](/code/model_and_plot_code/home_run_plots.R))

Probably! See [this](https://www.baseballprospectus.com/news/article/40170/prospectus-feature-mlb-commission-confirms-baseball-changed/) for some reasons to find the above plot suggestive.

### Bryce Harper Nationals Park Home Runs, By Field

![Bryce Harper Nationals Park Home Runs, By Field](/plots/Bryce_Harper_Nationals_Park_Home_Runs_By_Field.png)
([Plot code](/code/model_and_plot_code/bryce_harper_home_runs_nationals_park.R))

### Using expected wOBA to look for the hot hand, 2018

![Using expected wOBA to look for the hot hand, 2018](/plots/woba_daily_mean_top20_2018_1.png)
([Plot code](/code/model_and_plot_code/barrel_plots.R))

**An early attempt at a definition:** *A stretch of days where a player's LOESS line is entirely above their season-long average (i.e., the red line).*

TO-DO: Formalize this even a little bit, perhaps using ARIMA or other time series methods.
