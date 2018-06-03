# Baseball Data
Code to scrape, clean, organize, etc. Major League Baseball [Statcast data](https://baseballsavant.mlb.com/statcast_search). Also pretty plots and the sort-of-ugly code that produced them.

## Here are some plots:

### Bryce Harper Nationals Park Home Runs, By Field, through 2017

![Bryce Harper Nationals Park Home Runs, By Field, through 2017](/plots/Bryce%20Harper%20Nationals%20Park%20Home%20Runs%2C%20By%20Field.png)

### Home Run Rate per Batted Ball, 2008-2018

Is the ball still juiced?

![Home Run Rate per Batted Ball, 2008-2018](/plots/home_run_rate_bb_2008_2018_1.png)
([Plot code](/code/model_and_plot_code/home_run_plots.R))

Probably! See [this](https://www.baseballprospectus.com/news/article/40170/prospectus-feature-mlb-commission-confirms-baseball-changed/) for some reasons to find the above plot suggestive.

### Using expected wOBA to look for the hot hand, 2018

![Using expected wOBA to look for the hot hand, 2018](/plots/woba_daily_mean_top20_2018_1.png)
([Plot code](/code/model_and_plot_code/barrel_plots.R))

**An early attempt at a definition:** *A stretch of days where a player's LOESS line is entirely above their season-long average (i.e., the red line).*

TO-DO: Formalize this even a little bit, perhaps using ARIMA or other time series methods.
