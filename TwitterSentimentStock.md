Twitter Sentiment Analysis
================

In this project I will attempt to predict the price of TSLA, by
analyzing sentiment of tweets from the previous day. Tweets will be
retrieved from twitter by API calls. Tweets from the following Twitter
accounts will be analyzed: @YahooFinance, @MarketWatch, @SeekingAlpha,
@elonmusk.

Read in packages that are necessary for this project.

``` r
library(tidyverse)
library(httr)
library(jsonlite)
library(rtweet)
library(tidyquant)
library(lubridate)
library(tidytext)
```

Connect to Twitter API with app details that are not included in this
markdown output.

``` r
myToken = create_token(app = app_name,
             consumer_key = key, 
             consumer_secret = secret,
             access_token = access,
             access_secret = access_secret)
```

Use the API to retrieve up to 10,000 tweets from each account (10,000 is
the max allowed, the API will likely provide less). Look at the head and
length of this dataframe and decide which variables to keep.

``` r
tmls <- get_timelines(c("YahooFinance", "MarketWatch", "SeekingAlpha", "elonmusk"), n = 10000, include_rts = F)
head(tmls)
```

    ## # A tibble: 6 × 90
    ##   user_id  status_id           created_at          screen_name  text     source 
    ##   <chr>    <chr>               <dttm>              <chr>        <chr>    <chr>  
    ## 1 19546277 1475567404931506182 2021-12-27 20:40:58 YahooFinance “We don… Twitte…
    ## 2 19546277 1475561587637379081 2021-12-27 20:17:51 YahooFinance “The ol… Twitte…
    ## 3 19546277 1475553394878296082 2021-12-27 19:45:18 YahooFinance “What d… Twitte…
    ## 4 19546277 1475552777640239108 2021-12-27 19:42:51 YahooFinance “It is … Twitte…
    ## 5 19546277 1475548424753852421 2021-12-27 19:25:33 YahooFinance “If the… Twitte…
    ## 6 19546277 1475541093999648777 2021-12-27 18:56:25 YahooFinance “We’re … Twitte…
    ## # … with 84 more variables: display_text_width <dbl>, reply_to_status_id <chr>,
    ## #   reply_to_user_id <chr>, reply_to_screen_name <chr>, is_quote <lgl>,
    ## #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
    ## #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
    ## #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
    ## #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
    ## #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>, …

``` r
nrow(tmls)
```

    ## [1] 11238

``` r
tmls1 = tmls %>% select(screen_name, status_id, created_at, text, length = display_text_width, favorite_count, retweet_count) 
```

Write this dataframe to a csv file.

``` r
write_csv(tmls1, "ProjectTweets1.csv")
```

Read in data set.

``` r
t <- read_csv("ProjectTweets1.csv")
```

    ## Rows: 11238 Columns: 7

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): screen_name, text
    ## dbl  (4): status_id, length, favorite_count, retweet_count
    ## dttm (1): created_at

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Create a column that shows if a given tweet mentions Tesla. Text is
converted to lowercase make sure all cases are identified.

``` r
t1 = t %>% mutate(text = str_to_lower(text))
t1 = t1 %>% mutate(ab_tesla = str_detect(text, "tesla | tsla")) 
t1 = t1 %>% mutate(status_id = as.character(status_id))
droptext = select(t1,-text)
```

Use the NRC lexicon to analyze the sentiment of tweets. Here we will
read it in. This lexicon includes important words and their associated
sentiment

``` r
nrc_lexicon = get_sentiments("nrc")
```

Split each tweet into its individual words and remove unimportant words
that do not convey sentiment. Now each word is a row. An inner join
between the lexicon and the data frame is then done to keep words from
the lexicon. This table is then transformed to use sentiments as
columns, sorted by tweet Status ID for rows.

``` r
#find amount of each sentiment in every tweet
t2 = t1 %>% unnest_tokens(word, text) %>%  inner_join(nrc_lexicon, by = "word") %>% count(status_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) 

#join this table with other tweet stats from API call
t3 = t2 %>% right_join(droptext, by = "status_id")
#make dates into date type
t3 = t3 %>% mutate(created_at = as_date(created_at))
#change ab_tesla column into integers (1 for TRUE, 0 for FALSE)
t3 = t3 %>% mutate(TSLA = as.integer(ab_tesla))
```

This chunk will make each row a day and its values in each column the
sum of each sentiment from that day’s tweets.

``` r
#aggregate all sentiment columns and TSLA variable according to date
t3 = mutate(t3, date = created_at)
sen_by_date = aggregate(cbind(TSLA, anticipation, positive, trust, fear, negative, sadness, disgust, joy, surprise, anger) ~ date , data = t3, sum, value_fill = 0) 
sen_by_date %>% tail
```

    ##           date TSLA anticipation positive trust fear negative sadness disgust
    ## 158 2021-12-22    4          172      345   287  103      230      70      27
    ## 159 2021-12-23    4          178      319   240   98      196      67      16
    ## 160 2021-12-24    2          102      179   134   54      130      37      11
    ## 161 2021-12-25    0           54      119    88   32       72      20       3
    ## 162 2021-12-26    0           35       64    50   25       61       7       6
    ## 163 2021-12-27    1          103      149   129   61      162      34      14
    ##     joy surprise anger
    ## 158  94       66    47
    ## 159 115       65    64
    ## 160  75       34    35
    ## 161  44       24    17
    ## 162  18       10    10
    ## 163  43       29    33

Now, use tidyquant to get stock prices from the timeframe of our data.

``` r
#retrieve stock prices with tidyquant
stock = 'TSLA' %>% tq_get(get = "stock.prices",
                  from = "2021-11-03",
                  to = "2021-12-27")
```

    ## Registered S3 method overwritten by 'tune':
    ##   method                   from   
    ##   required_pkgs.model_spec parsnip

This chunk will join the stock prices to the sentiment of each day.

``` r
#join stock price with daily tweet sentiment
final = sen_by_date %>% left_join(stock, by = "date")
final %>% nrow
```

    ## [1] 163

This chunk will lag each row’s sentiment sum to show the sentiment of
the previous day with the price of the current day.

``` r
#add lagged variables for sentiments and drop NA rows 
final1 = final %>% mutate( TSLA1 = lag(TSLA), anticipation1 = lag(anticipation), positive1 = lag(positive), trust1 = lag(trust), fear1 = lag(fear), negative1 = lag(negative), sadness1 = lag(sadness), disgust1 = lag(disgust), joy1 = lag(joy), surprise1 = lag(surprise), anger1 = lag(anger)) %>% na.omit()
final1 %>% nrow
```

    ## [1] 36

Now we can visualize some of this data. This is a graph of the positive
sentiment on each day.

``` r
library(ggthemes)
ggplot(final1, aes(x = date, y= positive)) + geom_line() + ylab("Count") +xlab("") + ggtitle("Positive Tweet Sentiment") +theme_minimal()
```

![](TwitterSentimentStock_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Write this final table to a csv file in order to use for analysis.

``` r
#converting dataframes into csv files
write_csv(t3, "CompleteProjectTable.csv")
write_csv(final1, "ProjectTable.csv")
```

Import libraries that will be used for modeling.

``` r
library(timetk)
library(modeltime)
```

    ## 
    ## Attaching package: 'modeltime'

    ## The following object is masked from 'package:TTR':
    ## 
    ##     growth

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 0.1.4 ──

    ## ✓ broom        0.7.9      ✓ rsample      0.1.0 
    ## ✓ dials        0.0.10     ✓ tune         0.1.6 
    ## ✓ infer        1.0.0      ✓ workflows    0.2.4 
    ## ✓ modeldata    0.1.1      ✓ workflowsets 0.1.0 
    ## ✓ parsnip      0.1.7      ✓ yardstick    0.0.8 
    ## ✓ recipes      0.1.17

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## x scales::discard() masks purrr::discard()
    ## x dplyr::filter()   masks stats::filter()
    ## x xts::first()      masks dplyr::first()
    ## x recipes::fixed()  masks stringr::fixed()
    ## x rtweet::flatten() masks jsonlite::flatten(), purrr::flatten()
    ## x dplyr::lag()      masks stats::lag()
    ## x xts::last()       masks dplyr::last()
    ## x dials::momentum() masks TTR::momentum()
    ## x yardstick::spec() masks readr::spec()
    ## x recipes::step()   masks stats::step()
    ## • Use tidymodels_prefer() to resolve common conflicts.

``` r
#read in data for modeling
d = read_csv("ProjectTable.csv")
```

    ## Rows: 36 Columns: 30

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (1): symbol
    ## dbl  (28): TSLA, anticipation, positive, trust, fear, negative, sadness, dis...
    ## date  (1): date

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
d %>% head
```

    ## # A tibble: 6 × 30
    ##   date        TSLA anticipation positive trust  fear negative sadness disgust
    ##   <date>     <dbl>        <dbl>    <dbl> <dbl> <dbl>    <dbl>   <dbl>   <dbl>
    ## 1 2021-11-03     0            0        1     2     0        0       0       0
    ## 2 2021-11-04     3           46      119    50    25       54      22      11
    ## 3 2021-11-05     0           70      168    93    38       62      31       7
    ## 4 2021-11-08     0           48       93    51    22       39      12       5
    ## 5 2021-11-09     2           64      124    61    31       59      23       7
    ## 6 2021-11-10     2           62      125    62    46       57      21      10
    ## # … with 21 more variables: joy <dbl>, surprise <dbl>, anger <dbl>,
    ## #   symbol <chr>, open <dbl>, high <dbl>, low <dbl>, close <dbl>, volume <dbl>,
    ## #   adjusted <dbl>, TSLA1 <dbl>, anticipation1 <dbl>, positive1 <dbl>,
    ## #   trust1 <dbl>, fear1 <dbl>, negative1 <dbl>, sadness1 <dbl>, disgust1 <dbl>,
    ## #   joy1 <dbl>, surprise1 <dbl>, anger1 <dbl>

``` r
d %>% plot_time_series(date, adjusted, .interactive = F)
```

![](TwitterSentimentStock_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
d
```

    ## # A tibble: 36 × 30
    ##    date        TSLA anticipation positive trust  fear negative sadness disgust
    ##    <date>     <dbl>        <dbl>    <dbl> <dbl> <dbl>    <dbl>   <dbl>   <dbl>
    ##  1 2021-11-03     0            0        1     2     0        0       0       0
    ##  2 2021-11-04     3           46      119    50    25       54      22      11
    ##  3 2021-11-05     0           70      168    93    38       62      31       7
    ##  4 2021-11-08     0           48       93    51    22       39      12       5
    ##  5 2021-11-09     2           64      124    61    31       59      23       7
    ##  6 2021-11-10     2           62      125    62    46       57      21      10
    ##  7 2021-11-11     2           48      126    66    37       55      28       6
    ##  8 2021-11-12     2           49      107    63    29       57      25       6
    ##  9 2021-11-15     3           67      152    91    47       74      28       6
    ## 10 2021-11-16     4           77      120    72    41       64      29       4
    ## # … with 26 more rows, and 21 more variables: joy <dbl>, surprise <dbl>,
    ## #   anger <dbl>, symbol <chr>, open <dbl>, high <dbl>, low <dbl>, close <dbl>,
    ## #   volume <dbl>, adjusted <dbl>, TSLA1 <dbl>, anticipation1 <dbl>,
    ## #   positive1 <dbl>, trust1 <dbl>, fear1 <dbl>, negative1 <dbl>,
    ## #   sadness1 <dbl>, disgust1 <dbl>, joy1 <dbl>, surprise1 <dbl>, anger1 <dbl>

Set seed to control randomness in split and split data into training and
testing set.

``` r
set.seed(8)
splits  = d %>% time_series_split(date_var = date, assess = "1 week",
                        cumulative = TRUE)
trs = training(splits)
ts = testing(splits)
trs %>% head
```

    ## # A tibble: 6 × 30
    ##   date        TSLA anticipation positive trust  fear negative sadness disgust
    ##   <date>     <dbl>        <dbl>    <dbl> <dbl> <dbl>    <dbl>   <dbl>   <dbl>
    ## 1 2021-11-03     0            0        1     2     0        0       0       0
    ## 2 2021-11-04     3           46      119    50    25       54      22      11
    ## 3 2021-11-05     0           70      168    93    38       62      31       7
    ## 4 2021-11-08     0           48       93    51    22       39      12       5
    ## 5 2021-11-09     2           64      124    61    31       59      23       7
    ## 6 2021-11-10     2           62      125    62    46       57      21      10
    ## # … with 21 more variables: joy <dbl>, surprise <dbl>, anger <dbl>,
    ## #   symbol <chr>, open <dbl>, high <dbl>, low <dbl>, close <dbl>, volume <dbl>,
    ## #   adjusted <dbl>, TSLA1 <dbl>, anticipation1 <dbl>, positive1 <dbl>,
    ## #   trust1 <dbl>, fear1 <dbl>, negative1 <dbl>, sadness1 <dbl>, disgust1 <dbl>,
    ## #   joy1 <dbl>, surprise1 <dbl>, anger1 <dbl>

Make a basic recipe/formula to model the adjusted price of TSLA using
the lagged variables.

``` r
basicRecipe = recipe(adjusted~ TSLA1 + anticipation1 + positive1 + trust1 + fear1 + negative1 + sadness1 + disgust1 + joy1 + surprise1 + anger1 + date, trs) 


basicRecipe %>% prep() %>% juice() 
```

    ## # A tibble: 31 × 13
    ##    TSLA1 anticipation1 positive1 trust1 fear1 negative1 sadness1 disgust1  joy1
    ##    <dbl>         <dbl>     <dbl>  <dbl> <dbl>     <dbl>    <dbl>    <dbl> <dbl>
    ##  1     1             2         2      2     0         2        1        0     1
    ##  2     0             0         1      2     0         0        0        0     1
    ##  3     3            46       119     50    25        54       22       11    26
    ##  4     0            22        36     19    11        14        3        1    13
    ##  5     0            48        93     51    22        39       12        5    20
    ##  6     2            64       124     61    31        59       23        7    39
    ##  7     2            62       125     62    46        57       21       10    37
    ##  8     2            48       126     66    37        55       28        6    37
    ##  9     0            11        31     21     5        11        6        2    12
    ## 10     3            67       152     91    47        74       28        6    44
    ## # … with 21 more rows, and 4 more variables: surprise1 <dbl>, anger1 <dbl>,
    ## #   date <date>, adjusted <dbl>

Define the models that will be used. This is a regression problem, so
engines are set accordingly. The prophet and ARIMA models will be used
to predict stock price. XGboost versions of these models will also be
used.

``` r
prophet_reg = prophet_reg() %>%
    set_engine("prophet") 
prophet_reg_b = prophet_boost() %>%
    set_engine("prophet_xgboost") 
arima_reg = arima_reg() %>% 
    set_engine("auto_arima")
arima_reg_b = arima_boost() %>% 
    set_engine("auto_arima_xgboost")
```

I will define a basic workflow here with the regular model and basic
recipe.

``` r
bw = workflow() %>%  add_model(prophet_reg) %>% add_recipe(basicRecipe)
```

This section fits the model to the training dataset.

``` r
m1 =  bw %>% fit(trs) 
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

    ## n.changepoints greater than number of observations. Using 23

``` r
m2 = bw %>% update_model(prophet_reg_b) %>%  fit(trs) 
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

    ## n.changepoints greater than number of observations. Using 23

``` r
m3 = bw %>% update_model(arima_reg) %>% fit(trs)
```

    ## frequency = 5 observations per 1 week

``` r
m4 = bw %>% update_model(arima_reg_b) %>% fit(trs)
```

    ## frequency = 5 observations per 1 week

Now we will use the trained model and predict the test data set.

``` r
mt = modeltime_table(m1, m2, m3, m4)
ct = mt %>% modeltime_calibrate(ts)
```

With these predictions we will find the MAE and RMSE to the accuracy of
the predictions and compare the models.

``` r
ct %>% modeltime_accuracy() %>% select(.model_id, 
  .model_desc , mae, rmse) %>% table_modeltime_accuracy(.interactive = F)
```

<div id="ituldeflbl" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ituldeflbl .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ituldeflbl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ituldeflbl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ituldeflbl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ituldeflbl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ituldeflbl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ituldeflbl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ituldeflbl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ituldeflbl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ituldeflbl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ituldeflbl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ituldeflbl .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ituldeflbl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ituldeflbl .gt_from_md > :first-child {
  margin-top: 0;
}

#ituldeflbl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ituldeflbl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ituldeflbl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ituldeflbl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ituldeflbl .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ituldeflbl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ituldeflbl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ituldeflbl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ituldeflbl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ituldeflbl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ituldeflbl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ituldeflbl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ituldeflbl .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ituldeflbl .gt_left {
  text-align: left;
}

#ituldeflbl .gt_center {
  text-align: center;
}

#ituldeflbl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ituldeflbl .gt_font_normal {
  font-weight: normal;
}

#ituldeflbl .gt_font_bold {
  font-weight: bold;
}

#ituldeflbl .gt_font_italic {
  font-style: italic;
}

#ituldeflbl .gt_super {
  font-size: 65%;
}

#ituldeflbl .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Accuracy Table</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">.model_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.model_desc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mae</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rmse</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">1</td>
<td class="gt_row gt_left">PROPHET W/ REGRESSORS</td>
<td class="gt_row gt_right">51.13</td>
<td class="gt_row gt_right">65.81</td></tr>
    <tr><td class="gt_row gt_right">2</td>
<td class="gt_row gt_left">PROPHET W/ XGBOOST ERRORS</td>
<td class="gt_row gt_right">69.33</td>
<td class="gt_row gt_right">84.86</td></tr>
    <tr><td class="gt_row gt_right">3</td>
<td class="gt_row gt_left">REGRESSION WITH ARIMA(1,0,0) ERRORS</td>
<td class="gt_row gt_right">57.89</td>
<td class="gt_row gt_right">64.03</td></tr>
    <tr><td class="gt_row gt_right">4</td>
<td class="gt_row gt_left">ARIMA(0,1,0) W/ XGBOOST ERRORS</td>
<td class="gt_row gt_right">63.12</td>
<td class="gt_row gt_right">76.08</td></tr>
  </tbody>
  
  
</table>
</div>

Visualize the forecast for the stock prices.

``` r
ct %>% modeltime_forecast(new_data = ts, actual_data = trs) %>% 
  plot_modeltime_forecast(.interactive = F)
```

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

![](TwitterSentimentStock_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
