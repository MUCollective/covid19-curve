dotplots
================

## Libraries and setup

``` r
library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())
```

## Read in data

Here are predictions from a simple model from <https://covidactnow.org/>
as of March 24:

``` r
df = read_csv("covid_act_now.csv", col_types = cols(
  date = col_date(),
  not_flattened = col_integer(),
  flattened = col_integer(),
  beds = col_integer()
))

df
```

    ## # A tibble: 29 x 4
    ##    date       not_flattened flattened  beds
    ##    <date>             <int>     <int> <int>
    ##  1 2020-03-07            76        76 21010
    ##  2 2020-03-11           220       220 21010
    ##  3 2020-03-15           732       732 21010
    ##  4 2020-03-19          5360      5361 22060
    ##  5 2020-03-23         20769     20775 23163
    ##  6 2020-03-27         48888     34639 24322
    ##  7 2020-03-31        111019     56312 25538
    ##  8 2020-04-04        231190     87732 26814
    ##  9 2020-04-08        390700    127428 28155
    ## 10 2020-04-12        401125    165559 29563
    ## # ... with 19 more rows

And a long-format version for hospitalizations:

``` r
hosp = df %>%
  pivot_longer(c(-date, -beds), names_to = "model", values_to = "hospitalized") %>%
  mutate(
    flattened = model == "flattened",
    model = fct_recode(model, `Not flattened` = "not_flattened", Flattened = "flattened")
  )

hosp
```

    ## # A tibble: 58 x 5
    ##    date        beds model         hospitalized flattened
    ##    <date>     <int> <fct>                <int> <lgl>    
    ##  1 2020-03-07 21010 Not flattened           76 FALSE    
    ##  2 2020-03-07 21010 Flattened               76 TRUE     
    ##  3 2020-03-11 21010 Not flattened          220 FALSE    
    ##  4 2020-03-11 21010 Flattened              220 TRUE     
    ##  5 2020-03-15 21010 Not flattened          732 FALSE    
    ##  6 2020-03-15 21010 Flattened              732 TRUE     
    ##  7 2020-03-19 22060 Not flattened         5360 FALSE    
    ##  8 2020-03-19 22060 Flattened             5361 TRUE     
    ##  9 2020-03-23 23163 Not flattened        20769 FALSE    
    ## 10 2020-03-23 23163 Flattened            20775 TRUE     
    ## # ... with 48 more rows

## Area chart version

Area chart version folks tend to use:

``` r
hosp %>%
  ggplot(aes(x = date)) +
  geom_area(
    aes(y = hospitalized, color = flattened, fill = flattened),
    position = "identity", alpha = 0.5
  ) +
  geom_line(aes(y = beds), data = df)
```

![](dotplots_files/figure-gfm/area_chart-1.png)<!-- -->

## Dotplots

Let’s try to re-generate this using a frequency framing approach with
dotplots. First we’ll pick a dot size in terms of number of people:

``` r
people_per_dot = 10000
```

Then we’ll round counts accordingly:

``` r
hosp_dots = hosp %>%
  mutate(
    # TODO: need a better solution for the very small numbers
    hospitalized_dots = ceiling(hospitalized / people_per_dot),
    dot_number = map(hospitalized_dots, seq_len)
  ) %>%
  unnest(dot_number) %>%
  group_by(date, model) %>%
  mutate(
    # determine the number of people in this dot --- for the last
    # dot in a group it will be the leftovers (so, not people_per_dot)
    dot_people = ifelse(
      dot_number == max(dot_number), hospitalized %% people_per_dot,
      people_per_dot
    ),
    has_a_bed = cumsum(dot_people) <= beds
  ) %>%
  ungroup()

hosp_dots
```

    ## # A tibble: 276 x 9
    ##    date        beds model hospitalized flattened hospitalized_do~ dot_number
    ##    <date>     <int> <fct>        <int> <lgl>                <dbl>      <int>
    ##  1 2020-03-07 21010 Not ~           76 FALSE                    1          1
    ##  2 2020-03-07 21010 Flat~           76 TRUE                     1          1
    ##  3 2020-03-11 21010 Not ~          220 FALSE                    1          1
    ##  4 2020-03-11 21010 Flat~          220 TRUE                     1          1
    ##  5 2020-03-15 21010 Not ~          732 FALSE                    1          1
    ##  6 2020-03-15 21010 Flat~          732 TRUE                     1          1
    ##  7 2020-03-19 22060 Not ~         5360 FALSE                    1          1
    ##  8 2020-03-19 22060 Flat~         5361 TRUE                     1          1
    ##  9 2020-03-23 23163 Not ~        20769 FALSE                    3          1
    ## 10 2020-03-23 23163 Not ~        20769 FALSE                    3          2
    ## # ... with 266 more rows, and 2 more variables: dot_people <dbl>,
    ## #   has_a_bed <lgl>

Then we’ll draw as dots:

``` r
bin_width = 2
dodge_width = 3.5

hosp_dots %>%
  ggplot(aes(x = date)) +
  geom_area(aes(y = beds), data = df, fill = "gray85") +
  geom_dotplot(
    aes(fill = flattened), color = NA, binwidth = bin_width, alpha = 0.5, 
    position = position_dodge(width = dodge_width)
  ) +
  geom_dotplot(
    aes(group = flattened), fill = "gray50", color = NA,
    binwidth = bin_width,
    position = position_dodge(width = dodge_width),
    data = . %>% filter(has_a_bed)
  ) +
  annotate("text",
    x = as.Date("2020-05-25"), y = max(df$beds) + 10000, 
    label = "Has a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "gray65"
  ) +
  annotate("text",
    x = as.Date("2020-04-25"), y = max(df$flattened), 
    label = "Doesn't have a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set1") +
  # TODO: scaling of the area chart is slightly wrong here
  # because of how dotplots work in ggplot, probably could
  # manually lay out dots instead (wouldn't be hard using
  # hosp_dots$dot_number)
  scale_y_continuous(limits = c(0, max(hosp$hospitalized))) +
  labs(
    title = "Hospitalized People over time",
    subtitle = paste0("Each dot is ", people_per_dot, " people"),
    y = NULL,
    x = NULL
  )
```

![](dotplots_files/figure-gfm/dotplot_combined-1.png)<!-- -->

Or the facetted version:

``` r
hosp_dots %>%
  ggplot(aes(x = date)) +
  geom_area(aes(y = beds), data = hosp, fill = "gray85") +
  geom_dotplot(
    aes(fill = flattened), color = NA, binwidth = bin_width, alpha = 0.5, 
    position = position_dodge(width = dodge_width)
  ) +
  geom_dotplot(
    aes(group = flattened), fill = "gray50", color = NA,
    binwidth = bin_width,
    position = position_dodge(width = dodge_width),
    data = . %>% filter(has_a_bed)
  ) +
  annotate("text",
    x = as.Date("2020-05-25"), y = max(df$beds) + 10000, 
    label = "Has a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "gray65"
  ) +
  annotate("text",
    x = as.Date("2020-04-25"), y = max(df$flattened), 
    label = "Doesn't have a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "black"
  ) +
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  scale_y_continuous(limits = c(0, max(hosp$hospitalized))) +
  labs(
    title = "Hospitalized People over time",
    subtitle = paste0("Each dot is ", people_per_dot, " people"),
    y = NULL,
    x = NULL
  ) +
  facet_grid(flattened ~ .)
```

![](dotplots_files/figure-gfm/dotplot_faceted-1.png)<!-- -->

Could do a marginal icon array?

## Manual dot placement

Manually placing the dots will give us finer-grained control than using
the dotplot geom:

``` r
hosp_dots %>%
  ggplot(aes(x = date)) +
  geom_area(aes(y = beds), data = hosp, fill = "gray85") +
  geom_point(
    aes(y = dot_number * people_per_dot - people_per_dot/2, size = dot_people, color = flattened), 
    alpha = 0.25,
    data = . %>% filter(!has_a_bed)
  ) +
  geom_point(
    aes(y = dot_number * people_per_dot - people_per_dot/2, size = dot_people), 
    color = "gray65",
    data = . %>% filter(has_a_bed)
  ) +
  coord_fixed(ratio = 4 / people_per_dot, expand = FALSE) +
  scale_size_area(max_size = 3.5) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(. ~ fct_rev(model)) + 
  geom_vline(
    xintercept = as.Date("2020-03-25"), 
    color = "gray50", linetype = "dashed", size = .5
  ) +
  annotate("text",
    x = as.Date("2020-05-25"), y = max(df$beds) + people_per_dot, 
    label = "Has a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "gray65"
  ) +
  annotate("text",
    x = as.Date("2020-04-21"), y = max(df$flattened) + people_per_dot, 
    label = "Doesn't have a bed", fontface = "bold", hjust = 0, vjust = 0,
    color = "black"
  ) +
  guides(color = FALSE, size = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  labs(
    title = "People in the hospital at the same time",
    subtitle = paste0("Each dot is ", people_per_dot/1000, "k people"),
    y = NULL,
    x = NULL
  ) 
```

![](dotplots_files/figure-gfm/dotplot_manual-1.png)<!-- -->
