
yankee_stadium_trace <- 
    tibble(grid = seq(0.0, 90.0, .1)) %>% 
    mutate(B = (grid * pi) / 180) %>% 
    mutate(
        radius =
            case_when(    
                ((grid >= 0.0 ) & (grid <  3.2))  ~ (-752.7415 /  ((sin(B) - 2.397266    * cos(B)))),
                ((grid >= 3.2 ) & (grid <  4.9))  ~ (-1341.4764 / ((sin(B) - 4.22849     * cos(B)))),
                ((grid >= 4.9 ) & (grid <  30.6)) ~ (323.639 /                             cos(B)),
                ((grid >= 30.6) & (grid <  36.1)) ~ (2683.6147 /  ((sin(B) + 7.700602    * cos(B)))),
                ((grid >= 36.1) & (grid <  40.4)) ~ (913.27186 /  ((sin(B) + 2.139572    * cos(B)))),
                ((grid >= 40.4) & (grid <  44.4)) ~ (707.36801 /  ((sin(B) + 1.4653105   * cos(B)))),
                ((grid >= 44.4) & (grid <  48.4)) ~ (600.6388 /   ((sin(B) + 1.096466    * cos(B)))),
                ((grid >= 48.4) & (grid <  52.1)) ~ (496.311752 / ((sin(B) + 0.7103818   * cos(B)))),
                ((grid >= 52.1) & (grid <  56.7)) ~ (445.2994 /   ((sin(B) + 0.5053365   * cos(B)))),
                ((grid >= 56.7) & (grid <  62.8)) ~ (390.30014 /  ((sin(B) + 0.2548946   * cos(B)))),
                ((grid >= 62.8) & (grid <  80.6)) ~ (345.39856 /  ((sin(B) + 0.001719809 * cos(B)))),
                ((grid >= 80.6) & (grid <  84.8)) ~ (324.4985 /   ((sin(B) - 0.3638949   * cos(B)))),
                ((grid >= 84.8) & (grid <= 90.0)) ~ (316 /        ((sin(B) - 0.6421415   * cos(B))))
            )
    ) %>% 
    mutate(
        x = 0 + radius * cos(B),
        y = 0 + radius * sin(B)
    ) %>% 
    mutate(
        x = if_else(x < .0001, 0, x),
        y = if_else(y < .0001, 0, y)
        # x = case_when(x < .0001 ~ 0, TRUE ~ x),
    )
