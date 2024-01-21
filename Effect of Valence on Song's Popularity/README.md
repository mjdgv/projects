**Description**

With streaming services being more popular than ever, what makes a song popular is of enormous interest. Using data collected from Spotify API in Kaggle, we investigated the effect of valence, the musical positiveness of songs, on song popularity. We believed that valence would positively contribute to song popularity. We ran multiple regressions by song genre - pop, indie, rap and R&B, electronic, country and folk, rock and metal, classical and instrumental, and international. The other variables besides valence were chosen by variable selection criteria for each genre.

**Disclaimer**

This project was a team effort. I wrote most of the R code with the excpetion of the model diagnostics graphs.

**Findings**

In conclusion, our results showed that valence negatively affected popularity for most genres. The extent of valence’s effect on popularity was different for each genre. For example, rap and R&B songs had one of the highest negative effects of valence on popularity. On the other hand, for international songs, the effect was around three times smaller. This suggests that rap and R&B listeners have a stronger preference for less positive songs when compared to international music listeners.

**Limitations**

Because our dataset had fewer popular songs, our models were trained on less popular songs during the cross-validation stage. As a result, our model proved to be better at predicting the rankings of moderately popular songs than extremely popular or extremely unpopular songs. Most importantly, the models did not satisfy all the assumptions for linear regression models, as the  equal variance criterion was not fulfilled. We attempted to use Box-Cox transformations or log transformations to remedy this issue, but because our data was not strictly positive, we were unable to do so.

**Future Work**

For future research, we want to investigate other factors that influence song popularity besides intrinsic characteristics such as valence. Other factors such as what record label a song was produced at, what the marketing budget is and other variables that relate to the business side of the music industry could also serve as meaningful predictors for a song’s popularity. 
