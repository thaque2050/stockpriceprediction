# stockpriceprediction
A function to scarp news and stock price, and conduct sentiment analysis for each news; another function to predict stock price movement

Scrap News and Conduct Sentiment Analysis
* Running the file named - "Scarp News and Sentiment Analysis.R" will generate a function - "Extract_News_Stock_Price"
* Use this function to scrap web for news and stock prices
* The function also conducts sentiment analysis on the scapped news articles to generate following three variables:
  * Count of positive news for each day
  * Count of negative news for each day
  * Count of neutral news for each day
* The function may give error if the input variable leads to zero collection of news articles
* Reuters is used to source news articles
* NASDAQ website is used to source information related to stock prices
* Input the vector "Stock_Tickers_REUTERS" as c("AAPL.OQ","MSFT.OQ","LMT.N")
 * The tickers are sourced from the Reuters website and is consistent with NASDAQ
 * Some companies maynot be listed on NASDAQ, so please check before including them in analysis 

Model Training and Prediction
* Use the file - "Model Training and Prediction.R" for modeling and prediction
* I have used two model - SVM and XGBoost to build models and found that XGBoost has higher accuracy for sample data I collected
* Just store outout of the "Extract_News_Stock_Price" function and store it in a data frame titled "sentiment_data"
 * Run the code
 * Output will be saved in a file - "Model Training and Prediction.pdf" - in the working directory
