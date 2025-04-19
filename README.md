# DSE3101---Project
The Trio DSE3101 Project

Topic: Benchmarking forecasting models for GDP growth with real-time data

Run app locally
1. Clone the app to your local device
git clone https://github.com/thinhnd245/DSE3101Project-TheTrio.git
2. Open the app (either ui.R or server.R) in RStudio
3. Run the app

Model Selection:
1. Autoregressive (AR): available parameter is number of lags
2. Autoregressive Distributed Lag (ARDL): available parameter is number of lags
3. K Nearest Neighbours (KNN): available parameters are number of neighbours (can be either a number or a vector of numbers) and cf (the aggregrate function that determine our forecasts after identify the neighbours)


Feature Selection
Please use https://fred.stlouisfed.org/categories/ for looking up your interest features
