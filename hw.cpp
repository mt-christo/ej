// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include <Rcpp.h>
#include "BackTest.h"

// [[Rcpp::export]]
Rcpp::List sma_crossover(Rcpp::DataFrame ticks,
                         Rcpp::List parameters,
                         Rcpp::List options,
                         bool fast = false) {
  enum class ProcessingState { LONG, FLAT, SHORT };
  ProcessingState state = ProcessingState::FLAT;
  int idTrade = 1;

  int timeFrame = parameters["timeframe"];

  Processor bt(timeFrame);
  bt.SetOptions(options);
  bool isTradingHours = not bt.IsTradingHoursSet();

  bt.onMarketOpen = [&](){
    isTradingHours = true;
  };

  bt.onMarketClose = [&](){
    isTradingHours = false;

    if(state == ProcessingState::SHORT){
      bt.SendOrder(new Order(OrderSide::BUY, OrderType::MARKET, NA_REAL, "close short (EOD)", idTrade++));
    }
        
    if(state == ProcessingState::LONG){
      bt.SendOrder(new Order(OrderSide::SELL, OrderType::MARKET, NA_REAL, "close long (EOD)", idTrade++)); 
    }

    state = ProcessingState::FLAT;
  };

  bt.onCandle = [&](Candle candle){
    if(not bt.CanTrade()) return;
    if(not isTradingHours) return;

    if(candle.close > candle.open and state == ProcessingState::FLAT){
      bt.SendOrder(new Order(OrderSide::BUY, OrderType::MARKET, NA_REAL, "long", idTrade));
      state = ProcessingState::LONG;
    }
    if(candle.close > candle.open and state == ProcessingState::SHORT){
      bt.SendOrder(new Order(OrderSide::BUY, OrderType::MARKET, NA_REAL, "close short", idTrade++));
      bt.SendOrder(new Order(OrderSide::BUY, OrderType::MARKET, NA_REAL, "reverse short", idTrade));
      state = ProcessingState::LONG;
    }
    if(candle.close < candle.open and state == ProcessingState::FLAT){
      bt.SendOrder(new Order(OrderSide::SELL, OrderType::MARKET, NA_REAL, "short", idTrade));
      state = ProcessingState::SHORT;
    }
    if(candle.close < candle.open and state == ProcessingState::LONG){
      bt.SendOrder(new Order(OrderSide::SELL, OrderType::MARKET, NA_REAL, "close long", idTrade++));
      bt.SendOrder(new Order(OrderSide::SELL, OrderType::MARKET, NA_REAL, "reverse long", idTrade));
      state = ProcessingState::SHORT;
    }
  };

  bt.Feed(ticks);
  Rcpp::List summary = bt.GetSummary();
  
  if(fast) return summary;

  // combine candles and indicators history
  Rcpp::List indicators = ListBuilder().AsDataTable()
    .Add( bt.GetCandles()                                )
    .Add( "pnl"     , bt.GetOnCandleMarketValueHistory() )
    .Add( "drawdown", bt.GetOnCandleDrawDownHistory()    );

  // return back test summary, trades, orders and candles/indicators
  return ListBuilder()
    .Add( "summary"          , summary                              )
    .Add( "trades"           , bt.GetTrades()                       )
    .Add( "orders"           , bt.GetOrders()                       )
    .Add( "indicators"       , indicators                           )
    .Add( "daily_performance", bt.GetOnDayClosePerformanceHistory() );
    
}
