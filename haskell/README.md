# Portfolio Rebalancer


## Input Portfolio

### Properties

Allocation's (alloc) must sum to exactly one.

Prices must be GT 0.

Qty must be GTE 0.

Symbol must be non-null and unique.

Portfolio Size (portSize) must be GT 0.

### Sample

{
  "portSize":"10000.0",
  "assets":[
  {"qty":52,
   "symbol":"GOOG",
   "price":"98.0",
   "alloc":"0.6"},
  {"qty":136,
   "symbol":"AAPL",
   "price":"22.0",
   "alloc":"0.3"},
  {"qty":239,
   "symbol":"TSLA",
   "price":"8.0",
   "alloc":"0.1"}
  ]
}

## Rebalanced Portfolio

### Parameter's

Tracking Band (Float): The band, asset's percent allocation +/- band, of which is considered a valid rebalance. The entire space defined by this band is searched with the portfolio that minimized the uninvested portion being selected. Default value of this is 5%

### Sample with tracking band == default (5%)

{
  "portSize":"10000.0",
  "assets":[
    {"qty":62,
     "symbol":"GOOG",
     "price":"98.0",
     "alloc":"0.6076"},
    {"qty":134,
     "symbol":"AAPL",
     "price":"22.0",
     "alloc":"0.2948"},
    {"qty":122,
     "symbol":"TSLA",
     "price":"8.0",
     "alloc":"9.76e-2"}
    ]
}
