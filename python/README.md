# Portfolio Rebalancer


## Input Portfolio

### Properties

Allocation's (alloc) must sum to exactly one.

Prices must be GT 0.

Qty must be GTE 0.

Symbol must be non-null and unique.

Portfolio Size (portSize) must be GT 0.

### Sample

`sample_port.json`

```
{
  "size":"10000.0",
  "assets":[
  {"qty":"52",
   "symbol":"GOOG",
   "price":"98.0",
   "alloc":"0.6"},
  {"qty":"136",
   "symbol":"AAPL",
   "price":"22.0",
   "alloc":"0.3"},
  {"qty":"239",
   "symbol":"TSLA",
   "price":"8.0",
   "alloc":"0.1"}
  ]
}
```

## Rebalanced Portfolio

### Parameter's

Tracking Band (Float): Defined as alloc * (1 +/- band). This defines the space of valid rebalances. The entire space is searched with the portfolio that minimized the uninvested portion being selected. When multiple optimal solutions (rebalanced portfolio size == portfolio size) the first solution is returned. The default for the band is 0.05.

### Sample with tracking band

```
> python portreb.py --band 0.01 sample_port.json
BUY 8 shares of GOOG (target alloc: 0.6, actual: 0.588, error: 0.012)
BUY 4 shares of AAPL (target alloc: 0.3, actual: 0.308, error: 0.008)
SELL 109 shares of TSLA (target alloc: 0.1, actual: 0.104, error: 0.004)
Total Allocation: 10000.0
```
