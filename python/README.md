# Portfolio Rebalancer


## Input Portfolio

### Properties

Allocation's (alloc) must sum to exactly one.

Prices must be GT 0.

Qty must be an integer GTE 0.

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

## Testing

sample output of `python portreb.py --test still_needs_a_file_name`

```
Trying:
    json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "10000.0"}'
Expecting nothing
ok
Trying:
    portfolio_from_json(json_str)
Expecting:
    Portfolio(size=Decimal('10000.0'), assets=[Asset(price=Decimal('98.0'), qty=Decimal('52'), alloc=Decimal('0.6'), symbol=u'GOOG'), Asset(price=Decimal('22.0'), qty=Decimal('136'), alloc=Decimal('0.3'), symbol=u'AAPL'), Asset(price=Decimal('8.0'), qty=Decimal('239'), alloc=Decimal('0.1'), symbol=u'TSLA')])
ok
Trying:
    json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "-10000.0"}'
Expecting nothing
ok
Trying:
    portfolio_from_json(json_str)
Expecting:
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Portfolio's size '-10000.0' must be GT 0
    And it's assets has the following errors:
    <BLANKLINE>
ok
Trying:
    json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "10000.0"}'
Expecting nothing
ok
Trying:
    port = portfolio_from_json(json_str)
Expecting nothing
ok
Trying:
    rebalance_portfolio(port)
Expecting:
    Portfolio(size=Decimal('10000.0'), assets=(Asset(price=Decimal('98.0'), qty=60, alloc=Decimal('0.588'), symbol=u'GOOG'), Asset(price=Decimal('22.0'), qty=140, alloc=Decimal('0.308'), symbol=u'AAPL'), Asset(price=Decimal('8.0'), qty=130, alloc=Decimal('0.104'), symbol=u'TSLA')))
ok
Trying:
    validate_asset("100", "100", "0.01", "AAPL")
Expecting:
    Asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL')
ok
Trying:
    validate_asset("100", "100", "0.01", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("100", "100", "", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's alloc '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("100", "", "", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's alloc '' is not a valid rational number
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("", "", "", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '' is not a valid rational number
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("", "", "1.2", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("", "-100", "1.2", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '-100' must be an integer GTE 0
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_asset("0", "-100", "1.2", "")
Expecting:
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '0' must be GT 0
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '-100' must be an integer GTE 0
    The Asset's symbol is null: ''
    <BLANKLINE>
ok
Trying:
    validate_decimal("0.1", "alloc", "Asset", bounds_check=lambda x: 0 >= x >= 1, bounds_check_msg="bounded by [0,1]")
Expecting:
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc '0.1' must be bounded by [0,1]
ok
Trying:
    validate_decimal("0.1", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
Expecting:
    Decimal('0.1')
ok
Trying:
    validate_decimal("1", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
Expecting:
    Decimal('1')
ok
Trying:
    validate_decimal("1.001", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
Expecting:
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc '1.001' must be bounded by [0,1]
ok
Trying:
    validate_decimal("foo", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
Expecting:
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc 'foo' is not a valid rational number
ok
Trying:
    assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.60'), symbol=u'GOOG'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
Expecting nothing
ok
Trying:
    validate_portfolio(assets)
Expecting nothing
ok
Trying:
    assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.50'), symbol=u'GOOG'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
Expecting nothing
ok
Trying:
    validate_portfolio(assets)
Expecting:
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Portfolio Allocations must sum to 1: 0.90
    And it's assets has the following errors:
    <BLANKLINE>
ok
Trying:
    assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'MSFT'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.60'), symbol=u'GOOG'),
            mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
Expecting nothing
ok
Trying:
    validate_portfolio(assets)
Expecting:
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Asset's must be unique
    And it's assets has the following errors:
    <BLANKLINE>
ok
21 items had no tests:
    __main__
    __main__.DecimalValidationError
    __main__.InvalidAsset
    __main__.InvalidAsset.__init__
    __main__.InvalidAsset.__str__
    __main__.InvalidPortfolio
    __main__.InvalidPortfolio.__init__
    __main__.InvalidPortfolio.__str__
    __main__.asset_rebalance_options
    __main__.diff_portfolios
    __main__.main
    __main__.mk_asset
    __main__.mk_asset.__dict__
    __main__.mk_asset.alloc
    __main__.mk_asset.price
    __main__.mk_asset.qty
    __main__.mk_asset.symbol
    __main__.mk_portfolio
    __main__.mk_portfolio.__dict__
    __main__.mk_portfolio.assets
    __main__.mk_portfolio.size
5 items passed all tests:
   4 tests in __main__.portfolio_from_json
   3 tests in __main__.rebalance_portfolio
   8 tests in __main__.validate_asset
   5 tests in __main__.validate_decimal
   6 tests in __main__.validate_portfolio
26 tests in 26 items.
26 passed and 0 failed.
Test passed.
```
