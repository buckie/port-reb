from __future__ import \
    absolute_import, division, generators, unicode_literals, print_function, nested_scopes, with_statement

from decimal import Decimal, InvalidOperation
from collections import namedtuple
from os import linesep
import json
from math import floor, ceil
from itertools import product


class DecimalValidationError(ValueError):
    pass


class InvalidAsset(BaseException):
    def __init__(self, errors):
        self.errors = errors

    def __str__(self):
        val = "Asset had the following errors:" + linesep
        for i in self.errors:
            val += str(i) + linesep
        return val


class InvalidPortfolio(BaseException):
    def __init__(self, errors, asset_errors):
        self.errors = errors
        self.asset_errors = asset_errors

    def __str__(self):
        val = "Portfolio had the following errors:" + linesep
        for i in self.errors:
            val += str(i) + linesep
        val += "And it's assets has the following errors:" + linesep
        for i in self.asset_errors:
            val += str(i) + linesep
        return val


# Assets and Portfolios are created (after validation) by these class instance generating functions.
mk_asset = namedtuple('Asset', ['price', 'qty', 'alloc', 'symbol'])
mk_portfolio = namedtuple('Portfolio', ['size', 'assets'])


def validate_asset(price, qty, alloc, symbol):
    """
    For a given set of attributes, attempt to validate and create an Asset. Asset's are an instance of the named tuple
    class and thus immutable.

    :param symbol: must be a non-null to be valid
    :param price: must be numeric or able to be parsed into a decimal > 0 to be valid
    :param qty: must be numeric or able to be parsed into a decimal >= 0 to be valid
    :param alloc: must be numeric or able to be parsed into a decimal <= 1 & >= 0 to be valid
    :return: Asset
    :raises: InvalidAsset([collected validation errors])

    >>> validate_asset("100", "100", "0.01", "AAPL")
    Asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL')
    >>> validate_asset("100", "100", "0.01", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("100", "100", "", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's alloc '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("100", "", "", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's alloc '' is not a valid rational number
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("", "", "", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '' is not a valid rational number
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("", "", "1.2", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '' is not a valid rational number
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("", "-100", "1.2", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '' is not a valid rational number
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '-100' must be an integer GTE 0
    The Asset's symbol is null: ''
    <BLANKLINE>

    >>> validate_asset("0", "-100", "1.2", "")
    Traceback (most recent call last):
        ...
    InvalidAsset: Asset had the following errors:
    Asset's price '0' must be GT 0
    Asset's alloc '1.2' must be bounded by [0,1]
    Asset's qty '-100' must be an integer GTE 0
    The Asset's symbol is null: ''
    <BLANKLINE>

    """

    # I want the ability to gather all the error's encountered by the creation of the asset,
    # not just the first. They get stashed here for later use
    errors = []

    # one by one, validate the fields
    try:
        _price = validate_decimal(price, 'price', 'Asset',
                                  bounds_check=lambda x: x > 0,
                                  bounds_check_msg='GT 0')
    except DecimalValidationError as e:
        errors.append(e)

    try:
        _alloc = validate_decimal(alloc, 'alloc', 'Asset',
                                  bounds_check=lambda x: 1 >= x >= 0,
                                  bounds_check_msg='bounded by [0,1]')
    except DecimalValidationError as e:
        errors.append(e)

    try:
        _qty = validate_decimal(qty, 'qty', 'Asset',
                                bounds_check=lambda x: x >= 0 and int(x) == x,
                                bounds_check_msg='an integer GTE 0')

    except DecimalValidationError as e:
        errors.append(e)

    if symbol:
        _symbol = symbol
    else:
        errors.append(DecimalValidationError("The Asset's symbol is null: '{}'".format(symbol)))

    if errors:
        raise InvalidAsset(errors)
    else:
        return mk_asset(_price, _qty, _alloc, _symbol)


def validate_portfolio(assets):
    """
    Validate that the collection of assets is valid. This method returns nothing, but raises if it is invalid
    :param assets: list of validated assets
    :return: nothing
    :raises InvalidPortfolio

    >>> assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.60'), symbol=u'GOOG'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
    >>> validate_portfolio(assets)
    >>> assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'AAPL'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.50'), symbol=u'GOOG'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
    >>> validate_portfolio(assets)
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Portfolio Allocations must sum to 1: 0.90
    And it's assets has the following errors:
    <BLANKLINE>
    >>> assets=[mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.01'), symbol=u'MSFT'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.09'), symbol=u'MSFT'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.60'), symbol=u'GOOG'),
    ...         mk_asset(price=Decimal('100'), qty=Decimal('100'), alloc=Decimal('0.30'), symbol=u'IBM')]
    >>> validate_portfolio(assets)
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Asset's must be unique
    And it's assets has the following errors:
    <BLANKLINE>
    """
    errors = []
    total_alloc = sum(map(lambda x: x.alloc, assets))
    if total_alloc != 1:
        errors.append(ValueError("Portfolio Allocations must sum to 1: {}".format(total_alloc)))

    if len(set(map(lambda x: x.symbol, assets))) != len(assets):
        errors.append(ValueError("Asset's must be unique"))

    if errors:
        raise InvalidPortfolio(errors, [])


def validate_decimal(value, field_name, obj_class, bounds_check=None, bounds_check_msg=None):
    """
    Validates that the given rational string is valid & falls within bounds.

    :type field_name: unicode
    :type value: unicode or str or float or int
    :type bounds_check: function :: rational -> bool
    :type bounds_check_msg: unicode
    :type obj_class: unicode
    :param value: string to be converted to rational
    :param field_name: name of the field associated with the value
    :param obj_class: string name for the object calling this function, used for error messages
    :param bounds_check: the function used for checking if the rational number falls within its valid bounds
    :param bounds_check_msg: the string used in the error message should the bounds_check function return false
    :return: Validated Rational Number
    :raises: DecimalValidationError


    >>> validate_decimal("0.1", "alloc", "Asset", bounds_check=lambda x: 0 >= x >= 1, bounds_check_msg="bounded by [0,1]")
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc '0.1' must be bounded by [0,1]
    >>> validate_decimal("0.1", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
    Decimal('0.1')
    >>> validate_decimal("1", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
    Decimal('1')
    >>> validate_decimal("1.001", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc '1.001' must be bounded by [0,1]
    >>> validate_decimal("foo", "alloc", "Asset", bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg="bounded by [0,1]")
    Traceback (most recent call last):
        ...
    DecimalValidationError: Asset's alloc 'foo' is not a valid rational number
    """
    try:
        x = Decimal(value)
    except InvalidOperation:
        # We only want to catch the validation errors, any other errors are bugs
        raise DecimalValidationError("{}'s {} '{}' is not a valid rational number".format(obj_class, field_name, value))

    if bounds_check is not None:
        if not bounds_check(x):
            raise DecimalValidationError("{}'s {} '{}' must be {}"
                                         .format(obj_class, field_name, value, bounds_check_msg))
    return x


def portfolio_from_json(json_str):
    """
    For a given JSON object of the structure:
    { 'assets': [asset],
      'size': size
    }

    Where asset:
        {'symbol': symbol,
        'price': price,
        'qty': qty,
        'alloc': alloc}

    :param json_str: json version of portfolio
    :return: Portfolio
    :raises: InvalidPortfolio

    >>> json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "10000.0"}'
    >>> portfolio_from_json(json_str)
    Portfolio(size=Decimal('10000.0'), assets=[Asset(price=Decimal('98.0'), qty=Decimal('52'), alloc=Decimal('0.6'), symbol=u'GOOG'), Asset(price=Decimal('22.0'), qty=Decimal('136'), alloc=Decimal('0.3'), symbol=u'AAPL'), Asset(price=Decimal('8.0'), qty=Decimal('239'), alloc=Decimal('0.1'), symbol=u'TSLA')])

    >>> json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "-10000.0"}'

    >>> portfolio_from_json(json_str)
    Traceback (most recent call last):
        ...
    InvalidPortfolio: Portfolio had the following errors:
    Portfolio's size '-10000.0' must be GT 0
    And it's assets has the following errors:
    <BLANKLINE>
    """
    # no try-except here as we want the error to escape (for the purpose of the exercise)
    data = json.loads(json_str)

    # collect the validated assets and any errors that are found
    assets = []
    asset_errors = []

    # collect the portfolio's errors
    port_errors = []

    try:
        for a in data["assets"]:
            try:
                assets.append(validate_asset(a["price"], a["qty"], a["alloc"], a["symbol"]))
            except InvalidAsset as e:
                asset_errors.append(e)
            except KeyError as e:
                # in case any of the asset's fields are not found. This will only catch the first instance of one missing
                asset_errors.append(InvalidAsset([e]))
    except KeyError as e:
        # in case "assets" are not found
        raise InvalidPortfolio(e, [])

    # in case this fails
    size = None
    try:
        size = validate_decimal(data['size'], "size", 'Portfolio', bounds_check=lambda x: x > 0,
                                bounds_check_msg='GT 0')
    except DecimalValidationError as e:
        port_errors.append(e)
    except KeyError as e:
        # in case "size" isn't found
        port_errors.append(e)

    try:
        validate_portfolio(assets)
    except InvalidPortfolio as e:
        # this is for future proofing as the validate_portfolio method doesn't currently return any asset errors
        asset_errors.append(e.asset_errors)
        # ... but it does have portfolio level errors
        port_errors.append(e.errors)

    if asset_errors or port_errors:
        # bail if we have errors anywhere
        raise InvalidPortfolio(port_errors, asset_errors)
    else:
        return mk_portfolio(size, assets)


def asset_rebalance_options(asset, size, band=Decimal(0.05)):
    """
    For a given asset, portfolio size and tracking band, return a list of all valid rebalances for that asset. As Qty
    must be an integer, the valid list consists of an enumeration of the qty's satifying the band.

    :param asset: Asset class (should be pre-validated)
    :param size: size of portfolio (should be pre-validated)
    :param band: tracking error band with the domain of [0,1]
    :return: [possible valid rebalances for a given asset -- the options that fall in the band
    """
    # the perfect, but unattainable qty (it's non integer)
    lbound = int(ceil(((asset.alloc * (1 - band)) * size) / asset.price))
    ubound = int(floor(((asset.alloc * (1 + band)) * size) / asset.price))

    return [mk_asset(asset.price, qty, (qty * asset.price) / size, asset.symbol)
            for qty in range(lbound, ubound + 1)]


def rebalance_portfolio(portfolio, band=Decimal(0.05)):
    """
    Rebalance the portfolio by finding the qty of each asset which falls into the given band that minimizes the
    unallocated size of the portfolio. This approach has a couple drawbacks. For one it enumerates the space. Further,
    it does not optimize for minimizing the tracking error -- it could, I just ran out of time.

    :param portfolio: Valid Portfolio
    :param band: the tracking band for each asset.
    :return: Rebalanced portfolio
    :raises: InvalidOperation("Portfolio cannot be rebalanced")


    >>> json_str = '{"assets": [{"alloc": "0.6", "symbol": "GOOG", "price": "98.0", "qty": 52}, {"alloc": "0.3", "symbol": "AAPL", "price": "22.0", "qty": 136}, {"alloc": "0.1", "symbol": "TSLA", "price": "8.0", "qty": 239}], "size": "10000.0"}'
    >>> port = portfolio_from_json(json_str)
    >>> rebalance_portfolio(port)
    Portfolio(size=Decimal('10000.0'), assets=(Asset(price=Decimal('98.0'), qty=60, alloc=Decimal('0.588'), symbol=u'GOOG'), Asset(price=Decimal('22.0'), qty=140, alloc=Decimal('0.308'), symbol=u'AAPL'), Asset(price=Decimal('8.0'), qty=130, alloc=Decimal('0.104'), symbol=u'TSLA')))

    """
    # makes a list of lists of all asset rebalances that fall within the band
    asset_options = [asset_rebalance_options(asset, portfolio.size, band=band) for asset in portfolio.assets]
    # makes the cartesian product of the possible rebalances -- yes this can be optimized
    port_universe = product(*asset_options)

    best_port = None
    for port in port_universe:
        total_alloc = sum(map(lambda x: x.alloc, port))
        total_size = sum(map(lambda x: x.qty * x.price, port))
        if total_alloc > 1:
            # We've allocated over 100%
            continue

        if best_port is not None:
            if best_port.size < total_size:
                # means this port is the new max
                best_port = mk_portfolio(total_size, port)
            else:
                continue
        else:
            # NB: we don't need to check that total_size is <= size as checking allocation handles this
            best_port = mk_portfolio(total_size, port)

    if best_port is None:
        raise InvalidOperation("This portfolio cannot be rebalanced!")
    else:
        return best_port


def diff_portfolios(old, new):
    orders = []
    for asset_old in old.assets:
        for asset_new in new.assets:
            if asset_old.symbol == asset_new.symbol:
                delta = asset_old.qty - asset_new.qty
                if delta > 0:
                    orders.append("SELL {} shares of {} (target alloc: {}, actual: {}, error: {})"
                                  "".format(abs(delta), asset_new.symbol, asset_old.alloc, asset_new.alloc,
                                            abs(asset_new.alloc - asset_old.alloc)))
                elif delta < 0:
                    orders.append("BUY {} shares of {} (target alloc: {}, actual: {}, error: {})"
                                  "".format(abs(delta), asset_new.symbol, asset_old.alloc, asset_new.alloc,
                                            abs(asset_new.alloc - asset_old.alloc)))
                break
    return orders


def main():
    import argparse
    parser = argparse.ArgumentParser(description='Portfolio Rebalancer')
    parser.add_argument('filename', action='store', help='file to read the current portfolio from')
    parser.add_argument('--band', action='store', help='tracking band with the domain of [0,1]. Defaults to 5%')
    parser.add_argument('--test', action='store_true', help='run test suite', default=False)
    args = parser.parse_args()
    if args.test:
        import doctest
        doctest.testmod(verbose=True)
    elif args.filename:
        if args.band:
            band = validate_decimal(args.band, 'Tracking Band', 'Command Line',
                                    bounds_check=lambda x: 1 >= x >= 0, bounds_check_msg='in the domain [0,1]')
        else:
            band = Decimal('0.05')

        with open(args.filename, mode='r') as f:
            # validate the json portfolio -- may blow up here but that's what we want
            port = portfolio_from_json(f.read())

            # attempt to rebalance the portfolio -- may again blow up here
            rebport = rebalance_portfolio(rebalance_portfolio(port, band=band))

            orders = diff_portfolios(port, rebport)
            print(linesep.join(orders))
            print("Total Allocation: {}".format(rebport.size))


if __name__ == "__main__":
    main()


