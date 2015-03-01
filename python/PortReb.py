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

    def __repr__(self):
        val = "Asset had the following errors:\n"
        for i in self.errors:
            val += repr(i) + linesep
        return val


class InvalidPortfolio(BaseException):
    def __init__(self, errors, asset_errors):
        self.errors = errors
        self.asset_errors = asset_errors

    def __repr__(self):
        val = "Portfolio had the following errors:" + linesep
        for i in self.errors:
            val += repr(i) + linesep
        val += "And it's assets has the following errors:" + linesep
        for i in self.asset_errors:
            val += repr(i) + linesep
        return val


# Assets and Portfolios are created (after validation) by these class instance generating functions.
_asset = namedtuple('Asset', ['price', 'qty', 'alloc', 'symbol'])
_portfolio = namedtuple('Portfolio', ['size', 'assets'])


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
    """

    # I want the ability to gather all the error's encountered by the creation of the asset,
    # not just the first. They get stashed here for later use
    errors = []

    # one by one, validate the fields
    try:
        _price = _validate_decimal(price, 'price', 'Asset',
                                   bounds_check=lambda x: x > 0,
                                   bounds_check_msg='GT 0')
    except DecimalValidationError as e:
        errors.append(e)

    try:
        _alloc = _validate_decimal(alloc, 'alloc', 'Asset',
                                   bounds_check=lambda x: 1 >= x >= 0,
                                   bounds_check_msg='bounded by [0,1]')
    except DecimalValidationError as e:
        errors.append(e)

    try:
        _qty = _validate_decimal(qty, 'qty', 'Asset',
                                 bounds_check=lambda x: x >= 0,
                                 bounds_check_msg='GTE 0')
    except DecimalValidationError as e:
        errors.append(e)

    if symbol:
        _symbol = symbol
    else:
        errors.append(DecimalValidationError("The Asset's symbol is null: '{}'".format(symbol)))

    if errors:
        raise InvalidAsset(errors)
    else:
        return _asset(_price, _qty, _alloc, _symbol)


def validate_portfolio(assets):
    """
    Validate that the collection of assets is valid. This method returns nothing, but raises if it is invalid
    :param assets: list of validated assets
    :return: nothing
    :raises InvalidPortfolio
    """
    errors = []
    total_alloc = sum(map(lambda x: x.alloc, assets))
    if total_alloc != 1:
        errors.append(ValueError("Portfolio Allocations must sum to 1"))

    if len(set(map(lambda x: x.symbol, assets))) != len(assets):
        errors.append(ValueError("Asset's must be unique"))

    if errors:
        raise InvalidPortfolio(errors, [])


def _validate_decimal(value, field_name, obj_class, bounds_check=None, bounds_check_msg=None):
    """
    Validates that the given rational string is valid & falls within bounds.

    :type field_name: unicode
    :type value: unicode or str or float or int
    :type bounds_check: function :: rational -> bool
    :type bounds_check_msg: str
    :type obj_class: str
    :param value: string to be converted to rational
    :param field_name: name of the field associated with the value
    :param obj_class: string name for the object calling this function, used for error messages
    :param bounds_check: the function used for checking if the rational number falls within its valid bounds
    :param bounds_check_msg: the string used in the error message should the bounds_check function return false
    :return: Validated Rational Number
    :raises: DecimalValidationError
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
        size = _validate_decimal(data['size'], "size", 'Portfolio', bounds_check=lambda x: x > 0,
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
        return _portfolio(size, assets)


def asset_rebalance_options(asset, size, band=Decimal(0.05)):
    # the perfect, but unattainable qty (it's non integer)
    base_qty = (asset.alloc * size) / asset.price
    lbound = int(ceil((1 - band) * base_qty))
    ubound = int(floor((1 - band) * base_qty))

    return [_asset(asset.price, qty, (qty * asset.price) / size, asset.symbol)
            for qty in range(lbound, ubound + 1)]


def rebalance_portfolio(portfolio, band=Decimal(0.05)):
    """
    Rebalance the portfolio by finding the qty of each asset which falls into the given band that minimizes the
    unallocated size of the portfolio

    :param portfolio: Valid Portfolio
    :param band: the tracking band for each asset.
    :return: Rebalanced portfolio
    :raises: InvalidOperation("Portfolio cannot be rebalanced")
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

        if best_port is None:
            if best_port.size < total_size:
                # means this port is the new max
                best_port = _portfolio(total_size, port)
            else:
                continue
        else:
            # NB: we don't need to check that total_size is <= size as checking allocation handles this
            best_port = _portfolio(total_size, port)

    if best_port is None:
        raise InvalidOperation("This portfolio cannot be rebalanced!")


