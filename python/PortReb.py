from __future__ import \
    absolute_import, division, generators, unicode_literals, print_function, nested_scopes, with_statement

__author__ = 'wjm'

from decimal import Decimal, InvalidOperation
from collections import namedtuple
from os import linesep

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

_asset = namedtuple('Asset', ['price', 'qty', 'alloc', 'symbol'])


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

_portfolio = namedtuple('Portfolio', ['size', 'assets'])

def validate_portfolio(assets, size):
    size = _validate_decimal(size, "size", 'Portfolio', bounds_check=lambda x: x > 0, bounds_check_msg='GT 0')




def _validate_decimal(value, field_name, obj_class, bounds_check=None, bounds_check_msg=None):
    """
    Validates that the given rational string is valid & falls within bounds.

    :type field_name: str
    :type value: str
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
