"user strict";

// module Data.Date.Helpers

exports.nowImpl = function (ctor) {
  return function () {
    return ctor(new Date());
  };
};

exports.jsDateConstructor = function (x) {
  return new Date(x);
};

// jshint maxparams: 2
exports.jsDateMethod = function (method, date) {
  return date[method]();
};
