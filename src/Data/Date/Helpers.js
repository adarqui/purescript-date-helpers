"user strict";

// module Data.Date.Helpers

exports.jsDateMethod = function(method, date) {
    return date[method]();
};
