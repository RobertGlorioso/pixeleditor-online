// Generated by purs version 0.11.5
"use strict";
var Data_Semigroup = require("../Data.Semigroup");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var SProxy = (function () {
    function SProxy() {

    };
    SProxy.value = new SProxy();
    return SProxy;
})();
var IsSymbol = function (reflectSymbol) {
    this.reflectSymbol = reflectSymbol;
};
var reifySymbol = function (s) {
    return function (f) {
        return Unsafe_Coerce.unsafeCoerce(function (dictIsSymbol) {
            return f(dictIsSymbol);
        })({
            reflectSymbol: function (v) {
                return s;
            }
        })(SProxy.value);
    };
};
var reflectSymbol = function (dict) {
    return dict.reflectSymbol;
};
var isSymbolTypeConcat = function (dictIsSymbol) {
    return function (dictIsSymbol1) {
        return new IsSymbol(function (v) {
            return reflectSymbol(dictIsSymbol)(SProxy.value) + reflectSymbol(dictIsSymbol1)(SProxy.value);
        });
    };
};
module.exports = {
    SProxy: SProxy, 
    IsSymbol: IsSymbol, 
    reflectSymbol: reflectSymbol, 
    reifySymbol: reifySymbol, 
    isSymbolTypeConcat: isSymbolTypeConcat
};
