// Generated by purs version 0.11.5
"use strict";
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Blob = (function () {
    function Blob() {

    };
    Blob.value = new Blob();
    return Blob;
})();
var $$ArrayBuffer = (function () {
    function $$ArrayBuffer() {

    };
    $$ArrayBuffer.value = new $$ArrayBuffer();
    return $$ArrayBuffer;
})();
var toEnumBinaryType = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Blob.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just($$ArrayBuffer.value);
    };
    return Data_Maybe.Nothing.value;
};
var showBinaryType = new Data_Show.Show(function (v) {
    if (v instanceof Blob) {
        return "Blob";
    };
    if (v instanceof $$ArrayBuffer) {
        return "ArrayBuffer";
    };
    throw new Error("Failed pattern match at DOM.Websocket.BinaryType line 28, column 3 - line 29, column 3: " + [ v.constructor.name ]);
});
var printBinaryType = function (v) {
    if (v instanceof Blob) {
        return "blob";
    };
    if (v instanceof $$ArrayBuffer) {
        return "arraybuffer";
    };
    throw new Error("Failed pattern match at DOM.Websocket.BinaryType line 46, column 3 - line 48, column 20: " + [ v.constructor.name ]);
};
var fromEnumBinaryType = function (v) {
    if (v instanceof Blob) {
        return 0;
    };
    if (v instanceof $$ArrayBuffer) {
        return 1;
    };
    throw new Error("Failed pattern match at DOM.Websocket.BinaryType line 40, column 3 - line 44, column 1: " + [ v.constructor.name ]);
};
var eqBinaryType = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Blob && y instanceof Blob) {
            return true;
        };
        if (x instanceof $$ArrayBuffer && y instanceof $$ArrayBuffer) {
            return true;
        };
        return false;
    };
});
var ordBinaryType = new Data_Ord.Ord(function () {
    return eqBinaryType;
}, function (x) {
    return function (y) {
        if (x instanceof Blob && y instanceof Blob) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Blob) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Blob) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof $$ArrayBuffer && y instanceof $$ArrayBuffer) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at DOM.Websocket.BinaryType line 12, column 1 - line 12, column 48: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enumBinaryType = new Data_Enum.Enum(function () {
    return ordBinaryType;
}, Data_Enum.defaultPred(toEnumBinaryType)(fromEnumBinaryType), Data_Enum.defaultSucc(toEnumBinaryType)(fromEnumBinaryType));
var boundedBinaryType = new Data_Bounded.Bounded(function () {
    return ordBinaryType;
}, Blob.value, $$ArrayBuffer.value);
var boundedEnumBinaryType = new Data_Enum.BoundedEnum(function () {
    return boundedBinaryType;
}, function () {
    return enumBinaryType;
}, 2, fromEnumBinaryType, toEnumBinaryType);
module.exports = {
    Blob: Blob, 
    "ArrayBuffer": $$ArrayBuffer, 
    fromEnumBinaryType: fromEnumBinaryType, 
    printBinaryType: printBinaryType, 
    toEnumBinaryType: toEnumBinaryType, 
    eqBinaryType: eqBinaryType, 
    ordBinaryType: ordBinaryType, 
    boundedBinaryType: boundedBinaryType, 
    enumBinaryType: enumBinaryType, 
    boundedEnumBinaryType: boundedEnumBinaryType, 
    showBinaryType: showBinaryType
};
