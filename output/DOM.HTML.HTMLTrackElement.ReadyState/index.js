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
var NONE = (function () {
    function NONE() {

    };
    NONE.value = new NONE();
    return NONE;
})();
var LOADING = (function () {
    function LOADING() {

    };
    LOADING.value = new LOADING();
    return LOADING;
})();
var LOADED = (function () {
    function LOADED() {

    };
    LOADED.value = new LOADED();
    return LOADED;
})();
var ERROR = (function () {
    function ERROR() {

    };
    ERROR.value = new ERROR();
    return ERROR;
})();
var toEnumReadyState = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(NONE.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(LOADING.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(LOADED.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(ERROR.value);
    };
    return Data_Maybe.Nothing.value;
};
var showReadyState = new Data_Show.Show(function (v) {
    if (v instanceof NONE) {
        return "NONE";
    };
    if (v instanceof LOADING) {
        return "LOADING";
    };
    if (v instanceof LOADED) {
        return "LOADED";
    };
    if (v instanceof ERROR) {
        return "ERROR";
    };
    throw new Error("Failed pattern match at DOM.HTML.HTMLTrackElement.ReadyState line 30, column 3 - line 31, column 3: " + [ v.constructor.name ]);
});
var fromEnumReadyState = function (v) {
    if (v instanceof NONE) {
        return 0;
    };
    if (v instanceof LOADING) {
        return 1;
    };
    if (v instanceof LOADED) {
        return 2;
    };
    if (v instanceof ERROR) {
        return 3;
    };
    throw new Error("Failed pattern match at DOM.HTML.HTMLTrackElement.ReadyState line 46, column 3 - line 50, column 14: " + [ v.constructor.name ]);
};
var eqReadyState = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof NONE && y instanceof NONE) {
            return true;
        };
        if (x instanceof LOADING && y instanceof LOADING) {
            return true;
        };
        if (x instanceof LOADED && y instanceof LOADED) {
            return true;
        };
        if (x instanceof ERROR && y instanceof ERROR) {
            return true;
        };
        return false;
    };
});
var ordReadyState = new Data_Ord.Ord(function () {
    return eqReadyState;
}, function (x) {
    return function (y) {
        if (x instanceof NONE && y instanceof NONE) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof NONE) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof NONE) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof LOADING && y instanceof LOADING) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof LOADING) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof LOADING) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof LOADED && y instanceof LOADED) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof LOADED) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof LOADED) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ERROR && y instanceof ERROR) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at DOM.HTML.HTMLTrackElement.ReadyState line 14, column 1 - line 14, column 48: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enumReadyState = new Data_Enum.Enum(function () {
    return ordReadyState;
}, Data_Enum.defaultPred(toEnumReadyState)(fromEnumReadyState), Data_Enum.defaultSucc(toEnumReadyState)(fromEnumReadyState));
var boundedReadyState = new Data_Bounded.Bounded(function () {
    return ordReadyState;
}, NONE.value, ERROR.value);
var boundedEnumReadyState = new Data_Enum.BoundedEnum(function () {
    return boundedReadyState;
}, function () {
    return enumReadyState;
}, 4, fromEnumReadyState, toEnumReadyState);
module.exports = {
    NONE: NONE, 
    LOADING: LOADING, 
    LOADED: LOADED, 
    ERROR: ERROR, 
    fromEnumReadyState: fromEnumReadyState, 
    toEnumReadyState: toEnumReadyState, 
    eqReadyState: eqReadyState, 
    ordReadyState: ordReadyState, 
    boundedReadyState: boundedReadyState, 
    enumReadyState: enumReadyState, 
    boundedEnumReadyState: boundedEnumReadyState, 
    showReadyState: showReadyState
};
