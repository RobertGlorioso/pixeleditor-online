// Generated by purs version 0.11.5
"use strict";
var Data_Const = require("../Data.Const");
var Data_Function = require("../Data.Function");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant");
var Data_Newtype = require("../Data.Newtype");
var Data_Profunctor = require("../Data.Profunctor");
var Optic_Types = require("../Optic.Types");
var Prelude = require("../Prelude");
var view = function (asa) {
    return function (s) {
        return Data_Newtype.unwrap(Data_Const.newtypeConst)(asa(Data_Const.Const)(s));
    };
};
var weiv = Data_Function.flip(view);
var to = function (dictContravariant) {
    return function (dictFunctor) {
        return function (dictProfunctor) {
            return function (s2a) {
                return Data_Profunctor.dimap(dictProfunctor)(s2a)(Data_Functor_Contravariant.coerce(dictContravariant)(dictFunctor));
            };
        };
    };
};
module.exports = {
    to: to, 
    view: view, 
    weiv: weiv
};
