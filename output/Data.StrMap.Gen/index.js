// Generated by purs version 0.11.5
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Gen = require("../Control.Monad.Gen");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var genStrMap = function (dictMonadRec) {
    return function (dictMonadGen) {
        return function (genKey) {
            return function (genValue) {
                return Control_Monad_Gen_Class.sized(dictMonadGen)(function (size) {
                    return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(size))(function (v) {
                        return Control_Monad_Gen_Class.resize(dictMonadGen)(Data_Function["const"](v))(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_StrMap.fromFoldable(Data_List_Types.foldableList))(Control_Monad_Gen.unfoldable(dictMonadRec)(dictMonadGen)(Data_List_Types.unfoldableList)(Control_Apply.apply(((dictMonadGen.Monad0()).Bind1()).Apply0())(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Tuple.Tuple.create)(genKey))(genValue))));
                    });
                });
            };
        };
    };
};
module.exports = {
    genStrMap: genStrMap
};