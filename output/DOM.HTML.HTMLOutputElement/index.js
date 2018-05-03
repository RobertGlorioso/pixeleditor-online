// Generated by purs version 0.11.5
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var form = function ($0) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._form($0));
};
module.exports = {
    form: form, 
    checkValidity: $foreign.checkValidity, 
    defaultValue: $foreign.defaultValue, 
    labels: $foreign.labels, 
    name: $foreign.name, 
    setCustomValidity: $foreign.setCustomValidity, 
    setDefaultValue: $foreign.setDefaultValue, 
    setName: $foreign.setName, 
    setValue: $foreign.setValue, 
    type_: $foreign.type_, 
    validationMessage: $foreign.validationMessage, 
    validity: $foreign.validity, 
    value: $foreign.value, 
    willValidate: $foreign.willValidate
};
