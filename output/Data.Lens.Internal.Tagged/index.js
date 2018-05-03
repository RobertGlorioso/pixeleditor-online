// Generated by purs version 0.11.5
"use strict";
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Newtype = require("../Data.Newtype");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Profunctor_Choice = require("../Data.Profunctor.Choice");
var Data_Profunctor_Closed = require("../Data.Profunctor.Closed");
var Data_Profunctor_Costrong = require("../Data.Profunctor.Costrong");
var Data_Tuple = require("../Data.Tuple");
var Tagged = function (x) {
    return x;
};
var taggedProfunctor = new Data_Profunctor.Profunctor(function (v) {
    return function (g) {
        return function (v1) {
            return g(v1);
        };
    };
});
var taggedCostrong = new Data_Profunctor_Costrong.Costrong(function () {
    return taggedProfunctor;
}, function (v) {
    return v.value0;
}, function (v) {
    return v.value1;
});
var taggedClosed = new Data_Profunctor_Closed.Closed(function () {
    return taggedProfunctor;
}, function (v) {
    return Data_Function["const"](v);
});
var taggedChoice = new Data_Profunctor_Choice.Choice(function () {
    return taggedProfunctor;
}, function (v) {
    return new Data_Either.Left(v);
}, function (v) {
    return new Data_Either.Right(v);
});
var newtypeTagged = new Data_Newtype.Newtype(function (n) {
    return n;
}, Tagged);
module.exports = {
    Tagged: Tagged, 
    newtypeTagged: newtypeTagged, 
    taggedProfunctor: taggedProfunctor, 
    taggedChoice: taggedChoice, 
    taggedCostrong: taggedCostrong, 
    taggedClosed: taggedClosed
};
