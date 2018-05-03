// Generated by purs version 0.11.5
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var DOM = require("../DOM");
var Data_Function = require("../Data.Function");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_Time = require("../Signal.Time");
var windowDimensions = $foreign.windowDimensionsP(Signal.constant);
var touch = $foreign.touchP(Signal.constant);
var tap = function __do() {
    var v = touch();
    return Signal.flippedMap(Signal.functorSignal)(v)(function (t) {
        if (t.length === 0) {
            return false;
        };
        return true;
    });
};
var mousePos = $foreign.mousePosP(Signal.constant);
var mouseButton = $foreign.mouseButtonP(Signal.constant);
var keyPressed = $foreign.keyPressedP(Signal.constant);
var animationFrame = $foreign.animationFrameP(Signal.constant)(Signal_Time.now);
module.exports = {
    animationFrame: animationFrame, 
    keyPressed: keyPressed, 
    mouseButton: mouseButton, 
    mousePos: mousePos, 
    tap: tap, 
    touch: touch, 
    windowDimensions: windowDimensions
};
