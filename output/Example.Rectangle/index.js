// Generated by purs version 0.11.5
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Gen = require("../Control.Monad.Gen");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var DOM = require("../DOM");
var Data_Array = require("../Data.Array");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic = require("../Data.Generic");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Graphics_Canvas = require("../Graphics.Canvas");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_DOM = require("../Signal.DOM");
var Signal_Time = require("../Signal.Time");
var Test_QuickCheck = require("../Test.QuickCheck");
var Test_QuickCheck_Gen = require("../Test.QuickCheck.Gen");
var Model$prime = function (x) {
    return x;
};
var yellow = "#FFFF00";
var white = "#FFFFFF";
var step = function (dictPartial) {
    return function (c) {
        return function (m) {
            var $34 = {};
            for (var $35 in m) {
                if ({}.hasOwnProperty.call(m, $35)) {
                    $34[$35] = m[$35];
                };
            };
            $34.mouse = c;
            return $34;
        };
    };
};
var square = function (size) {
    return function (x) {
        return function (y) {
            return {
                x: Data_Int.toNumber(size * x | 0), 
                y: Data_Int.toNumber(size * y | 0), 
                w: Data_Int.toNumber(size), 
                h: Data_Int.toNumber(size)
            };
        };
    };
};
var renderStep = function (dictPartial) {
    return function (mod) {
        return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
            var v = Graphics_Canvas.getCanvasElementById("canvas")();
            var __unused = function (dictPartial1) {
                return function ($dollar21) {
                    return $dollar21;
                };
            };
            return __unused(dictPartial)((function () {
                if (v instanceof Data_Maybe.Just) {
                    return function __do() {
                        var v1 = Graphics_Canvas.getContext2D(v.value0)();
                        Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle(white)(v1))();
                        return Graphics_Canvas.fillPath(v1)(Graphics_Canvas.rect(v1)({
                            x: 300.0, 
                            y: 300.0, 
                            w: 50.0, 
                            h: 50.0
                        }))();
                    };
                };
                throw new Error("Failed pattern match at Example.Rectangle line 73, column 3 - line 74, column 3: " + [ v.constructor.name ]);
            })())();
        });
    };
};
var red = "#FF0000";
var randomPoint = function (xmax) {
    return function (ymax) {
        return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(Control_Monad_Gen_Class.chooseInt(Test_QuickCheck_Gen.monadGenGen)(1)(xmax))(function (v) {
            return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(Control_Monad_Gen_Class.chooseInt(Test_QuickCheck_Gen.monadGenGen)(1)(ymax))(function (v1) {
                return Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(new Data_Tuple.Tuple(v, v1));
            });
        });
    };
};
var purple = "#800080";
var mouseColor = red;
var initStep = {
    xd: 1000, 
    yd: 1000, 
    size: 10, 
    mouse: {
        x: 0, 
        y: 0
    }, 
    path: [ new Data_Tuple.Tuple(1, 1) ], 
    dir: new Data_Tuple.Tuple(1, 0)
};
var init = function __do() {
    var v = Signal_DOM.windowDimensions();
    return Signal.flippedMap(Signal.functorSignal)(v)(function (d) {
        return {
            xd: d.w, 
            yd: d.h, 
            size: 10, 
            mouse: {
                x: 0, 
                y: 0
            }, 
            path: [ new Data_Tuple.Tuple(1, 1) ], 
            dir: new Data_Tuple.Tuple(1, 0)
        };
    });
};
var ifs = function ($copy_li) {
    return function ($copy_z) {
        var $tco_var_li = $copy_li;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(li, z) {
            var v = Data_Array.uncons(li);
            if (v instanceof Data_Maybe.Just) {
                if (v.value0.head.value0) {
                    $tco_done = true;
                    return v.value0.head.value1;
                };
                $tco_var_li = v.value0.tail;
                $copy_z = z;
                return;
            };
            if (v instanceof Data_Maybe.Nothing) {
                $tco_done = true;
                return z;
            };
            throw new Error("Failed pattern match at Example.Rectangle line 114, column 12 - line 116, column 34: " + [ v.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_li, $copy_z);
        };
        return $tco_result;
    };
};
var inputDir = (function () {
    var f = function (l) {
        return function (u) {
            return function (d) {
                return function (r) {
                    return ifs([ Data_Tuple.Tuple.create(l)(new Data_Tuple.Tuple(-1 | 0, 0)), Data_Tuple.Tuple.create(u)(new Data_Tuple.Tuple(0, -1 | 0)), Data_Tuple.Tuple.create(d)(new Data_Tuple.Tuple(0, 1)), Data_Tuple.Tuple.create(r)(new Data_Tuple.Tuple(1, 0)) ])(new Data_Tuple.Tuple(0, 0));
                };
            };
        };
    };
    return Control_Apply.apply(Control_Monad_Eff.applyEff)(Control_Apply.apply(Control_Monad_Eff.applyEff)(Control_Apply.apply(Control_Monad_Eff.applyEff)(Data_Functor.map(Control_Monad_Eff.functorEff)(Signal.map4(f))(Signal_DOM.keyPressed(37)))(Signal_DOM.keyPressed(38)))(Signal_DOM.keyPressed(40)))(Signal_DOM.keyPressed(39));
})();
var green = "#008000";
var wallColor = green;
var genericModel$prime = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Example.Rectangle.Model'" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Model$prime))((function (r) {
            if (r instanceof Data_Generic.SRecord && r.value0.length === 6) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (dir1) {
                    return function (mouse1) {
                        return function (path1) {
                            return function (size1) {
                                return function (xd1) {
                                    return function (yd1) {
                                        return {
                                            dir: dir1, 
                                            mouse: mouse1, 
                                            path: path1, 
                                            size: size1, 
                                            xd: xd1, 
                                            yd: yd1
                                        };
                                    };
                                };
                            };
                        };
                    };
                }))(Data_Generic.fromSpine(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt))(r["value0"][0].recValue(Data_Unit.unit))))((function (r1) {
                    if (r1 instanceof Data_Generic.SRecord && r1.value0.length === 2) {
                        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(function (x1) {
                            return function (y1) {
                                return {
                                    x: x1, 
                                    y: y1
                                };
                            };
                        }))(Data_Generic.fromSpine(Data_Generic.genericInt)(r1["value0"][0].recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)(r1["value0"][1].recValue(Data_Unit.unit)));
                    };
                    return Data_Maybe.Nothing.value;
                })(r["value0"][1].recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt)))(r["value0"][2].recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)(r["value0"][3].recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)(r["value0"][4].recValue(Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericInt)(r["value0"][5].recValue(Data_Unit.unit)));
            };
            return Data_Maybe.Nothing.value;
        })(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Example.Rectangle.Model'", [ {
        sigConstructor: "Example.Rectangle.Model'", 
        sigValues: [ function ($dollarq1) {
            return new Data_Generic.SigRecord([ {
                recLabel: "dir", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt))(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "mouse", 
                recValue: function ($dollarq2) {
                    return new Data_Generic.SigRecord([ {
                        recLabel: "x", 
                        recValue: function ($dollarq3) {
                            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                        }
                    }, {
                        recLabel: "y", 
                        recValue: function ($dollarq3) {
                            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                        }
                    } ]);
                }
            }, {
                recLabel: "path", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt)))(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "size", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "xd", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                }
            }, {
                recLabel: "yd", 
                recValue: function ($dollarq2) {
                    return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
                }
            } ]);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Example.Rectangle.Model'", [ function ($dollarq) {
        return new Data_Generic.SRecord([ {
            recLabel: "dir", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt))(v.dir);
            }
        }, {
            recLabel: "mouse", 
            recValue: function ($dollarq1) {
                return new Data_Generic.SRecord([ {
                    recLabel: "x", 
                    recValue: function ($dollarq2) {
                        return Data_Generic.toSpine(Data_Generic.genericInt)(v.mouse.x);
                    }
                }, {
                    recLabel: "y", 
                    recValue: function ($dollarq2) {
                        return Data_Generic.toSpine(Data_Generic.genericInt)(v.mouse.y);
                    }
                } ]);
            }
        }, {
            recLabel: "path", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericInt)(Data_Generic.genericInt)))(v.path);
            }
        }, {
            recLabel: "size", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericInt)(v.size);
            }
        }, {
            recLabel: "xd", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericInt)(v.xd);
            }
        }, {
            recLabel: "yd", 
            recValue: function ($dollarq1) {
                return Data_Generic.toSpine(Data_Generic.genericInt)(v.yd);
            }
        } ]);
    } ]);
});
var showModel = new Data_Show.Show(Data_Generic.gShow(genericModel$prime));
var fps = function (x) {
    return Signal_Time.every(Signal_Time.second / x);
};
var input = Data_Functor.map(Control_Monad_Eff.functorEff)(Signal.sampleOn(fps(20.0)))(Signal_DOM.mousePos);
var main$prime = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
    var v = Graphics_Canvas.getCanvasElementById("canvas")();
    var __unused = function (dictPartial1) {
        return function ($dollar25) {
            return $dollar25;
        };
    };
    return __unused()((function () {
        if (v instanceof Data_Maybe.Just) {
            return function __do() {
                var v1 = Graphics_Canvas.getContext2D(v.value0)();
                var v2 = init();
                var v3 = input();
                var v4 = Signal.foldp(step())(initStep)(v3);
                return Signal.runSignal(Data_Functor.map(Signal.functorSignal)(renderStep())(v4))();
            };
        };
        throw new Error("Failed pattern match at Example.Rectangle line 56, column 3 - line 57, column 3: " + [ v.constructor.name ]);
    })())();
});
var colorSquare = function (size) {
    return function (x) {
        return function (y) {
            return function (color) {
                return function (ctx) {
                    return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                        Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle(color)(ctx))();
                        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.fillPath(ctx)(Graphics_Canvas.rect(ctx)(square(size)(x)(y))))();
                    });
                };
            };
        };
    };
};
var blue = "#0000FF";
var black = "#000000";
var renderInit = function (dictPartial) {
    return function (mod) {
        return Data_Functor["void"](Control_Monad_Eff.functorEff)((function () {
            var canWidth = Data_Int.toNumber(mod.xd) * 0.75;
            var canHeight = Data_Int.toNumber(mod.yd) * 0.9;
            return function __do() {
                var v = Graphics_Canvas.getCanvasElementById("canvas")();
                var __unused = function (dictPartial1) {
                    return function ($dollar29) {
                        return $dollar29;
                    };
                };
                return __unused(dictPartial)((function () {
                    if (v instanceof Data_Maybe.Just) {
                        return function __do() {
                            var v1 = Graphics_Canvas.getContext2D(v.value0)();
                            var v2 = Graphics_Canvas.setCanvasHeight(canHeight)(v.value0)();
                            var v3 = Graphics_Canvas.setCanvasWidth(canWidth)(v.value0)();
                            Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle(black)(v1))();
                            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.fillPath(v1)(Graphics_Canvas.rect(v1)({
                                x: 0.0, 
                                y: 0.0, 
                                w: canWidth, 
                                h: canHeight
                            })))();
                        };
                    };
                    throw new Error("Failed pattern match at Example.Rectangle line 88, column 3 - line 89, column 3: " + [ v.constructor.name ]);
                })())();
            };
        })());
    };
};
var bindR = function (dictMonad) {
    return function (mx) {
        return function (my) {
            return Control_Bind.bind(dictMonad.Bind1())(mx)(Data_Function["const"](my));
        };
    };
};
var bgColor = black;
module.exports = {
    "Model'": Model$prime, 
    bgColor: bgColor, 
    bindR: bindR, 
    black: black, 
    blue: blue, 
    colorSquare: colorSquare, 
    fps: fps, 
    green: green, 
    ifs: ifs, 
    init: init, 
    initStep: initStep, 
    input: input, 
    inputDir: inputDir, 
    "main'": main$prime, 
    mouseColor: mouseColor, 
    purple: purple, 
    randomPoint: randomPoint, 
    red: red, 
    renderInit: renderInit, 
    renderStep: renderStep, 
    square: square, 
    step: step, 
    wallColor: wallColor, 
    white: white, 
    yellow: yellow, 
    "genericModel'": genericModel$prime, 
    showModel: showModel
};
