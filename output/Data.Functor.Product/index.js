// Generated by purs version 0.11.5
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var Product = function (x) {
    return x;
};
var showProduct = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(product " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var product = function (fa) {
    return function (ga) {
        return new Data_Tuple.Tuple(fa, ga);
    };
};
var newtypeProduct = new Data_Newtype.Newtype(function (n) {
    return n;
}, Product);
var functorProduct = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Data_Functor.Functor(function (f) {
            return function (v) {
                return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(Data_Functor.map(dictFunctor)(f))(Data_Functor.map(dictFunctor1)(f))(v);
            };
        });
    };
};
var foldableProduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return new Data_Foldable.Foldable(function (dictMonoid) {
            return function (f) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value0))(Data_Foldable.foldMap(dictFoldable1)(dictMonoid)(f)(v.value1));
                };
            };
        }, function (f) {
            return function (z) {
                return function (v) {
                    return Data_Foldable.foldl(dictFoldable1)(f)(Data_Foldable.foldl(dictFoldable)(f)(z)(v.value0))(v.value1);
                };
            };
        }, function (f) {
            return function (z) {
                return function (v) {
                    return Data_Foldable.foldr(dictFoldable)(f)(Data_Foldable.foldr(dictFoldable1)(f)(z)(v.value1))(v.value0);
                };
            };
        });
    };
};
var traversableProduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return new Data_Traversable.Traversable(function () {
            return foldableProduct(dictTraversable.Foldable1())(dictTraversable1.Foldable1());
        }, function () {
            return functorProduct(dictTraversable.Functor0())(dictTraversable1.Functor0());
        }, function (dictApplicative) {
            return function (v) {
                return Control_Apply.lift2(dictApplicative.Apply0())(product)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v.value0))(Data_Traversable.sequence(dictTraversable1)(dictApplicative)(v.value1));
            };
        }, function (dictApplicative) {
            return function (f) {
                return function (v) {
                    return Control_Apply.lift2(dictApplicative.Apply0())(product)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)(v.value0))(Data_Traversable.traverse(dictTraversable1)(dictApplicative)(f)(v.value1));
                };
            };
        });
    };
};
var eq1Product = function (dictEq1) {
    return function (dictEq11) {
        return new Data_Eq.Eq1(function (dictEq) {
            return function (v) {
                return function (v1) {
                    return Data_Eq.eq1(dictEq1)(dictEq)(v.value0)(v1.value0) && Data_Eq.eq1(dictEq11)(dictEq)(v.value1)(v1.value1);
                };
            };
        });
    };
};
var eqProduct = function (dictEq1) {
    return function (dictEq11) {
        return function (dictEq) {
            return new Data_Eq.Eq(Data_Eq.eq1(eq1Product(dictEq1)(dictEq11))(dictEq));
        };
    };
};
var ord1Product = function (dictOrd1) {
    return function (dictOrd11) {
        return new Data_Ord.Ord1(function () {
            return eq1Product(dictOrd1.Eq10())(dictOrd11.Eq10());
        }, function (dictOrd) {
            return function (v) {
                return function (v1) {
                    var v2 = Data_Ord.compare1(dictOrd1)(dictOrd)(v.value0)(v1.value0);
                    if (v2 instanceof Data_Ordering.EQ) {
                        return Data_Ord.compare1(dictOrd11)(dictOrd)(v.value1)(v1.value1);
                    };
                    return v2;
                };
            };
        });
    };
};
var ordProduct = function (dictOrd1) {
    return function (dictOrd11) {
        return function (dictOrd) {
            return new Data_Ord.Ord(function () {
                return eqProduct(dictOrd1.Eq10())(dictOrd11.Eq10())(dictOrd.Eq0());
            }, Data_Ord.compare1(ord1Product(dictOrd1)(dictOrd11))(dictOrd));
        };
    };
};
var bihoistProduct = function (natF) {
    return function (natG) {
        return function (v) {
            return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(natF)(natG)(v);
        };
    };
};
var applyProduct = function (dictApply) {
    return function (dictApply1) {
        return new Control_Apply.Apply(function () {
            return functorProduct(dictApply.Functor0())(dictApply1.Functor0());
        }, function (v) {
            return function (v1) {
                return product(Control_Apply.apply(dictApply)(v.value0)(v1.value0))(Control_Apply.apply(dictApply1)(v.value1)(v1.value1));
            };
        });
    };
};
var bindProduct = function (dictBind) {
    return function (dictBind1) {
        return new Control_Bind.Bind(function () {
            return applyProduct(dictBind.Apply0())(dictBind1.Apply0());
        }, function (v) {
            return function (f) {
                return product(Control_Bind.bind(dictBind)(v.value0)(function ($103) {
                    return Data_Tuple.fst(Data_Newtype.unwrap(newtypeProduct)(f($103)));
                }))(Control_Bind.bind(dictBind1)(v.value1)(function ($104) {
                    return Data_Tuple.snd(Data_Newtype.unwrap(newtypeProduct)(f($104)));
                }));
            };
        });
    };
};
var applicativeProduct = function (dictApplicative) {
    return function (dictApplicative1) {
        return new Control_Applicative.Applicative(function () {
            return applyProduct(dictApplicative.Apply0())(dictApplicative1.Apply0());
        }, function (a) {
            return product(Control_Applicative.pure(dictApplicative)(a))(Control_Applicative.pure(dictApplicative1)(a));
        });
    };
};
var monadProduct = function (dictMonad) {
    return function (dictMonad1) {
        return new Control_Monad.Monad(function () {
            return applicativeProduct(dictMonad.Applicative0())(dictMonad1.Applicative0());
        }, function () {
            return bindProduct(dictMonad.Bind1())(dictMonad1.Bind1());
        });
    };
};
module.exports = {
    Product: Product, 
    bihoistProduct: bihoistProduct, 
    product: product, 
    newtypeProduct: newtypeProduct, 
    eqProduct: eqProduct, 
    eq1Product: eq1Product, 
    ordProduct: ordProduct, 
    ord1Product: ord1Product, 
    showProduct: showProduct, 
    functorProduct: functorProduct, 
    foldableProduct: foldableProduct, 
    traversableProduct: traversableProduct, 
    applyProduct: applyProduct, 
    applicativeProduct: applicativeProduct, 
    bindProduct: bindProduct, 
    monadProduct: monadProduct
};
