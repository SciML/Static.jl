module Static

import IfElse: ifelse
using Base: @propagate_inbounds, Slice

export static, StaticInt, StaticSymbol, True, False, StaticBool

@static if VERSION >= v"1.7.0-DEV.421"
    using Base: @aggressive_constprop
else
    macro aggressive_constprop(ex)
        ex
    end
end

include("static_implementation.jl")

end
