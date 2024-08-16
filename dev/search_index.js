var documenterSearchIndex = {"docs":
[{"location":"","page":"Home","title":"Home","text":"CurrentModule = Static","category":"page"},{"location":"#Static","page":"Home","title":"Static","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"","category":"page"},{"location":"","page":"Home","title":"Home","text":"Modules = [Static]","category":"page"},{"location":"#Static.IntType","page":"Home","title":"Static.IntType","text":"IntType(x::Integer) -> Union{Int,StaticInt}\n\nIntType is a union of Int and StaticInt. As a function, it ensures that x one of the two.\n\n\n\n\n\n","category":"type"},{"location":"#Static.NDIndex","page":"Home","title":"Static.NDIndex","text":"NDIndex(i, j, k...)   -> I\nNDIndex((i, j, k...)) -> I\n\nA multidimensional index that refers to a single element. Each dimension is represented by a single Int or StaticInt.\n\njulia> using Static\n\njulia> i = NDIndex(static(1), 2, static(3))\nNDIndex(static(1), 2, static(3))\n\njulia> i[static(1)]\nstatic(1)\n\njulia> i[1]\n1\n\n\n\n\n\n\n","category":"type"},{"location":"#Static.OptionallyStaticStepRange","page":"Home","title":"Static.OptionallyStaticStepRange","text":"OptionallyStaticStepRange(start, step, stop) <: OrdinalRange{Int,Int}\n\nSimilarly to OptionallyStaticUnitRange, OptionallyStaticStepRange permits a combination of static and standard primitive Ints to construct a range. It specifically enables the use of ranges without a step size of 1. It may be constructed through the use of OptionallyStaticStepRange directly or using static integers with the range operator (i.e., :).\n\njulia> using Static\n\njulia> x = static(2);\n\njulia> x:x:10\nstatic(2):static(2):10\n\njulia> Static.OptionallyStaticStepRange(x, x, 10)\nstatic(2):static(2):10\n\n\n\n\n\n\n","category":"type"},{"location":"#Static.OptionallyStaticUnitRange","page":"Home","title":"Static.OptionallyStaticUnitRange","text":"OptionallyStaticUnitRange(start, stop) <: AbstractUnitRange{Int}\n\nSimilar to UnitRange except each field may be an Int or StaticInt. An OptionallyStaticUnitRange is intended to be constructed internally from other valid indices. Therefore, users should not expect the same checks are used to ensure construction of a valid OptionallyStaticUnitRange as a UnitRange.\n\n\n\n\n\n","category":"type"},{"location":"#Static.SOneTo","page":"Home","title":"Static.SOneTo","text":"SOneTo(n::Int)\n\nAn alias for OptionallyStaticUnitRange usfeul for statically sized axes.\n\n\n\n\n\n","category":"type"},{"location":"#Static.SUnitRange","page":"Home","title":"Static.SUnitRange","text":"SUnitRange(start::Int, stop::Int)\n\nAn alias for OptionallyStaticUnitRange where both the start and stop are known statically.\n\n\n\n\n\n","category":"type"},{"location":"#Static.StaticBool","page":"Home","title":"Static.StaticBool","text":"StaticBool(x::Bool) -> True/False\n\nA statically typed Bool.\n\n\n\n\n\n","category":"type"},{"location":"#Static.StaticFloat64","page":"Home","title":"Static.StaticFloat64","text":"StaticFloat64{N}\n\nA statically sized Float64. Use StaticFloat64(N) instead of Val(N) when you want it to behave like a number.\n\n\n\n\n\n","category":"type"},{"location":"#Static.StaticInt","page":"Home","title":"Static.StaticInt","text":"StaticInt(N::Int) -> StaticInt{N}()\n\nA statically sized Int. Use StaticInt(N) instead of Val(N) when you want it to behave like a number.\n\n\n\n\n\n","category":"type"},{"location":"#Static.StaticSymbol","page":"Home","title":"Static.StaticSymbol","text":"StaticSymbol\n\nA statically typed Symbol.\n\n\n\n\n\n","category":"type"},{"location":"#Static.add-Tuple{Any}","page":"Home","title":"Static.add","text":"add(x) -> Base.Fix2(+, x)\nadd(x, y) ->\n\nEquivalent to + but allows for lazy addition when passing functions.\n\n\n\n\n\n","category":"method"},{"location":"#Static.dynamic-Tuple{StaticInt}","page":"Home","title":"Static.dynamic","text":"dynamic(x)\n\nReturns the \"dynamic\" or non-static form of x. If x is not a static type, then it is returned unchanged.\n\ndynamic ensures that the type of the returned value is always inferred, even if the compiler fails to infer the exact value.\n\nSee also: known\n\nExamples\n\njulia> dynamic(static(1))\n1\n\njulia> dynamic(1)\n1\n\n\n\n\n\n\n","category":"method"},{"location":"#Static.eachop-Union{Tuple{T}, Tuple{F}, Tuple{F, Tuple{T, Vararg{Any}}, Vararg{Any}}} where {F, T}","page":"Home","title":"Static.eachop","text":"eachop(op, args...; iterator::Tuple{Vararg{StaticInt}}) -> Tuple\n\nProduces a tuple of (op(args..., iterator[1]), op(args..., iterator[2]),...).\n\n\n\n\n\n","category":"method"},{"location":"#Static.eachop_tuple-Tuple{Any, Any, Any, Vararg{Any}}","page":"Home","title":"Static.eachop_tuple","text":"eachop_tuple(op, arg, args...; iterator::Tuple{Vararg{StaticInt}}) -> Type{Tuple}\n\nProduces a tuple type of Tuple{op(arg, args..., iterator[1]), op(arg, args..., iterator[2]),...}. Note that if one of the arguments passed to op is a Tuple type then it should be the first argument instead of one of the trailing arguments, ensuring type inference of each element of the tuple.\n\n\n\n\n\n","category":"method"},{"location":"#Static.eq-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.eq","text":"eq(x, y)\n\nEquivalent to != but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.field_type-Tuple{Type, Union{Int64, Symbol}}","page":"Home","title":"Static.field_type","text":"field_type(::Type{T}, f)\n\nFunctionally equivalent to fieldtype(T, f) except f may be a static type.\n\n\n\n\n\n","category":"method"},{"location":"#Static.ge-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.ge","text":"ge(x, y)\n\nEquivalent to >= but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.gt-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.gt","text":"gt(x, y)\n\nEquivalent to > but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.is_static-Tuple{Any}","page":"Home","title":"Static.is_static","text":"is_static(::Type{T}) -> StaticBool\n\nReturns True if T is a static type.\n\nSee also: static, known\n\n\n\n\n\n","category":"method"},{"location":"#Static.known-Tuple{Any}","page":"Home","title":"Static.known","text":"known(T::Type)\n\nReturns the known value corresponding to a static type T. If T is not a static type then nothing is returned.\n\nknown ensures that the type of the returned value is always inferred, even if the compiler fails to infer the exact value.\n\nSee also: static, is_static, dynamic\n\nExamples\n\njulia> known(StaticInt{1})\n1\n\njulia> known(Int)\n\n\n\n\n\n\n","category":"method"},{"location":"#Static.le-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.le","text":"le(x, y)\n\nEquivalent to <= but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.lt-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.lt","text":"lt(x, y)\n\nEquivalent to < but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.mul-Tuple{Any}","page":"Home","title":"Static.mul","text":"mul(x) -> Base.Fix2(*, x)\nmul(x, y) ->\n\nEquivalent to * but allows for lazy multiplication when passing functions.\n\n\n\n\n\n","category":"method"},{"location":"#Static.ne-Union{Tuple{Y}, Tuple{X}, Tuple{X, Y}} where {X, Y}","page":"Home","title":"Static.ne","text":"ne(x, y)\n\nEquivalent to != but if x and y are both static returns a `StaticBool.\n\n\n\n\n\n","category":"method"},{"location":"#Static.reduce_tup-Union{Tuple{N}, Tuple{F}, Tuple{F, Tuple{Vararg{Any, N}}}} where {F, N}","page":"Home","title":"Static.reduce_tup","text":"reduce_tup(f::F, inds::Tuple{Vararg{Any,N}}) where {F,N}\n\nAn optimized reduce for tuples. Base.reduce's afoldl will often not inline. Additionally, reduce_tup attempts to order the reduction in an optimal manner.\n\njulia> using StaticArrays, Static, BenchmarkTools\n\njulia> rsum(v::SVector) = Static.reduce_tup(+, v.data)\nrsum (generic function with 2 methods)\n\njulia> for n ∈ 2:16\n           @show n\n           v = @SVector rand(n)\n           s1 = @btime  sum($(Ref(v))[])\n           s2 = @btime rsum($(Ref(v))[])\n       end\nn = 2\n  0.863 ns (0 allocations: 0 bytes)\n  0.863 ns (0 allocations: 0 bytes)\nn = 3\n  0.862 ns (0 allocations: 0 bytes)\n  0.863 ns (0 allocations: 0 bytes)\nn = 4\n  0.862 ns (0 allocations: 0 bytes)\n  0.862 ns (0 allocations: 0 bytes)\nn = 5\n  1.074 ns (0 allocations: 0 bytes)\n  0.864 ns (0 allocations: 0 bytes)\nn = 6\n  0.864 ns (0 allocations: 0 bytes)\n  0.862 ns (0 allocations: 0 bytes)\nn = 7\n  1.075 ns (0 allocations: 0 bytes)\n  0.864 ns (0 allocations: 0 bytes)\nn = 8\n  1.077 ns (0 allocations: 0 bytes)\n  0.865 ns (0 allocations: 0 bytes)\nn = 9\n  1.081 ns (0 allocations: 0 bytes)\n  0.865 ns (0 allocations: 0 bytes)\nn = 10\n  1.195 ns (0 allocations: 0 bytes)\n  0.867 ns (0 allocations: 0 bytes)\nn = 11\n  1.357 ns (0 allocations: 0 bytes)\n  1.400 ns (0 allocations: 0 bytes)\nn = 12\n  1.543 ns (0 allocations: 0 bytes)\n  1.074 ns (0 allocations: 0 bytes)\nn = 13\n  1.702 ns (0 allocations: 0 bytes)\n  1.077 ns (0 allocations: 0 bytes)\nn = 14\n  1.913 ns (0 allocations: 0 bytes)\n  0.867 ns (0 allocations: 0 bytes)\nn = 15\n  2.076 ns (0 allocations: 0 bytes)\n  1.077 ns (0 allocations: 0 bytes)\nn = 16\n  2.273 ns (0 allocations: 0 bytes)\n  1.078 ns (0 allocations: 0 bytes)\n\nMore importantly, reduce_tup(_pick_range, inds) often performs better than reduce(_pick_range, inds).\n\njulia> using ArrayInterface, BenchmarkTools, Static\n\njulia> inds = (Base.OneTo(100), 1:100, 1:static(100))\n(Base.OneTo(100), 1:100, 1:static(100))\n\njulia> @btime reduce(ArrayInterface._pick_range, $(Ref(inds))[])\n  6.405 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):static(100))\n\njulia> @btime Static.reduce_tup(ArrayInterface._pick_range, $(Ref(inds))[])\n  2.570 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):static(100))\n\njulia> inds = (Base.OneTo(100), 1:100, 1:UInt(100))\n(Base.OneTo(100), 1:100, 0x0000000000000001:0x0000000000000064)\n\njulia> @btime reduce(ArrayInterface._pick_range, $(Ref(inds))[])\n  6.411 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):100)\n\njulia> @btime Static.reduce_tup(ArrayInterface._pick_range, $(Ref(inds))[])\n  2.592 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):100)\n\njulia> inds = (Base.OneTo(100), 1:100, 1:UInt(100), Int32(1):Int32(100))\n(Base.OneTo(100), 1:100, 0x0000000000000001:0x0000000000000064, 1:100)\n\njulia> @btime reduce(ArrayInterface._pick_range, $(Ref(inds))[])\n  9.048 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):100)\n\njulia> @btime Static.reduce_tup(ArrayInterface._pick_range, $(Ref(inds))[])\n  2.569 ns (0 allocations: 0 bytes)\nBase.Slice(static(1):100)\n\n\n\n\n\n","category":"method"},{"location":"#Static.static-Tuple{Union{StaticBool, StaticFloat64, StaticInt, StaticSymbol}}","page":"Home","title":"Static.static","text":"static(x)\n\nReturns a static form of x. If x is already in a static form then x is returned. If there is no static alternative for x then an error is thrown.\n\nSee also: is_static, known\n\nExamples\n\njulia> using Static\n\njulia> static(1)\nstatic(1)\n\njulia> static(true)\nTrue()\n\njulia> static(:x)\nstatic(:x)\n\n\n\n\n\n\n","category":"method"},{"location":"#Static.static_promote-Tuple{AbstractUnitRange{<:Integer}, AbstractUnitRange{<:Integer}}","page":"Home","title":"Static.static_promote","text":"static_promote(x::AbstractRange{<:Integer}, y::AbstractRange{<:Integer})\n\nA type stable method for combining two equal ranges into a new range that preserves static parameters. Throws an error if x != y.\n\nExamples\n\njulia> static_promote(static(1):10, 1:static(10))\nstatic(1):static(10)\n\njulia> static_promote(1:2:9, static(1):static(2):static(9))\nstatic(1):static(2):static(9)\n\n\n\n\n\n","category":"method"},{"location":"#Static.static_promote-Union{Tuple{X}, Tuple{Union{StaticBool{X}, StaticFloat64{X}, StaticInt{X}, StaticSymbol{X}}, Union{StaticBool{X}, StaticFloat64{X}, StaticInt{X}, StaticSymbol{X}}}} where X","page":"Home","title":"Static.static_promote","text":"static_promote(x, y)\n\nThrows an error if x and y are not equal, preferentially returning the one that is known at compile time.\n\n\n\n\n\n","category":"method"}]
}
