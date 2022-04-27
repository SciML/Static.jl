function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(one),Type{StaticInt}})   # time: 0.39711076
    Base.precompile(Tuple{typeof(+),StaticInt{0},Float64})   # time: 0.08886288
    #Base.precompile(Tuple{Type{BigInt},StaticInt{1}})   # time: 0.0176033
    #Base.precompile(Tuple{typeof(fadd),StaticFloat64{-2.0},StaticFloat64{-10.0}})   # time: 0.01721382
    #Base.precompile(Tuple{typeof(/),StaticInt{10},StaticFloat64{-1.0}})   # time: 0.016930187
    #Base.precompile(Tuple{typeof(+),StaticInt{-1},StaticFloat64{6.0}})   # time: 0.015538697
    #Base.precompile(Tuple{Type{StaticSymbol},StaticSymbol{:x},StaticSymbol{:y}})   # time: 0.014889704
    #Base.precompile(Tuple{typeof(+),StaticInt{-4},StaticFloat64{-7.0}})   # time: 0.014473403
    #Base.precompile(Tuple{typeof(convert),Type{Float64},StaticFloat64{-1.1111111111111112}})   # time: 0.013155565
    #Base.precompile(Tuple{typeof(^),StaticFloat64{2.0},Float64})   # time: 0.010662053
    Base.precompile(Tuple{typeof(isless),NDIndex,NDIndex})   # time: 0.008023545
    #Base.precompile(Tuple{Type{NDIndex{3, I} where I<:Tuple{Union{Int64, StaticInt}, Union{Int64, StaticInt}, Union{Int64, StaticInt}}},Int64,Vararg{Any}})   # time: 0.007168689

    Base.precompile(Tuple{typeof(*),StaticInt,StaticFloat64})   # time: 0.003624196
    Base.precompile(Tuple{typeof(promote_rule),Type{Union{Missing, Nothing, Int64}},Type{<:StaticInt}})   # time: 0.003546621
    Base.precompile(Tuple{typeof(==),NDIndex,NDIndex})   # time: 0.002984272

    Base.precompile(Tuple{typeof(show),IOBuffer,NDIndex})   # time: 0.019357698
    Base.precompile(Tuple{typeof(show),IOBuffer,True})   # time: 0.003572621
    Base.precompile(Tuple{typeof(show),IOBuffer,StaticFloat64})   # time: 0.002958393
    Base.precompile(Tuple{typeof(show),IOBuffer,StaticInt})   # time: 0.002594136
    Base.precompile(Tuple{typeof(show),IOBuffer,StaticSymbol})   # time: 0.002391705

    #Base.precompile(Tuple{var"##s3#14",Any,Any,Any})   # time: 0.002793403
    Base.precompile(Tuple{typeof(/),StaticInt,StaticFloat64})   # time: 0.002654927

    Base.precompile(Tuple{typeof(promote_rule),Type{Union{Missing, Int64}},Type{<:StaticInt}})   # time: 0.001982201
    #Base.precompile(Tuple{Type{NDIndex{3, I} where I<:Tuple{Union{Int64, StaticInt}, Union{Int64, StaticInt}, Union{Int64, StaticInt}}},Tuple{Int64, StaticInt{2}}})   # time: 0.001934272
    #Base.precompile(Tuple{typeof(eachop),typeof(getindex),Tuple{StaticInt{3}, StaticInt{2}, StaticInt{1}},Tuple{StaticInt{1}, StaticInt{2}, StaticInt{3}}})   # time: 0.001778377
    Base.precompile(Tuple{typeof(*),StaticFloat64,StaticInt})   # time: 0.001707933
    Base.precompile(Tuple{Type{UnitRange{Int16}},StaticInt,Int64})   # time: 0.001681156
    #Base.precompile(Tuple{typeof(known),NDIndex{3, Tuple{StaticInt{3}, StaticInt{3}, StaticInt{3}}}})   # time: 0.001667353
    #Base.precompile(Tuple{typeof(Base.setindex),NDIndex{3, Tuple{Int64, Int64, Int64}},Int64,Int64})   # time: 0.001646679

    #Base.precompile(Tuple{typeof(is_static),Type{Tuple{StaticSymbol{:x}, StaticSymbol{:x}}}})   # time: 0.013371942
    Base.precompile(Tuple{typeof(is_static),Type{<:Tuple}})   # time: 0.001552308
    #Base.precompile(Tuple{typeof(is_static),Type{Tuple{StaticSymbol{:x}, Symbol}}})   # time: 0.001484531

    Base.precompile(Tuple{typeof(promote_rule),Type{Union{Nothing, Int64}},Type{<:StaticInt}})   # time: 0.001524335
    #Base.precompile(Tuple{Type{NDIndex},Tuple{NDIndex{3, Tuple{Int64, Int64, Int64}}, NDIndex{3, Tuple{Int64, StaticInt{2}, Int64}}}})   # time: 0.00148243
    #Base.precompile(Tuple{typeof(eachop),typeof(_tuple_static),Tuple{StaticInt{1}, StaticInt{2}},Type})   # time: 0.001358611
    Base.precompile(Tuple{typeof(==),NDIndex{3, Tuple{Int64, StaticInt{2}, Int64}},NDIndex{3, Tuple{Int64, StaticInt{2}, Int64}}})   # time: 0.001344325
    Base.precompile(Tuple{typeof(promote_rule),Type{Missing},Type{<:StaticInt}})   # time: 0.00123387
    #Base.precompile(Tuple{typeof(which(find_first_eq,(X,I<:Tuple{Vararg{Any, N}},)).generator.gen),Any,Any,Any,Any,Any,Any})   # time: 0.001225901
    Base.precompile(Tuple{typeof(-),Rational{Int64},StaticInt{0}})   # time: 0.001210711
    Base.precompile(Tuple{typeof(+),Float64,StaticInt{0}})   # time: 0.001203178
    Base.precompile(Tuple{typeof(+),Rational{Int64},StaticInt{0}})   # time: 0.001144629
    Base.precompile(Tuple{typeof(-),StaticInt{0},Rational{Int64}})   # time: 0.001136529
    Base.precompile(Tuple{typeof(-),StaticInt{0},Float64})   # time: 0.001081821
    #Base.precompile(Tuple{typeof(nstatic),Val{2}})   # time: 0.001079646
    Base.precompile(Tuple{typeof(permute),Tuple,Val})   # time: 0.001067344
    Base.precompile(Tuple{typeof(+),StaticInt{0},Rational{Int64}})   # time: 0.001055237
    Base.precompile(Tuple{typeof(zero),NDIndex})   # time: 0.001055035
    Base.precompile(Tuple{typeof(*),Float64,StaticInt{0}})   # time: 0.001045911
    Base.precompile(Tuple{typeof(+),Float64,StaticInt{1}})   # time: 0.001033542
    Base.precompile(Tuple{typeof(*),StaticInt{0},Float64})   # time: 0.001021948

    Base.precompile(Tuple{typeof(static),String})   # time: 0.003214999
    Base.precompile(Tuple{typeof(static),CartesianIndex})   # time: 0.002560627
    Base.precompile(Tuple{typeof(static),UInt64})   # time: 0.001017416
end
